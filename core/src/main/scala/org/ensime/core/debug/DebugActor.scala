package org.ensime.core.debug

import java.io.File

import akka.actor.{Actor, ActorLogging, ActorRef}
import akka.event.LoggingReceive
import org.ensime.api._
import org.scaladebugger.api.lowlevel.breakpoints.{BreakpointRequestInfo, PendingBreakpointSupportLike}
import org.scaladebugger.api.profiles.traits.info.{IndexedVariableInfoProfile, ObjectInfoProfile, ThreadInfoProfile}
import org.scaladebugger.api.virtualmachines.ScalaVirtualMachine

import scala.util.{Failure, Success, Try}

/**
 * Represents the main entrypoint into the debugging interface of Ensime.
 *
 * @param broadcaster The actor used to send messages that should be broadcasted
 *                    to all listening clients
 * @param config The Ensime-specific configuration to associate with the
 *               debugger
 */
class DebugActor(
  private val broadcaster: ActorRef,
  private val config: EnsimeConfig
) extends Actor with ActorLogging {
  private val vmm: VirtualMachineManager = new VirtualMachineManager()

  /**
   * Receives user-based events and processes them.
   *
   * @return The response to the user event
   */
  override def receive: Receive = LoggingReceive {
    // ========================================================================
    case DebugStartReq(commandLine: String) =>
      vmm.stop()

      // Include Ensime's runtime classpath for the launched JVM and
      val options = Seq(
        "-classpath",
        config.runtimeClasspath.mkString("\"", File.pathSeparator, "\"")
      ) ++ config.debugVMArgs

      vmm.start(VmStart(commandLine), options)

      sender ! DebugVmSuccess()
    // ========================================================================
    case DebugAttachReq(hostname, port) =>
      vmm.stop()

      // Include Ensime's runtime classpath for the launched JVM and
      val options = Seq(
        "-classpath",
        config.runtimeClasspath.mkString("\"", File.pathSeparator, "\"")
      ) ++ config.debugVMArgs

      vmm.start(VmAttach(hostname, port), options)

      sender ! DebugVmSuccess()
    // ========================================================================
    case DebugActiveVmReq =>
      // Send a true response if the VM is still available, otherwise false
      sender ! withVM(_ => TrueResponse)
    // ========================================================================
    case DebugStopReq =>
      sender ! withVM(s => {
        // Force JVM exit if mode indicates to do so
        if (vmm.activeMode.nonEmpty && vmm.activeMode.get.shouldExit)
          s.underlyingVirtualMachine.exit(0)

        vmm.stop()
        TrueResponse
      })
    // ========================================================================
    case DebugRunReq =>
      sender ! withVM(s => {
        s.underlyingVirtualMachine.resume()
        TrueResponse
      })
    // ========================================================================
    case DebugContinueReq(threadId) =>
      sender ! withThread(threadId.id, { case (s, t) =>
        // TODO: Why is this resuming the entire VM instead of the single thread?
        s.underlyingVirtualMachine.resume()
        TrueResponse
      })
    // ========================================================================
    case DebugSetBreakReq(file, line: Int) =>
      sender ! withVM(s => {
        fullFileName(s, file.getName) match {
          case Some(fileName) =>
            // TODO: Figure out why location examining needed to compare the
            //       sourcePath, sourceName, and lineNumber for each location
            //       pulled from the file's reference types AND its methods, why
            //       do we need to look up the line that way?
            // NOTE: If Scala Debugger API needs to change to match the old
            //       functionality, just need to change the low-level class
            //       manager's linesAndLocationsForFile method.
            s.tryGetOrCreateBreakpointRequest(fileName, line) match {
              case Success(bp) =>
                val isPending = s.isBreakpointRequestPending(fileName, line)

                if (!isPending) {
                  bgMessage(s"Resolved breakpoint at: $fileName : $line")
                } else {
                  bgMessage("Location not loaded. Set pending breakpoint.")
                }

                TrueResponse
              case Failure(ex) =>
                FalseResponse
            }
          case None => FalseResponse
        }
      })
    // ========================================================================
    case DebugClearBreakReq(file, line: Int) =>
      vmm.withVM(s => {
        fullFileName(s, file.getName)
          .foreach(s.removeBreakpointRequests(_, line))
      })

      // Always send true response
      sender ! TrueResponse

    // ========================================================================
    case DebugClearAllBreaksReq =>
      vmm.withVM(_.removeAllBreakpointRequests())
      sender ! TrueResponse

    // ========================================================================
    case DebugListBreakpointsReq =>
      val (activeBreakpoints, pendingBreakpoints) = vmm.withVM(s => {
        val bps = s.breakpointRequests

        (bps.filterNot(_.isPending), bps.filter(_.isPending))
      }).map { case (a, p) =>
        (a, p)
      }.getOrElse((Nil, Nil))

      sender ! BreakpointList(activeBreakpoints, pendingBreakpoints)
    // ========================================================================
    case DebugNextReq(threadId: DebugThreadId) =>
      sender ! withThread(threadId.id, { case (s, t) =>
        s.stepOverLine(t)
        TrueResponse
      })
    // ========================================================================
    case DebugStepReq(threadId: DebugThreadId) =>
      sender ! withThread(threadId.id, { case (s, t) =>
        s.stepIntoLine(t)
        TrueResponse
      })
    // ========================================================================
    case DebugStepOutReq(threadId: DebugThreadId) =>
      sender ! withThread(threadId.id, { case (s, t) =>
        s.stepOutLine(t)
        TrueResponse
      })
    // ========================================================================
    case DebugLocateNameReq(threadId: DebugThreadId, name: String) =>
      sender ! withThread(threadId.id, { case (s, t) =>
        if (name == "this") {
          // TODO: Cache retrieved object
          t.tryGetTopFrame.flatMap(_.tryGetThisObject).map {
            case objectReference =>
              DebugObjectReference(objectReference.uniqueId)
          }.getOrElse(FalseResponse)
        } else {
          // TODO: Cache retrieved object
          t.findVariableByName(name).flatMap {
            case v: IndexedVariableInfoProfile  =>
              Some(DebugStackSlot(DebugThreadId(t.uniqueId), v.frameIndex, v.offsetIndex))
            case v if v.isField                 => v.toValue match {
              case o: ObjectInfoProfile =>
                Some(DebugObjectField(DebugObjectId(o.uniqueId), v.name))
              case _                    =>
                None
            }
          }.getOrElse(FalseResponse)
        }
      })
    // ========================================================================
    case DebugBacktraceReq(threadId: DebugThreadId, index: Int, count: Int) =>
      sender ! withThread(threadId.id, { case (s, t) =>
        // NOTE: -1 allows retrieving all frames starting at index going
        //       backwards (3, 4, 5, ...)

        // NOTE: Makes DebugStackFrame instances out of frames

        // TODO: This object is cached for each stack frame
        DebugBacktrace(frames.toList, DebugThreadId(t.uniqueId), t.name)
      })
    // ========================================================================
    case DebugValueReq(location) =>
      // TODO: Handles different cases
      //
      //    1. DebugObjectReference => valueForId
      //    2. DebugObjectField => valueForField
      //    3. DebugArrayElement => valueForIndex
      //    4. DebugStackSlot => looks up thread by id, then valueForStackVar
      //
      sender ! withVM(s => (location match {
        case DebugObjectReference(objectId) =>
          None // Retrieves cached object
        case DebugObjectField(objectId, fieldName) =>
          // Uses cached object with id to find associated field
          None // Caches retrieved field object
        case DebugArrayElement(objectId, index) =>
          // Uses cached object with id as array to find element
          None // Caches retrieved element object
        case DebugStackSlot(threadId, frame, offset) =>
          None // Caches retrieved slot object
        case _ =>
          None
          // TODO: Exception is cached when an exception event is received
          s.valueAtLocation(location).getOrElse(FalseResponse)
      }).getOrElse(FalseResponse))
    // ========================================================================
    case DebugToStringReq(threadId, location) =>
      sender ! withVM(s => (location match {
        case DebugObjectReference(objectId) =>
          None // Retrieves cached object
        case DebugObjectField(objectId, fieldName) =>
          // Uses cached object with id to find associated field
          None // Caches retrieved field object
        case DebugArrayElement(objectId, index) =>
          // Uses cached object with id as array to find element
          None // Caches retrieved element object
        case DebugStackSlot(_, frame, offset) =>
          None // Caches retrieved slot object
        case _ =>
          None
          // TODO: Exception is cached when an exception event is received
          s.valueAtLocation(location)
            .map(_.toPrettyString)
            .map(StringResponse(_))
            .getOrElse(FalseResponse)
      }).getOrElse(FalseResponse))
    // ========================================================================
    case DebugSetValueReq(location, newValue) =>
      sender ! withVM(s => {
        location match {
          case DebugStackSlot(threadId, frame, offset) => s.tryGetThread(threadId.id) match {
            case Success(t) =>
              // TODO: Set value needs to perform some sort of casting OR
              //       we do that earlier to provide the casting for setValue
              //       based on the desired type (value type) since the new
              //       value always comes in as a string
              val actualNewValue = newValue ---

              val result = t.tryGetFrame(frame)
                .map(_.getLocalVariables)
                .flatMap(v => Try(v(offset)))
                .flatMap(_.trySetValue(actualNewValue))

              if (result.isSuccess) TrueResponse else FalseResponse
            case Failure(_) =>
              log.error(s"Unknown thread $threadId for debug-set-value")
              FalseResponse
          }
          case unknown =>
            log.error(s"Unsupported location type for debug-set-value.: $unknown")
            FalseResponse
        }
      })
  }

  // ==========================================================================

  /**
   * Sends a background message through the broadcaster.
   *
   * @param msg The message content to send as a background message
   */
  private def bgMessage(msg: String): Unit = {
    broadcaster ! SendBackgroundMessageEvent(msg)
  }

  /**
   * Attempts to invoke the provided action against the active VM.
   *
   * @param action The action to execute against the virtual machine
   * @tparam T The type of RpcResponse to return from the invocation
   * @return An RPC response as the result of the action or a false response
   *         if the action fails or VM is unavailable
   */
  private def withVM[T <: RpcResponse](action: ScalaVirtualMachine => T): RpcResponse = {
    vmm.withVM(action).getOrElse({
      // TODO: Log more information based on type of error
      log.warning("Could not access debug VM.")
      FalseResponse
    })
  }

  /**
   * Attempts to invoke the provided action against the specified thread.
   *
   * @param threadId The unique id of the thread to execute against
   * @param action The action to execute against the thread
   * @tparam T The type of RpcResponse to return from the invocation
   * @return An RPC response as the result of the action or a false response
   *         if the action fails or thread is unavailable
   */
  private def withThread[T <: RpcResponse](threadId: Long, action: (ScalaVirtualMachine, ThreadInfoProfile) => T): RpcResponse = {
    withVM(s => {
      s.tryGetThread(threadId).flatMap(t => Try(action(s, t))).getOrElse({
        // TODO: Log more information based on type of error
        log.warning(s"Unable to retrieve thread with id: $threadId")
        FalseResponse
      })
    })
  }

  /**
   * Retrieves the proper full file name from the virtual machine.
   *
   * @param scalaVirtualMachine The virtual machine whose classes to search
   *                            through for the file name
   * @param fileName The short file name to match against full names
   * @return Some file name if a match is found, otherwise None
   */
  private def fullFileName(
    scalaVirtualMachine: ScalaVirtualMachine,
    fileName: String
  ): Option[String] = {
    val fileNames = scalaVirtualMachine.sourceNameToPaths(fileName)

    val choice = fileNames.headOption

    if (fileNames.isEmpty)
      log.warning(s"$fileName was not found in available classes!")
    else if (fileNames.size > 1)
      log.warning(s"Ambiguous file $fileName, choosing ${choice.get}")

    choice
  }
}
