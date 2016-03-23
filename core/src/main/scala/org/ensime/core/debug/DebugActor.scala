package org.ensime.core.debug

import java.io.File

import akka.actor.{ActorRef, Actor, ActorLogging}
import akka.event.LoggingReceive
import com.sun.jdi.request.StepRequest
import org.ensime.api._

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
      sender ! vmm.withVM(_ => TrueResponse).getOrElse({
        // TODO: Log failure
        FalseResponse
      })
    // ========================================================================
    case DebugStopReq =>
      sender ! handleRPCWithVM() { vm =>
        if (vm.mode.shouldExit) {
          vm.exit(0)
        }
        vm.dispose()
        TrueResponse
      }
    // ========================================================================
    case DebugRunReq =>
      sender ! handleRPCWithVM() { vm =>
        vm.resume()
        TrueResponse
      }
    // ========================================================================
    case DebugContinueReq(threadId) =>
      sender ! handleRPCWithVMAndThread(threadId) {
        (vm, thread) =>
          vm.resume()
          TrueResponse
      }
    // ========================================================================
    case DebugSetBreakReq(file, line: Int) =>
      if (!setBreakpoint(file, line)) {
        bgMessage("Location not loaded. Set pending breakpoint.")
      }
      sender ! TrueResponse
    // ========================================================================
    case DebugClearBreakReq(file, line: Int) =>
      clearBreakpoint(file, line)
      sender ! TrueResponse

    // ========================================================================
    case DebugClearAllBreaksReq =>
      clearAllBreakpoints()
      sender ! TrueResponse

    // ========================================================================
    case DebugListBreakpointsReq =>
      val breaks = BreakpointList(activeBreakpoints.toList, pendingBreakpoints)
      sender ! breaks

    // ========================================================================
    case DebugNextReq(threadId: DebugThreadId) =>
      sender ! handleRPCWithVMAndThread(threadId) {
        (vm, thread) =>
          vm.newStepRequest(
            thread,
            StepRequest.STEP_LINE,
            StepRequest.STEP_OVER
          )
          TrueResponse
      }

    // ========================================================================
    case DebugStepReq(threadId: DebugThreadId) =>
      sender ! handleRPCWithVMAndThread(threadId) {
        (vm, thread) =>
          vm.newStepRequest(
            thread,
            StepRequest.STEP_LINE,
            StepRequest.STEP_INTO
          )
          TrueResponse
      }

    // ========================================================================
    case DebugStepOutReq(threadId: DebugThreadId) =>
      sender ! handleRPCWithVMAndThread(threadId) {
        (vm, thread) =>
          vm.newStepRequest(
            thread,
            StepRequest.STEP_LINE,
            StepRequest.STEP_OUT
          )
          TrueResponse
      }

    // ========================================================================
    case DebugLocateNameReq(threadId: DebugThreadId, name: String) =>
      sender ! handleRPCWithVMAndThread(threadId) {
        (vm, thread) =>
          vm.locationForName(thread, name).getOrElse(FalseResponse)
      }
    // ========================================================================
    case DebugBacktraceReq(threadId: DebugThreadId, index: Int, count: Int) =>
      sender ! handleRPCWithVMAndThread(threadId) { (vm, thread) =>
        vm.backtrace(thread, index, count)
      }
    // ========================================================================
    case DebugValueReq(location) =>
      sender ! handleRPCWithVM() { vm =>
        vm.debugValueAtLocation(location).getOrElse(FalseResponse)
      }
    // ========================================================================
    case DebugToStringReq(threadId, location) =>
      sender ! handleRPCWithVM() { vm =>
        vm.debugValueAtLocationToString(threadId, location) match {
          case Some(strValue) => StringResponse(strValue)
          case None => FalseResponse
        }
      }

    // ========================================================================
    case DebugSetValueReq(location, newValue) =>
      sender ! handleRPCWithVM() { vm =>
        location match {
          case DebugStackSlot(threadId, frame, offset) => vm.threadById(threadId) match {
            case Some(thread) =>
              val status = vm.setStackVar(thread, frame, offset, newValue)
              status match {
                case true =>
                  TrueResponse
                case false =>
                  FalseResponse
              }
            case _ =>
              log.error(s"Unknown thread $threadId for debug-set-value")
              FalseResponse
          }
          case unknown =>
            log.error(s"Unsupported location type for debug-set-value.: $unknown")
            FalseResponse
        }
      }
  }
}
