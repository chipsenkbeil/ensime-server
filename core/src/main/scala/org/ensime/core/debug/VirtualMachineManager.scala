package org.ensime.core.debug

import com.sun.jdi.VMDisconnectedException
import org.scaladebugger.api.debuggers.{LaunchingDebugger, AttachingDebugger, Debugger}
import org.scaladebugger.api.lowlevel.requests.properties.SuspendPolicyProperty
import org.scaladebugger.api.virtualmachines.ScalaVirtualMachine
import org.slf4j.LoggerFactory

import scala.util.{Failure, Try}

/**
 * Represents a manager of virtual machines connected to Ensime.
 *
 * @param globalStartFunc Invoked whenever a virtual machine is started
 * @param globalStopFunc Invoked whenever a virtual machine is stopped or disconnects
 *                 unexpectedly
 */
class VirtualMachineManager(
  private val globalStartFunc: (ScalaVirtualMachine) => Unit = _ => {},
  private val globalStopFunc: (ScalaVirtualMachine) => Unit = _ => {}
) {
  private val log = LoggerFactory.getLogger(this.getClass)
  @volatile private var debugger: Option[Debugger] = None
  @volatile private var vm: Option[ScalaVirtualMachine] = None
  @volatile private var mode: Option[VmMode] = None

  /**
   * Represents the mode actively being used by the internal debugger.
   *
   * @return Some mode instance if active, otherwise None
   */
  def activeMode: Option[VmMode] = mode

  /**
   * Starts a new debugger using the specified VM mode.
   *
   * @param mode The mode (launching/attaching) to use when determining which
   *             kind of debugger to start
   * @param options Optional JVM options to provide to the JVM started when
   *                using a launching debugger
   * @param startFunc Optional function to be invoked once the debugger has
   *                  connected with the virtual machine
   */
  def start(
    mode: VmMode,
    options: Seq[String] = Nil,
    startFunc: (ScalaVirtualMachine) => Unit = _ => {}
  ): Unit = synchronized {
    import scala.concurrent.duration._

    // Start the specific debugger based on mode and retrieve a
    // ScalaVirtualMachine instance from launching or attaching
    val (d, s) = mode match {
      case VmStart(commandLine) =>
        val d = LaunchingDebugger(
          className = commandLine,
          jvmOptions = options,
          suspend = true
        )

        val s = d.start(timeout = 10.seconds, startProcessingEvents = false)

        (d, s)
      case VmAttach(hostname, port) =>
        val d = AttachingDebugger(
          port = port.toInt,
          hostname = hostname
        )

        val s = d.start(timeout = 10.seconds, startProcessingEvents = false)

        // Place JVM in running state
        s.underlyingVirtualMachine.resume()

        (d, s)
    }

    // Bind our event handles like class prepare and thread start/stop
    bindEvents(s)

    // Mark our debugger and acquired virtual machine as ready
    debugger = Some(d)
    vm = Some(s)
    this.mode = Some(mode)

    // Invoke our start function, ignoring any error that may arise
    withVM(globalStartFunc)
    withVM(startFunc)

    (d, s)
  }

  /**
   * Stops and shuts down the currently-active debugger and its associated
   * virtual machines.
   *
   * @param stopFunc Optional function to call before the virtual machine
   *                 shuts down
   */
  def stop(stopFunc: (ScalaVirtualMachine) => Unit = _ => {}): Unit = synchronized {
    // Invoke our stop function, ignoring any error that may arise
    withVM(globalStopFunc)
    withVM(stopFunc)

    // Clear our associated mode
    mode = None

    // Dispose of the virtual machine and discard the reference
    withVM(_.underlyingVirtualMachine.dispose())
    vm = None

    // Shutdown the associated debugger
    Try(debugger.foreach(_.stop()))
    debugger = None
  }

  /**
   * Returns whether or not this manager has an active JVM that is being
   * debugged.
   *
   * @return True if a remote virtual machine is connected, otherwise false
   */
  def hasActiveVM: Boolean = vm.nonEmpty

  /**
   * Retrieves the active JVM if available and runs the specified action on
   * top of it.
   *
   * @param action The action to evaluate on top of the JVM
   * @tparam T The expected return value from the action
   * @return Some containing the result if successful, otherwise None
   */
  def withVM[T](action: (ScalaVirtualMachine => T)): Try[T] = vm.synchronized {
    if (!hasActiveVM) {
      log.error("No VM active for debugging!")
      return Failure(new NoActiveVirtualMachineException)
    }

    val result = Try(action(vm.get))

    result.failed.foreach {
      case e: VMDisconnectedException =>
        log.error("Attempted interaction with disconnected VM:", e)
        stop()
      case e: Throwable =>
        log.error("Exception thrown whilst handling vm action", e)
    }

    result
  }

  /**
   * Attaches common event handlers to the virtual machine.
   *
   * @param s The Scala virtual machine with which to attach event handlers
   */
  private def bindEvents(s: ScalaVirtualMachine): Unit = {
    import org.scaladebugger.api.dsl.Implicits._

    // Listen for new classes, suspending all JVM threads while trying to add
    // any associated breakpoints
    s.onUnsafeClassPrepare(SuspendPolicyProperty.AllThreads).foreach(e => {
      val refType = e.referenceType()
      typeAdded(refType)
      tryPendingBreaksForSourcename(refType.sourceName())
    })

    // Listen for thread start events, but do not suspend any thread
    s.getOrCreateThreadStartRequest(SuspendPolicyProperty.NoThread)

    // Listen for thread death events, but do not suspend any thread
    s.getOrCreateThreadDeathRequest(SuspendPolicyProperty.NoThread)

    // Listen for all uncaught exceptions, suspending the entire JVM when we
    // encounter an uncaught exception event
    s.getOrCreateAllExceptionsRequest(
      notifyCaught = false,
      notifyUncaught = true,
      SuspendPolicyProperty.AllThreads
    )

    // If our VM disconnects, stop the debugger
    // TODO: This is not wise if we are using a Listening debugger which can have
    //       more than one JVM connected at once
    s.onUnsafeVMDisconnect().foreach(_ => stop())
  }
}
