package org.ensime.core.debug

import com.sun.jdi._
import org.ensime.api._

import scala.collection.mutable.ListBuffer

/**
 * Converts normal JDI structures into their equivalent Ensime-oriented
 * messages.
 */
class StructureConverter {

  def makeFields(tpeIn: ReferenceType, obj: ObjectReference): List[DebugClassField] = {
    tpeIn match {
      case tpeIn: ClassType =>
        var fields = List[DebugClassField]()
        var tpe = tpeIn
        while (tpe != null) {
          var i = -1
          fields = tpe.fields().map { f =>
            i += 1
            val value = obj.getValue(f)
            DebugClassField(
              i, f.name(),
              f.typeName(),
              valueSummary(value)
            )
          }.toList ++ fields
          tpe = tpe.superclass
        }
        fields
      case _ => List.empty
    }
  }

  def makeDebugObj(value: ObjectReference): DebugObjectInstance = {
    DebugObjectInstance(
      valueSummary(value),
      makeFields(value.referenceType(), value),
      value.referenceType().name(),
      DebugObjectId(value.uniqueID())
    )
  }

  def makeDebugStr(value: StringReference): DebugStringInstance = {
    DebugStringInstance(
      valueSummary(value),
      makeFields(value.referenceType(), value),
      value.referenceType().name(),
      DebugObjectId(value.uniqueID())
    )
  }

  def makeDebugArr(value: ArrayReference): DebugArrayInstance = {
    DebugArrayInstance(
      value.length,
      value.referenceType().name,
      value.referenceType().asInstanceOf[ArrayType].componentTypeName(),
      DebugObjectId(value.uniqueID)
    )
  }

  def makeDebugPrim(value: PrimitiveValue): DebugPrimitiveValue = DebugPrimitiveValue(
    valueSummary(value),
    value.`type`().name()
  )

  def makeDebugNull(): DebugNullValue = DebugNullValue("Null")

  def makeDebugValue(value: Value): DebugValue = {
    if (value == null) makeDebugNull()
    else {
      value match {
        case v: ArrayReference => makeDebugArr(v)
        case v: StringReference => makeDebugStr(v)
        case v: ObjectReference => makeDebugObj(v)
        case v: PrimitiveValue => makeDebugPrim(v)
      }
    }
  }

  def locationForName(thread: ThreadReference, name: String): Option[DebugLocation] = {
    val stackFrame = thread.frame(0)
    val objRef = stackFrame.thisObject()
    if (name == "this") {
      Some(DebugObjectReference(remember(objRef).uniqueID))
    } else {
      stackSlotForName(thread, name).map({ slot =>
        DebugStackSlot(DebugThreadId(thread.uniqueID), slot.frame, slot.offset)
      }).orElse(
        fieldByName(objRef, name).flatMap { f =>
          Some(DebugObjectField(DebugObjectId(objRef.uniqueID), f.name))
        }
      )
    }
  }

  def makeStackFrame(index: Int, frame: StackFrame): DebugStackFrame = {
    val locals = ignoreErr({
      frame.visibleVariables.zipWithIndex.map {
        case (v, i) =>
          DebugStackLocal(i, v.name,
            valueSummary(frame.getValue(v)),
            v.typeName())
      }.toList
    }, List.empty)

    val numArgs = ignoreErr(frame.getArgumentValues.length, 0)
    val methodName = ignoreErr(frame.location.method().name(), "Method")
    val className = ignoreErr(frame.location.declaringType().name(), "Class")
    val pcLocation = sourceMap.locToPos(frame.location).getOrElse(
      LineSourcePosition(
        File(frame.location.sourcePath()).canon,
        frame.location.lineNumber
      )
    )
    val thisObjId = ignoreErr(remember(frame.thisObject()).uniqueID, -1L)
    DebugStackFrame(index, locals, numArgs, className, methodName, pcLocation, DebugObjectId(thisObjId))
  }

  def backtrace(thread: ThreadReference, index: Int, count: Int): DebugBacktrace = {
    val frames = ListBuffer[DebugStackFrame]()
    var i = index
    while (i < thread.frameCount && (count == -1 || i < count)) {
      val stackFrame = thread.frame(i)
      frames += makeStackFrame(i, stackFrame)
      i += 1
    }
    DebugBacktrace(frames.toList, DebugThreadId(thread.uniqueID()), thread.name())
  }

}
