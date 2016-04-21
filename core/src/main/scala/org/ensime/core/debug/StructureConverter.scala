package org.ensime.core.debug

import org.ensime.api._
import org.scaladebugger.api.dsl.Implicits._
import org.scaladebugger.api.profiles.traits.info._

/**
 * Converts normal JDI structures into their equivalent Ensime-oriented
 * messages.
 *
 * @param sourceMap Used to include relevant source information when
 *                  constructing various Ensime messages
 */
class StructureConverter(private val sourceMap: SourceMap) {

  /**
   * Converts a debugger API value into an Ensime message.
   *
   * @param valueInfo The debugger API value
   * @return The Ensime message
   */
  def makeDebugValue(valueInfo: ValueInfoProfile): DebugValue = valueInfo match {
    case v if v.isNull                      => makeDebugNull()
    case v if v.isVoid                      => makeDebugVoid(v)
    case v: ArrayInfoProfile                => makeDebugArr(v)
    case v: ObjectInfoProfile if v.isString => makeDebugStr(v)
    case v: ObjectInfoProfile               => makeDebugObj(v)
    case v: PrimitiveInfoProfile            => makeDebugPrim(v)
  }

  def makeDebugObj(value: ObjectInfoProfile): DebugObjectInstance = {
    DebugObjectInstance(
      value.toPrettyString,
      makeFields(value.referenceType, value),
      value.referenceType.name,
      DebugObjectId(value.uniqueId)
    )
  }

  def makeDebugStr(value: ObjectInfoProfile): DebugStringInstance = {
    DebugStringInstance(
      value.toPrettyString,
      makeFields(value.referenceType, value),
      value.referenceType.name,
      DebugObjectId(value.uniqueId)
    )
  }

  def makeDebugArr(value: ArrayInfoProfile): DebugArrayInstance = {
    DebugArrayInstance(
      value.length,
      value.referenceType.name,
      value.referenceType.toArrayType.elementTypeName,
      DebugObjectId(value.uniqueId)
    )
  }

  def makeDebugPrim(value: PrimitiveInfoProfile): DebugPrimitiveValue = DebugPrimitiveValue(
    value.toPrettyString,
    value.typeInfo.name
  )

  def makeDebugVoid(value: ValueInfoProfile): DebugPrimitiveValue = {
    DebugPrimitiveValue(
      value.toPrettyString,
      value.typeInfo.name
    )
  }

  def makeDebugNull(): DebugNullValue = DebugNullValue("Null")

  def makeFields(
    tpeIn: ReferenceTypeInfoProfile,
    obj: ObjectInfoProfile
  ): List[DebugClassField] = {
    if (!tpeIn.isClassType) return List.empty

    var fields = List[DebugClassField]()
    var tpe: Option[ClassTypeInfoProfile] = Some(tpeIn.toClassType)
    while (tpe.nonEmpty) {
      var i = -1
      fields = tpe.get.allFields.map { f =>
        i += 1
        DebugClassField(
          i, f.name,
          f.typeName,
          f.toValue.toPrettyString
        )
      }.toList ++ fields

      tpe = tpe.get.superclass
    }
    fields
  }

  def makeStackFrame(frame: FrameInfoProfile): DebugStackFrame = {
    val locals = ignoreErr(
      frame.localVariables.map(makeStackLocal).toList,
      List.empty
    )

    val numArgs = ignoreErr(frame.argumentValues.length, 0)
    val methodName = ignoreErr(frame.location.method.name, "Method")
    val className = ignoreErr(frame.location.declaringType.name, "Class")

    import org.ensime.util.file._
    val pcLocation = sourceMap.newLineSourcePosition(frame.location).getOrElse(
      LineSourcePosition(
        File(frame.location.sourcePath).canon,
        frame.location.lineNumber
      )
    )
    val thisObjId = ignoreErr(frame.thisObject.cache().uniqueId, -1L)
    DebugStackFrame(frame.index, locals, numArgs, className, methodName, pcLocation, DebugObjectId(thisObjId))
  }

  def makeStackLocal(variableInfo: IndexedVariableInfoProfile): DebugStackLocal = {
    DebugStackLocal(
      variableInfo.offsetIndex,
      variableInfo.name,
      variableInfo.toValue.toPrettyString,
      variableInfo.typeName
    )
  }

  /**
   * Executes the provided action, yielding a executing a different action if
   * it fails.
   *
   * @param action The action to execute
   * @param orElse The other action to execute if the first fails
   * @tparam T The return type of both actions
   * @return The result from executing the first or second action
   */
  private def ignoreErr[T](action: => T, orElse: => T): T = {
    try { action } catch { case e: Exception => orElse }
  }
}
