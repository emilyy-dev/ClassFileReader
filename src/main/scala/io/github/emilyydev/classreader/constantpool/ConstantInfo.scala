package io.github.emilyydev.classreader.constantpool

import io.github.emilyydev.classreader.Indentable

import java.io.{DataInput, Writer}

sealed trait ConstantInfo {
  def render(writer: Writer with Indentable, rendered: String): Unit =
    ConstantInfo.render(this, writer, rendered)
}

final case class ClassConstantInfo(nameIndex: Int) extends ConstantInfo
final case class FieldRefConstantInfo(classIndex: Int, nameAndTypeIndex: Int) extends ConstantInfo
final case class MethodRefConstantInfo(classIndex: Int, nameAndTypeIndex: Int) extends ConstantInfo
final case class InterfaceMethodRefConstantInfo(classIndex: Int, nameAndTypeIndex: Int) extends ConstantInfo
final case class StringConstantInfo(valueIndex: Int) extends ConstantInfo
final case class IntegerConstantInfo(value: Int) extends ConstantInfo
final case class FloatConstantInfo(value: Float) extends ConstantInfo
final case class LongConstantInfo(value: Long) extends ConstantInfo
final case class DoubleConstantInfo(value: Double) extends ConstantInfo
final case class NameAndTypeConstantInfo(nameIndex: Int, descriptorIndex: Int) extends ConstantInfo
final case class Utf8ConstantInfo(value: String) extends ConstantInfo
final case class MethodHandleConstantInfo(kind: MethodHandleKind, referenceIndex: Int) extends ConstantInfo
final case class MethodTypeConstantInfo(descriptorIndex: Int) extends ConstantInfo
final case class DynamicConstantInfo(bootstrapMethodIndex: Int, nameAndTypeIndex: Int) extends ConstantInfo
final case class InvokeDynamicConstantInfo(bootstrapMethodIndex: Int, nameAndTypeIndex: Int) extends ConstantInfo
final case class ModuleConstantInfo(nameIndex: Int) extends ConstantInfo
final case class PackageConstantInfo(nameIndex: Int) extends ConstantInfo

case object DummyConstantInfo extends ConstantInfo

object ConstantInfo {

  val CONSTANT_Class: Int = 7
  //noinspection SpellCheckingInspection
  val CONSTANT_Fieldref: Int = 9
  //noinspection SpellCheckingInspection
  val CONSTANT_Methodref: Int = 10
  //noinspection SpellCheckingInspection
  val CONSTANT_InterfaceMethodref: Int = 11
  val CONSTANT_String: Int = 8
  val CONSTANT_Integer: Int = 3
  val CONSTANT_Float: Int = 4
  val CONSTANT_Long: Int = 5
  val CONSTANT_Double: Int = 6
  val CONSTANT_NameAndType: Int = 12
  val CONSTANT_Utf8: Int = 1
  val CONSTANT_MethodHandle: Int = 15
  val CONSTANT_MethodType: Int = 16
  val CONSTANT_Dynamic: Int = 17
  val CONSTANT_InvokeDynamic: Int = 18
  val CONSTANT_Module: Int = 19
  val CONSTANT_Package: Int = 20

  def read(in: DataInput): ConstantInfo = {
    in.readUnsignedByte() match {
      case CONSTANT_Class => ClassConstantInfo(in.readUnsignedShort())
      case CONSTANT_Fieldref => FieldRefConstantInfo(in.readUnsignedShort(), in.readUnsignedShort())
      case CONSTANT_Methodref =>
        MethodRefConstantInfo(in.readUnsignedShort(), in.readUnsignedShort())
      case CONSTANT_InterfaceMethodref =>
        InterfaceMethodRefConstantInfo(in.readUnsignedShort(), in.readUnsignedShort())
      case CONSTANT_String => StringConstantInfo(in.readUnsignedShort())
      case CONSTANT_Integer => IntegerConstantInfo(in.readInt())
      case CONSTANT_Float => FloatConstantInfo(in.readFloat())
      case CONSTANT_Long => LongConstantInfo(in.readLong())
      case CONSTANT_Double => DoubleConstantInfo(in.readDouble())
      case CONSTANT_NameAndType =>
        NameAndTypeConstantInfo(in.readUnsignedShort(), in.readUnsignedShort())
      case CONSTANT_Utf8 => Utf8ConstantInfo(in.readUTF())
      case CONSTANT_MethodHandle =>
        MethodHandleConstantInfo(
          MethodHandleKind.KindMap(in.readUnsignedByte()),
          in.readUnsignedShort()
        )
      case CONSTANT_MethodType => MethodTypeConstantInfo(in.readUnsignedShort())
      case CONSTANT_Dynamic => DynamicConstantInfo(in.readUnsignedShort(), in.readUnsignedShort())
      case CONSTANT_InvokeDynamic =>
        InvokeDynamicConstantInfo(in.readUnsignedShort(), in.readUnsignedShort())
      case CONSTANT_Module => ModuleConstantInfo(in.readUnsignedShort())
      case CONSTANT_Package => PackageConstantInfo(in.readUnsignedShort())
    }
  }

  private val LongestConstantName = List(
    "Class",
    "Fieldref",
    "Methodref",
    "InterfaceMethodref",
    "String",
    "Integer",
    "Float",
    "Long",
    "Double",
    "NameAndType",
    "Utf8",
    "MethodHandle",
    "MethodType",
    "Dynamic",
    "InvokeDynamic",
    "Module",
    "Package"
  ).map(_.length).max

  private def padded(str: Any, padding: Int = LongestConstantName) =
    s"%-${padding}s".format(str)

  private def write(
    writer: Writer with Indentable,
    name: String,
    value: String,
    rendered: String
  ): Unit = {
    writer.write(padded(name))
    writer.write(' ')
    writer.write(padded(value, 16))
    writer.writeln(s"// $rendered")
  }

  private def render(
    constant: ConstantInfo,
    writer: Writer with Indentable,
    rendered: String
  ): Unit = {
    val filteredRendered = filterRendered(rendered)
    constant match {
      case ClassConstantInfo(nameIndex) => write(writer, "Class", s"#$nameIndex", filteredRendered)
      case FieldRefConstantInfo(classIndex, nameAndTypeIndex) =>
        write(writer, "Fieldref", s"#$classIndex.#$nameAndTypeIndex", filteredRendered)
      case MethodRefConstantInfo(classIndex, nameAndTypeIndex) =>
        write(writer, "Methodref", s"#$classIndex.#$nameAndTypeIndex", filteredRendered)
      case InterfaceMethodRefConstantInfo(classIndex, nameAndTypeIndex) =>
        write(writer, "InterfaceMethodref", s"#$classIndex.#$nameAndTypeIndex", filteredRendered)
      case StringConstantInfo(valueIndex) => write(writer, "String", s"#$valueIndex", filteredRendered)
      case IntegerConstantInfo(value) => writer.writeln(s"${padded("Integer")} $value")
      case FloatConstantInfo(value) => writer.writeln(s"${padded("Float")} $value")
      case LongConstantInfo(value) => writer.writeln(s"${padded("Long")} $value")
      case DoubleConstantInfo(value) => writer.writeln(s"${padded("Double")} $value")
      case NameAndTypeConstantInfo(nameIndex, descriptorIndex) =>
        write(writer, "NameAndType", s"#$nameIndex:#$descriptorIndex", filteredRendered)
      case Utf8ConstantInfo(_) => writer.writeln(s"${padded("Utf8")} $filteredRendered")
      case MethodHandleConstantInfo(kind, referenceIndex) =>
        val kindValue = MethodHandleKind.KindMap
          .withFilter { case (_, v) => v == kind }
          .map { case (k, _) => k }
          .head
        write(writer, "MethodHandle", s"$kindValue:#$referenceIndex", filteredRendered)
      case MethodTypeConstantInfo(descriptorIndex) =>
        write(writer, "MethodType", s"#$descriptorIndex", filteredRendered)
      case DynamicConstantInfo(bootstrapMethodIndex, nameAndTypeIndex) =>
        write(writer, "Dynamic", s"#$bootstrapMethodIndex:#$nameAndTypeIndex", filteredRendered)
      case InvokeDynamicConstantInfo(bootstrapMethodIndex, nameAndTypeIndex) =>
        write(writer, "InvokeDynamic", s"#$bootstrapMethodIndex:#$nameAndTypeIndex", filteredRendered)
      case ModuleConstantInfo(nameIndex) =>
        write(writer, "Module", s"#$nameIndex", filteredRendered)
      case PackageConstantInfo(nameIndex) =>
        write(writer, "Package", s"#$nameIndex", filteredRendered)
    }
  }

  private def filterRendered(str: String): String = str.flatMap(filterRendered)
  private def filterRendered(c: Char): String = c match {
    case '"' => "\\\""
    case '\\' => "\\\\"
    case '\b' => "\\b"
    case '\f' => "\\f"
    case '\n' => "\\n"
    case '\r' => "\\r"
    case '\t' => "\\t"
    case c if c.isControl => "\\u%04x".format(c.toInt)
    case c => Character.toString(c)
  }
}
