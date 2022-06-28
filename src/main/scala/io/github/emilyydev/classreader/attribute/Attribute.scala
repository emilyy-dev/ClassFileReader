package io.github.emilyydev.classreader.attribute

import io.github.emilyydev.classreader.accessflag.{AccessFlag, AccessFlagHolder, MandatedAccessFlag, OpenAccessFlag, SyntheticAccessFlag}
import io.github.emilyydev.classreader.attribute.module.{ExportsInfo, OpensInfo, ProvidesInfo, RequiresInfo}
import io.github.emilyydev.classreader.constantpool.ConstantPool

import java.io.DataInput
import scala.annotation.tailrec

sealed trait Attribute

final case class UnknownAttribute(name: String, data: Array[Byte]) extends Attribute

final case class ConstantValueAttribute(valueIndex: Int) extends Attribute
final case class CodeAttribute(
  maxStack: Int,
  maxLocals: Int,
  code: Array[Byte],
  exceptionTable: List[ExceptionInfo],
  attributes: Map[String, Attribute]
) extends Attribute
/*
final case class StackMapTableAttribute() extends Attribute
*/
final case class ExceptionsAttribute(exceptionIndexes: Array[Int]) extends Attribute
final case class InnerClassesAttribute(innerClasses: List[InnerClassInfo]) extends Attribute
final case class EnclosingMethodAttribute(classIndex: Int, methodIndex: Int) extends Attribute
case object SyntheticAttribute extends Attribute
final case class SignatureAttribute(signatureIndex: Int) extends Attribute
final case class SourceFileAttribute(sourceFileIndex: Int) extends Attribute
final case class SourceDebugExtensionAttribute(debugExtension: Array[Byte]) extends Attribute
final case class LineNumberTableAttribute(lineNumberTable: List[LineNumberInfo]) extends Attribute
final case class LocalVariableTableAttribute(localVariableTable: List[LocalVariableInfo]) extends Attribute
final case class LocalVariableTypeTableAttribute(localVariableTypeTable: List[LocalVariableTypeInfo]) extends Attribute
case object DeprecatedAttribute extends Attribute
final case class RuntimeVisibleAnnotationsAttribute(annotations: List[AnnotationInfo]) extends Attribute
final case class RuntimeInvisibleAnnotationsAttribute(annotations: List[AnnotationInfo]) extends Attribute
final case class RuntimeVisibleParameterAnnotationsAttribute(parameterAnnotations: List[List[AnnotationInfo]]) extends Attribute
final case class RuntimeInvisibleParameterAnnotationsAttribute(parameterAnnotations: List[List[AnnotationInfo]]) extends Attribute
/*
final case class RuntimeVisibleTypeAnnotationsAttribute() extends Attribute
final case class RuntimeInvisibleTypeAnnotationsAttribute() extends Attribute
*/
final case class AnnotationDefaultAttribute(value: AnnotationValue) extends Attribute
final case class BootstrapMethodsAttribute(bootstrapMethods: List[BootstrapMethodInfo]) extends Attribute
final case class MethodParametersAttribute(parameters: List[MethodParameterInfo]) extends Attribute
final case class ModuleAttribute(
  nameIndex: Int,
  accessFlagSet: Set[AccessFlag],
  versionIndex: Option[Int],
  requiresList: List[RequiresInfo],
  exportsList: List[ExportsInfo],
  opensList: List[OpensInfo],
  usesIndexes: Array[Int],
  providesList: List[ProvidesInfo]
) extends Attribute
final case class ModulePackagesAttribute(packageIndexes: Array[Int]) extends Attribute
final case class ModuleMainClassAttribute(mainClassIndex: Int) extends Attribute
final case class NestHostAttribute(hostClassIndex: Int) extends Attribute
final case class NestMembersAttribute(nestClassIndexes: Array[Int]) extends Attribute
final case class RecordAttribute(components: List[RecordComponentInfo]) extends Attribute
final case class PermittedSubclassesAttribute(permittedSubclassIndexes: Array[Int]) extends Attribute

object ModuleAttribute extends AccessFlagHolder {
  override val LegalFlags: Set[AccessFlag] = Set(
    OpenAccessFlag,
    SyntheticAccessFlag,
    MandatedAccessFlag
  )
}

object Attribute {

  val ConstantValue: String = "ConstantValue"
  val Code: String = "Code"
  val StackMapTable: String = "StackMapTable"
  val Exceptions: String = "Exceptions"
  val InnerClasses: String = "InnerClasses"
  val EnclosingMethod: String = "EnclosingMethod"
  val Synthetic: String = "Synthetic"
  val Signature: String = "Signature"
  val SourceFile: String = "SourceFile"
  val SourceDebugExtension: String = "SourceDebugExtension"
  val LineNumberTable: String = "LineNumberTable"
  val LocalVariableTable: String = "LocalVariableTable"
  val LocalVariableTypeTable: String = "LocalVariableTypeTable"
  val Deprecated: String = "Deprecated"
  val RuntimeVisibleAnnotations: String = "RuntimeVisibleAnnotations"
  val RuntimeInvisibleAnnotations: String = "RuntimeInvisibleAnnotations"
  val RuntimeVisibleParameterAnnotations: String = "RuntimeVisibleParameterAnnotations"
  val RuntimeInvisibleParameterAnnotations: String = "RuntimeInvisibleParameterAnnotations"
  val RuntimeVisibleTypeAnnotations: String = "RuntimeVisibleTypeAnnotations"
  val RuntimeInvisibleTypeAnnotations: String = "RuntimeInvisibleTypeAnnotations"
  val AnnotationDefault: String = "AnnotationDefault"
  val BootstrapMethods: String = "BootstrapMethods"
  val MethodParameters: String = "MethodParameters"
  val Module: String = "Module"
  val ModulePackages: String = "ModulePackages"
  val ModuleMainClass: String = "ModuleMainClass"
  val NestHost: String = "NestHost"
  val NestMembers: String = "NestMembers"
  val Record: String = "Record"
  val PermittedSubclasses: String = "PermittedSubclasses"

  def read(in: DataInput, constantPool: ConstantPool): (String, Attribute) = {
    val name = constantPool.render(in.readUnsignedShort())
    val length = in.readInt()
    val attribute = name match {
      case ConstantValue => ConstantValueAttribute(in.readUnsignedShort())
      case Code =>
        val maxStack = in.readUnsignedShort()
        val maxLocals = in.readUnsignedShort()
        val codeLength = in.readInt()
        val code = new Array[Byte](codeLength)
        in.readFully(code)
        val exceptionCount = in.readUnsignedShort()
        val exceptionTable = for {
          _ <- 0 until exceptionCount
        } yield ExceptionInfo.read(in)
        val attributeCount = in.readUnsignedShort()
        val attributes = for {
          _ <- 0 until attributeCount
        } yield Attribute.read(in, constantPool)
        CodeAttribute(
          maxStack,
          maxLocals,
          code,
          exceptionTable.toList,
          attributes.toMap
        )
      /*
            case StackMapTable =>
      */
      case Exceptions =>
        val exceptionCount = in.readUnsignedShort()
        val exceptionIndexes = for {
          _ <- 0 until exceptionCount
        } yield in.readUnsignedShort()
        ExceptionsAttribute(exceptionIndexes.toArray)
      case InnerClasses =>
        val innerClassCount = in.readUnsignedShort()
        val innerClasses = for {
          _ <- 0 until innerClassCount
        } yield InnerClassInfo.read(in)
        InnerClassesAttribute(innerClasses.toList)
      case EnclosingMethod =>
        EnclosingMethodAttribute(in.readUnsignedShort(), in.readUnsignedShort())
      case Synthetic => SyntheticAttribute
      case Signature => SignatureAttribute(in.readUnsignedShort())
      case SourceFile => SourceFileAttribute(in.readUnsignedShort())
      case SourceDebugExtension =>
        val debugExtension = new Array[Byte](length)
        in.readFully(debugExtension)
        SourceDebugExtensionAttribute(debugExtension)
      case LineNumberTable =>
        val lineNumberTableLength = in.readUnsignedShort()
        val lineNumberTable = for {
          _ <- 0 until lineNumberTableLength
        } yield LineNumberInfo.read(in)
        LineNumberTableAttribute(lineNumberTable.toList)
      case LocalVariableTable =>
        val localVariableTableLength = in.readUnsignedShort()
        val localVariableTable = {
          val builder = List.newBuilder[LocalVariableInfo]

          @tailrec
          def readEntries(
            remaining: Int = localVariableTableLength,
            prev: LocalVariableInfo = DummyLocalVariableInfo
          ): Unit = {
            if (remaining != 0) {
              if (prev.descriptorIndex > 0) {
                val renderedType = constantPool.render(prev.descriptorIndex)
                if (renderedType == "D" || renderedType == "J") {
                  builder += DummyLocalVariableInfo
                }
              }

              val next = LocalVariableInfo.read(in)
              builder += next
              readEntries(remaining - 1, next)
            }
          }

          readEntries()
          builder.result()
        }

        LocalVariableTableAttribute(localVariableTable)
      case LocalVariableTypeTable =>
        val localVariableTypeTableLength = in.readUnsignedShort()
        val localVariableTypeTable = for {
          _ <- 0 until localVariableTypeTableLength
        } yield LocalVariableTypeInfo.read(in)
        LocalVariableTypeTableAttribute(localVariableTypeTable.toList)
      case Deprecated => DeprecatedAttribute
      case RuntimeVisibleAnnotations => RuntimeVisibleAnnotationsAttribute(readAnnotations(in))
      case RuntimeInvisibleAnnotations => RuntimeInvisibleAnnotationsAttribute(readAnnotations(in))
      case RuntimeVisibleParameterAnnotations =>
        RuntimeVisibleParameterAnnotationsAttribute(readParameterAnnotations(in))
      case RuntimeInvisibleParameterAnnotations =>
        RuntimeInvisibleParameterAnnotationsAttribute(readParameterAnnotations(in))
      /*
            case RuntimeVisibleTypeAnnotations =>
            case RuntimeInvisibleTypeAnnotations =>
      */
      case AnnotationDefault => AnnotationDefaultAttribute(AnnotationValue.read(in))
      case BootstrapMethods =>
        val bootstrapMethodCount = in.readUnsignedShort()
        val bootstrapMethods = for {
          _ <- 0 until bootstrapMethodCount
        } yield BootstrapMethodInfo.read(in)
        BootstrapMethodsAttribute(bootstrapMethods.toList)
      case MethodParameters =>
        val methodParameterCount = in.readUnsignedByte()
        val methodParameters = for {
          _ <- 0 until methodParameterCount
        } yield MethodParameterInfo.read(in)
        MethodParametersAttribute(methodParameters.toList)
      case Module =>
        val moduleNameIndex = in.readUnsignedShort()
        val accessFlagSet = ModuleAttribute.ToAccessFlagSet(in.readUnsignedShort())
        val moduleVersionIndex = in.readUnsignedShort()
        val requiresCount = in.readUnsignedShort()
        val requiresList = for {
          _ <- 0 until requiresCount
        } yield RequiresInfo.read(in)
        val exportsCount = in.readUnsignedShort()
        val exportsList = for {
          _ <- 0 until exportsCount
        } yield ExportsInfo.read(in)
        val opensCount = in.readUnsignedShort()
        val opensList = for {
          _ <- 0 until opensCount
        } yield OpensInfo.read(in)
        val usesCount = in.readUnsignedShort()
        val usesList = for {
          _ <- 0 until usesCount
        } yield in.readUnsignedShort()
        val providesCount = in.readUnsignedShort()
        val providesList = for {
          _ <- 0 until providesCount
        } yield ProvidesInfo.read(in)
        ModuleAttribute(
          moduleNameIndex,
          accessFlagSet,
          if (moduleVersionIndex == 0) None else Some(moduleVersionIndex),
          requiresList.toList,
          exportsList.toList,
          opensList.toList,
          usesList.toArray,
          providesList.toList
        )
      case ModulePackages =>
        val packageCount = in.readUnsignedShort()
        val packages = for {
          _ <- 0 until packageCount
        } yield in.readUnsignedShort()
        ModulePackagesAttribute(packages.toArray)
      case ModuleMainClass => ModuleMainClassAttribute(in.readUnsignedShort())
      case NestHost => NestHostAttribute(in.readUnsignedShort())
      case NestMembers =>
        val nestMemberCount = in.readUnsignedShort()
        val nestMembers = for {
          _ <- 0 until nestMemberCount
        } yield in.readUnsignedShort()
        NestMembersAttribute(nestMembers.toArray)
      case Record =>
        val recordComponentCount = in.readUnsignedShort()
        val recordComponents = for {
          _ <- 0 until recordComponentCount
        } yield RecordComponentInfo.read(in, constantPool)
        RecordAttribute(recordComponents.toList)
      case PermittedSubclasses =>
        val permittedSubclassCount = in.readUnsignedShort()
        val permittedSubclasses = for {
          _ <- 0 until permittedSubclassCount
        } yield in.readUnsignedShort()
        PermittedSubclassesAttribute(permittedSubclasses.toArray)

      case _ =>
        val data = new Array[Byte](length)
        in.readFully(data)
        UnknownAttribute(name, data)
    }

    (name, attribute)
  }

  private def readAnnotations(in: DataInput): List[AnnotationInfo] = {
    val annotationsNumber = in.readUnsignedShort()
    val annotations = for {
      _ <- 0 until annotationsNumber
    } yield AnnotationInfo.read(in)
    annotations.toList
  }

  private def readParameterAnnotations(in: DataInput): List[List[AnnotationInfo]] = {
    val parametersNumber = in.readUnsignedByte()
    val parameterAnnotations = for {
      _ <- 0 until parametersNumber
    } yield readAnnotations(in)
    parameterAnnotations.toList
  }
}
