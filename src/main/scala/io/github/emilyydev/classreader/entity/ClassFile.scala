package io.github.emilyydev.classreader.entity

import io.github.emilyydev.classreader.accessflag.{AbstractAccessFlag, AccessFlag, AccessFlagHolder, AnnotationAccessFlag, EnumAccessFlag, FinalAccessFlag, InterfaceAccessFlag, ModuleAccessFlag, PublicAccessFlag, SuperAccessFlag, SyntheticAccessFlag}
import io.github.emilyydev.classreader.attribute.Attribute
import io.github.emilyydev.classreader.constantpool.{ConstantInfo, ConstantPool, DoubleConstantInfo, DummyConstantInfo, LongConstantInfo}
import io.github.emilyydev.classreader.{Indentable, countDigits}

import java.io.{DataInput, Writer}
import scala.annotation.tailrec

final case class ClassFile(
  version: Int,
  constantPoolEntryCount: Int,
  constantPool: ConstantPool,
  accessFlagSet: Set[AccessFlag],
  thisClassIndex: Int,
  superClassIndex: Option[Int],
  interfaceIndexes: Array[Int],
  fields: List[Field],
  methods: List[Method],
  attributes: Map[String, Attribute]
) {
  def render(writer: Writer with Indentable): Unit = {
    val accessFlagNames = accessFlagSet.map(AccessFlag.Names(_)).mkString(" ")
    writer.writeln(constantPool.render(thisClassIndex).replace('/', '.'))
      .writeln(s"Class file version ${version + 44} ($version)")
      .writeln(s"Access flags (0x${AccessFlag.setAsNumber(accessFlagSet).toHexString}): $accessFlagNames")
    superClassIndex.map(constantPool.render).map(s"Superclass: ".concat).foreach { s => writer.writeln(s) }
    writer.writeln(s"Direct superinterface count: ${interfaceIndexes.length}")
      .write(interfaceIndexes.map(constantPool.render).mkString(", "))
    if (!interfaceIndexes.isEmpty) {
      writer.newLine()
    }

    writer.writeln(s"Constant pool entry count: $constantPoolEntryCount")
      .newLine()
      .increaseIndentation()

    val constantPoolSizeDigitCount = 1 + countDigits(constantPool.pool.size)
    for ((constant, index) <- constantPool.pool.zipWithIndex) {
      constant match {
        case DummyConstantInfo =>
        case _ =>
          writer.write(s"%${constantPoolSizeDigitCount}s: ".format(s"#$index"))
          constant.render(writer, constantPool.render(index))
      }
    }
    writer.decreaseIndentation()


  }
}

object ClassFile extends AccessFlagHolder {
  def read(in: DataInput): ClassFile = {
    /*val minorVersion =*/ in.readUnsignedShort()
    val majorVersion = in.readUnsignedShort() - 44
    val constantPoolEntryCount = in.readUnsignedShort()
    val constantPool = {
      val builder = List.newBuilder[ConstantInfo] += DummyConstantInfo

      @tailrec
      def readEntries(
        remaining: Int = constantPoolEntryCount - 1,
        prev: ConstantInfo = DummyConstantInfo
      ): Unit = {
        if (remaining != 0) {
          if (prev.isInstanceOf[LongConstantInfo] || prev.isInstanceOf[DoubleConstantInfo]) {
            builder += DummyConstantInfo
          }

          val next = ConstantInfo.read(in)
          builder += next
          readEntries(remaining - 1, next)
        }
      }

      readEntries()
      ConstantPool(builder.result())
    }

    val accessFlagSet = ToAccessFlagSet(in.readUnsignedShort())
    val thisClassIndex = in.readUnsignedShort()
    val superClassIndex = in.readUnsignedShort()
    val interfaceCount = in.readUnsignedShort()
    val interfaceIndexes = for {
      _ <- 0 until interfaceCount
    } yield in.readUnsignedShort()
    val fieldCount = in.readUnsignedShort()
    val fields = for {
      _ <- 0 until fieldCount
    } yield Field.read(in, constantPool)
    val methodCount = in.readUnsignedShort()
    val methods = for {
      _ <- 0 until methodCount
    } yield Method.read(in, constantPool)
    val attributeCount = in.readUnsignedShort()
    val attributes = for {
      _ <- 0 until attributeCount
    } yield Attribute.read(in, constantPool)

    ClassFile(
      majorVersion,
      constantPoolEntryCount,
      constantPool,
      accessFlagSet,
      thisClassIndex,
      if (superClassIndex == 0) None else Some(superClassIndex),
      interfaceIndexes.toArray,
      fields.toList,
      methods.toList,
      attributes.toMap
    )
  }

  override val LegalFlags: Set[AccessFlag] = Set(
    PublicAccessFlag,
    FinalAccessFlag,
    SuperAccessFlag,
    InterfaceAccessFlag,
    AbstractAccessFlag,
    SyntheticAccessFlag,
    AnnotationAccessFlag,
    EnumAccessFlag,
    ModuleAccessFlag
  )
}
