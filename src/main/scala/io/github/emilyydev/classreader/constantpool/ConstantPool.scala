package io.github.emilyydev.classreader.constantpool

final case class ConstantPool(pool: List[ConstantInfo]) {
  def render(index: Int): String = pool(index) match {
    case ClassConstantInfo(nameIndex) => render(nameIndex)
    case FieldRefConstantInfo(classIndex, nameAndTypeIndex) =>
      s"${render(classIndex)}.${render(nameAndTypeIndex)}"
    case MethodRefConstantInfo(classIndex, nameAndTypeIndex) =>
      s"${render(classIndex)}.${render(nameAndTypeIndex)}"
    case InterfaceMethodRefConstantInfo(classIndex, nameAndTypeIndex) =>
      s"${render(classIndex)}.${render(nameAndTypeIndex)}"
    case StringConstantInfo(valueIndex) => render(valueIndex)
    case IntegerConstantInfo(value) => String.valueOf(value)
    case FloatConstantInfo(value) => String.valueOf(value)
    case LongConstantInfo(value) => String.valueOf(value)
    case DoubleConstantInfo(value) => String.valueOf(value)
    case NameAndTypeConstantInfo(nameIndex, descriptorIndex) =>
      s"${render(nameIndex)}:${render(descriptorIndex)}"
    case Utf8ConstantInfo(value) => value
    case MethodHandleConstantInfo(kind, referenceIndex) =>
      s"${MethodHandleKind.Names(kind)} ${render(referenceIndex)}"
    case MethodTypeConstantInfo(descriptorIndex) => render(descriptorIndex)
    case DynamicConstantInfo(bootstrapMethodIndex, nameAndTypeIndex) =>
      s"#$bootstrapMethodIndex:${render(nameAndTypeIndex)}"
    case InvokeDynamicConstantInfo(bootstrapMethodIndex, nameAndTypeIndex) =>
      s"#$bootstrapMethodIndex:${render(nameAndTypeIndex)}"
    case ModuleConstantInfo(nameIndex) => render(nameIndex)
    case PackageConstantInfo(nameIndex) => render(nameIndex)
  }
}
