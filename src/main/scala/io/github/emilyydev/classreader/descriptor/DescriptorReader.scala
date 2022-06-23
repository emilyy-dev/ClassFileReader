package io.github.emilyydev.classreader.descriptor

/**
 * Utility object to convert from raw java type descriptors (also called "JDI type signatures") into a format
 * more similar to that used in Java source code
 *
 * @author rymiel
 */
object DescriptorReader {
  def field(descriptor: String): String = convertField(descriptor)._1

  def convertField(descriptor: String): (String, Int) = {
    val typeChar = descriptor(0)
    typeChar match {
      case 'B' => ("byte", 1)
      case 'C' => ("char", 1)
      case 'D' => ("double", 1)
      case 'F' => ("float", 1)
      case 'I' => ("int", 1)
      case 'J' => ("long", 1)
      case 'S' => ("short", 1)
      case 'Z' => ("boolean", 1)
      case 'L' =>
        val terminator = descriptor.indexOf(';')
        (descriptor.substring(1, terminator).replace('/', '.'), terminator + 1)
      case '[' => convertField(descriptor.substring(1)) match {
        case (name, offs) => (name + "[]", offs + 1)
      }
      case _ => throw new NotImplementedError
    }
  }

  def method(descriptor: String): String = {
    assert(descriptor(0) == '(')
    var runningOffset = 1
    val args: Array[String] = {
      val builder = Array.newBuilder[String]
      while (descriptor(runningOffset) != ')') {
        val (name, size) = convertField(descriptor.substring(runningOffset))
        builder += name
        runningOffset += size
      }
      builder.result()
    }
    val returnType: String = if (descriptor(runningOffset + 1) == 'V')
      "void"
    else
      field(descriptor.substring(runningOffset + 1))

    args.mkString(s"$returnType (", ", ", ")")
  }
}
