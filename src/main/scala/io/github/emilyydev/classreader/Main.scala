package io.github.emilyydev.classreader

import io.github.emilyydev.classreader.entity.ClassFile

import java.io.{DataInputStream, OutputStreamWriter}
import java.nio.file.{Files, Path}
import scala.util.{Failure, Success, Try, Using}

object Main {

  private val Magic = 0xCAFEBABE

  def main(args: Array[String]): Unit = args.length match {
    case 0 => println("Provide a class file")
    case _ => tryRead(Path.of(args.head))
  }

  def tryRead(classFileFile: Path): Unit = {
    val triedClassFile: Try[ClassFile] = Using.Manager { use =>
      val in = use(Files.newInputStream(classFileFile))
      val dataIn = use(new DataInputStream(in))

      val maybeMagic = dataIn.readInt()
      if (maybeMagic != Magic) {
        throw new IllegalArgumentException("Not a class file")
      } else {
        ClassFile.read(dataIn)
      }
    }

    triedClassFile match {
      case Failure(exception) =>
        Console.err.print(Console.RED)
        exception.printStackTrace(Console.err)
        Console.err.print(Console.RESET)
        Console.err.flush()
      case Success(classFile) =>
        val writer = new IndentingWriter(new OutputStreamWriter(Console.out, System.console().charset()))
        classFile.render(writer)
        writer.flush()
    }
  }
}
