package io.kaitai.struct

import io.kaitai.struct.languages.{PythonCompiler, JavaCompiler, RubyCompiler}

object Main {
  def main(args : Array[String]): Unit = {
    if (args.length != 3) {
      Console.println("Usage: compiler <lang> <input.yaml> <output.lang>")
      return
    }

    val langCompiler = args(0) match {
      case "java" => new JavaCompiler(args(2))
      case "python" => new PythonCompiler(args(2))
      case "ruby" => new RubyCompiler(args(2))
    }

    val compiler = new ClassCompiler(args(1), langCompiler)
    compiler.compile
  }
}
