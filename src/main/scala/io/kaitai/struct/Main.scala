package io.kaitai.struct

import java.io.File

import io.kaitai.struct.languages.{PythonCompiler, JavaCompiler, RubyCompiler}

object Main {
  def allInputFilesInDir(dir: String): List[String] = {
    val f = new File(dir)
    f.listFiles.filter((f) =>
      f.isFile && f.getName.endsWith(".ksy")
    ).map(_.toString).toList
  }

  def main(args : Array[String]): Unit = {
    if (args.length != 3) {
      Console.println("Usage: compiler <lang> <input.yaml> <output.lang>")
      return
    }

    val langCompiler = args(0) match {
      case "all" =>
        val outDir = args(2)
        allInputFilesInDir(args(1)).foreach((fn) => {
          val origId = new File(fn).getName.replace(".ksy", "")
          new ClassCompiler(fn, new JavaCompiler(s"${outDir}/java")).compile
          new ClassCompiler(fn, new PythonCompiler(s"${outDir}/python/${origId}.py")).compile
          new ClassCompiler(fn, new RubyCompiler(s"${outDir}/ruby/${origId}.rb")).compile
        })
        return
      case "java" => new JavaCompiler(args(2))
      case "python" => new PythonCompiler(args(2))
      case "ruby" => new RubyCompiler(args(2))
    }

    val compiler = new ClassCompiler(args(1), langCompiler)
    compiler.compile
  }
}
