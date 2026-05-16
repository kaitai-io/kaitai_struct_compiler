package io.kaitai.struct.formats

import io.kaitai.struct.Log
import io.kaitai.struct.format.{ClassSpec, ClassSpecs}
import io.kaitai.struct.problems.ErrorInInput

import java.io.{File, FileNotFoundException, InputStream, InputStreamReader, BufferedReader, IOException}
import java.net.{URI, HttpURLConnection}
import java.nio.charset.StandardCharsets
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.collection.concurrent
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

/**
  * Java implementation of ClassSpec container, doing imports from local files.
  */
class JavaClassSpecs(relPath: String, absPaths: Seq[String], firstSpec: ClassSpec)
  extends ClassSpecs(firstSpec) {

  // We're using thread-safe `ConcurrentHashMap` for `relFiles` and `absFiles`,
  // because these hash maps may be mutated concurrently by multiple threads in
  // `JavaClassSpecs.cached()`. Using a non-thread-safe hash map here could
  // occasionally cause `cacheMap.get(name)` in `JavaClassSpecs.cached()` to
  // fail internally and throw an `ArrayIndexOutOfBoundsException`, see
  // https://github.com/kaitai-io/kaitai_struct/issues/951
  private val relFiles: concurrent.Map[String, ClassSpec] = new ConcurrentHashMap[String, ClassSpec]().asScala
  private val absFiles: concurrent.Map[String, ClassSpec] = new ConcurrentHashMap[String, ClassSpec]().asScala
  private val urlFiles: concurrent.Map[String, ClassSpec] = new ConcurrentHashMap[String, ClassSpec]().asScala

  override def importRelative(name: String, path: List[String], inFile: Option[String]): Future[Option[ClassSpec]] = Future {
    Log.importOps.info(() => s".. importing relative $name")
    JavaClassSpecs.cached(path, inFile, relFiles, name, (_) =>
      JavaKSYParser.fileNameToSpec(s"$relPath/$name.ksy")
    )
  }

  override def importAbsolute(name: String, path: List[String], inFile: Option[String]): Future[Option[ClassSpec]] = Future {
    Log.importOps.info(() => s".. importing absolute $name")
    JavaClassSpecs.cached(path, inFile, absFiles, name, tryAbsolutePaths)
  }

  override def importUrl(url: String, inFile: Option[String]): Future[Option[ClassSpec]] = Future {
    Log.importOps.info(() => s".. importing URL $url")
    JavaClassSpecs.cached(List(url), inFile, urlFiles, url, downloadFromUrl)
  }

  def tryAbsolutePaths(name: String): ClassSpec = {
    absPaths.foreach { (path) =>
      val fn = s"$path/$name.ksy"
      val f = new File(fn)
      if (f.exists) {
        if (f.canRead) {
          if (f.isFile) {
            return JavaKSYParser.fileNameToSpec(fn)
          } else {
            Log.importOps.warn(() => s".... $fn exists, but is not a regular file, skipping")
          }
        } else {
          Log.importOps.warn(() => s".... $fn exists, but not readable, skipping")
        }
      }
    }
    throw new FileNotFoundException(s"unable to find '$name.ksy' in import search paths, using: $absPaths")
  }

  def downloadFromUrl(url: String): ClassSpec = {
    Log.importOps.info(() => s".... downloading from $url")
    val connection = new URI(url).toURL().openConnection().asInstanceOf[HttpURLConnection]
    try {
      connection.setRequestMethod("GET")
      connection.setConnectTimeout(10000)
      connection.setReadTimeout(30000)
      connection.connect()

      val responseCode = connection.getResponseCode
      if (responseCode != 200) {
        throw new IOException(s"HTTP error $responseCode for URL: $url")
      }

      val inputStream: InputStream = connection.getInputStream
      try {
        val reader = new BufferedReader(new InputStreamReader(inputStream, StandardCharsets.UTF_8))
        try {
          val scalaSrc = JavaKSYParser.readerToYaml(reader)
          ClassSpec.fromYaml(scalaSrc, Some(url))
        } finally {
          reader.close()
        }
      } finally {
        inputStream.close()
      }
    } finally {
      connection.disconnect()
    }
  }
}

object JavaClassSpecs {
  def cached(
    path: List[String],
    inFile: Option[String],
    cacheMap: mutable.Map[String, ClassSpec],
    name: String,
    importOp: (String) => ClassSpec
  ): Option[ClassSpec] = {
    // Have we loaded it previously?
    cacheMap.get(name) match {
      case Some(_) =>
        // Yes, it's already loaded and processed, nothing new here
        Log.importOps.info(() => s".... cached")
        None
      case None =>
        // Nope, let's import it
        try {
          val spec = importOp(name)
          cacheMap(name) = spec
          Some(spec)
        } catch {
          case err: Throwable => throw ErrorInInput(err, path, inFile).toException
        }
    }
  }
}
