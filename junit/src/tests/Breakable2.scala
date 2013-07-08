package tests

import scala.util.control.Breaks._
import callgraph.annotation.invocations

/**
 * Test case created while debugging missing edges in source-level call graph compared to bytecode-level call graph.
 * The missing edges are basically all those from the constructor of DumpCollector. The reason is that the constructor
 * probably divided across multiple classes that gets created later on in the compiler ($anonfun$1, $anonfun$2) to model
 * the breakable parts. The general problem we're facing here is that some call sites in the Scala source where we actually
 * calculate the edges from it, however we do not include in the call graph because they do not belong to a "reachable"
 * method.
 *
 * June, 6th, 2013.
 *
 * @author Karim Ali
 */
object Breakable2 {

  @invocations("22: <unannotated> tests.Breakable2.DumpCollector: <init>(path: String,ctx: String)")
  def main(args: Array[String]) = {
    val dc = new DumpCollector("path", "context")
  }

  /* Karim: These invocations are all on methods that can't be annotated in the test case here, even with the @reachable
   * annotation. That's why I've commented them out here. That's all because we now do our analysis after uncurry.
   * @invocations("31: <unannotated> java.lang.Object: <init>()",
               "49: <unannotated> java.io.File: <init>(x$1: String)",
               "67: <unannotated> scala.Tuple2: <init>(_1: T1,_2: T2)",
               "80: <unannotated> scala.Tuple2: <init>(_1: T1,_2: T2)",
               "88: <unannotated> tests.Breakable2.Unserializer: <init>(content: String,ctx: String)",
               "89: <unannotated> tests.Breakable2.Unserializer: <init>(content: String,ctx: String)")*/
  case class DumpCollector(path: String, ctx: String) {
    private val content = List[String]("1", "2", "3", "4")
    var lineNr = 3

    var files: List[String] = Nil
    var functions: Map[String, (String, Int)] = Map()
    var classes: Map[String, (String, Int)] = Map()

    def restore(str: String): String = {
      "restore"
    }

    breakable {
      for (l <- content.drop(lineNr)) {
        if (l.startsWith("#")) break
        l.split(":", 2).toList match {
          case fmtime :: file :: Nil =>
            // Check timestamp against the file
            val f = new java.io.File(file)
            val t = f.lastModified / 1000
            if (t > fmtime.toLong) {
              println("File '" + file + "' modified after dumping, could result is mismatched line numbers");
            }
            files = file :: files
          case _ =>
        }
      }
    }

    lineNr += files.size + 1

    breakable {
      for (l <- content.drop(lineNr)) {
        if (l.startsWith("#")) break
        l.split(":", 3).toList match {
          case name :: line :: file :: Nil =>
            functions += name -> (file, line.toInt)
          case _ =>
        }
      }
    }

    lineNr += functions.size + 1

    breakable {
      for (l <- content.drop(lineNr)) {
        if (l.startsWith("#")) break
        l.split(":", 3).toList match {
          case name :: line :: file :: Nil =>
            classes += name -> (file, line.toInt)
          case _ =>
        }
      }
    }

    lineNr += classes.size + 1

    val constants: Unserializer = new Unserializer(restore(content(lineNr)), ctx)
    val heap: Unserializer = new Unserializer(restore(content(lineNr + 2)), ctx)

  }

  class Unserializer(content: String, ctx: String)

}