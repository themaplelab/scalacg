package tests.tca

import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.FileInputStream
import java.io.FileNotFoundException
import callgraph.annotation.target

object ImplicitArguments1 {

  @target("filereader") def filereader (name : String, encoding : String = "UTF-8") : BufferedReader =
        try {
            new BufferedReader (
                new InputStreamReader (
                    new FileInputStream (name),
                    encoding))
        } catch {
            case e : java.io.FileNotFoundException =>
                throw new FileNotFoundException (e.getMessage)
  }
  
  def main(args: Array[String]): Unit = {
    val reader = { "filereader"; this}.filereader ("foo")
  }

}