package tests

import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.FileInputStream
import java.io.FileNotFoundException

object ImplicitArgument1 {

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