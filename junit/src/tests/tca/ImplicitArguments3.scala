package tests.tca

import callgraph.annotation.target
import callgraph.annotation.invocations

object ImplicitArguments3 {

  sealed case class C (
                        a : String = "foo",
                        b : Int = 17,
                        c : Any = "bar"
  )
  
  @target("foo") def foo(p : C) {
    p match {
      case C(a,b,c) => println(a + "," + b + "," + c)
    }
  }
  
  @invocations("28: <unannotated> tests.tca.ImplicitArguments3.C: <init>$default$1()",
               "28: <unannotated> tests.tca.ImplicitArguments3.C: <init>$default$2()",
               "28: <unannotated> tests.tca.ImplicitArguments3.C: <init>(a: String,b: Int,c: Any)",
               "31: <unannotated> tests.tca.ImplicitArguments3.C: <init>$default$1()",
               "31: <unannotated> tests.tca.ImplicitArguments3.C: <init>$default$2()",
               "31: <unannotated> tests.tca.ImplicitArguments3.C: <init>$default$3()",
               "31: <unannotated> tests.tca.ImplicitArguments3.C: <init>(a: String,b: Int,c: Any)")
  def main(args: Array[String]) {
    val x = new C(c = "zap"); // TODO: add assertion on constructor call
    {"foo"; this}.foo(x)

    val y = new C(); // TODO: add assertion on constructor call
    {"foo"; this}.foo(y)
  }
  
  def fail(){}


  
}