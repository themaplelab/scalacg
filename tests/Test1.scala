class Test1 {
  @target("Test1.callee") def callee(): Int = {
    5
  }

  {
    "Test1.callee";
    new Test1
  }.callee()
}
