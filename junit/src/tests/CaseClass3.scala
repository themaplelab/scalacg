package tests

object CaseClass3 {
  
  case class Settings(val inlineMode: InlineMode = InlineManual);
  
  sealed abstract class InlineMode;
  case object InlineNone   extends InlineMode;
  case object InlineLeaves extends InlineMode;
  case object InlineFull   extends InlineMode;
  case object InlineManual extends InlineMode;
  
  object Settings {
    private var stgs: Option[Settings] = None

    @target("get") def get = stgs.getOrElse(throw new RuntimeException("No global settings defined"))

    @target("set") def set(stgs: Settings) = this.stgs = Some(stgs)
}
  
  
  def main(args: Array[String]): Unit = {
    { "set"; Settings}.set(Settings(InlineNone));
    val x = { "get"; Settings}.get;
    println(x.inlineMode);  
  }

}