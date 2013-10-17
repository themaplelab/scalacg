package tests.tca

object CaseClassCopy {

  case class Persona(serviceName: String, serviceId: String, sentMessages: Set[String]) {
  }

  def main(args: Array[String]) = {
    val existingPersona = Persona("service", "id", Set("1", "2", "3"))
    val newPersona = existingPersona.copy(sentMessages = existingPersona.sentMessages + "4")
  }

}