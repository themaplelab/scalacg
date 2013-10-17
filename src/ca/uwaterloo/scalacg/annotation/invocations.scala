

package ca.uwaterloo.scalacg.annotation

import scala.annotation.StaticAnnotation

/**
 * The annotation specifies a subset of the methods called from the annotated method. Each annotation parameter
 * contains the line number of the invoking call site and the name of the invoked method.
 * 
 * Example:
 * @invocations("5: name1", "6: name2")
 * def M { ... }
 * 
 * Indicates that M contains a call site in line 5 that resolves to method "name1",
 * and a call site in line 6 that resolves to method "name2".
 */
class invocations(names: String*) extends StaticAnnotation