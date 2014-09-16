package org.scalalabs.basic.lab02
/**
 * This Lab contains exercises where the usage of
 * higher order collection methods can be rehearsed.
 */
import sys._

object CollectionExercise01 {

  /**
   * Taken from: <a href="http://code.google.com/codejam/contest/1460488/dashboard">Problem A. Speaking in Tongues</a>
   *
   * Problem
   * The aim of this task is to translate a language into a new language called Googlerese.
   * To translate we take any message and replace each English letter with another English letter.
   * This mapping is one-to-one and onto, which means that the same input letter always gets replaced
   * with the same output letter, and different input letters always get replaced with different output letters.
   * A letter may be replaced by itself. Spaces are left as-is.
   *
   * For example (and here is a hint!), the translation algorithm includes the following three mappings:
   * 'a' -> 'y', 'o' -> 'e', and 'z' -> 'q'. This means that "a zoo" will become "y qee".
   *
   * Sample Input/Output
   * Input:
   * Case 1: ejp mysljylc kd kxveddknmc re jsicpdrysi
   * Case 2: rbcpc ypc rtcsra dkh wyfrepkym veddknkmkrkcd
   * Case 3: de kr kd eoya kw aej tysr re ujdr lkgc jv
   *
   * Output:
   * Case 1: our language is impossible to understand
   * Case 2: there are twenty six factorial possibilities
   * Case 3: so it is okay if you want to just give up
   *
   */
  def googleCodeJamGooglerese(lines: String*): Seq[String] = {
    val string1a : String = "ejp mysljylc kd kxveddknmc re jsicpdrysi"
    val string1b : String = "our language is impossible to understand"
      
    val string2a : String = "rbcpc ypc rtcsra dkh wyfrepkym veddknkmkrkcd"
    val string2b : String = "there are twenty six factorial possibilities"
      
    val string3a : String = "de kr kd eoya kw aej tysr re ujdr lkgc jv"
    val string3b : String = "so it is okay if you want to just give up"
      
    val string1zip = string1a.zip(string1b)
    val string2zip = string2a.zip(string2b)
    val string3zip = string3a.zip(string3b)
    
    val cypher : Map[Char, Char] = Map() ++ string1zip ++ string2zip ++ string3zip
    
    lines.map(_.map(cypher(_)))
  }
}
/*========================================================== */

object CollectionExercise02 {

  class Person(val age: Int, val name: String)

  /**
   * Take a look at the java class: {@link ImperativeSample}. The
   * groupAdultsPerAgeGroup is implemented using an imperative programming
   * style.
   * Rewrite the method groupAdultsPerAgeGroup in the ImperativeSample java class
   * using a functional approach.
   */
  def groupAdultsPerAgeGroup(persons: Seq[Person]): Map[Int, Seq[Person]] = {
   (persons.filter(_.age > 17).sortBy(_.name)).groupBy(_.age/10*10)
  }
}

/*========================================================== */

object CollectionExercise03 {
  /**
   * Create a method that checks that each subsequent value is greater than
   * the previous one.
   * E.g.:
   * checkValuesIncrease(Seq(1,2,3)) == true
   * checkValuesIncrease(Seq(1,2,2)) == false
   */
  def checkValuesIncrease[T <% Ordered[T]](seq: Seq[T]): Boolean =
   seq.sliding(2,1).forall{x: (Seq[T]) => if (x.length>1) x(1) > x(0) else true}

}
/*========================================================== */

object CollectionExercise04 {
  /**
   * Calculate the length of the longest word in a list of sentences.
   * To keep it simple it's ok to use String.split to extract all words of a sentence.
   */
  def calcLengthLongestWord(lines: String*): Int = {
    lines.map(_.split(" ")).flatten.map(_.length()).max
  }
}



