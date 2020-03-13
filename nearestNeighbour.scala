

package main.scala


import spire.math.Point

import scala.math._

case class Person(name: String, age : Double, income: Double, cards: Double,
label: Int, squaredDistance:Double = 0, reciprocal : Double = 0, contribution: Double = 0, centroidId: Int= 0)
case class Point(x: Double, y: Double, z: Double)

object  nearestNeighbour extends App {
  println(s"Cluster test set ${new java.util.Date()}")
  type S = String ; type D = Double ; type VD = Vector[D]; type VP = Vector[Person];type I = Int

  def squaredDistance(pt1 : Point, pt2: Point):D = { pow(pt1.x - pt2.x,2) + pow(pt1.y - pt2.y,2) + pow(pt1.z - pt2.z,2) }
  def personTargetSquaredDistance(persons: VP, target: Person):VP= {
    persons.map{ p => {
      val pt1 = Point(p.age, p.income, p.cards )
      val pt2 = Point(target.age, target.income, target.cards)
      val d2 = squaredDistance(pt1, pt2)
      p.copy(squaredDistance = d2)
    }
    }
  }
  def recip(persons: Vector[Person]):Vector[Person]= persons.map{ p =>{
    val r = p.squaredDistance
    val newRecip = if (r != 0.0) 1.0/r else 0.0
    p.copy(reciprocal = newRecip)
  }
  }
  def personContribution( p: Person, contributionBase: D): Person= {
    val c = p.reciprocal/contributionBase
    p.copy(contribution = c)
  }

  val p1 = Person("David", 37, 50, 2, -1)
  val p2 = Person("John", 35, 35, 3, 1)
  val p3 = Person("Rachael", 22, 50, 2, 0)
  val p4 = Person("Ruth", 63, 200, 1, 0)
  val p5 = Person("Jefferson", 59, 170, 1, 0)
  val p6 = Person("Norah", 25, 40, 4, 1)
  val persons = Vector(p1,p2, p3, p4, p5, p6)
  println(persons)
  val personsD2 = personTargetSquaredDistance(persons , p1)
  println(personsD2)
  val personsRecip = recip(personsD2)
  println(personsRecip)
  val contributionBase:D = personsRecip.foldLeft(0.0)( (accum, p) => accum + p.reciprocal)
  println(f"""contributionBase: ${contributionBase}%3.3f""")
  val personsContributions = personsRecip.map{ p => personContribution( p, contributionBase)   }
  println(personsContributions)

  val sortedPersons = personsContributions.sortBy(p => (-p.contribution,-p.label ))
  println(sortedPersons)


  sortedPersons.foreach(
    p => println(f" ${p.name} distanceFromTarget:${sqrt(p.squaredDistance)}%3.3f contribution: ${p.contribution}%3.3f label: ${p.label}")
  )
val prob = sortedPersons.filter(_.label == 1)
val probability:D = prob.foldLeft(0.0)((accum,p) => accum + p.contribution)
  println(f""" Probablity that Dave will say Yes is : ${probability}%2.2f""")

}
