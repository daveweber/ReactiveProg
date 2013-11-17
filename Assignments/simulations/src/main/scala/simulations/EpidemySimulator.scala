package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
    // Base rules
    val prevalenceRate = 0.01
    val transmissibilityRate = 0.4
    val moveDelay = randomBelow(5) + 1
    val incubationDelay = 6
    val deathDelay = 14
    val deathChance = 0.25
    val immuneDelay = 16
    val healthyDelay = 18

    // Add-ins
    val airplaneMode = false
    val airplaneChance = 0.01
    
    val mobilityMode = false
    
    val choseFewMode = false
    val vipChance = 0.05 
  }

  import SimConfig._

  val persons: List[Person] = for {
    i <- (0 until population).toList
  } yield {
    val person = new Person(i)
    if (i < population * prevalenceRate) person.setInfected
    person.mode
    person
  }

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    //
    // to complete with simulation logic
    //
    def setInfected {
      infected = true
      afterDelay(incubationDelay)(setSick)
      afterDelay(deathDelay)(setDead)
      afterDelay(immuneDelay)(setImmune)
      afterDelay(healthyDelay)(setHealthy)
    }

    def setSick = sick = true

    def setDead = if (random < deathChance) dead = true

    def setImmune {
      if (dead) return
      sick = false
      immune = true
    }

    def setHealthy {
      if (dead) return
      immune = false
      infected = false
    }

    def mode = afterDelay(moveDelay)(move)

    def move {
      // Fail fast: can't move if dead
      if (dead) return

      // Get neighboring rooms and find one without a visibly infectious person 
      val neighbors = List(
        ((row - 1 + roomRows) % roomRows, col),
        ((row + 1) % roomRows, col),
        (row, (col - 1 + roomColumns) % roomColumns),
        (row, (col + 1) % roomColumns)
      )

      // Avoid rooms with sick or dead (visibly infectious) people
      def notVisiblyInfected(room: (Int, Int)): Boolean = room match {
        case (x, y) => (persons.find {
          person => person.row == x && person.col == y && (person.sick || person.dead)
        }).isEmpty
      }

      def validRooms = neighbors filter notVisiblyInfected

      // Move only when there's a valid room to go to
      if (!validRooms.isEmpty) {
        val destination = validRooms(randomBelow(validRooms.length))
        destination match {
          case (x, y) => {
            row = x
            col = y
          }
        }
      }
      
      if (!immune && !infected)
        if (random < transmissibilityRate)
          if (!(persons.find{person => person.row == row && person.col == col && person.infected}).isEmpty)
            setInfected

      // Begin countdown for next move
      mode
      
    }

  }

}