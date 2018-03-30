import scala.collection.mutable.ListBuffer

/**
  * Created by Sergey on 12.11.2017.
  */
object LevelsSolutions {
  val emptyFigure: Figure = Figure(List())

  def correctQuad(squares: List[Square]): Boolean ={
    squares(0).get(3) +
      squares(1).get(2) +
      squares(2).get(1) +
      squares(3).get(0) == 10
  }

  def correctTriple(squares: List[Square], mode: String): Boolean = {
    val sq0 = squares(0)
    val sq1 = squares(1)
    val sq2 = squares(2)
    mode match {
      case "up and right" => sq0.get(3) + sq1.get(1) + sq2.get(0) <= 10
      case "up and left" => sq0.get(2) + sq1.get(0) + sq2.get(1) <= 10
      case "down and left" => sq0.get(3) + sq1.get(2) + sq2.get(0) <= 10
      case "down and right" => sq0.get(1) + sq1.get(3) + sq2.get(2) <= 10
    }
  }

  def correctPair(squares: List[Square], mode: String): Boolean  ={
    val square1 = squares(0)
    val square2 = squares(1)
    mode match {
      case "up" => (square1.get(1) + square2.get(0)) <= 10
      case "right" => square1.get(3) + square2.get(1) <= 10
      case "left" => square1.get(2) + square2.get(0) <= 10
      case "down" => square1.get(3) + square2.get(2) <= 10
    }
  }


  def swap(figure: Figure, ind1: Int, ind2: Int): Figure = {
    val buff = figure.get(ind1)
    figure.updated(ind1, figure.get(ind2)).updated(ind2, buff)
  }


  def getFirstLevelSolutions(startFigure: Figure): List[Figure] = {
    var firstLevelSolutions: ListBuffer[Figure] = ListBuffer()
    try {
      val currentSquares = startFigure.squares
      for (square1 <- startFigure.squares) {
        val list1 = swap(startFigure, currentSquares.indexOf(square1), 3)
        for (square2 <- currentSquares.filter(x => x != square1)) {
          val list2 = swap(list1, list1.indexOf(square2), 4)
          for (square3 <- currentSquares.filter(x => x != square1 && x != square2)) {
            val list3 = swap(list2, list2.indexOf(square3), 7)
            for (square4 <- currentSquares.filter(x => x != square1 && x != square2 && x != square3)) {
              val list4 = swap(list3, list3.indexOf(square4), 8)
              if(correctQuad(list4.kernel)) firstLevelSolutions += list4
            }
          }
        }
      }
      firstLevelSolutions.distinct.toList
    }
    catch {
      case _: OutOfMemoryError => println("Error = " + firstLevelSolutions.size); List()
    }
  }

  def getSecondLevelSolutions(firstLevelSolution: Figure): List[Figure] = {
    var secondLevelSolution: ListBuffer[Figure] = ListBuffer()
    try {
      val otherSquares = firstLevelSolution.filter(x =>
        x != firstLevelSolution.get(3) &&
          x != firstLevelSolution.get(4) &&
          x != firstLevelSolution.get(7) &&
          x != firstLevelSolution.get(8))
      for (square1 <- otherSquares.squares) {
        val list1 = swap(firstLevelSolution, firstLevelSolution.indexOf(square1), 0)
        for (square2 <- otherSquares.filter(x => x != square1).squares) {
          val list2 = swap(list1, list1.indexOf(square2), 1)
          if (correctQuad(list2.topQuad) && correctPair(list2.upPair, "up"))
            secondLevelSolution += list2
        }
      }
      secondLevelSolution.toList
    }
    catch{
      case _: OutOfMemoryError =>
        println("Error = " + secondLevelSolution.size )
        List()
    }
  }

  def getThirdLevelSolutions(secondLevelSolution: Figure): List[Figure] = {
    var thirdLevelSolution: ListBuffer[Figure] = ListBuffer()
    try{
      val otherSquares = secondLevelSolution.get(2) ::
        secondLevelSolution.get(5) ::
        secondLevelSolution.get(6) ::
        secondLevelSolution.get(9) ::
        secondLevelSolution.get(10) ::
        secondLevelSolution.get(11) :: Nil

      for (square1 <- otherSquares){
        val list1 = swap(secondLevelSolution, secondLevelSolution.indexOf(square1), 5)
        for(square2 <- otherSquares.filter(x => x != square1)){
          val list2 = swap(list1, list1.indexOf(square2), 9)
          if(correctQuad(list2.leftQuad) &&
            correctTriple(list2.tripleUR, "up and right") &&
            correctPair(list2.rightPair, "right"))
            thirdLevelSolution += list2
        }
      }
      thirdLevelSolution.toList
    }
    catch{
      case _: OutOfMemoryError =>
        println("Error = " + thirdLevelSolution.size )
        List()
    }
  }

  def getFourthLevelSolutions(thirdLevelSolution: Figure): List[Figure] = {
    var fourthLevelSolution: ListBuffer[Figure] = ListBuffer()
    try{
      val otherSquares = thirdLevelSolution.get(2) ::
        thirdLevelSolution.get(6) ::
        thirdLevelSolution.get(10) ::
        thirdLevelSolution.get(11) :: Nil

      for (square1 <- otherSquares){
        val list1 = swap(thirdLevelSolution, thirdLevelSolution.indexOf(square1), 2)
        for (square2 <- otherSquares.filter(x => x != square1)){
          val list2 = swap(list1, list1.indexOf(square2), 6)
          if(correctQuad(list2.rightQuad) &&
            correctTriple(list2.tripleUL, "up and left") &&
            correctPair(list2.leftPair, "left"))
            fourthLevelSolution += list2
        }
      }

      if(fourthLevelSolution.nonEmpty){
        fourthLevelSolution = fourthLevelSolution.map{ x =>
          if (correctQuad(x.downQuad) &&
            correctTriple(x.tripleDL, "down and left") &&
            correctTriple(x.tripleDR, "down and right") &&
            correctPair(x.downPair, "down")) x
          else {
            val xSwaped = swap(x, 10, 11)
            if (correctQuad(xSwaped.downQuad) &&
              correctTriple(xSwaped.tripleDL, "down and left") &&
              correctTriple(xSwaped.tripleDR, "down and right") &&
              correctPair(xSwaped.downPair, "down")) xSwaped
            else Figure(List())
          }
        }
      }

      fourthLevelSolution.toList
    }
    catch{
      case _: OutOfMemoryError =>
        println("Error = " + fourthLevelSolution.size )
        List()
    }
  }

  def rightSolution(solution: Figure): Boolean = {
    //right pairs(correct)
    val pairs = solution.get(0).get(1) + solution.get(1).get(0) <= 10 &&
      solution.get(2).get(2) + solution.get(6).get(0) <= 10 &&
      solution.get(5).get(3) + solution.get(9).get(1) <= 10 &&
      solution.get(10).get(3) + solution.get(11).get(2) <= 10
    //right triples(correct)
    val triples = solution.get(0).get(2) + solution.get(3).get(0) + solution.get(2).get(1) <= 10 &&
      solution.get(1).get(3) + solution.get(4).get(1) + solution.get(5).get(0) <= 10 &&
      solution.get(6).get(3) + solution.get(7).get(2) + solution.get(10).get(0) <= 10 &&
      solution.get(11).get(1) + solution.get(8).get(3) + solution.get(9).get(2) <= 10
    //right quads(correct)
    val quads = solution.get(0).get(3) + solution.get(1).get(2) + solution.get(3).get(1) + solution.get(4).get(0) == 10 &&
      solution.get(2).get(3) + solution.get(3).get(2) + solution.get(6).get(1) + solution.get(7).get(0) == 10 &&
      solution.get(3).get(3) + solution.get(4).get(2) + solution.get(7).get(1) + solution.get(8).get(0) == 10 &&
      solution.get(4).get(3) + solution.get(5).get(2) + solution.get(8).get(1) + solution.get(9).get(0) == 10 &&
      solution.get(7).get(3) + solution.get(8).get(2) + solution.get(10).get(1) + solution.get(11).get(0) == 10
    pairs && triples && quads
  }

}
