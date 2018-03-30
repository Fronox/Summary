import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Created by Sergey on 12.11.2017.
  */
case class Solution(startSquares: List[Square]) {
  var rightSolutions: ListBuffer[Figure] = ListBuffer()

  def startSolution(): Unit ={
    println("Please wait, solution is calculating...\n")
    val startFigure: Figure = Figure(startSquares)
    val firstLevelSolution = LevelsSolutions.getFirstLevelSolutions(startFigure)
    if(firstLevelSolution.isEmpty) println("Solutions is not found")
    else {
      firstLevelSolution.foreach(x => createFirstLevelFuture(x))
      Thread.sleep(firstLevelSolution.size * 2)
      if(rightSolutions.isEmpty) println("Solutions is not found")
      else {
        rightSolutions.foreach(x => println(x))
      }
    }
  }

  def createFirstLevelFuture(beforeSolution: Figure): Unit ={
    Future {
      val secondLevelSolutions = LevelsSolutions.getSecondLevelSolutions(beforeSolution)
      if(secondLevelSolutions.nonEmpty)
        secondLevelSolutions.filter(x => x.nonEmpty).foreach(x => createSecondLevelFuture(x))
    }
  }

  def createSecondLevelFuture(beforeSolution: Figure): Unit ={
    Future{
      val thirdLevelSolutions = LevelsSolutions.getThirdLevelSolutions(beforeSolution)
      if(thirdLevelSolutions.nonEmpty)
        thirdLevelSolutions.filter(x => x.nonEmpty).foreach(x => createThirdLevelFuture(x))
    }
  }

  def createThirdLevelFuture(beforeSolution: Figure): Unit ={
    Future{
      val fourthLevelSolutions = LevelsSolutions.getFourthLevelSolutions(beforeSolution)
      if(fourthLevelSolutions.nonEmpty)
        fourthLevelSolutions.filter(x => x.nonEmpty).foreach(x => rightSolutions += x)
    }
  }


}
