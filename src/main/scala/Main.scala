import java.io.{FileNotFoundException, IOException}

import scala.io.{Source, StdIn}

object Main {
  def main(args: Array[String]): Unit = {
    val fileName: String = if(args.length != 0) args(0) else {
      println("Input file name")
      StdIn.readLine()
    }
    try {
      val mainSquares: List[Square] = inputFileData(fileName)
      val solution = Solution(mainSquares)
      solution.startSolution()
    }
    catch {
      case _: FileNotFoundException => println("File not found")
      case _: IOException => println("Input/Output exception")
    }
  }

  @throws(classOf[FileNotFoundException])
  @throws(classOf[IOException])
  def inputFileData(filename: String): List[Square] ={
    val data = Source.fromFile(filename).getLines().map(x => x.split(" ").map(x => x.toInt))
    data.map(x => Square(x.toList)).toList
  }

}
