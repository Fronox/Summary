/**
  * Created by Sergey on 12.11.2017.
  */
case class Figure(squares: List[Square]) {
  lazy val kernel: List[Square] = squares(3) :: squares(4) :: squares(7) :: squares(8) :: Nil

  lazy val topQuad: List[Square] = squares.take(2) ::: List(squares(3), squares(4))

  lazy val leftQuad: List[Square] = squares(4) :: squares(5) :: squares(8) :: squares(9) :: Nil

  lazy val rightQuad: List[Square] = squares(2) :: squares(3) :: squares(6) :: squares(7) :: Nil

  lazy val downQuad: List[Square] = squares(7) :: squares(8) :: squares(10) :: squares(11) :: Nil

  lazy val tripleUL: List[Square] = squares(0) :: squares(3) :: squares(2) :: Nil

  lazy val tripleUR: List[Square] = squares(1) :: squares(4) :: squares(5) :: Nil

  lazy val tripleDL: List[Square] = squares(6) :: squares(7) :: squares(10) :: Nil

  lazy val tripleDR: List[Square] = squares(11) :: squares(8) :: squares(9) :: Nil

  lazy val upPair: List[Square] = squares.take(2)

  lazy val leftPair: List[Square] = squares(2) :: squares(6) :: Nil

  lazy val rightPair: List[Square] = squares(5) :: squares(9) :: Nil

  lazy val downPair: List[Square] = squares(10) :: squares(11) :: Nil

  def get(index: Int): Square = squares(index)

  def indexOf(square: Square): Int = squares.indexOf(square)

  def filter(predicate: Square => Boolean): Figure = Figure(squares.filter(predicate))

  override def toString: String = {
    squares.mkString("\n") + "\n"
  }
  def nonEmpty: Boolean = squares.nonEmpty

  def isEmpty: Boolean = squares.isEmpty

  def updated(index: Int, square: Square) = Figure(squares.updated(index, square))
}
