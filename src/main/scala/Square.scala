/**
  * Created by Sergey on 12.11.2017.
  */
case class Square(values: List[Int]) {
  override def toString: String = {
    s"${values(0)} ${values(1)} ${values(2)} ${values(3)}"
  }
  def get(index: Int): Int = values(index)
}
