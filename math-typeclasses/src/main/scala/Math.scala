package math

object Math {

  case class Vector(x: Double, y: Double, z: Double)
  case class Matrix(data: Seq[Seq[Double]])


  trait Addable[T] {
    def zero: T
    def +(a: T, b: T): T
  }

  implicit class AddableOps[T :Addable](data: T) {
    def +(a: T): T  = implicitly[Addable[T]].+(data, a)
  }


  implicit object VectorOps extends Addable[Vector] {
    def zero = Vector(0.0, 0.0, 0.0)

    def +(a: Vector, b: Vector) = Vector(a.x + b.x, a.y + b.y, a.z + b.z)
  }

  implicit class MatrixOps(length: Int) extends Addable[Matrix] {
    def zero = Matrix(Seq.fill(length)(Seq.fill(length)(0)))

    def +(a: Matrix, b: Matrix) = {
      require(a.data.size == b.data.size)
      (a.data zip b.data).map { case (aa, bb) =>
        require (aa.size == bb.size)
      }

      val resData = (a.data zip b.data).map { case (aa, bb) =>
        (aa.zip(bb).map(x => x._1 + x._2))
      }

      Matrix(resData)
    }
  }
}
