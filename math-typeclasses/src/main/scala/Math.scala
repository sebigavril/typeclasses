package math

object Math {

  trait Addable[T] {
    def zero: T
    def +(toSum: T): T
  }

  case class Vector(x: Double, y: Double, z: Double) extends Addable[Vector] {
    def zero = Vector(0.0, 0.0, 0.0)

    def +(v: Vector) = Vector(x + v.x, y + v.y, z + v.z)
  }
  object Vector {
    val Zero = Vector(0.0, 0.0, 0.0)
  }

  case class Matrix(data: Seq[Seq[Double]]) extends Addable[Matrix] {
    def zero = Matrix.zero(data.size)

    def +(m: Matrix) = {
      require(data.size == m.data.size)
      (data zip m.data).map { case (a, b) =>
        require (a.size == b.size)
      }

      val resData = (data zip m.data).map { case (a, b) =>
        (a.zip(b).map(x => x._1 + x._2))
      }

      Matrix(resData)
    }
  }
  object Matrix {
    def zero(length: Int) = Matrix(Seq.fill(length)(Seq.fill(length)(0)))
  }
}
