package math

object Math {

  trait Addable {
    def zero: Addable
    def +(toSum: Addable): Addable
  }

  case class Vector(x: Double, y: Double, z: Double) extends Addable {
    def zero = Vector(0.0, 0.0, 0.0)

    def +(toSum: Addable) = {
      toSum match {
        case v: Vector => Vector(x + v.x, y + v.y, z + v.z)
        case _         => throw new IllegalArgumentException(s"I need a $Vector")
      }
    }
  }
  object Vector {
    val Zero = Vector(0.0, 0.0, 0.0)
  }

  case class Matrix(data: Seq[Seq[Double]]) extends Addable {
    def zero = Matrix.zero(data.size)

    def +(toSum: Addable) = {
      toSum match {
        case m: Matrix => addMatrix(m)
        case _         => throw new IllegalArgumentException(s"I need a $Matrix")
      }
    }

    private def addMatrix(m: Matrix) = {
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
