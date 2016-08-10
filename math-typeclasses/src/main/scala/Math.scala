package math

object Math {

  case class Vector(x: Double, y: Double, z: Double)
  case class Matrix(data: Seq[Seq[Double]])


  trait Addable[T] {
    def zero: T
    def +(a: T, b: T): T
  }

  implicit class AddableOps[T](data: T)(implicit ops: Addable[T]) {
    def zero: T     = ops.zero
    def +(a: T): T  = ops.+(data, a)
  }

  implicit object VectorOps extends Addable[Vector] {
    def zero = Vector(0.0, 0.0, 0.0)

    def +(a: Vector, b: Vector) = Vector(a.x + b.x, a.y + b.y, a.z + b.z)
  }
  object Vector {
    val Zero = Vector(0.0, 0.0, 0.0)
  }

  implicit class MatrixOps(length: Int) extends Addable[Matrix] {
    def zero = Matrix.zero(length)

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
  object Matrix {
    def zero(length: Int) = Matrix(Seq.fill(length)(Seq.fill(length)(0)))
  }
}
