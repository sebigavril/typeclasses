import org.scalatest._
import math.Math.{Vector, Matrix}
import math.Math.{VectorOps, MatrixOps}

class MathSpec extends WordSpecLike with Matchers {

  "Math implemented via typeclasses" when {

    "working with vectors" should {
      "add them" in {
        VectorOps.+(Vector(1, 1, 1), Vector(2, 2, 2)) shouldEqual
        Vector(3, 3, 3)
      }
      "add zero" in {
        VectorOps.+(Vector(1, 1, 1), Vector.Zero) shouldEqual
        Vector(1, 1, 1)
      }
    }

    "working with matrices" should {
      "add them" in {
        new MatrixOps(2).+(Matrix(Seq(Seq(1, 1), Seq(1, 1))), Matrix(Seq(Seq(2, 2), Seq(2, 2)))) shouldEqual
        Matrix(Seq(Seq(3, 3), Seq(3, 3)))
      }
      "add zero" in {
        new MatrixOps(2).+(Matrix(Seq(Seq(1, 1), Seq(1, 1))), Matrix.zero(2)) shouldEqual
        Matrix(Seq(Seq(1, 1), Seq(1, 1)))
      }

      "throw if attempting to add a matrix of different size" in {
        an [IllegalArgumentException] should be thrownBy
        new MatrixOps(2).+(Matrix(Seq(Seq(1, 1), Seq(1, 1))), Matrix(Seq(Seq(1, 1, 1), Seq(1, 1, 1), Seq(1, 1, 1))))
      }
    }
  }
}
