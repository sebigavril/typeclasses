import org.scalatest._
import math.Math.{Addable, Vector, Matrix}
import math.Math.{AddableOps}

class MathSpec extends WordSpecLike with Matchers {

  "Math implemented via typeclasses" when {

    "working with vectors" should {
      "add them" in {
        import math.Math.VectorOps
        Vector(1, 1, 1) +
        Vector(2, 2, 2) shouldEqual
        Vector(3, 3, 3)
      }
      "add zero" in {
        import math.Math.VectorOps
        Vector(1, 1, 1) +
        VectorOps.zero  shouldEqual
        Vector(1, 1, 1)
      }

      "consider them 2d vetors" in {
        implicit object twoDVectorOps extends Addable[Vector] {
          def zero = math.Math.VectorOps.zero
          def +(a: Vector, b: Vector) = Vector(a.x + b.x, a.y + b.y, 0)
        }

        Vector(1, 1, 1) +
        Vector(2, 2, 2) shouldEqual
        Vector(3, 3, 0)
      }
    }

    "working with matrices" should {

      import math.Math.MatrixOps
      implicit val matrixOps = new MatrixOps(2)

      "add them" in {
        Matrix(Seq(Seq(1, 1), Seq(1, 1))) +
        Matrix(Seq(Seq(2, 2), Seq(2, 2))) shouldEqual
        Matrix(Seq(Seq(3, 3), Seq(3, 3)))
      }
      "add zero" in {
        Matrix(Seq(Seq(1, 1), Seq(1, 1))) +
        matrixOps.zero                    shouldEqual
        Matrix(Seq(Seq(1, 1), Seq(1, 1)))
      }

      "throw if attempting to add a matrix of different size" in {
        an [IllegalArgumentException] should be thrownBy
        Matrix(Seq(Seq(1, 1), Seq(1, 1)))                     +
        Matrix(Seq(Seq(1, 1, 1), Seq(1, 1, 1), Seq(1, 1, 1)))
      }
    }
  }
}
