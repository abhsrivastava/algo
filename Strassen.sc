// multiply a matrix using Strassen Algorithm
@main
def main() = {
    val matrix1 = List(
        List(1,2,3,4),
        List(5,6,7,8),
        List(9,10,11,12),
        List(13,14,15,16)
    )

    val matrix2 = List(
        List(17,18,19,20),
        List(21,22,23,24),
        List(25,26,27,28),
        List(29,30,31,32)
    )

    matrix1 * matrix2

    val matrix3 = List(
        List(1,2,3),
        List(4,5,6),
        List(7,8,9)
    )

    val matrix4 = List(
        List(10, 11, 12),
        List(13, 14, 15),
        List(16, 17, 18)
    )

    matrix3 * matrix4

}

implicit class MatrixOps(matrix1: List[List[Int]]) {

    // return 4 matrix. ABCD
    def split(matrix: List[List[Int]]) : List[List[List[Int]]] = {
        val size = matrix.size
        val mid = size / 2
        val t1 = matrix.map(_.splitAt(mid))
        val t2 = t1.map(_._1)
        val t3 = t1.map(_._2)
        val t4 = t2.splitAt(mid)
        val t5 = t3.splitAt(mid)
        List(t4._1, t5._1, t4._2, t5._2)
    }

    def *(matrix2: List[List[Int]]) : List[List[Int]] = {
        println(s"came inside $matrix1 $matrix2")
        (matrix1, matrix2) match {
            case (Nil, Nil) => Nil
            case (List(List(a)), List(List(b))) => List(List(a * b))
            case (List(List(a, b)), List(List(c), List(d))) => List(List(a * c + b * d)) 
            case (List(List(a), List(b)), List(List(c, d))) => 
                List(
                    List(a * c, a * d),
                    List(b * c, b * d)
                )
            case (List(List(a1, b1), List(c1, d1)), List(List(a2, b2), List(c2, d2))) =>
                List(
                    List(a1 * a2 + b1 * c2, a1 * b2 + b1 * d2),
                    List(c1 * a2 + d1 * c2, c1 * b2 + d1 * d2)
                )
            case _ => 
                val List(mA, mB, mC, mD) = split(matrix1)
                val List(mE, mF, mG, mH) = split(matrix2)
                val p1 = (mA * mF) - (mA * mH)
                val p2 = (mA * mH) + (mB * mH)
                val p3 = (mC * mE) + (mD * mE)
                val p4 = (mD * mG) - (mD * mE)
                val p5 = (mA * mE) + (mA * mH) + (mD * mE) + (mD * mH)
                val p6 = (mB * mG) + (mB * mH) - (mD * mG) - (mD * mH)
                val p7 = (mA * mE) + (mA * mF) - (mC * mE) - (mC * mF)
                val t1 = p5 + p4 - p2 + p6
                val t2 = p1 + p2
                val m1 = t1.zip(t2).map{case (list1, list2) => list1.foldRight(list2){case (i, acc) => i :: acc}}
                val t3 = p3 + p4
                val t4 = p1 + p5 - p3 - p7
                val m2 = t3.zip(t4).map{case (list1, list2) => list1.foldRight(list2){case (i, acc) => i :: acc}}
                m1.foldRight(m2){case (i, acc) => i :: acc}
        }
    }

    def +(matrix2: List[List[Int]]) : List[List[Int]] = {
        process(matrix2, (a: Int,b : Int) => a + b)
    }

    def -(matrix2: List[List[Int]]) : List[List[Int]] = {
        process(matrix2, (a: Int,b : Int) => a - b)
    }

    def process(matrix2: List[List[Int]], f: (Int, Int) => Int) : List[List[Int]] = {
        matrix1.zip(matrix2).map{case (l1, l2) => l1.zip(l2)}.map(_.map{case (a,b) => f(a, b)})
    }
}
