// this program counts the number of inversions in an array
// input: Array(3, 6, 2, 8, 1, 5, 4)
// inversions: List((3, 2), (3, 1), (6, 2), (6, 1), (6, 5), (6, 4), (2, 1), (8, 1), (8, 5), (8, 4), (5, 4))
// we are attempting nlogn
import $file.Sort, Sort.Sort._
@main
def main() = {
    val input = List(3, 6, 2, 8, 1, 5, 4)
    getInversions(input)
}

// sum of 1, 2, 3
// 1: number of inversions in the left half
// 2: number of inversions in the right half
// 3: number of split inversions
// total complexity 3nlogn + 2n 
// ignoring constants and lower order. nlogn
def getInversions(input: List[Int]) : List[(Int, Int)] = {
    input match {
        case Nil => Nil
        case List(a) => Nil
        case _ => 
            val mid = input.size / 2
            val left = input.slice(0, mid)
            val right = input.slice(mid, input.size)
            val l1 = getInversions(left)
            val l2 = getInversions(right)
            val l3 = splitInversions(left, right)
            if (l1.nonEmpty && l2.nonEmpty)
                l1.foldRight(l2)(_ :: _).foldRight(l3)(_ :: _)
            else if (l1.nonEmpty && l2.isEmpty)
                l1.foldRight(l3)(_ :: _)
            else if (l1.isEmpty && l2.nonEmpty)
                l2.foldRight(l3)(_ :: _)
            else if (l1.isEmpty && l2.isEmpty)
                l3
            else {
                List.empty[(Int, Int)]
            }
    }
}

// assuming l1 and l2 are almost of same size.
// total complexity 2(nlogn + n)
def splitInversions(l1: List[Int], l2: List[Int]) : List[(Int, Int)] = {
    val sortedL1 = mergeSort(l1) // nlogn
    val sortedL2 = mergeSort(l2) // nlogn
    (sortedL1, sortedL2) match {
        case (Nil, Nil) => List.empty[(Int, Int)]
        case (Nil, _) => List.empty[(Int, Int)]
        case (_, Nil) => List.empty[(Int, Int)]
        case (_, _) if (sortedL1.head > sortedL2.head) => 
            val result = splitInversions(sortedL1, sortedL2.tail)
            sortedL1.foldLeft(result){case (acc, x) => (x, sortedL2.head) :: acc}
        case (_, _) => 
            splitInversions(sortedL1.tail, sortedL2)
    }
}