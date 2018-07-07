// use divide and conquer to find the kth largest element in an array
@main
def main() = {
    val input = List(9, 1, 7, 2, 6, 3, 10, 8, 4, 5)
    val output = kthLargest(input, 3)
    assert(output == 8)
    output
}

def kthLargest(input: List[Int], kth: Int) : Int = {
    input match {
        case Nil => -1
        case _ => 
            val r = new scala.util.Random
            val pivot = r.nextInt(input.size)
            val (smaller, larger) = input.partition(_ < input(pivot))
            if (larger.size == kth) input(pivot)
            else if (larger.size > kth) {
                kthLargest(larger, kth)
            } else {
                kthLargest(smaller, kth - larger.size)
            }
    }

}
