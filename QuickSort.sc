import $file.Sort, Sort.Sort._

@main
def main() = {
    val input = List(9, 1, 7, 2, 6, 3, 10, 8, 4, 5)
    val output = quickSort(input)
    assert(output == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    output
}
