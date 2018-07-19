// use Karatsuba Algorithm to multiply large numbers
@main
def main() = {
    val input1 = "5675757575765"
    val input2 = "45646464"
    val result = add(input1, input2)
    assert(result == "5675803222229")
    result
}

def multiply(s1: String, s2: String) : String = {
    if (s1.nonEmpty && s2.nonEmpty) {
        val (a, b) = s1.splitAt(s1.size / 2)
        val (c, d) = s2.splitAt(s2.size / 2)
        val t1 = multiply(a, c)
        val t2 = multiply(b, d)
        val t3 = multiply(add(a, c), add(b, d))
    } else ""
}
def add(i1: String, i2: String) : String = {
    def recurse(l1: List[Char], l2: List[Char], carry: Int = 0) : List[Char] = {
        if (l1.isEmpty && l2.isEmpty && carry == 0) List.empty[Char]
        else if (l1.isEmpty && l2.isEmpty && carry > 0) List((48 + carry).toChar)
        else if (l1.isEmpty && l2.nonEmpty && carry == 0) l2
        else if (l1.isEmpty && l2.nonEmpty && carry > 0) recurse(List((48 + carry).toChar), l2)
        else if (l1.nonEmpty && l2.isEmpty && carry == 0) l1
        else if (l1.nonEmpty && l2.isEmpty && carry > 0) recurse(List((48 + carry).toChar), l1)
        else {
            val n1 = l1.head.toString.toInt
            val n2 = l2.head.toString.toInt
            val sum = n1 + n2 + carry
            val rem = sum / 10
            val num = sum % 10
            (48 + num).toChar :: recurse(l1.tail, l2.tail, rem)
        }
    }
    recurse(i1.toList.reverse, i2.toList.reverse, 0).reverse.mkString
}

def subtract(i1: String, i2: String) : String = {
    def recurse(l1: List[Char], l2: List[Char]) : List[Char] = {
        
    }
    recurse(i1.toList.reverse, i2.toList.reverse).reverse.mkString
}