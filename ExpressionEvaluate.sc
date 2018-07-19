// program to convert infix to postfile
import scala.collection.mutable.Stack

@main
def main(input: String) : Unit = {
    def toPostFix(input: List[Char], postfix: List[Char] = List.empty[Char], operator: Stack[Char] = Stack.empty[Char]) : (List[Char], Stack[Char]) = {
        if (input.isEmpty) {
            (postfix, operator)
        } else {
            input.head match {
                case '(' => 
                    toPostFix(input.tail, postfix, operator.push('('))
                case ')' => 
                    val (before, after) = operator.span(_ != '(')
                    toPostFix(input.tail, postfix ++ before.toList, after.tail)
                case '-' =>
                    val (before, after) = operator.span("/*+-".contains(_))
                    toPostFix(input.tail, postfix ++ before.toList, after.push('-'))
                case '+' => 
                    val (before, after) = operator.span("/*+".contains(_))
                    toPostFix(input.tail, postfix ++ before.toList, after.push('+'))
                case '*' =>
                    val (before, after) = operator.span("/*".contains(_))
                    toPostFix(input.tail, postfix ++ before.toList, after.push('*'))
                case '/' => 
                    val (before, after) = operator.span("/".contains(_))
                    toPostFix(input.tail, postfix ++ before.toList, after.push('/'))
                case t => 
                    toPostFix(input.tail, postfix :+ t, operator)
            }
        }
    }

    def evaluate(input: String) : Double = {
        input.foldLeft(Stack.empty[Double]){case (acc, c) => 
            c match {
                case x if x.isDigit => 
                    acc.push(x.toString.toDouble)
                case '+' => 
                    acc.push(acc.pop + acc.pop)
                case '-' => 
                    acc.push(acc.pop - acc.pop)
                case '*' => 
                    acc.push(acc.pop * acc.pop)
                case '/' => 
                    val x = acc.pop
                    val y = acc.pop
                    acc.push(y / x)
            }
        }.pop
    }
    val (postfix, operator) = toPostFix(input.toList.filterNot(_ == ' '))
    println(s"expression ${(postfix ++ operator.toList).mkString}")
    println(evaluate((postfix ++ operator.toList).mkString))
}