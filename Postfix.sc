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
    val (postfix, operator) = toPostFix(input.toList.filterNot(_ == ' '))
    println(s"postfix $postfix")
    println(s"operator ${operator}")
    println((postfix ++ operator.toList).mkString)
}