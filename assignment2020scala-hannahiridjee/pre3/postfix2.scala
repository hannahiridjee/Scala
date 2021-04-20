// Shunting Yard Algorithm 
// including Associativity for Operators 
// =====================================

object CW8b {


// type of tokens
type Toks = List[String]

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// left- and right-associativity
abstract class Assoc
case object LA extends Assoc
case object RA extends Assoc


// power is right-associative,
// everything else is left-associative
def assoc(s: String) : Assoc = s match {
  case "^" => RA
  case _ => LA
}


// the precedences of the operators
val precs = Map("+" -> 1,
  		"-" -> 1,
		"*" -> 2,
		"/" -> 2,
                "^" -> 4)

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/", "^")

// (3) Implement the extended version of the shunting yard algorithm.
// This version should properly account for the fact that the power 
// operation is right-associative. Apart from the extension to include
// the power operation, you can make the same assumptions as in 
// basic version.

def is_op(op: String) : Boolean = {
	ops.contains(op)
}
//is_op("2")

def prec(op1: String, op2: String) : Boolean = { //return true if  op1 has higher precedence
	if(precs.getOrElse(op1,0) >= precs.getOrElse(op2,0).toInt) true else false
}
def prec2(op1: String, op2: String) : Boolean = { //return true if  op1 has higher precedence
	if(precs.getOrElse(op1,0) > precs.getOrElse(op2,0).toInt) true else false
}
//prec2("/","^")

def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = (toks,st,out) match{

	case (Nil,hd::rst,out) => syard(Nil, rst, hd::out )
	case (Nil,Nil,out) => out.reverse
	case(x::rest, st, out) if x.forall(_.isDigit) => syard(rest,st, x::out)
	case(x::rest, Nil , out) if is_op(x) => syard(rest, x::Nil, out)

	case("^"::rest, hd::rst , out) => if(prec2(hd,"^")) syard(rest, "^"::rst ,hd::out) else syard(rest,"^"::hd::rst,out)

	case(x::rest, hd::rst , out) if is_op(x) => if(prec(hd,x)) syard(rest, x::rst ,hd::out) else syard(rest,x::hd::rst,out)
	case ("("::rest,st,out) => syard(rest, "("::st,out)
	case (")"::rest,hd::rst,out) => if(st.head != "(") syard(")"::rest, rst, hd::out) else syard(rest, rst, out)
}

// test cases
// syard(split("3 + 4 * 8 / ( 5 - 1 ) ^ 2 ^ 3"))  // 3 4 8 * 5 1 - 2 3 ^ ^ / +


// (4) Implement a compute function that produces an Int for an
// input list of tokens in postfix notation.

def compute(toks: Toks, st: List[Int] = Nil) : Int = (toks,st) match {
	case (Nil, st) => st.head
	case (x::toks, st) if x.forall(_.isDigit) => compute(toks, x.toInt::st)
	case (x::toks,a::b::st) if is_op(x) => x match{
		case "*" => compute(toks, a*b ::st)
		case "+" => compute(toks, a+b ::st)
		case "/" => compute(toks, b/a ::st)
		case "-" => compute(toks, b-a ::st)
    case "^" => compute(toks, scala.math.pow(b,a).toInt ::st)
	}
}


// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))   // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15
// compute(syard(split("4 ^ 3 ^ 2")))      // 262144
// compute(syard(split("4 ^ ( 3 ^ 2 )")))  // 262144
// compute(syard(split("( 4 ^ 3 ) ^ 2")))  // 4096
// compute(syard(split("( 3 + 1 ) ^ 2 ^ 3")))   // 65536

}
