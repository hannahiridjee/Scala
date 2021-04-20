// Core Part about finding a single tour for a board using the
// Warnsdorf Rule
//==============================================================

object CW9b {


// !!! Copy any function you need from file knight1.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
  if(x._1 >= 0 && x._1 < dim && x._2 >= 0 && x._2 < dim && !(path.contains(x))) true else false
}

def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    val xMoves = List(1,2,2,1,-1,-2,-2,-1)
    val yMoves = List(2,1,-1,-2,-2,-1,1,2)
    for (n <- (0 to 7).toList if is_legal(dim, path, (x._1+xMoves(n),x._2+yMoves(n)))) yield (x._1+xMoves(n),x._2+yMoves(n))       
}

def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = 
xs match{
  case Nil => None
  case x::xs => {
      val v = f(x)
      if(v.isDefined) v else first(xs,f)
  }
  
}

//(6) Complete the function that calculates a list of onward
//    moves like in (2) but orders them according to Warnsdorf’s 
//    rule. That means moves with the fewest legal onward moves 
//    should come first.


def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    val legMoves = legal_moves(dim,path,x)
    legMoves.sortBy(legal_moves(dim,path,_).length)
}


//(7) Complete the function that searches for a single *closed* 
//    tour using the ordered_moves function from (6). This
//    function will be tested on a 6 x 6 board. 


def first_closed_tour_heuristics(dim: Int, path: Path) : Option[Path] = {
    val closed = legal_moves(dim,List(path.head),path.head).contains(path.last)
    if (path.length == dim*dim && closed) Some(path)
    else{
        val m = ordered_moves(dim,path,path.head)
        first(m,x => first_closed_tour_heuristics(dim,x::path))  
    }  
}


//(8) Same as (7) but searches for *non-closed* tours. This 
//    version of the function will be called with dimensions of 
//    up to 30 * 30.

def first_tour_heuristics(dim: Int, path: Path) : Option[Path] = {
    if(path.length == dim*dim) Some(path)
    else{
        val m = ordered_moves(dim,path,path.head)
        val func = (x: Pos) => first_tour_heuristics(dim,x::path)
        first(m, func)
    }  
}



}
