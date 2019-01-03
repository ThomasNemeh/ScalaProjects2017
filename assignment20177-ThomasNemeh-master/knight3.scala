// Part 3 about finding a single tour using the Warnsdorf Rule
//=============================================================

// copy any function you need from files knight1.scala and
// knight2.scala

object CW7c {

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

//(3a) Complete the function that calculates a list of onward
//     moves like in (1b) but orders them according to Warnsdorf’s 
//     rule. That means moves with the fewest legal onward moves 
//     should come first.

def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  def f_definition(position: Pos) : Int = {
    legal_moves(dim, position :: path, position).size
  }
  legal_moves(dim, path, x).sortBy(f_definition)
}


def is_legal(dim: Int, path: Path)(x: Pos) : Boolean = {
  !(x._1 >= dim || x._2 >= dim || x._1 < 0 || x._2 < 0 || path.contains(x)) 
}

def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  val potentialMoves = List((x._1 + 1, x._2 + 2), (x._1 + 2, x._2 + 1), (x._1 + 2, x._2 - 1), (x._1 + 1, x._2 - 2), (x._1 - 1, x._2 - 2), (x._1 - 2, x._2 - 1), (x._1 - 2, x._2 + 1), (x._1 - 1, x._2 + 2))
  for (n <- potentialMoves if is_legal(dim, path)(n)) yield n
}



//(3b) Complete the function that searches for a single *closed* 
//     tour using the ordered moves function.

def first_closed_tour_heuristic(dim: Int, path: Path) : Option[Path] = {
  def f_definition(x: Pos) : Option[Path] = {
      val newPath = x :: path
      val potentialMoves = List((x._1 + 1, x._2 + 2), (x._1 + 2, x._2 + 1), (x._1 + 2, x._2 - 1), (x._1 + 1, x._2 - 2), (x._1 - 1, x._2 - 2), (x._1 - 2, x._2 - 1), (x._1 - 2, x._2 + 1), (x._1 - 1, x._2 + 2))
      if (newPath.size == dim * dim && potentialMoves.contains(newPath(newPath.size - 1))) {
        Some(newPath)
      }
      else {
         val potentialMoves = legal_moves(dim, newPath, x)
         if (potentialMoves.size == 0) {
           None
         }
         else {
           first_closed_tour_heuristic(dim, newPath)
         }
      }
  }
  if (path.size == 0 ) {
    val board = board_to_list(dim, List(), 0)
    first(board, f_definition)
  }
  else {
    first(legal_moves(dim, path, path(0)), f_definition)
  }
}

def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = {
  if (xs.size == 0) {
    None
  }
  else {
    val path = f(xs(0))
    if (path.isDefined) {
      path
    }
    else {
      first(xs.drop(1), f)
    }
  }
}

def row_to_list(dim: Int, row: Int) : List[Pos] = {
  for (x <- (0 to (dim - 1)).toList) yield (x, row)
}

def board_to_list(dim:Int, board: List[Pos], rowNum: Int) : List[Pos] = {
  if (rowNum >= dim) {
    board
  }
  else {
    val rowList = row_to_list(dim, rowNum)
    board_to_list(dim, rowList ::: board, rowNum + 1)
  }
}


//(3c) Same as (3b) but searches for *non-closed* tours. However, 
//     you have to be careful to write a tail-recursive version as this 
//     function will be called with dimensions of up to 40 * 40.

def first_tour_heuristic(dim: Int, path: Path) : Option[Path] = {
  def helper(path: Path, potentialMoves: Path) : Option[Path] = {
      if (path.size == dim * dim) {
        Some(path)
      }
      else {
        if (potentialMoves.size == 0) {
          None
        } 
        else {
          val newPath = helper(potentialMoves(0) :: path, ordered_moves(dim, potentialMoves(0) :: path, potentialMoves(0)))
          if (newPath.isDefined) {
            newPath
          }
          else {
            helper(path, potentialMoves.drop(1))
          }
        }
      }
      
  }
  val potentialMoves = ordered_moves(dim, path, path(0))
  helper(path, potentialMoves)
}

}
