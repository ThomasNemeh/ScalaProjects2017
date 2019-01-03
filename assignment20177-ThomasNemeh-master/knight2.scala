// Part 2 about finding a single tour for a board
//================================================

// copy any function you need from file knight1.scala

object CW7b {

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions


//(2a) Implement a first-function that finds the first 
//     element, say x, in the list xs where f is not None. 
//     In that case Return f(x), otherwise None. If possible,
//     calculate f(x) only once.

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

//(2b) Implement a function that uses the first-function for
//     trying out onward moves, and searches recursively for a
//     knight tour on a dim * dim-board.

/* Here, I also accounted for the possibility of an empty path being passed as an argument,
 * in which case every position on the board would have to be tested for a tour. In this case,
 * the function passed into first() is defined differently.
 */
def first_tour(dim: Int, path: Path) : Option[Path] = {
  def f_definition(position: Pos) : Option[Path] = {
      val newPath = position :: path
      if (newPath.size == dim * dim) {
        Some(path)
      }
      else {
         val potentialMoves = legal_moves(dim, newPath, position)
         if (potentialMoves.size == 0) {
           None
         }
         else {
           first_tour(dim, newPath)
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


def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  val potentialMoves = List((x._1 + 1, x._2 + 2), (x._1 + 2, x._2 + 1), (x._1 + 2, x._2 - 1), (x._1 + 1, x._2 - 2), (x._1 - 1, x._2 - 2), (x._1 - 2, x._2 - 1), (x._1 - 2, x._2 + 1), (x._1 - 1, x._2 + 2))
  for (n <- potentialMoves if is_legal(dim, path)(n)) yield n
}
 
def is_legal(dim: Int, path: Path)(x: Pos) : Boolean = {
  !(x._1 >= dim || x._2 >= dim || x._1 < 0 || x._2 < 0 || path.contains(x)) 
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

}
