// Part 1 about the 3n+1 conjecture
//=================================

object CW6a {

//(1) Complete the collatz function below. It should
//    recursively calculate the number of steps needed 
//    until the collatz series reaches the number 1.
//    If needed, you can use an auxiliary function that
//    performs the recursion. The function should expect
//    arguments in the range of 1 to 1 Million.

def collatz(n: Long) : Long = {
  conjecture(n, 1)
}

def conjecture(n: Long, x: Long) : Long = {
  if (n == 1) {
    x 
  }
  else if (n % 2 == 0) {
    conjecture(n/2, x + 1)
  }
  else {
    conjecture((3 * n) + 1, x + 1)
  }
}

//(2)  Complete the collatz-bound function below. It should
//     calculate how many steps are needed for each number 
//     from 1 up to a bound and then calculate the maximum number of
//     steps and the corresponding number that needs that many 
//     steps. Again, you should expect bounds in the range of 1
//     up to 1 Million. The first component of the pair is
//     the maximum number of steps and the second is the 
//     corresponding number.

def collatz_max(bnd: Long) : (Long, Long) = {
    findMax(1, bnd, 0, 0)
}

def findMax(pos: Long, finish: Long, maxNum: Long, maxCol: Long): (Long, Long) = {
  if (pos == finish) {
    (maxCol,maxNum)
  }
  else if (maxCol < collatz(pos)) {
    findMax(pos + 1, finish, pos, collatz(pos))
  }
  else {
    findMax(pos + 1, finish, maxNum, maxCol)
  }
}


}
