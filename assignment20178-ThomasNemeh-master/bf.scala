// Part 2 about an Interpreter for the Brainf*** language
//========================================================

object CW8b {

type Mem = Map[Int, Int]

// (2a) Complete the functions for safely reading  
// and writing brainf*** memory. Safely read should
// Return the value stored in the Map for a given memory
// pointer, if it exists; otherwise it Returns 0. The
// writing function generates a new Map with the
// same data, except at the given memory pointer the
// value v is stored.


def sread(mem: Mem, mp: Int) : Int = {
  mem.get(mp).getOrElse(0)
}

def write(mem: Mem, mp: Int, v: Int) : Mem = {
  mem + (mp -> v)
}


// (2b) Implement the two jumping instructions in the 
// brainf*** language. In jumpRight, given a program and 
// a program counter move the counter to the right 
// until the command after the *matching* ]-command. Similarly, 
// jumpLeft implements the move to the left to just after
// the *matching* [-command. The levels are used to find the
// *matching* bracket.

def jumpRight(prog: String, pc: Int, level: Int) : Int = {
  if (pc == prog.size) {
    pc
  }
  else {
    if (prog(pc) == '[') jumpRight(prog, pc + 1, level + 1)
    else if(prog(pc) == ']') {
      if (level > 0) jumpRight(prog, pc + 1, level - 1)
      else pc + 1
    }
    else jumpRight(prog, pc + 1, level)
  }
  
}

def jumpLeft(prog: String, pc: Int, level: Int) : Int = {
  if (pc == 0) {
    if (prog(pc) == '[' && level == 0) pc + 1
    else -1
  }
  else {
    if (prog(pc) == ']') jumpLeft(prog, pc - 1, level + 1)
    else if(prog(pc) == '[') {
      if (level > 0) jumpLeft(prog, pc - 1, level - 1)
      else pc + 1
    }
    else jumpLeft(prog, pc - 1, level)
  }
}

def main(args: Array[String]) {
  /*
   Console.println(jumpRight("[******]***", 1, 0) == 8)
   Console.println(jumpRight("[**[*]*]***", 1, 0) == 8)
   Console.println(jumpRight("[**[*]*]***", 1, 0) == 8)
   Console.println(jumpRight("[**[***]***", 1, 0) == 11)
   Console.println(jumpRight("[*[][]*]***", 1, 0) == 8)
   Console.println(jumpLeft("[******]***", 6, 0) == 1)
   Console.println(jumpLeft("[******]***", 7, 0) == -1)
   Console.println(jumpLeft("[*[][]*]***", 6, 0) == 1)
   */
   Console.println(start("[-]", Map(0 -> 100)) == Map(0 -> 0))
   Console.println(start("[->+<]", Map(0 -> 10)) == Map(0 -> 0, 1 -> 10))
   Console.println(start("[>>+>>+<<<<-]", Map(0 -> 42)) == Map(0 -> 0, 2 -> 42, 4 -> 42))
   //Console.println(start({{hello world prg 1}}, Map()) == Map(0 -> 0, 5 -> 33, 1 -> 0, 6 -> 10, 2 -> 72, 3 -> 100, 4 -> 87))
}

// (2c) Complete the run function that interprets (runs) a brainf***
// program: the arguments are a program, a program counter,
// a memory counter and a brainf*** memory. It Returns the
// memory at the stage when the execution of the brainf*** program
// finishes. The interpretation finishes once the program counter
// pc is pointing to something outside the program string.
// If the pc points to a character inside the program, the pc, 
// memory pointer and memory need to be updated according to 
// rules of the brainf*** language. Then, recursively, the run 
// function continues with the command at the new program
// counter. 
//
// Implement the start function that calls run with the program
// counter and memory counter set to 0.


def run(prog: String, pc: Int, mp: Int, mem: Mem) : Mem = {
  if (pc == prog.size) {
    mem
  }
  else {
    helper(prog, pc, mp, mem)
  }
}

def helper(prog: String, pc: Int, mp: Int, mem: Mem) : Mem = prog(pc) match {
  case '>' => run(prog, pc + 1, mp + 1, mem)
  case '<' => run(prog, pc + 1, mp - 1, mem)
  case '+' => run(prog, pc + 1, mp, write(mem, mp, sread(mem, mp) + 1))
  case '-' => run(prog, pc + 1, mp, write(mem, mp, sread(mem, mp) - 1))
  case '.' => {
    Console.print(mem(mp).asInstanceOf[Char])
    run(prog, pc + 1, mp, mem)
  }
  case ',' => {
    val x = Console.in.read()
    run(prog, pc + 1, mp, write(mem, mp, x))
  }
  case '[' => {
    if (mem(mp) == 0) {
      run(prog, jumpRight(prog, pc + 1, 0), mp, mem)
    }
    else {
      run(prog, pc + 1, mp, mem)
    }
  }
  case ']' => {
    if (mem(mp) != 0) {
      run(prog, jumpLeft(prog, pc - 1, 0), mp, mem)
    }
    else {
      run(prog, pc + 1, mp, mem)
    }
  }
  case _ => run(prog, pc + 1, mp, mem)
}

def start(prog: String, mem: Mem) = {
  run(prog, 0, 0, mem)
}






// some sample bf programs collected from the Internet
//==================================================


/*
// first some contrived (small) programs
// clears the 0-cell
start("[-]", Map(0 -> 100)) 
// copies content of the 0-cell to 1-cell
start("[->+<]", Map(0 -> 10))
// copies content of the 0-cell to 2-cell and 4-cell
start("[>>+>>+<<<<-]", Map(0 -> 42))
start("+++[>+++++<-]", Map(0 -> 10))
// prints out numbers 0 to 9
start("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""", Map())
// some more "useful" programs
// hello world program 1
start("""++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++
       ..+++.>>.<-.<.+++.------.--------.>>+.>++.""", Map())
// hello world program 2
start("""++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>+
      +.<<+++++++++++++++.>.+++.------.--------.>+.>.""", Map())
// draws the Sierpinski triangle
start("""++++++++[>+>++++<<-]>++>>+<[-[>>+<<-]+>>]>+[-<<<[
      ->[+[-]+>++>>>-<<]<[<]>>++++++[<<+++++>>-]+<<++.[-]<<
      ]>.>+[>>]>+]""", Map())
//Fibonacci numbers below 100
start("""+++++++++++
      >+>>>>++++++++++++++++++++++++++++++++++++++++++++
      >++++++++++++++++++++++++++++++++<<<<<<[>[>>>>>>+>
      +<<<<<<<-]>>>>>>>[<<<<<<<+>>>>>>>-]<[>++++++++++[-
      <-[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<[>>>+<<<
      -]>>[-]]<<]>>>[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]
      >[<<+>>[-]]<<<<<<<]>>>>>[+++++++++++++++++++++++++
      +++++++++++++++++++++++.[-]]++++++++++<[->-<]>++++
      ++++++++++++++++++++++++++++++++++++++++++++.[-]<<
      <<<<<<<<<<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]<-[>>.>.<<<
      [-]]<<[>>+>+<<<-]>>>[<<<+>>>-]<<[<+>-]>[<+>-]<<<-]""", Map())
//outputs the square numbers up to 10000
start("""++++[>+++++<-]>[<+++++>-]+<+[
    >[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+
    >>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]
    <<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-]""", Map())
//Collatz numbers (need to be typed in)
start(""">,[[----------[
      >>>[>>>>]+[[-]+<[->>>>++>>>>+[>>>>]++[->+<<<<<]]<<<]
      ++++++[>------<-]>--[>>[->>>>]+>+[<<<<]>-],<]>]>>>++>+>>[
      <<[>>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<<]]<[>+<-]>]
      >[>[>>>>]+[[-]<[+[->>>>]>+<]>[<+>[<<<<]]+<<<<]>>>[->>>>]+>+[<<<<]]
      >[[>+>>[<<<<+>>>>-]>]<<<<[-]>[-<<<<]]>>>>>>>
      ]>>+[[-]++++++>>>>]<<<<[[<++++++++>-]<.[-]<[-]<[-]<]<,]""", Map())
// infinite Collatz (never stops)
start(""">>+>+<[[->>[>>]>>>[>>]+[<<]<<<[<<]>[>[>>]>>+>[>>]<+<[<<]<<<[<
      <]>-]>[>>]>>[<<<<[<<]>+>[>>]>>-]<<<<[<<]+>>]<<[+++++[>+++++++
      +<-]>.<++++++[>--------<-]+<<]>>[>>]+[>>>>[<<+>+>-]<-[>+<-]+<
      [<<->>-[<<+>>[-]]]>>>[<<<+<<+>>>>>-]<<<[>>>+<<<-]<<[[-]>+>>->
      [<+<[<<+>>-]<[>+<-]<[>+<-]>>>>-]<[>+<-]+<[->[>>]<<[->[<+++>-[
      <+++>-[<+++>-[<[-]++>>[-]+>+<<-[<+++>-[<+++>-[<[-]+>>>+<<-[<+
      ++>-[<+++>-]]]]]]]]]<[>+<-]+<<]>>>+<[->[<+>-[<+>-[<+>-[<+>-[<
      +>-[<+>-[<+>-[<+>-[<+>-[<[-]>>[-]+>+<<-[<+>-]]]]]]]]]]]<[>+<-
      ]+>>]<<[<<]>]<[->>[->+>]<[-[<+>-[<->>+<-[<+>-[<->>+<-[<+>-[<-
      >>+<-[<+>-[<->>+<-[<+>-[<->>+<-[<+>-[<->>+<-[<+>-[<->>+<-[<+>
      -[<->>+<-[<+>-[<->>+<-[<+>-]]]]]]]]]]]]]]]]]]]>[<+>-]<+<[<+++
      +++++++>-]<]>>[<+>->>]<<[>+>+<<-]>[<+>-]+>[<->[-]]<[-<<-]<<[<
      <]]++++++[>+++++++<-]>++.------------.[-]>[>>]<<[+++++[>+++++
      +++<-]>.<++++++[>--------<-]+<<]+<]>[<+>-]<]>>>[>>]<<[>[-]<-<
      <]++++++++++.[-]<<<[<<]>>>+<[->[<+>-[<+>-[<+>-[<+>-[<+>-[<+>-
      [<+>-[<+>-[<+>-[<[-]>>[-]+>+<<-]]]]]]]]]]<[>+<-]+>>]<<[<<]>>]""", Map())
*/ 

}
