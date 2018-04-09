package proj

import scala.util.Random

object Prog {
  def main(args: Array[String]) {
    println("OooooOOOOolooOO There goes tokyo go go godzilla!")

    val (rows, cols) = (7,7)
    //print a game board of minesweeper
    //probably going to want to move this to a method later
    for(i <- 0 to rows){
      for(j <- 0 to cols){
        print('x')
      }
      println()
    }

    //print out the list containing mines
    //the 2d represents our board
    //0s are clear and 1s are mines
    val board = mineList(cols, rows)
    for (i <- 0 to rows){
      for (j <- 0 to cols){
        print(board(i)(j))
      }
      println()
    }

    //get user input on what coordinates they want to test
    print("Enter a 2d coordinate")
    var userInput = scala.io.StdIn.readLine()
    print(userInput)
  }

  //returns a 2d list containing the location of mines
  def mineList(rows:Int, cols:Int):Array[Array[Int]] ={
    //creates a 2 dimensional list to contain board
    //right now very weighted towards the early rows so might want to adjust that
    //also might not create 10 mines
    val board = Array.ofDim[Int](rows+1, cols+1)
    var count = 0
    var temp = 0
    for(i <- 0 to rows) {
      for (j <- 0 to cols) {
        temp = Random.nextInt(5)
        if(temp == 1 && count < 10) {
          count = count + 1
          board(i)(j) = temp
        }
        else
          board(i)(j) = 0
      }
    }
    return board
  }
}
