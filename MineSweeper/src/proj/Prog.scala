package proj

import scala.util.Random

object Prog {
  def main(args: Array[String]) {
    println("OooooOOOOolooOO There goes tokyo go go godzilla!")


    val (rows, cols) = (8, 8)
    //print a game board of minesweeper
    //probably going to want to move this to a method later
    for (i <- 0 to rows - 1) {
      for (j <- 0 to cols - 1) {
        print('x')
      }
      println()
    }

    //print out the list containing mines
    //the 2d represents our board
    //0s are clear and 1s are mines
    val board = mineList(cols, rows)

    // print out board test
    printBoard(board)
    //get user input on what coordinates they want to test
    print("Enter a 2d coordinate")
//    var userInput = scala.io.StdIn.readLine()
//    print(userInput)

    val cords = generateCords(10, rows, cols)
    for(i <- 0 to 9){
      println(cords(i))
    }
  }
  def generateCords(amountOfMines:Int, rows:Int, cols:Int): Array[(Int, Int)]  ={
    val cords = Array.ofDim[(Int, Int)](amountOfMines+1)
    return generateCordsHelper(rows, cols, amountOfMines, cords)
  }
  def generateCordsHelper(rows:Int, cols:Int, amountOfMines:Int, cords:Array[(Int, Int)]): Array[(Int, Int)] ={
    //TODO:check to make sure coord is unique
    if(amountOfMines == 0){
      cords(amountOfMines) = (Random.nextInt(rows), Random.nextInt(cols))
      return cords
    }
    else{
      cords(amountOfMines) = (Random.nextInt(rows), Random.nextInt(cols))
      generateCordsHelper(rows, cols, amountOfMines-1, cords)
    }
  }
  //returns a 2d list containing the location of mines
  def mineList(rows: Int, cols: Int): Array[Array[Int]] = {
    //creates a 2 dimensional list to contain board
    //right now very weighted towards the early rows so might want to adjust that
    //also might not create 10 mines
    val board = Array.ofDim[Int](rows, cols)
    var count = 0
    var temp = 0
    for (i <- 0 to rows - 1) {
      for (j <- 0 to cols - 1) {
        temp = Random.nextInt(5)
        if (temp == 1 && count < 10) {
          count = count + 1
          board(i)(j) = temp
        }
        else
          board(i)(j) = 0
      }
    }
    return board
  }

  def printBoard(visibleSquares: Array[Array[Int]]): Unit = {
    printRows(visibleSquares)
  }

  def printRows(visibleSquares: Array[Array[Int]]): Unit = {
    if (visibleSquares.length == 0) {
      println()
    } else {
      printCols(visibleSquares(0))
      printRows(visibleSquares.slice(1, visibleSquares.length))
    }
  }

  def printCols(visibleSquares: Array[Int]): Unit = {
    if (visibleSquares.length == 0) {
      println()
    } else {
      print(visibleSquares(0) + " ")
      printCols(visibleSquares.slice(1, visibleSquares.length))
    }

  }


}
