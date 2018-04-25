package proj

import scala.util.Random;
import scala.math;
import scala.util.matching.Regex;

object Prog {
  def main(args: Array[String]) {
    println("OooooOOOOolooOO There goes tokyo go go godzilla!")

    val (rows, cols, numMines) = (5,5,4)
    executeGame(rows, cols, numMines);

  }

  def executeGame(rows: Int, cols: Int, numMines: Int): Unit = {
    val visibleBoard = newVisibleBoard(rows, cols);
    val board = newBoard(rows, cols, numMines);
    if(gameLoop(rows, cols, numMines, visibleBoard, board)){
      printBoard(board, 0);
      println("you win");
    } else {
      printBoard(visibleBoard, 0);
      println("you lose");
    }

  }

  def gameLoop(rows: Int, cols: Int, flags:Int, visible: Array[Array[String]], board: Array[Array[String]]): Boolean = {
    printBoard(visible, flags);
    if(checkWinCondition(visible,board)){
      return true;
    }


    val flag = chooseFlag();
    val input = userCoordInput(rows,cols,flag,visible);
    val hit = revealCoord(input, visible, board, flag);
    if (hit == "x") {
      return false;
    }
    if(flag){
      if(hit == "F"){
        return gameLoop(rows,cols,flags-1,visible,board);
      } else {
        return gameLoop(rows,cols,flags+1,visible,board);
      }
    } else {
      return gameLoop(rows, cols, flags, visible, board);
    }
  }

  def checkWinCondition(visible:Array[Array[String]], board:Array[Array[String]]): Boolean = {
    if(visible.length==0){
      return true;
    } else {
      val inRow = checkWinConditionCols(visible(0), board(0));
      return inRow && checkWinCondition(visible.drop(1), board.drop(1))
    }


  }

  def checkWinConditionCols(visible:Array[String], board:Array[String]): Boolean = {
    if(visible.length==0){
      return true;
    } else {
      if(visible(0) == "0" && board(0) != "x"){
        return false;
      } else{
        return true && checkWinConditionCols(visible.drop(1), board.drop(1));
      }
    }
  }

  def chooseFlag() : Boolean = {
    println("Type f to place a flag")
    val input = scala.io.StdIn.readLine();
    return input.matches("[fF([fF][lL][aA][gG])]");
  }

  def userCoordInput(rows: Int, cols: Int, flag:Boolean, visible: Array[Array[String]]): (Int, Int) = {
    print("Enter Row: ");
    val row = isInputInt(scala.io.StdIn.readLine(), "Enter Row: ");
    print("Enter Column: ");
    val col = isInputInt(scala.io.StdIn.readLine(), "Enter Column: ");

    if (isInvalidCoord((row, col), rows, cols,flag, visible)) {
      println("Invalid Coordinate.");
      return userCoordInput(rows, cols, flag, visible);
    }
    return (row, col);

  }

  def isInvalidCoord(coord: (Int, Int), rows: Int, cols: Int, flag:Boolean, visible: Array[Array[String]]): Boolean = {
    return coord._1 >= rows || coord._1 < 0 || coord._2 >= cols || coord._2 < 0 ||
      (flag && visible(coord._1)(coord._2) != "F" && visible(coord._1)(coord._2) != "0") ||
      (!flag && visible(coord._1)(coord._2) != "0");
  }

  def isInputInt(input: String, message: String): Int = {
    if (input.matches("\\d+")) {
      return input.toInt;
    } else {
      println("Invalid.");
      print(message);
      return isInputInt(scala.io.StdIn.readLine(), message);
    }
  }

  def generateCords(amountOfMines: Int, rows: Int, cols: Int): Array[(Int, Int)] = {
    return generateCordsHelper(rows, cols, amountOfMines)
  }

  def generateCordsHelper(rows: Int, cols: Int, amountOfMines: Int): Array[(Int, Int)] = {
    //TODO:check to make sure coord is unique
    if (amountOfMines == 0) {
      return Array.ofDim[(Int, Int)](0);
    }
    else {
      val coords = generateCordsHelper(rows, cols, amountOfMines - 1);
      return generateSingleCoord(rows, cols, coords) +: coords;
    }
  }

  def generateSingleCoord(rows: Int, cols: Int, coords: Array[(Int, Int)]): (Int, Int) = {
    val newCoord = (Random.nextInt(rows), Random.nextInt(cols));
    if (doesCoordExist(coords, newCoord)) {
      return generateSingleCoord(rows, cols, coords);
    }
    else {
      return newCoord;
    }
  }

  def doesCoordExist(coords: Array[(Int, Int)], check: (Int, Int)): Boolean = {
    if (coords.length == 0) {
      return false;
    } else {
      val temp = isTupleEqual(check, coords.head) || doesCoordExist(coords.drop(1), check);
      return temp;
    }
  }

  def isTupleEqual(coord1: (Int, Int), coord2: (Int, Int)): Boolean = {
    val temp = coord1._1 == coord2._1 && coord1._2 == coord2._2;
    return temp;
  }

  def printBoard(visibleSquares: Array[Array[String]], flags:Int): Unit = {
    printHeader(visibleSquares(0).length, flags);
    printTopBotBound(visibleSquares(0).length);
    printRows(visibleSquares, 0);
    printTopBotBound(visibleSquares(0).length);
  }

  def printHeader(cols: Int, flags:Int): Unit = {
    println("    " + getHeader(cols) + "     Flags Left: " + flags);
  }

  def printTopBotBound(cols: Int): Unit = {
    println("" + getTopBotBound(cols));
  }

  def getTopBotBound(cols: Int): String = {
    if(cols==0){
      return "  ---"
    }
    return getTopBotBound(cols-1) + "--"
  }

  def getHeader(cols: Int): String = {
    if (cols <= 1) {
      return (cols-1).toString;
    }
    val temp = getHeader(cols - 1) + " " + (cols-1).toString;
    return temp;
  }

  def printRows(visibleSquares: Array[Array[String]], curr: Int): Unit = {
    if (visibleSquares.length == 0) {
      return;
    } else {
      print(curr + " | ")
      printCols(visibleSquares(0))
      printRows(visibleSquares.drop(1),curr+1)
    }
  }

  def printCols(visibleSquares: Array[String]): Unit = {
    if (visibleSquares.length == 0) {
      println("|")
    } else {
      print(visibleSquares(0) + " ")
      printCols(visibleSquares.drop(1))
    }
  }

  def newVisibleBoard(rows:Int, cols:Int): Array[Array[String]] ={
    if(rows == 0){
      return Array.ofDim[Array[String]](0);
    } else {
      return newVisibleRow(cols) +: newVisibleBoard(rows-1, cols);
    }

  }

  def newVisibleRow(cols:Int): Array[String] = {
    if(cols == 0){
      return Array.ofDim[String](0);
    } else {
      return "0" +: newVisibleRow(cols-1);
    }
  }

  def newBoard(rows:Int, cols:Int, numMines:Int): Array[Array[String]] = {
    val mines = generateCords(numMines,rows,cols);
    return newBoardHelper(rows-1,cols-1,mines);
  }

  def newBoardHelper(rows:Int, cols:Int, mines:Array[(Int,Int)]): Array[Array[String]] = {
    if(rows < 0){
      return Array.ofDim[Array[String]](0)
    } else {
      val temp1 = newBoardHelper(rows-1, cols, mines)
      val temp2 = newRow(rows, cols, mines) ;
      val temp = temp1 :+ temp2;
      return temp;
    }
  }

  def newRow(rows: Int, cols:Int, mines:Array[(Int,Int)]): Array[String] = {
    if(cols < 0){
      return Array.ofDim[String](0);
    } else {
      if(doesCoordExist(mines, (rows,cols))) {
        val temp = newRow(rows, cols - 1, mines) :+ "x";
        return temp;
      } else {
        val emptyConverter = (in: Int) => {
          if(in == 0) " " else in.toString();
        }
        val temp1 = emptyConverter(numAdjacentMines((rows,cols), mines));
        val temp = newRow(rows, cols-1, mines) :+ temp1;
        return temp;
      }
    }
  }

  def numAdjacentMines(coord:(Int,Int), mines:Array[(Int,Int)]): Int = {
    if(mines.length==0){
      return 0;
    } else{
      val adjacent = (coord1:(Int,Int), coord2:(Int,Int)) => {
        if (isAdjacentCoord(coord1, coord2)) 1 else 0
      }
      return numAdjacentMines(coord, mines.drop(1)) + adjacent(coord,mines.head);
    }
  }

  def isAdjacentCoord(first: (Int,Int), second: (Int,Int)): Boolean ={
    return  isDifferenceOneOrZero(first._1, second._1) && isDifferenceOneOrZero(first._2, second._2);
  }

  def isDifferenceOneOrZero(first: Int, second: Int) : Boolean = {
    return Math.abs(first-second) == 1 || first-second == 0;
  }

  def revealCoord(coord: (Int,Int), visible: Array[Array[String]], board:Array[Array[String]],flag:Boolean) : String = {
    if(flag) {
      if (visible(coord._1)(coord._2) == "F") {
        visible(coord._1)(coord._2) = "0"
      } else {
        visible(coord._1)(coord._2) = "F"
      }
    } else if(board(coord._1)(coord._2) != " "){
      visible(coord._1)(coord._2) = board(coord._1)(coord._2);
    } else {
      revealBlanks(coord, visible, board);
    }
    return visible(coord._1)(coord._2);
  }

  def revealBlanks(coord: (Int,Int), visible: Array[Array[String]], board:Array[Array[String]]): Unit = {
    if(coord._1 < 0 || coord._2 < 0 || coord._1 >= visible.length || coord._2 >= visible(0).length){
      return;
    }
    if (visible(coord._1)(coord._2) != "0") {
      return;
    }
    visible(coord._1)(coord._2) = board(coord._1)(coord._2);
    if(board(coord._1)(coord._2) != " "){
      return;
    }
    revealBlanks((coord._1+1, coord._2+1),visible, board);
    revealBlanks((coord._1+1, coord._2  ),visible, board);
    revealBlanks((coord._1+1, coord._2-1),visible, board);
    revealBlanks((coord._1-1, coord._2+1),visible, board);
    revealBlanks((coord._1-1, coord._2  ),visible, board);
    revealBlanks((coord._1-1, coord._2-1),visible, board);
    revealBlanks((coord._1,   coord._2+1),visible, board);
    revealBlanks((coord._1,   coord._2-1),visible, board);
  }

}
