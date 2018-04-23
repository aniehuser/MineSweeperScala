package proj

import scala.util.Random
import scala.math

object Prog {
  def main(args: Array[String]) {
    println("OooooOOOOolooOO There goes tokyo go go godzilla!")


    val (rows, cols) = (8, 8)


    //print out the list containing mines
    //the 2d represents our board
    //0s are clear and 1s are mines
//    val board = mineList(cols, rows)

    // print out board test
//    printBoard(board)


    val mines = generateCords(10, rows, cols)
    val visibleBoard = newVisibleBoard(10,10);
    val board = newBoard(10,10,mines);
    printBoard(visibleBoard);
    printBoard(board);
    for(i <- 0 to 9){
      println(mines(i))
    }
  }
  def generateCords(amountOfMines:Int, rows:Int, cols:Int): Array[(Int, Int)]  ={
    return generateCordsHelper(rows, cols, amountOfMines)
  }
  def generateCordsHelper(rows:Int, cols:Int, amountOfMines:Int): Array[(Int, Int)] ={
    //TODO:check to make sure coord is unique
    if(amountOfMines == 0){
      return Array.ofDim[(Int,Int)](0);
    }
    else{
      val coords = generateCordsHelper(rows,cols,amountOfMines-1);
      return generateSingleCoord(rows, cols, coords) +: coords;

    }
  }

  def generateSingleCoord(rows:Int, cols:Int, coords:Array[(Int,Int)]): (Int, Int) = {
    val newCoord = (Random.nextInt(rows), Random.nextInt(cols));
    if(doesCoordExist(coords, newCoord)) {
      return generateSingleCoord(rows, cols, coords);
    }
    else {
      return newCoord;
    }
  }

  def doesCoordExist(coords:Array[(Int, Int)], check:(Int,Int)) : Boolean = {
    if(coords.length==0){
      return false;
    } else {
      return isTupleEqual(check, coords.head) || doesCoordExist(coords.drop(1), check);
    }
  }

  def isTupleEqual(coord1:(Int,Int), coord2:(Int,Int)) : Boolean = {
      return coord1._1 == coord2._1 && coord1._2 == coord2._2;
  }

  def printBoard(visibleSquares:Array[Array[String]]): Unit ={
    printRows(visibleSquares)
  }

  def printRows(visibleSquares: Array[Array[String]]): Unit = {
    if (visibleSquares.length == 0) {
      println()
    } else {
      printCols(visibleSquares(0))
      printRows(visibleSquares.slice(1, visibleSquares.length))
    }
  }

  def printCols(visibleSquares: Array[String]): Unit = {
    if (visibleSquares.length == 0) {
      println()
    } else {
      print(visibleSquares(0) + " ")
      printCols(visibleSquares.slice(1, visibleSquares.length))
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

  def newBoard(rows:Int, cols:Int, mines:Array[(Int,Int)]): Array[Array[String]] = {
    if(rows == 0){
      return Array.ofDim[Array[String]](0)
    } else {
      return newBoard(rows-1, cols, mines) :+ newRow(rows, cols, mines) ;
    }
  }

  def newRow(rows: Int, cols:Int, mines:Array[(Int,Int)]): Array[String] = {
    if(cols == 0){
      return Array.ofDim[String](0);
    } else {
      if(doesCoordExist(mines, (rows,cols))) {
        return  newRow(rows, cols - 1, mines) :+ "x";
      } else {
        return  newRow(rows, cols-1, mines) :+ "0" //numAdjacentMines((rows,cols), mines).toString();
      }
    }
  }

  def numAdjacentMines(coord:(Int,Int), mines:Array[(Int,Int)]): Int = {
    if(mines.length==0){
      return 0;
    } else{
      val adjacent = (coord1:(Int,Int), coord2:(Int,Int)) => {
        if (isAdjacentCoord(coord1, coord2))
          1;
        else
          0;
      }
      return numAdjacentMines(coord, mines.drop(1)) + adjacent(coord,mines.head);
    }
  }

  def isAdjacentCoord(first: (Int,Int), second: (Int,Int)): Boolean ={
    return isDifferenceOneOrZero(first._1, second._1) && isDifferenceOneOrZero(first._1, second._2);
  }

  def isDifferenceOneOrZero(first: Int, second: Int) : Boolean = {
    return Math.abs(first-second) == 1 || first-second == 0;
  }


}
