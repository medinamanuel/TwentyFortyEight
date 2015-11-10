package org.mmg.codeeval.game2048

import scala.annotation.tailrec
import scala.io.Source

/**
 * Created by mmedina on 15/08/18.
 */



/* State handler. Not making this a Monad now */

case class State[S,A](run: S => (A,S)) {
  def eval(s: S): A = run(s)._1
  def evalS(s: S):S = run(s)._2
}

/* Data definition */

/* Class that holds each tile position in row, col

   Extending ordered to be able to compare position by row first, and then col.
 */
case class Position(row: Int, col: Int) extends Ordered[Position] {
  def compare(that: Position) = {
    if (this.row > that.row)
      1
    else if (this.row < that.row)
      -1
    else {
      if (this.col > that.col)
        1
      else if (this.col < that.col)
        -1
      else
        1
    }
  }
}

/* The bpard consits of tiles. Each one contains its own position in the board, the number they hold
   and whether they have been merged or not.

   Extending Ordered trait to be able to compare their positions
 */
case class Tile(position: Position, number: Int, merged: Boolean) extends Ordered[Tile] {
  val BLANK_TILE = 0

  def isBlank = number == BLANK_TILE

  override def toString = number.toString

  def compare(that: Tile) = {
    this.position.compare(that.position)
  }
}


/*
  The game board.

  The field tiles is the representation of the grid, and it's just a map
  of positions to tiles.
 */
case class Board(size: Int, tiles: Map[Position, Tile]) {

  val RIGHT_EDGE = size  - 1
  val BOTTOM_EDGE = size - 1

  def getTilesFromRow(row: Int): List[Tile] = tiles.toList.filter{case (pos, _) => pos.row == row}.map{_._2}.sorted
  def getTilesFromColumn(col: Int): List[Tile] = tiles.toList.filter{case (pos, _) => pos.col == col}.map{_._2}.sorted

  override def toString =  {
    def rowTiles(row: Int): List[List[Tile]] = {
      if (row > RIGHT_EDGE)
        Nil
      else
        getTilesFromRow(row) :: rowTiles(row + 1)
    }

    rowTiles(0).map(_.mkString(" ")).mkString("|")

  }

}

/*
  Main object.

  Sliding the board means to slide each row or col depending on the move:

  UP or DOWN: slide rows
  LEFT or RIGHT: slide cols

  Sliding a row or col means to move it to the given direction as much as possible,
  solving collisions when then happen.

  The first row or col to be moved is always the closest one to one of the edges,
  depending on the given direction:

  UP: Top row
  DOWN: Bottom row
  LEFT: Left-most col
  RIGHT: Right-most col

  From there, continue moving the next one farther to the direction until reaching the
  final row or col opposite to the direction of the move.

 */
object TileMover {

  /* Valid moves */
  sealed abstract class Move
  case object UP extends Move
  case object DOWN extends Move
  case object LEFT extends Move
  case object RIGHT extends Move

  private val BLANK_TILE = 0

  type BoardState[A] = State[Board, A]
  type TileSeq = List[Tile]

  def canRowBeSlided(board: Board, direction: Move, row: Int): Boolean = direction match {
    case UP => row > 0
    case DOWN => row < board.BOTTOM_EDGE
    case LEFT | RIGHT => true
  }

  def canColBeSlided(board: Board, direction: Move, col: Int): Boolean = direction match {
    case LEFT => col > 0
    case RIGHT => col < board.RIGHT_EDGE
    case UP | DOWN => true
  }

  def canBeMerged(collidedTile: Tile, movedTile: Tile): Boolean = {
    !(collidedTile.merged || movedTile.merged) && !collidedTile.isBlank && movedTile.number == collidedTile.number
  }

  def mergeTiles(collidedTile: Tile, movedTile: Tile): (Tile, Tile) = {
    val newTile = Tile(movedTile.position, BLANK_TILE, false)
    val mergedTile = Tile(collidedTile.position, collidedTile.number * 2, true)

    (mergedTile, newTile)
  }


  def solveTileCollision(collidedSeq: TileSeq, movedSeq: TileSeq): (TileSeq, TileSeq) = {
    (collidedSeq zip movedSeq).map { case (collidedTile, movedTile) =>

      if (canBeMerged(collidedTile, movedTile)) {
        mergeTiles(collidedTile, movedTile)
      }
      else if (collidedTile.isBlank) {
        (Tile(collidedTile.position, movedTile.number, false), Tile(movedTile.position, BLANK_TILE, false))
      }
      else {
        (collidedTile, movedTile)
      }
    }.unzip
  }

  def getAffectedTiles(b: Board, direction: Move, tileSeqIndex: Int): (TileSeq, TileSeq, Int) = {
    val (movedTileSeq, collidedTileSeq, collidedIndex) =  direction match {
      case UP =>
        (b.getTilesFromRow(tileSeqIndex), b.getTilesFromRow(tileSeqIndex - 1), tileSeqIndex - 1)
      case DOWN =>
        (b.getTilesFromRow(tileSeqIndex), b.getTilesFromRow(tileSeqIndex + 1), tileSeqIndex + 1)
      case LEFT =>
        (b.getTilesFromColumn(tileSeqIndex), b.getTilesFromColumn(tileSeqIndex - 1), tileSeqIndex - 1 )
      case RIGHT =>
        (b.getTilesFromColumn(tileSeqIndex), b.getTilesFromColumn(tileSeqIndex + 1), tileSeqIndex + 1)
    }

    (movedTileSeq, collidedTileSeq, collidedIndex)
  }

  def updateBoard(b: Board, direction: Move, newCollidedTileSeq: TileSeq, collidedIndex: Int, newMovedTileSeq: TileSeq, tileSeqIndex: Int): Board = {

    def updateAll(f: (Move, Int, Int) => Position, direction: Move, tileRow: TileSeq, tiles: Map[Position, Tile], tileSeqIndex: Int): Map[Position, Tile] = {
      tileRow.zipWithIndex.foldLeft(tiles) { case (boardTiles, tileWithRelativePosition) =>
        val (newTile, tilePosition) = tileWithRelativePosition
        boardTiles.updated(f(direction, tileSeqIndex, tilePosition), newTile)
      }
    }

    val f = (d: Move, i1: Int, i2: Int) => direction match {
      case UP | DOWN => Position(i1,i2)
      case LEFT | RIGHT => Position(i2,i1)
    }

    val updatedTiles = {
      val tempTiles = updateAll(f, direction, newCollidedTileSeq, b.tiles, collidedIndex)
      updateAll(f, direction, newMovedTileSeq, tempTiles, tileSeqIndex)
    }


    Board(b.size, updatedTiles)
  }


  def slideTiles(direction: Move, tileSeqIndex: Int): BoardState[Unit] = State { b =>

    val canBeSlided = direction match {
      case UP | DOWN => (canRowBeSlided _).curried(b)(direction)
      case LEFT | RIGHT => (canColBeSlided _).curried(b)(direction)
    }

    if (!canBeSlided(tileSeqIndex))
      ((), b)
    else {
      val (movedTileSeq, collidedTileSeq, collidedIndex) = getAffectedTiles(b, direction, tileSeqIndex)
      val (newCollidedTileSeq, newMovedTileSeq) = solveTileCollision(collidedTileSeq, movedTileSeq)
      val newBoard = updateBoard(b, direction, newCollidedTileSeq,collidedIndex,newMovedTileSeq, tileSeqIndex)

      slideTiles(direction, collidedIndex) run newBoard

    }

  }

  def slide(direction: Move): BoardState[Unit] = State { b =>

    val rangeToMove = direction match {
      case UP | LEFT => 0 to b.size -1
      case DOWN | RIGHT => b.BOTTOM_EDGE - 1 to 0 by -1

    }

    val newBoard = rangeToMove.foldLeft(b){case (board, index) =>
      slideTiles(direction,index) evalS board
    }

    ((), newBoard)

  }


  /* Board from String */

  def createBoardFromString(s: String, size: Int): Board = {
    val rows = s.split("\\|").toList
    val splittedRows = rows.map { row =>
      row.split("\\s").toList
    }

    val tiles = splittedRows.zipWithIndex.foldLeft(Map[Position,Tile]()){case (tiles, rowInfo) =>
        val (rowElements, rowIndex) = rowInfo
        val mapElements = rowElements.zipWithIndex.map{case (element, colIndex) =>
            val elementPosition = Position(rowIndex,colIndex)
            elementPosition -> Tile(elementPosition, element.toInt, false)
        }

        tiles ++ mapElements
    }

    Board(size, tiles)
  }


  def getLinesFromFile(path: String) = Source.fromFile(path).getLines().filterNot(_.isEmpty)

  def parseLine(line: String): (Move,Board) = {
    val elements = line.split(";")
    val direction = elements(0).trim match {
      case "LEFT" => LEFT
      case "RIGHT" => RIGHT
      case "UP" => UP
      case "DOWN" => DOWN
    }

    val size = elements(1).trim.toInt
    val initialBoard = createBoardFromString(elements(2).trim, size)

    (direction,initialBoard)

  }

  def main(a: Array[String]): Unit = {

    if (a.length != 1) {
      println("Need a file to work with")
      System.exit(1)
    }

    getLinesFromFile(a(0)) map parseLine foreach { case (direction, initialBoard) =>
      println(slide(direction) evalS initialBoard)
    }

  }
}
