package com.blevinstein.phutball

class Position(val x: Int, val y: Int) {
  val cols = List("A", "B", "C", "D", "E", "F", "G", "H", "J", "K", "L", "M", "N", "O", "P")
  val rows = List("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14",
      "15", "16", "17", "18", "19")

  override def toString = cols(x) + rows(y)
}

abstract class Move
case class Add(position: Position) extends Move
case class Jump(positions: List[Position]) extends Move

abstract class Square
case class Empty() extends Square
case class Man() extends Square
case class Ball() extends Square

object Board {
  val height = 19
  val width = 15
  val center = new Position(7, 9)

  val allPositions = for (i <- 0 until width; j <- 0 until height) yield new Position(i, j)
  val allDirections = for (di <- -1 until 2; dj <- -1 until 2 if di != 0 || dj != 0) yield (di, dj)

  val empty = new Board(Array.fill[Square](width, height) { Empty() })
  val newBoard = empty.update(Map(center -> Ball()))
}
class Board(state: Array[Array[Square]]) {
  val ballPosition = (for (pos <- Board.allPositions if get(pos) == Ball())
    yield pos
  ).headOption

  def update(changes: Map[Position, Square]): Board = {
    var newState = copy2d(state)
    for ((pos, square) <- changes) {
      newState = update2d(newState, pos.x, pos.y, square)
    }
    new Board(newState)
  }

  def get(position: Position): Option[Square] =
    if (position.x > 0 && position.x < Board.width && position.y > 0 && position.y < Board.height)
      Some(state(position.x)(position.y))
    else
      None
  
  def getJump(start: Position, dir: (Int, Int)) : Option[Position] = {
    var current = add(start, dir)
    if (get(current) != Man) {
      None // Nothing to jump over
    } else {
      // Jump over one or more men
      while (get(current) == Man) {
        current = add(current, dir)
      }
      if (get(current) == Empty()) {
        // Land on empty space
        Some(current)
      } else {
        // Went off edge of board
        None
      }
    }
  }
  
  // TODO: This function returns all legal chains of jumps, use recursion
  // This function returns all jumps that can be made in this position
  def getJumpMoves: List[Move] = {
    def getJumpMoves(prefix: List[Position]): List[Move] = {
      val jumpDests = (for (dir <- Board.allDirections)
        yield getJump(ballPosition.get, dir)
      ).flatten
      (for (dest <- jumpDests)
        yield Jump(prefix :+ dest)
      ).toList
    }
    getJumpMoves(List())
  }

  def getMoves: List[Move] = {
    val addMoves = (for (pos <- Board.allPositions if get(pos) == Empty()) yield Add(pos)).toList
    val jumpMoves = getJumpMoves
    addMoves ++ jumpMoves
  }
  def after(move: Move): Board = {
    move match {
      case Add(pos) => update(Map(pos -> Man()))
      // TODO: remove men who are jumped over
      case Jump(positions) => {
        var updateMap = Map[Position, Square]()
        updateMap += ((ballPosition.get, Empty()))
        updateMap += ((positions.last, Ball()))
        update(updateMap)
      }
    }
  }
 
  def add(position : Position, direction : (Int, Int)) =
    new Position(position.x + direction._1, position.y + direction._2)

  // 2D Array Manipulation
  def copy2d(source : Array[Array[Square]]): Array[Array[Square]] = source.map(_.clone).toArray
  def update2d(source : Array[Array[Square]], x : Int, y : Int, value : Square): Array[Array[Square]] =
    (for (i <- 0 until Board.width) yield
      if (i == x) {
        (for (j <- 0 until Board.height) yield
          if (j == y) {
            value
          } else {
            source(i)(j)
          }
        ).toArray
      } else {
        source(i)
      }
    ).toArray
}

