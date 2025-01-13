package checkers.models

sealed trait GameStatus
case object InProgress extends GameStatus
case class GameOver(winner: Option[Color]) extends GameStatus

case class Game(
                 board: Board,
                 currentPlayer: Color,
                 status: GameStatus = InProgress
               ) {
  def makeMove(move: Move): Option[Game] = {
    if (status != InProgress) None
    else {
      board.makeMove(move, currentPlayer).map { newBoard =>
        val nextPlayer = if (currentPlayer == White) Black else White
        val newStatus = determineGameStatus(newBoard, nextPlayer)
        Game(newBoard, nextPlayer, newStatus)
      }
    }
  }

  private def determineGameStatus(board: Board, nextPlayer: Color): GameStatus = {
    // ตรวจสอบว่ามีการเดินที่เป็นไปได้สำหรับผู้เล่นถัดไปหรือไม่
    val hasValidMoves = board.pieces.indices.exists { y =>
      board.pieces(y).indices.exists { x =>
        val pos = Position(x, y)
        board(pos).exists(_.color == nextPlayer) && hasAnyValidMove(board, pos)
      }
    }

    if (!hasValidMoves) {
      GameOver(Some(currentPlayer)) // ผู้เล่นปัจจุบันชนะ
    } else {
      InProgress
    }
  }

  private def hasAnyValidMove(board: Board, from: Position): Boolean = {
    // ตรวจสอบการเดินทุกทิศทางที่เป็นไปได้
    val possibleMoves = for {
      dx <- List(-2, -1, 1, 2) // รวมทั้งการเดินปกติและการกินหมาก
      dy <- List(-2, -1, 1, 2)
      to = Position(from.x + dx, from.y + dy)
      move = Move(from, to)
    } yield move

    possibleMoves.exists(board.isValidMove(_, currentPlayer))
  }
}

object Game {
  def initial: Game = Game(Board.initial, White)
}