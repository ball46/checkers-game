package checkers.models

sealed trait GameStatus
case object InProgress extends GameStatus
case class GameOver(winner: Option[Color]) extends GameStatus

sealed trait GameError
case class GameMovementError(boardError: BoardError) extends GameError
case object GameAlreadyOver extends GameError

case class Game(
                 board: Board,
                 currentPlayer: Color,
                 status: GameStatus = InProgress
               ) {
  def makeMove(move: Move): Either[GameError, Game] = {
    status match {
      case GameOver(_) => Left(GameAlreadyOver)
      case InProgress =>
        board.makeMove(move, currentPlayer).map { newBoard =>
          val nextPlayer = if (currentPlayer == White) Black else White
          val newStatus = determineGameStatus(newBoard, nextPlayer)
          Game(newBoard, nextPlayer, newStatus)
        }.left.map(GameMovementError.apply)
    }
  }

  private def determineGameStatus(board: Board, nextPlayer: Color): GameStatus = {
    // ตรวจสอบว่ามีการเดินที่เป็นไปได้สำหรับผู้เล่นถัดไปหรือไม่
    val hasValidMoves = board.pieces.indices.exists { y =>
      board.pieces(y).indices.exists { x =>
        val pos = Position(x, y)
        board(pos).toOption.flatten.exists(_.color == nextPlayer) && 
        hasAnyValidMove(board, pos)
      }
    }
    if (!hasValidMoves) GameOver(Some(currentPlayer))
    else InProgress
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