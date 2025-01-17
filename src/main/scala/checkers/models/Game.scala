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
        }.left.map {
          case InvalidPosition(pos) => GameMovementError(InvalidPosition(pos))
          case InvalidMove(m) => GameMovementError(InvalidMove(m))
          case WrongPlayer(color) => GameMovementError(WrongPlayer(color))
        }
    }
  }
  
  private def determineGameStatus(board: Board, nextPlayer: Color): GameStatus = {
    val hasValidMoves = board.pieces.indices.exists { y =>
      board.pieces(y).indices.exists { x =>
        val pos = Position(x, y)
        board(pos).toOption.flatten.exists(_.color == nextPlayer) && getPossibleMoves(pos).nonEmpty
      }
    }
    if (!hasValidMoves) GameOver(Some(currentPlayer)) else InProgress
  }

  private def getPossibleMoves(pos: Position): List[Move] = {
    val normalMoves = for {
      dx <- List(-1, 1)
      dy <- List(-1, 1)
      to = Position(pos.x + dx, pos.y + dy)
      move = Move(pos, to)
      if board.isValidMove(move, currentPlayer)
    } yield move

    val jumpMoves = for {
      dx <- List(-2, 2)
      dy <- List(-2, 2)
      to = Position(pos.x + dx, pos.y + dy)
      move = Move(pos, to)
      if board.isValidMove(move, currentPlayer)
    } yield move

    normalMoves ++ jumpMoves
  }
  
  def getValidMoves(position: Position): List[Move] = {
    status match {
      case GameOver(_) => List.empty
      case InProgress =>
        board(position).toOption.flatten match {
          case Some(piece) if piece.color == currentPlayer => getPossibleMoves(position)
          case _ => List.empty
        }
    }
  }
}

object Game {
  def initialTuneBasePlayer: Game = Game(Board.initial, White)
  def initialSinglePlayer: Game = Game(Board.initial, White)
}