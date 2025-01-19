package checkers.models

sealed trait GameStatus
case object InProgress extends GameStatus
case class GameOver(winner: Option[Color]) extends GameStatus

sealed trait GameError
case class GameMovementError(boardError: BoardError) extends GameError
case class GameNotFound(id: String) extends GameError
case object GameIsProgress extends GameError
case object GameAlreadyOver extends GameError
case object DuplicateGameName extends GameError

case class Game(
  name: String,
  board: Board,
  currentPlayer: Color,
  status: GameStatus = InProgress
) {
  def makeMove(move: Move): Either[GameError, Game] = {
    status match {
      case GameOver(_) => Left(GameAlreadyOver)
      case InProgress =>
        val allMoves = getValidMovesForPlayer(currentPlayer)
        val jumpMoves = allMoves.filter { case (_, moves) => moves.exists(_.isJump) }

        val validMoves = if (jumpMoves.nonEmpty) jumpMoves else allMoves

        if (validMoves.values.flatten.exists(_ == move)) {
          board.makeMove(move, currentPlayer).map { newBoard =>
            val nextPlayer = if (currentPlayer == White) Black else White
            val newStatus = determineGameStatus(newBoard, nextPlayer)
            val continuationMoves = getPossibleMoves(move.to, currentPlayer, isContinuation = true)
            if (continuationMoves.nonEmpty) {
              Game(name, newBoard, currentPlayer, InProgress)
            } else {
              Game(name, newBoard, nextPlayer, newStatus)
            }
          }.left.map {
            case InvalidPosition(pos) => GameMovementError(InvalidPosition(pos))
            case InvalidMove(m) => GameMovementError(InvalidMove(m))
            case WrongPlayer(color) => GameMovementError(WrongPlayer(color))
          }
        } else {
          Left(GameMovementError(InvalidMove(move)))
        }
    }
  }

  def getValidMovesForPlayer(player: Color): Map[Position, List[Move]] = {
    board.pieces.indices.flatMap { y =>
      board.pieces(y).indices.collect {
        case x if board(Position(x, y)).toOption.flatten.exists(_.color == player) =>
          Position(x, y) -> getValidMoves(Position(x, y))
      }
    }.toMap.filter(_._2.nonEmpty)
  }

  private def hasValidMoves(board: Board, player: Color): Boolean = {
    board.pieces.indices.exists { y =>
      board.pieces(y).indices.exists { x =>
        val pos = Position(x, y)
        board(pos).toOption.flatten.exists(_.color == player) && getPossibleMoves(pos, player).nonEmpty
      }
    }
  }

  private def determineGameStatus(board: Board, nextPlayer: Color): GameStatus = {
    val currentPlayerHasMoves = hasValidMoves(board, currentPlayer)
    val nextPlayerHasMoves = hasValidMoves(board, nextPlayer)

    if (!currentPlayerHasMoves && !nextPlayerHasMoves) GameOver(None)
    else if (!nextPlayerHasMoves) GameOver(Some(currentPlayer))
    else InProgress
  }

  private def getPossibleMoves(pos: Position, currentPlayer: Color = currentPlayer, isContinuation: Boolean = false): List[Move] = {
    val jumpMoves = for {
      dx <- List(-2, 2)
      dy <- List(-2, 2)
      to = Position(pos.x + dx, pos.y + dy)
      move = Move(pos, to)
      if board.isValidMove(move, currentPlayer)
    } yield move

    if (jumpMoves.nonEmpty) jumpMoves
    else if (!isContinuation) {
      val normalMoves = for {
        dx <- List(-1, 1)
        dy <- List(-1, 1)
        to = Position(pos.x + dx, pos.y + dy)
        move = Move(pos, to)
        if board.isValidMove(move, currentPlayer)
      } yield move
      normalMoves
    } else {
      List.empty
    }
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
  def initialTuneBasePlayer(name: String): Game = Game(name,Board.initial, White)
  def initialSinglePlayer(name: String): Game = Game(name, Board.initial, White)
}