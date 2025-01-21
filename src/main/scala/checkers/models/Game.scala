package checkers.models

import scala.annotation.tailrec

/**
 * Represents the status of a game.
 */
sealed trait GameStatus
case object InProgress extends GameStatus
case class GameOver(winner: Option[Color]) extends GameStatus

/**
 * Represents different types of game errors.
 */
sealed trait GameError
case class GameMovementError(boardError: BoardError) extends GameError
case class GameNotFound(id: String) extends GameError
case object GameIsProgress extends GameError
case object GameAlreadyOver extends GameError
case object DuplicateGameName extends GameError

/**
 * Represents a game with its name, board, current player, and status.
 *
 * @param name          The name of the game.
 * @param board         The board of the game.
 * @param currentPlayer The current player.
 * @param status        The status of the game.
 */
case class Game(
  name: String,
  board: Board,
  currentPlayer: Color,
  status: GameStatus = InProgress
) {
  /**
   * Makes a move in the game.
   *
   * @param move The move to be made.
   * @return Either a GameError or the updated Game.
   */
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

  /**
   * Retrieves the valid moves for the current player.
   *
   * @param player The current player.
   * @return A map of positions to lists of valid moves.
   */
  def getValidMovesForPlayer(player: Color): Map[Position, List[Move]] = {
    board.pieces.indices.flatMap { y =>
      board.pieces(y).indices.collect {
        case x if board(Position(x, y)).toOption.flatten.exists(_.color == player) =>
          Position(x, y) -> getValidMoves(Position(x, y))
      }
    }.toMap.filter(_._2.nonEmpty)
  }

  /**
   * Checks if the given player has valid moves.
   *
   * @param board  The board of the game.
   * @param player The player to check.
   * @return True if the player has valid moves, false otherwise.
   */
  private def hasValidMoves(board: Board, player: Color): Boolean = {
    board.pieces.indices.exists { y =>
      board.pieces(y).indices.exists { x =>
        val pos = Position(x, y)
        board(pos).toOption.flatten.exists(_.color == player) && getPossibleMoves(pos, player).nonEmpty
      }
    }
  }

  /**
   * Determines the status of the game based on the board and the next player.
   *
   * @param board      The board of the game.
   * @param nextPlayer The next player.
   * @return The status of the game.
   */
  private def determineGameStatus(board: Board, nextPlayer: Color): GameStatus = {
    val currentPlayerHasMoves = hasValidMoves(board, currentPlayer)
    val nextPlayerHasMoves = hasValidMoves(board, nextPlayer)

    if (!currentPlayerHasMoves && !nextPlayerHasMoves) GameOver(None)
    else if (!nextPlayerHasMoves) GameOver(Some(currentPlayer))
    else InProgress
  }

  private def getDiagonalMoves(pos: Position, currentPlayer: Color): List[Move] = {
  // ทิศทางทั้ง 4 ทิศทางของการเดินแนวทแยง
  val directions = List((-1, -1), (-1, 1), (1, -1), (1, 1))
  
  // flatMap: แปลงแต่ละทิศทางเป็น List[Move] แล้วรวมเป็น List เดียว
  directions.flatMap { case (dx, dy) =>
    // getDiagonalMovesInDirection: หา moves ในทิศทางเดียว
    getDiagonalMovesInDirection(pos, currentPlayer, dx, dy)
  }
}

  // Helper function สำหรับหา moves ในทิศทางที่กำหนด
  @tailrec
  private def getDiagonalMovesInDirection(
    pos: Position,
    currentPlayer: Color,
    dx: Int,
    dy: Int,
    step: Int = 1,
    acc: List[Move] = List.empty
  ): List[Move] = {
    // คำนวณตำแหน่งถัดไปตามทิศทาง
    val newX = pos.x + (dx * step)
    val newY = pos.y + (dy * step)
    val to = Position(newX, newY)
    val move = Move(pos, to)

    // Base cases: เงื่อนไขการหยุด recursion
    if (!to.isValid) {
      acc  // ออกนอกกระดาน
    } else if (!board.isValidMove(move, currentPlayer)) {
      acc  // เจอหมากขวาง หรือ invalid move
    } else {
      // Recursive case: เพิ่ม move ปัจจุบัน และหา moves ถัดไป
      getDiagonalMovesInDirection(
        pos,
        currentPlayer,
        dx,
        dy,
        step + 1,
        move :: acc
      )
    }
  }

  private def getJumpMoves(pos: Position, currentPlayer: Color): List[Move] = {
    val directions = List((-2, -2), (-2, 2), (2, -2), (2, 2))
    Moves(pos, currentPlayer, directions)
  }

  private def getNormalMoves(pos: Position, currentPlayer: Color): List[Move] = {
    val directions = List((-1, -1), (-1, 1), (1, -1), (1, 1))
    Moves(pos, currentPlayer, directions)
  }

  private def Moves(pos: Position, currentPlayer: Color, possibleList: List[(Int, Int)]): List[Move] = {
    val directions = possibleList
    directions.map { case (dx, dy) =>
      Move(pos, Position(pos.x + dx, pos.y + dy))
    }.filter(board.isValidMove(_, currentPlayer))
  }

  /**
   * Retrieves the possible moves for a piece at the given position.
   *
   * @param pos            The position of the piece.
   * @param currentPlayer  The current player.
   * @param isContinuation Boolean indicating if the move is a continuation.
   * @return A list of possible moves.
   */
  private def getPossibleMoves(pos: Position, currentPlayer: Color = currentPlayer, isContinuation: Boolean = false): List[Move] = {
    val piece = board(pos).toOption.flatten
    
    piece match {
      case Some(p) if p.isKing =>
        // King can move in any diagonal direction
        if (isContinuation) {
          // During continuation, only allow jumps
          getJumpMoves(pos, currentPlayer)
        } else {
          val jumps = getJumpMoves(pos, currentPlayer)
          if (jumps.nonEmpty) jumps
          else getDiagonalMoves(pos, currentPlayer)
        }
        
      case Some(_) =>
        // Normal piece movement
        val jumpMoves = getJumpMoves(pos, currentPlayer)
        if (jumpMoves.nonEmpty) jumpMoves
        else if (!isContinuation) getNormalMoves(pos, currentPlayer)
        else List.empty
        
      case None => List.empty
    }
  }

  /**
   * Retrieves the valid moves for a piece at the given position.
   *
   * @param position The position of the piece.
   * @return A list of valid moves.
   */
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
  /**
   * Creates an initial game with two players.
   *
   * @param name The name of the game.
   * @return The initial game.
   */
  def initialTuneBasePlayer(name: String): Game = Game(name,Board.initial, White)

  /**
   * Creates an initial single-player game.
   *
   * @param name The name of the game.
   * @return The initial game.
   */
  def initialSinglePlayer(name: String): Game = Game(name, Board.initial, White)
}