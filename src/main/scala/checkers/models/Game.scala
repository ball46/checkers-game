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

case class DirectionalMoves(
  normalMoves: List[Move] = List.empty,
  jumpMoves: List[Move] = List.empty
)

case class StackMoves(
  normalMoves: Map[Position, List[Move]] = Map.empty,
  jumpMoves: Map[Position, List[Move]] = Map.empty
)

/**
 * Represents a game with its name, board, current player, and status.
 *
 * @param name          The name of the game.
 * @param board         The board of the game.
 * @param currentPlayer The current player.
 * @param status        The status of the game.
 * @param isContinuation Indicates if the current move is a continuation of a previous jump.
 * @param lastMovedPosition The position of the last moved piece, used for continuation moves.
 */
case class Game(
  name: String,
  board: Board,
  currentPlayer: Color,
  status: GameStatus = InProgress,
  isContinuation: Boolean = false,
  lastMovedPosition: Option[Position] = None
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
        for {
          currentMoves <- Right(getPossibleMoves(move.from, currentPlayer))
          _ <- validateMoveConditions(move, currentMoves)
          newBoard <- board.makeMove(move, currentPlayer).left.map(GameMovementError.apply)
          updatedGame = updateGameAfterMove(move, newBoard)
        } yield updatedGame
    }
  }

  private def validateMoveConditions(move: Move, currentMoves: DirectionalMoves): Either[GameError, Boolean] = {
    if (isContinuation) {
      // ตรวจสอบเฉพาะการเคลื่อนที่ต่อเนื่อง
      Either.cond(
        lastMovedPosition.contains(move.from) && currentMoves.jumpMoves.contains(move),
        true,
        GameMovementError(InvalidMove(move))
      )
    } else {
      val hasJumpMoves = currentMoves.jumpMoves.nonEmpty
      Either.cond(
        if (hasJumpMoves) currentMoves.jumpMoves.contains(move)
        else currentMoves.normalMoves.contains(move),
        true,
        GameMovementError(InvalidMove(move))
      )
    }
  }

  private def updateGameAfterMove(move: Move, newBoard: Board): Game = {
    if (shouldContinueJump(move, newBoard)) continueMoveForJump(move, newBoard)
    else finishMove(newBoard)
  }

  private def shouldContinueJump(move: Move, newBoard: Board): Boolean =
    move.isJumpMove &&
      !board.isDiagonalPathClear(move) &&
      getPossibleMoves(move.to, currentPlayer, newBoard).jumpMoves.nonEmpty

  private def continueMoveForJump(move: Move, newBoard: Board): Game =
    copy(
      board = newBoard,
      isContinuation = true,
      lastMovedPosition = Some(move.to)
    )

  private def finishMove(newBoard: Board): Game = {
    val nextPlayer = if (currentPlayer == White) Black else White
    val newStatus = determineGameStatus(newBoard, nextPlayer)
    copy(
      board = newBoard,
      currentPlayer = nextPlayer,
      status = newStatus,
      isContinuation = false,
      lastMovedPosition = None
    )
  }

  /**
   * Retrieves the valid moves for the current player.
   *
   * @param player The current player.
   * @return A map of positions to lists of valid moves.
   */
  def getValidMovesForPlayer(player: Color): Map[Position, List[Move]] = {
    if (isContinuation) {
      // During continuation, only get moves from the last moved piece
      val lastMove = lastMovedPosition.get
      validMovesList(lastMove, player)
    } else {
      // Collect all moves into StackMoves
      val stackMoves = collectStackMoves(player)

      // Return jump moves if available, otherwise normal moves
      if (stackMoves.jumpMoves.nonEmpty) stackMoves.jumpMoves
      else stackMoves.normalMoves
    }
  }

  private def collectStackMoves(player: Color): StackMoves = {
    (for {
      y <- board.pieces.indices
      x <- board.pieces(y).indices
      pos = Position(x, y)
      piece <- board(pos).toOption.flatten
      if piece.color == player
    } yield {
      val moves = getPossibleMoves(pos, player)
      (pos -> moves.normalMoves, pos -> moves.jumpMoves)
    }).foldLeft(StackMoves()) { case (acc, (normal, jump)) =>
      StackMoves(
        normalMoves = if (normal._2.nonEmpty) acc.normalMoves + normal else acc.normalMoves,
        jumpMoves = if (jump._2.nonEmpty) acc.jumpMoves + jump else acc.jumpMoves
      )
    }
  }

  /**
   * Retrieves the valid moves for a piece at the given position.
   *
   * @param pos    The position of the piece.
   * @param player The current player.
   * @return A map of positions to lists of valid moves.
   */
  private def validMovesList(pos: Position, player: Color): Map[Position, List[Move]] = {
    val moves = getPossibleMoves(pos, player)
    val validMoves = if (moves.jumpMoves.nonEmpty) {
      moves.jumpMoves
    } else {
      moves.normalMoves
    }
    if (validMoves.nonEmpty) Map(pos -> validMoves) else Map.empty
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
        board(pos).toOption.flatten.exists(_.color == player) && {
          val moves = getPossibleMoves(pos, player)
          moves.jumpMoves.nonEmpty || moves.normalMoves.nonEmpty
        }
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
    val currentPlayerMoves = hasValidMoves(board, currentPlayer)
    val nextPlayerMoves = hasValidMoves(board, nextPlayer)
    (currentPlayerMoves, nextPlayerMoves) match {
      case (false, false) => GameOver(None)
      case (_, false) => GameOver(Some(currentPlayer))
      case _ => InProgress
    }
  }

  /**
   * Finds the possible moves in a given direction for a piece.
   *
   * @param from          The starting position of the piece.
   * @param dx            The change in x direction.
   * @param dy            The change in y direction.
   * @param currentPlayer The current player.
   * @param isKing        Indicates if the piece is a king.
   * @param board         The board of the game.
   * @return The possible moves in the given direction.
   */
  private def findMovesInDirection(
    from: Position,
    dx: Int,
    dy: Int,
    currentPlayer: Color,
    isKing: Boolean,
    board: Board = board
  ): DirectionalMoves = {
    def isValidDirection(move: Move): Boolean = {
      val moveDirection = move.to.y - move.from.y
      isKing || (currentPlayer match {
        case White => moveDirection < 0  // White moves up
        case Black => moveDirection > 0  // Black moves down
      })
    }

    @tailrec
    def findMoves(
      currentX: Int,
      currentY: Int,
      normalMoves: List[Move],
      jumpMoves: List[Move]
    ): DirectionalMoves = {
      if (currentX < 0 || currentX >= 8 || currentY < 0 || currentY >= 8) {
        DirectionalMoves(normalMoves, jumpMoves)
      } else {
        val currentPos = Position(currentX, currentY)
        val currentMove = Move(from, currentPos)
        
        board(currentPos).toOption.flatten match {
          case None =>
            if (!isKing) {
              if ((currentX - from.x).abs == 1 && isValidDirection(currentMove)) {
                DirectionalMoves(currentMove :: normalMoves, jumpMoves)
              } else {
                DirectionalMoves(normalMoves, jumpMoves)
              }
            } else {
              findMoves(
                currentX + dx,
                currentY + dy,
                currentMove :: normalMoves,
                jumpMoves
              )
            }
            
          case Some(piece) if piece.color != currentPlayer =>
            val jumpX = currentX + dx
            val jumpY = currentY + dy
            val jumpPos = Position(jumpX, jumpY)
            val jumpMove = Move(from, jumpPos)
            
            if (jumpPos.isValid &&
                board(jumpPos).toOption.flatten.isEmpty && 
                isValidDirection(jumpMove)) {
              DirectionalMoves(normalMoves, jumpMove :: jumpMoves)
            } else {
              DirectionalMoves(normalMoves, jumpMoves)
            }
            
          case Some(_) =>
            DirectionalMoves(normalMoves, jumpMoves)
        }
      }
    }

    findMoves(from.x + dx, from.y + dy, List.empty, List.empty)
  }

  /**
   * Retrieves the possible moves for a piece at the given position.
   *
   * @param pos           The position of the piece.
   * @param currentPlayer The current player.
   * @param board         The board of the game.
   * @return The possible moves for the piece.
   */
  private def getMoves(pos: Position, currentPlayer: Color, isKing: Boolean, isContinuation: Boolean, board: Board): DirectionalMoves = {
    val directions = List((-1, -1), (-1, 1), (1, -1), (1, 1))
    val moves = directions.foldLeft(DirectionalMoves()) { case (acc, (dx, dy)) =>
      val newMoves = findMovesInDirection(pos, dx, dy, currentPlayer, isKing, board)
      DirectionalMoves(
        normalMoves = acc.normalMoves ++ newMoves.normalMoves,
        jumpMoves = acc.jumpMoves ++ newMoves.jumpMoves
      )
    }
    moves
  }

  /**
   * Retrieves the possible moves for a piece at the given position.
   *
   * @param pos            The position of the piece.
   * @param currentPlayer  The current player.
   * @param board          The board of the game.
   * @return A list of possible moves.
   */
  private def getPossibleMoves(pos: Position, currentPlayer: Color = currentPlayer, board: Board = board): DirectionalMoves = {
    val piece = board(pos).toOption.flatten
    piece match {
      case Some(p) => getMoves(pos, currentPlayer, p.isKing, isContinuation, board)
      case None => DirectionalMoves()
    }
  }

  /**
   * Retrieves the list of moves from the given DirectionalMoves.
   *
   * @param allMoves The DirectionalMoves to retrieve the moves from.
   * @return A list of moves.
   */
  private def getListMoves(allMoves: DirectionalMoves): List[Move] = {
    allMoves.jumpMoves match {
      case jumps if jumps.nonEmpty => jumps
      case _ if !isContinuation => allMoves.normalMoves
      case _ => List.empty
    }
  }
}

object Game {
  private def initial(name: String): Game = Game(name, Board.initial, White, InProgress)

  /**
   * Creates an initial single-player game.
   *
   * @param name The name of the game.
   * @return The initial game.
   */
  def initialSinglePlayer(name: String): Game = initial(name)

  /**
   * Creates an initial game with two players.
   *
   * @param name The name of the game.
   * @return The initial game.
   */
  def initialTuneBasePlayer(name: String): Game = initial(name)
}