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
 * Contains the possible moves from a position, categorized by type.
 *
 * @param normalMoves Regular moves (non-jumps)
 * @param jumpMoves   Jump moves that capture opponent pieces
 */
case class DirectionalMoves(
  normalMoves: List[Move] = List.empty,
  jumpMoves: List[Move] = List.empty
)

/**
 * Groups moves by their starting position and type.
 *
 * @param normalMoves Map of positions to their possible regular moves
 * @param jumpMoves   Map of positions to their possible jump moves
 */
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

  import Game.MoveLogic._
  import Game.GameStateLogic._

  /**
   * Executes a move and returns the updated game state.
   *
   * @param move The move to execute
   * @return Either a GameError if the move is invalid, or the updated Game state
   */
  def makeMove(move: Move): Either[GameError, Game] = {
    status match {
      case GameOver(_) => Left(GameAlreadyOver)
      case InProgress =>
        val currentMoves = getPossibleMoves(move.from, currentPlayer, board)
        val isValidMove = validateMove(move, currentMoves, lastMovedPosition, isContinuation)

        if (isValidMove) {
          board.makeMove(move, currentPlayer).map { newBoard =>
            val continuationMoves = getPossibleMoves(move.to, currentPlayer, newBoard).jumpMoves

            if (shouldContinueWithSamePlayer(move, board, continuationMoves)) {
              copy(
                board = newBoard,
                isContinuation = true,
                lastMovedPosition = Some(move.to)
              )
            } else {
              val nextPlayer = getNextPlayer(currentPlayer)
              val newStatus = determineGameStatus(newBoard, nextPlayer)

              copy(
                board = newBoard,
                currentPlayer = nextPlayer,
                status = newStatus,
                isContinuation = false,
                lastMovedPosition = None
              )
            }
          }.left.map(GameMovementError.apply)
        } else {
          Left(GameMovementError(InvalidMove(move)))
        }
    }
  }

  /**
   * Retrieves all valid moves for a specific player.
   *
   * @param player The player whose moves to retrieve
   * @return A map with positions as keys and their possible moves as values
   */
  def getValidMovesForPlayer(player: Color): Map[Position, List[Move]] = {
    if (isContinuation) {
      lastMovedPosition.map(pos => validMovesList(pos, player, board)).getOrElse(Map.empty)
    } else {
      getAllValidMovesForPlayer(player, board)
    }
  }
}

object Game {
  /**
   * Creates a new game with the initial board setup.
   *
   * @param name The name for the new game
   * @return A new Game instance in its initial state
   */
  private def initial(name: String): Game = Game(name, Board.initial, White, InProgress)

  /**
   * Creates a new single-player game.
   *
   * @param name The name for the new game
   * @return A new Game instance in single player mode
   */
  def initialSinglePlayer(name: String): Game = initial(name)

  /**
   * Creates a new tune-based player game.
   *
   * @param name The name for the new game
   * @return A new Game instance in tune-based player mode
   */
  def initialTuneBasePlayer(name: String): Game = initial(name)

  private object MoveLogic {
    /**
     * Validates if a move is legal according to the current game state.
     *
     * @param move              The move to validate
     * @param currentMoves      Available moves from the starting position
     * @param lastMovedPosition The position of the last moved piece (for continuations)
     * @param isContinuation    Whether this is a continuation move
     * @return True if the move is valid, false otherwise
     */
    def validateMove(
                      move: Move,
                      currentMoves: DirectionalMoves,
                      lastMovedPosition: Option[Position],
                      isContinuation: Boolean
                    ): Boolean = {
      val hasJumpMoves = currentMoves.jumpMoves.nonEmpty

      if (isContinuation) {
        lastMovedPosition.contains(move.from) && currentMoves.jumpMoves.contains(move)
      } else if (hasJumpMoves) {
        currentMoves.jumpMoves.contains(move)
      } else {
        currentMoves.normalMoves.contains(move)
      }
    }

    /**
     * Determines if the player should continue moving the same piece (multi-jump case).
     *
     * @param move              The move just executed
     * @param board             The current board state
     * @param continuationMoves Possible subsequent jumps
     * @return True if the player should make another move with the same piece
     */
    def shouldContinueWithSamePlayer(
                                      move: Move,
                                      board: Board,
                                      continuationMoves: List[Move]
                                    ): Boolean = {
      move.isJumpMove && !board.isDiagonalPathClear(move) && continuationMoves.nonEmpty
    }

    /**
     * Gets the opposing player.
     *
     * @param currentPlayer The current player
     * @return The opposing player
     */
    def getNextPlayer(currentPlayer: Color): Color = {
      if (currentPlayer == White) Black else White
    }

    /**
     * Gets a list of valid moves for a specific position.
     *
     * @param pos    The position to check for moves
     * @param player The player whose piece is at the position
     * @param board  The current board state
     * @return A map with the position and its valid moves, or empty if no valid moves exist
     */
    def validMovesList(pos: Position, player: Color, board: Board): Map[Position, List[Move]] = {
      val moves = getPossibleMoves(pos, player, board)
      val validMoves = if (moves.jumpMoves.nonEmpty) moves.jumpMoves else moves.normalMoves
      if (validMoves.nonEmpty) Map(pos -> validMoves) else Map.empty
    }

    /**
     * Gets all valid moves for a player across the entire board.
     *
     * @param player The player to check
     * @param board  The current board state
     * @return A map of positions to their valid moves
     */
    def getAllValidMovesForPlayer(player: Color, board: Board): Map[Position, List[Move]] = {
      val stackMoves = (for {
        y <- board.pieces.indices
        x <- board.pieces(y).indices
        pos = Position(x, y)
        piece <- board(pos).toOption.flatten
        if piece.color == player
      } yield {
        val moves = getPossibleMoves(pos, player, board)
        (pos -> moves.normalMoves, pos -> moves.jumpMoves)
      }).foldLeft(StackMoves()) { case (acc, (normal, jump)) =>
        StackMoves(
          normalMoves = if (normal._2.nonEmpty) acc.normalMoves + normal else acc.normalMoves,
          jumpMoves = if (jump._2.nonEmpty) acc.jumpMoves + jump else acc.jumpMoves
        )
      }

      if (stackMoves.jumpMoves.nonEmpty) stackMoves.jumpMoves
      else stackMoves.normalMoves
    }

    /**
     * Finds all possible moves from a specific position.
     *
     * @param pos           The position to check
     * @param currentPlayer The current player
     * @param board         The current board state
     * @return DirectionalMoves containing all possible normal and jump moves
     */
    def getPossibleMoves(pos: Position, currentPlayer: Color, board: Board): DirectionalMoves = {
      board(pos).toOption.flatten match {
        case Some(piece) =>
          val directions = List((-1, -1), (-1, 1), (1, -1), (1, 1))
          directions.foldLeft(DirectionalMoves()) { case (acc, (dx, dy)) =>
            val dirMoves = DirectionLogic.findMovesInDirection(pos, dx, dy, currentPlayer, piece.isKing, board)
            DirectionalMoves(
              normalMoves = acc.normalMoves ++ dirMoves.normalMoves,
              jumpMoves = acc.jumpMoves ++ dirMoves.jumpMoves
            )
          }
        case None => DirectionalMoves()
      }
    }
  }

  private object DirectionLogic {
    /**
     * Checks if a move direction is valid for the current piece.
     *
     * @param move          The move to validate
     * @param currentPlayer The player making the move
     * @param isKing        Whether the piece is a king
     * @return True if the direction is valid, false otherwise
     */
    private def isValidDirection(move: Move, currentPlayer: Color, isKing: Boolean): Boolean = {
      val moveDirection = move.to.y - move.from.y
      isKing || (currentPlayer match {
        case White => moveDirection < 0 // White moves up
        case Black => moveDirection > 0 // Black moves down
      })
    }

    /**
     * Finds all possible moves in a specific direction from a position.
     *
     * @param from          The starting position
     * @param dx            The x-direction delta (-1 or 1)
     * @param dy            The y-direction delta (-1 or 1)
     * @param currentPlayer The player making the move
     * @param isKing        Whether the piece is a king
     * @param board         The current board state
     * @return DirectionalMoves containing all possible moves in the specified direction
     */
    def findMovesInDirection(
                              from: Position,
                              dx: Int,
                              dy: Int,
                              currentPlayer: Color,
                              isKing: Boolean,
                              board: Board
                            ): DirectionalMoves = {
      @tailrec
      def findMovesRec(
                        currentX: Int,
                        currentY: Int,
                        normalMoves: List[Move],
                        jumpMoves: List[Move]
                      ): DirectionalMoves = {
        if (!Position(currentX, currentY).isValid) {
          DirectionalMoves(normalMoves, jumpMoves)
        } else {
          val currentPos = Position(currentX, currentY)
          val currentMove = Move(from, currentPos)

          board(currentPos).toOption.flatten match {
            case None if !isKing =>
              // Non-king piece on empty square
              if ((currentX - from.x).abs == 1 && isValidDirection(currentMove, currentPlayer, isKing)) {
                DirectionalMoves(currentMove :: normalMoves, jumpMoves)
              } else {
                DirectionalMoves(normalMoves, jumpMoves)
              }

            case None =>
              // King piece on empty square - continue in the same direction
              findMovesRec(
                currentX + dx,
                currentY + dy,
                currentMove :: normalMoves,
                jumpMoves
              )

            case Some(piece) if piece.color != currentPlayer =>
              // Opponent's piece - check for jump
              val jumpX = currentX + dx
              val jumpY = currentY + dy
              val jumpPos = Position(jumpX, jumpY)
              val jumpMove = Move(from, jumpPos)

              if (jumpPos.isValid &&
                board(jumpPos).toOption.flatten.isEmpty &&
                isValidDirection(jumpMove, currentPlayer, isKing)) {
                DirectionalMoves(normalMoves, jumpMove :: jumpMoves)
              } else {
                DirectionalMoves(normalMoves, jumpMoves)
              }

            case Some(_) =>
              // Own piece - blocked
              DirectionalMoves(normalMoves, jumpMoves)
          }
        }
      }

      findMovesRec(from.x + dx, from.y + dy, List.empty, List.empty)
    }
  }

  private object GameStateLogic {
    /**
     * Checks if a player has any valid moves.
     *
     * @param board  The current board state
     * @param player The player to check
     * @return True if the player has at least one valid move, false otherwise
     */
    private def hasValidMoves(board: Board, player: Color): Boolean = {
      board.pieces.indices.exists { y =>
        board.pieces(y).indices.exists { x =>
          val pos = Position(x, y)
          board(pos).toOption.flatten.exists(_.color == player) && {
            val moves = MoveLogic.getPossibleMoves(pos, player, board)
            moves.jumpMoves.nonEmpty || moves.normalMoves.nonEmpty
          }
        }
      }
    }

    /**
     * Determines the game status after a move.
     *
     * @param board      The current board state
     * @param nextPlayer The next player to move
     * @return The updated game status (InProgress or GameOver)
     */
    def determineGameStatus(board: Board, nextPlayer: Color): GameStatus = {
      val currentPlayer = MoveLogic.getNextPlayer(nextPlayer)
      val currentPlayerMoves = hasValidMoves(board, currentPlayer)
      val nextPlayerMoves = hasValidMoves(board, nextPlayer)

      (currentPlayerMoves, nextPlayerMoves) match {
        case (false, false) => GameOver(None)
        case (_, false) => GameOver(Some(currentPlayer))
        case _ => InProgress
      }
    }
  }
}