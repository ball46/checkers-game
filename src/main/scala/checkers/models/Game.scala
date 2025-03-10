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

  import Game.MoveLogic._
  import Game.GameStateLogic._

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

  def getValidMovesForPlayer(player: Color): Map[Position, List[Move]] = {
    if (isContinuation) {
      lastMovedPosition.map(pos => validMovesList(pos, player, board)).getOrElse(Map.empty)
    } else {
      getAllValidMovesForPlayer(player, board)
    }
  }
}

object Game {
  private def initial(name: String): Game = Game(name, Board.initial, White, InProgress)

  def initialSinglePlayer(name: String): Game = initial(name)

  def initialTuneBasePlayer(name: String): Game = initial(name)

  private object MoveLogic {
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

    def shouldContinueWithSamePlayer(
                                      move: Move,
                                      board: Board,
                                      continuationMoves: List[Move]
                                    ): Boolean = {
      move.isJumpMove && !board.isDiagonalPathClear(move) && continuationMoves.nonEmpty
    }

    def getNextPlayer(currentPlayer: Color): Color = {
      if (currentPlayer == White) Black else White
    }

    def validMovesList(pos: Position, player: Color, board: Board): Map[Position, List[Move]] = {
      val moves = getPossibleMoves(pos, player, board)
      val validMoves = if (moves.jumpMoves.nonEmpty) moves.jumpMoves else moves.normalMoves
      if (validMoves.nonEmpty) Map(pos -> validMoves) else Map.empty
    }

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
    private def isValidDirection(move: Move, currentPlayer: Color, isKing: Boolean): Boolean = {
      val moveDirection = move.to.y - move.from.y
      isKing || (currentPlayer match {
        case White => moveDirection < 0 // White moves up
        case Black => moveDirection > 0 // Black moves down
      })
    }

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