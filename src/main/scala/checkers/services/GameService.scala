package checkers.services

import cats.effect.IO
import checkers.models.*
import io.circe.generic.auto.*

case class GameNotFound(id: String) extends Exception(s"Game $id not found")

trait GameService {
  def createGame(singlePlayer: Boolean): IO[GameResponse]
  def findGame(id: String): IO[Option[GameResponse]]
  def makeMove(gameId: String, from: Position, to: Position): IO[Either[GameError, GameResponse]]
  def getValidMoves(game: Game): Map[Position, List[Move]]
}

case class GameResponse(
  id: String,
  board: Board,
  currentPlayer: Color,
  status: GameStatus,
  validMoves: Map[Position, List[Move]] = Map.empty
)

class GameServiceImpl extends GameService {
  private val games = scala.collection.concurrent.TrieMap[String, Game]()

  def createGame(singlePlayer: Boolean): IO[GameResponse] = IO {
    val gameId = java.util.UUID.randomUUID().toString
    val game = if (singlePlayer) Game.initialSinglePlayer else Game.initialTuneBasePlayer
    val validMoves = getValidMoves(game)
    games.put(gameId, game)
    GameResponse(gameId, game.board, game.currentPlayer, game.status, validMoves)
  }

  def findGame(id: String): IO[Option[GameResponse]] = 
    IO.pure(games.get(id).map(game => 
      GameResponse(id, game.board, game.currentPlayer, game.status)))

  def makeMove(gameId: String, from: Position, to: Position): IO[Either[GameError, GameResponse]] = {
  IO.pure(games.get(gameId)).flatMap {
    case None => 
      IO.pure(Left(GameMovementError(InvalidMove(Move(from, to)))))
    case Some(game) => 
      IO.pure(
        game.makeMove(Move(from, to)).map { newGame =>
          games.update(gameId, newGame)
          val validMoves = getValidMoves(newGame)
          GameResponse(gameId, newGame.board, newGame.currentPlayer, newGame.status, validMoves)
        }
      )
  }
}

  def getValidMoves(game: Game): Map[Position, List[Move]] = {
    game.board.pieces.indices.flatMap { y =>
      game.board.pieces(y).indices.collect {
        case x if game.board(Position(x, y)).toOption.flatten.exists(_.color == game.currentPlayer) =>
          Position(x, y) -> game.getValidMoves(Position(x, y))
      }
    }.toMap.filter(_._2.nonEmpty)
  }
}