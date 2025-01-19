package checkers.services

import cats.effect.IO
import checkers.models.*
import io.circe.generic.auto.*

trait GameService {
  def createGame(name: String, singlePlayer: Boolean): IO[Either[GameError, GameResponse]]
  def getListGame: IO[List[GameListResponse]]
  def findGameById(id: String): IO[Option[GameResponse]]
  def findGameByName(name: String): IO[Option[GameResponse]]
  def deleteGame(gameId: String): IO[Either[GameError, Unit]]
  def makeMove(gameId: String, from: Position, to: Position): IO[Either[GameError, GameResponse]]
  def getValidMoves(game: Game): Map[Position, List[Move]]
}

case class GameResponse(
  id: String,
  name: String,
  board: Board,
  currentPlayer: Color,
  status: GameStatus,
  validMoves: Map[Position, List[Move]] = Map.empty
)

case class GameListResponse(name: String, id: String, status: GameStatus)

class GameServiceImpl extends GameService {
  private val games = scala.collection.concurrent.TrieMap[String, Game]()
  private val boardGameName = scala.collection.concurrent.TrieMap[String, String]()

  def createGame(name: String, singlePlayer: Boolean): IO[Either[GameError, GameResponse]] = {
    IO.pure(boardGameName.contains(name)).flatMap {
      case true => IO.pure(Left(DuplicateGameName))
      case false =>
        val gameId = java.util.UUID.randomUUID().toString
        val game = if (singlePlayer) Game.initialSinglePlayer(name) else Game.initialTuneBasePlayer(name)
        val validMoves = getValidMoves(game)
        games.put(gameId, game)
        boardGameName.put(name, gameId)
        IO.pure(Right(GameResponse(gameId, game.name, game.board, game.currentPlayer, game.status, validMoves)))
    }
  }

  def getListGame: IO[List[GameListResponse]] = {
    IO.pure(boardGameName.toList.flatMap { case (name, id) =>
      games.get(id).map(game => GameListResponse(name, id, game.status))
    })
  }

  def findGameById(id: String): IO[Option[GameResponse]] =
    IO.pure(games.get(id).map(game =>
      val validMoves = getValidMoves(game)
      GameResponse(id, game.name, game.board, game.currentPlayer, game.status, validMoves)))

  def findGameByName(name: String): IO[Option[GameResponse]] =
    IO.pure(boardGameName.get(name).flatMap(games.get).map(game =>
      val validMoves = getValidMoves(game)
      GameResponse(boardGameName(name), game.name, game.board, game.currentPlayer, game.status, validMoves)))

  def deleteGame(gameId: String): IO[Either[GameError, Unit]] = {
    IO.pure(games.get(gameId)).flatMap {
      case None => IO.pure(Left(GameNotFound(gameId)))
      case Some(game) =>
        game.status match {
          case InProgress => IO.pure(Left(GameIsProgress))
          case GameOver(_) =>
            games.remove(gameId)
            boardGameName.filterInPlace((_, id) => id != gameId)
            IO.pure(Right(()))
        }
    }
  }

  def makeMove(gameId: String, from: Position, to: Position): IO[Either[GameError, GameResponse]] = {
    IO.pure(games.get(gameId)).flatMap {
      case None => IO.pure(Left(GameNotFound(gameId)))
      case Some(game) =>
        game.makeMove(Move(from, to)) match {
          case Left(error) => IO.pure(Left(error))
          case Right(newGame) =>
            games.update(gameId, newGame)
            val validMoves = getValidMoves(newGame)
            IO.pure(Right(GameResponse(gameId, game.name, newGame.board, newGame.currentPlayer, newGame.status, validMoves)))
        }
    }
  }

  def getValidMoves(game: Game): Map[Position, List[Move]] = {
    val allMoves = game.getValidMovesForPlayer(game.currentPlayer)

    val jumpMoves = allMoves.filter { case (_, moves) => moves.exists(_.isJump) }

    if (jumpMoves.nonEmpty) jumpMoves else allMoves
  }
}