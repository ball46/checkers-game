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

  /**
   * Creates a new game with the given name and player mode.
   * 
   * @param name         The name of the game.
   * @param singlePlayer Boolean indicating if the game is single player.
   * @return An IO containing either a GameError or a GameResponse.
   */
  def createGame(name: String, singlePlayer: Boolean): IO[Either[GameError, GameResponse]] = {
    for {
      exists <- IO.pure(boardGameName.contains(name))
      result <- if (exists) {
        IO.pure(Left(DuplicateGameName))
      } else {
        for {
          gameId <- IO.pure(java.util.UUID.randomUUID().toString)
          game = if (singlePlayer) Game.initialSinglePlayer(name) else Game.initialTuneBasePlayer(name)
          validMoves = getValidMoves(game).filter(_._2.nonEmpty)
          _ <- IO.delay {
            games.put(gameId, game)
            boardGameName.put(name, gameId)
          }
        } yield Right(GameResponse(gameId, game.name, game.board, game.currentPlayer, game.status, validMoves))
      }
    } yield result
  }

  /**
   * Retrieves a list of all games with their names, IDs, and statuses.
   * 
   * @return An IO containing a list of GameListResponse.
   */
  def getListGame: IO[List[GameListResponse]] = IO.delay {
    boardGameName.toList.flatMap { case (name, id) =>
      games.get(id).map(game => GameListResponse(name, id, game.status))
    }
  }

  /**
   * Finds a game by its ID.
   *
   * @param id The ID of the game.
   * @return An IO containing an Option of GameResponse.
   */
  def findGameById(id: String): IO[Option[GameResponse]] = IO.delay {
    games.get(id).map { game =>
      val validMoves = getValidMoves(game).filter(_._2.nonEmpty)
      GameResponse(id, game.name, game.board, game.currentPlayer, game.status, validMoves)
    }
  }

  /**
   * Finds a game by its name.
   *
   * @param name The name of the game.
   * @return An IO containing an Option of GameResponse.
   */
  def findGameByName(name: String): IO[Option[GameResponse]] = IO.delay {
    for {
      gameId <- boardGameName.get(name)
      game <- games.get(gameId)
      validMoves = getValidMoves(game).filter(_._2.nonEmpty)
    } yield GameResponse(gameId, game.name, game.board, game.currentPlayer, game.status, validMoves)
  }

  /**
   * Deletes a game by its ID.
   *
   * @param gameId The ID of the game to delete.
   * @return An IO containing either a GameError or Unit.
   */
  def deleteGame(gameId: String): IO[Either[GameError, Unit]] = IO.delay {
    games.get(gameId) match {
      case None => Left(GameNotFound(gameId))
      case Some(game) =>
        games.remove(gameId)
        boardGameName.remove(game.name)
        Right(())
    }
  }

  /**
   * Makes a move in the specified game.
   *
   * @param gameId The ID of the game.
   * @param from   The starting position of the move.
   * @param to     The ending position of the move.
   * @return An IO containing either a GameError or a GameResponse.
   */
  def makeMove(gameId: String, from: Position, to: Position): IO[Either[GameError, GameResponse]] = {
    IO.delay {
      for {
        game <- games.get(gameId).toRight(GameNotFound(gameId))
        newGame <- game.makeMove(Move(from, to))
        _ = games.update(gameId, newGame)
        validMoves = newGame.getValidMovesForPlayer(newGame.currentPlayer)
      } yield GameResponse(gameId, game.name, newGame.board, newGame.currentPlayer, newGame.status, validMoves)
    }
  }

  /**
   * Retrieves the valid moves for the current player in the given game.
   *
   * @param game The game for which to get valid moves.
   * @return A map of positions to lists of valid moves.
   */
  private def getValidMoves(game: Game): Map[Position, List[Move]] = {
    val allMoves = game.getValidMovesForPlayer(game.currentPlayer)
    val jumpMoves = allMoves.filter(_._2.exists(_.isJumpMove))
    if (jumpMoves.nonEmpty) jumpMoves else allMoves
  }
}