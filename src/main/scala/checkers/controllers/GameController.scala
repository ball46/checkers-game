package checkers.controllers

import cats.effect.IO
import checkers.models.*
import checkers.services.{GameResponse, GameService}
import io.circe.generic.auto.*, io.circe.syntax.*
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.io.*
import checkers.encoders.JsonEncoders.*

case class CreateGameRequest(name: String, singlePlayer: Boolean)

class GameController(gameService: GameService) {
  
  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ POST -> Root / "games" =>
      for {
        createReq <- req.as[CreateGameRequest]
        game <- gameService.createGame(createReq.name, createReq.singlePlayer).flatMap {
          case Left(error) => BadRequest(error.toString)
          case Right(gameResponse) => Ok(gameResponse.asJson)
        }
      } yield game

    case GET -> Root / "games" =>
      for {
        games <- gameService.getListGame
        response <- Ok(games.asJson)
      } yield response

    case GET -> Root / "games" / "id" / gameId =>
      for {
        maybeGame <- gameService.findGameById(gameId)
        response <- maybeGame.fold(NotFound())(Ok(_))
      } yield response

    case GET -> Root / "games" / "name" / gameName =>
      for {
        maybeGame <- gameService.findGameByName(gameName)
        response <- maybeGame.fold(NotFound())(Ok(_))
      } yield response

    case DELETE -> Root / "games" / gameId =>
      for {
        result <- gameService.deleteGame(gameId)
        response <- result.fold(
          err => BadRequest(err.toString),
          _ => Ok()
        )
      } yield response

    case req @ POST -> Root / "games" / gameId / "move" =>
      for {
        moveRequest <- req.as[MoveRequest]
        result <- gameService.makeMove(gameId, moveRequest.from, moveRequest.to)
        response <- result.fold(
          err => BadRequest(err.toString),
          game => Ok(game)
        )
      } yield response
  }
}