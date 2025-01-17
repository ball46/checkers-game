package checkers.controllers

import cats.effect.IO
import checkers.models.*
import checkers.services.{GameResponse, GameService}
import io.circe.generic.auto.*, io.circe.syntax.*
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.io.*
import checkers.encoders.JsonEncoders.*

case class CreateGameRequest(singlePlayer: Boolean)

class GameController(gameService: GameService) {
  
  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ POST -> Root / "games" =>
      for {
        createReq <- req.as[CreateGameRequest]
        game <- gameService.createGame(createReq.singlePlayer)
        response <- Ok(game.asJson)
      } yield response

    case GET -> Root / "games" / gameId =>
      for {
        maybeGame <- gameService.findGame(gameId)
        response <- maybeGame.fold(NotFound())(Ok(_))
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