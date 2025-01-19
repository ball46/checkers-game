package checkers.controllers

import cats.effect.IO
import checkers.services.GameService
import org.http4s.HttpRoutes

class CheckersRoutes(gameService: GameService) {
  private val gameController = new GameController(gameService)

  val routes: HttpRoutes[IO] =
    gameController.routes
}

object CheckersRoutes {
  def apply(gameService: GameService): CheckersRoutes =
    new CheckersRoutes(gameService)
}