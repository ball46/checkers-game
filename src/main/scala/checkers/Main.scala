package checkers

import cats.effect.{IO, IOApp}
import checkers.controllers.CheckersRoutes
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.CORS
import com.comcast.ip4s.*
import checkers.services.GameServiceImpl

import scala.concurrent.duration.*

object Main extends IOApp.Simple {
  def run: IO[Unit] = {
    val gameService = new GameServiceImpl()
    val routes = CheckersRoutes.apply(gameService).routes

    val corsConfig = CORS.policy.withAllowCredentials(false)
      .withAllowOriginHost(_ => true)
      .withAllowMethodsIn(Set(
        org.http4s.Method.GET, 
        org.http4s.Method.POST, 
        org.http4s.Method.PUT, 
        org.http4s.Method.DELETE, 
        org.http4s.Method.OPTIONS))
      .withAllowHeadersIn(Set(org.typelevel.ci.CIString("Content-Type")))
      .withMaxAge(1.day)

    EmberServerBuilder
      .default[IO]
      .withHost(ipv4"0.0.0.0")
      .withPort(port"8080")
      .withHttpApp(corsConfig(routes).orNotFound)
      .build
      .use(_ => IO.never)
  }
}