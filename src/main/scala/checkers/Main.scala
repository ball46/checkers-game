package checkers

import cats.effect.{IO, IOApp}
import checkers.ui.CheckersGUI

object Main extends IOApp.Simple {
  def run: IO[Unit] = for {
    gui <- CheckersGUI.create
    _ <- IO.delay(gui.visible = true)
    _ <- IO.never
  } yield ()
}