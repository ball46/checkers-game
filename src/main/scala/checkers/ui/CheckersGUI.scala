package checkers.ui

import cats.effect._
import cats.effect.std.Supervisor
import cats.syntax.all._
import checkers.models._
import scala.swing._
import scala.swing.event._
import cats.effect.unsafe.implicits.global

class CheckersGUI(supervisor: Supervisor[IO]) extends MainFrame {
  private case class GameState(
    game: Game,
    selectedPosition: Option[Position] = None
  )
  
  private case class UIError(message: String)
  
  // Use Ref for thread-safe state management
  private val stateRef: Ref[IO, GameState] = 
    Ref.of[IO, GameState](GameState(Game(Board.initial, White))).unsafeRunSync()

  // Convert state updates to IO
  private def updateState(newState: GameState): IO[Unit] = for {
    _ <- stateRef.set(newState)
    _ <- IO.delay(repaint())
  } yield ()

  // Convert move handling to IO
  private def handleMove(from: Position, to: Position): IO[Either[UIError, Unit]] = for {
    currentState <- stateRef.get
    result <- IO.pure(currentState.game.makeMove(Move(from, to)))
      .map(_.left.map(err => UIError(s"Invalid move: ${errorToMessage(err)}")))
    _ <- result.traverse_(newGame => 
      updateState(GameState(newGame)) *>
      IO.delay(Dialog.showMessage(null, "Move completed", "Success", Dialog.Message.Info)))
    _ <- result.swap.traverse_(err => 
      IO.delay(Dialog.showMessage(null, err.message, "Error", Dialog.Message.Error)))
  } yield result.void

  // Convert position selection to IO
  private def handlePositionSelect(pos: Position): IO[Unit] = for {
    state <- stateRef.get
    _ <- state.selectedPosition match {
      case Some(from) => 
        handleMove(from, pos) *>
        updateState(state.copy(selectedPosition = None))
      case None if state.game.board(pos).toOption.flatten
          .exists(_.color == state.game.currentPlayer) =>
        updateState(state.copy(selectedPosition = Some(pos)))
      case None =>
        updateState(state.copy(selectedPosition = None))
    }
  } yield ()

  private def errorToMessage(error: GameError): String = error match {
    case GameMovementError(boardError) => boardErrorToMessage(boardError)
    case GameAlreadyOver => "Game is already over"
  }

  private def boardErrorToMessage(error: BoardError): String = error match {
    case InvalidPosition(pos) => s"Invalid position: $pos"
    case InvalidMove(move) => s"Invalid move: $move"
    case WrongPlayer(color) => s"Wrong player: $color"
  }

  private def pixelToPosition(point: Point): Position = {
    val squareSize = 50
    Position(point.x / squareSize, point.y / squareSize)
  }

  contents = new BoardPanel {
    preferredSize = new Dimension(400, 400)
    
    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      val state = stateRef.get.unsafeRunSync()
      drawBoard(g, state.game.board, state.selectedPosition)
    }

    listenTo(mouse.clicks)
    reactions += {
      case MouseClicked(_, point, _, _, _) =>
        val pos = pixelToPosition(point)
        if (pos.isValid) 
          supervisor.supervise(handlePositionSelect(pos)).unsafeRunSync()
    }
  }
}

object CheckersGUI {
  def create: IO[CheckersGUI] = 
    Supervisor[IO].use { supervisor =>
      IO.delay(new CheckersGUI(supervisor))
    }
}