package checkers.ui

import checkers.models.*
import scalafx.Includes.*
import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.*
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Circle, Rectangle}

object CheckersGUI extends JFXApp3 {
  private val SquareSize = 60
  private val PieceRadius = 20.0
  private var game = Game.initial
  private var selectedPos: Option[Position] = None

  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title = "Scala Checkers"
      scene = new Scene(SquareSize * 8, SquareSize * 8) {
        root = new Pane {
          children = createBoard() ++ createPieces()
        }
      }
    }
  }

  private def createBoard() = {
    for {
      boardX <- 0 until 8
      boardY <- 0 until 8
    } yield {
      val square = new Rectangle {
        x = boardX * SquareSize
        y = boardY * SquareSize
        width = SquareSize
        height = SquareSize
        fill = if ((boardX + boardY) % 2 == 0) Color.White else Color.Gray
      }

      square.onMouseClicked = (e: MouseEvent) => handleSquareClick(Position(boardX, boardY))
      square
    }
  }

  private def createPieces() = {
    for {
      x <- 0 until 8
      y <- 0 until 8
      piece <- game.board(Position(x, y))
    } yield createPiece(x, y, piece)
  }

  private def createPiece(x: Int, y: Int, piece: Piece) = {
    new Circle {
      centerX = x * SquareSize + SquareSize / 2
      centerY = y * SquareSize + SquareSize / 2
      radius = PieceRadius
      fill = piece.color match {
        case White => Color.White
        case Black => Color.Black
      }
      stroke = Color.Black
      strokeWidth = 2
    }
  }

  private def handleSquareClick(pos: Position): Unit = {
    selectedPos match {
      case None =>
        if (game.board(pos).exists(_.color == game.currentPlayer)) {
          selectedPos = Some(pos)
          // TODO: Highlight selected piece
        }

      case Some(from) =>
        val move = Move(from, pos)
        game.makeMove(move) match {
          case Some(newGame) =>
            game = newGame
            updateBoard()
          case None =>
            println("Invalid move")
        }
        selectedPos = None
    }
  }

  private def updateBoard(): Unit = {
    val board = stage.scene().root.asInstanceOf[Pane]
    board.children.clear()
    board.children = createBoard() ++ createPieces()
  }
}