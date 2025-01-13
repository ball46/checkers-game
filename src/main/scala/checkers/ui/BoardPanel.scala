package checkers.ui

import scala.swing.Panel
import java.awt.{Color, Graphics2D, BasicStroke}
import checkers.models._

class BoardPanel extends Panel {
  private val squareSize = 50

  def drawBoard(g: Graphics2D, board: Board, selected: Option[Position]): Unit = {
    // Draw squares
    for {
      y <- 0 until 8
      x <- 0 until 8
    } {
      val color = if ((x + y) % 2 == 0) Color.WHITE else Color.BLACK
      g.setColor(color)
      g.fillRect(x * squareSize, y * squareSize, squareSize, squareSize)
    }

    // Draw pieces
    board.pieces.zipWithIndex.foreach { case (row, y) =>
      row.zipWithIndex.foreach { case (piece, x) =>
        piece.foreach { p =>
          g.setColor(if (p.color == White) Color.RED else Color.BLUE)
          val centerX = x * squareSize + squareSize / 2
          val centerY = y * squareSize + squareSize / 2
          g.fillOval(centerX - 20, centerY - 20, 40, 40)
          if (p.isKing) {
            g.setColor(Color.YELLOW)
            g.drawOval(centerX - 15, centerY - 15, 30, 30)
          }
        }
      }
    }

    // Draw selected position
    selected.foreach { pos =>
      g.setColor(Color.GREEN)
      g.setStroke(new BasicStroke(2))
      g.drawRect(pos.x * squareSize, pos.y * squareSize, squareSize, squareSize)
    }
  }
}