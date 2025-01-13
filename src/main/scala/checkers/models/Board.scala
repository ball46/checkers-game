package checkers.models

case class Board(pieces: Vector[Vector[Option[Piece]]]) {
  def apply(pos: Position): Option[Piece] =
    if (pos.isValid) pieces(pos.y)(pos.x) else None

  private def updated(pos: Position, piece: Option[Piece]): Board = {
    if (!pos.isValid) this
    else Board(pieces.updated(pos.y, pieces(pos.y).updated(pos.x, piece)))
  }

  // ตรวจสอบว่าการเดินนั้นถูกกติกาทั้งหมดหรือไม่
  def isValidMove(move: Move, currentPlayer: Color): Boolean = {
    if (!move.isValid) false
    else {
      val piece = apply(move.from)
      piece.exists(p =>
        p.color == currentPlayer &&
          isValidDirection(move, p) &&
          (if (move.isJump) canJump(move, currentPlayer) else canMove(move))
      )
    }
  }

  // ตรวจสอบทิศทางการเดินว่าถูกต้องไหม
  private def isValidDirection(move: Move, piece: Piece): Boolean = {
    val dy = move.to.y - move.from.y
    piece match {
      case Piece(White, false) => dy < 0 // เดินขึ้นเท่านั้น
      case Piece(Black, false) => dy > 0 // เดินลงเท่านั้น
      case Piece(_, true) => true // King เดินได้ทุกทิศทาง
    }
  }

  // ตรวจสอบการเดินปกติ (1 ช่อง)
  private def canMove(move: Move): Boolean = {
    apply(move.to).isEmpty &&
      (move.from.x - move.to.x).abs == 1
  }

  // ตรวจสอบการกินหมาก (2 ช่อง)
  private def canJump(move: Move, currentPlayer: Color): Boolean = {
    move.capturedPosition.exists { pos =>
      apply(move.to).isEmpty && // ปลายทางต้องว่าง
        apply(pos).exists(_.color != currentPlayer) // มีหมากฝ่ายตรงข้ามให้กิน
    }
  }

  // ทำการเดินหมากและคืนค่า board ใหม่
  def makeMove(move: Move, currentPlayer: Color): Option[Board] = {
    if (!isValidMove(move, currentPlayer)) None
    else Some {
      val piece = apply(move.from).get
      val updatedBoard = updated(move.from, None)
        .updated(move.to, Some(shouldPromote(move.to, piece)))

      move.capturedPosition.fold(updatedBoard) { pos =>
        updatedBoard.updated(pos, None)
      }
    }
  }

  private def shouldPromote(pos: Position, piece: Piece): Piece = {
    if (piece.isKing) piece
    else piece.color match {
      case White if pos.y == 0 => piece.promote
      case Black if pos.y == 7 => piece.promote
      case _ => piece
    }
  }
}

object Board {
  def initial: Board = {
    val emptyBoard = Vector.fill(8)(Vector.fill(8)(None: Option[Piece]))

    // วางหมากเริ่มต้น
    def initialPieces = (0 until 8).foldLeft(emptyBoard) { case (board, x) =>
      // วางหมากสีดำในแถวที่ 0 ถึง 2 โดยวางในตำแหน่งที่เป็นช่องสีดำ
      val withBlack = (0 until 3).foldLeft(board) { case (b, y) =>
        if ((x + y) % 2 == 1) {
          b.updated(y, b(y).updated(x, Some(Piece(Black))))
        } else b
      }

      // วางหมากสีขาวในแถวที่ 5 ถึง 7 โดยวางในตำแหน่งที่เป็นช่องสีดำ
      (5 until 8).foldLeft(withBlack) { case (b, y) =>
        if ((x + y) % 2 == 1) {
          b.updated(y, b(y).updated(x, Some(Piece(White))))
        } else b
      }
    }

    Board(initialPieces)
  }
}