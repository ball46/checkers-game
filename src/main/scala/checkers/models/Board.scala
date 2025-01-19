package checkers.models

sealed trait BoardError
case class InvalidPosition(pos: Position) extends BoardError
case class InvalidMove(move: Move) extends BoardError
case class WrongPlayer(color: Color) extends BoardError

case class Board(pieces: Vector[Vector[Option[Piece]]]) {
  /**
   * Retrieves the piece at the given position.
   *
   * @param pos The position to retrieve the piece from.
   * @return Either a BoardError or an Option of Piece.
   */
  def apply(pos: Position): Either[BoardError, Option[Piece]] = 
    if (pos.isValid) Right(pieces(pos.y)(pos.x))
    else Left(InvalidPosition(pos))

  /**
   * Updates the board with a new piece at the given position.
   *
   * @param pos   The position to update.
   * @param piece The piece to place at the position.
   * @return Either a BoardError or the updated Board.
   */
  private def updated(pos: Position, piece: Option[Piece]): Either[BoardError, Board] =
    if (!pos.isValid) Left(InvalidPosition(pos))
    else Right(Board(pieces.updated(pos.y, pieces(pos.y).updated(pos.x, piece))))

  /**
   * Checks if the move is valid according to the game rules.
   * @param move The move to validate.
   * @param currentPlayer The current player making the move.
   * @return True if the move is valid, false otherwise.
   */
  def isValidMove(move: Move, currentPlayer: Color): Boolean = {
    if (!move.isValid) false
    else {
      apply(move.from).fold(
        _ => false, // กรณีเกิด error
        _.exists(p => // กรณีสำเร็จ
          p.color == currentPlayer &&
            isValidDirection(move, p) &&
            (if (move.isJump) canJump(move, currentPlayer) else canMove(move))
        )
      )
    }
  }

  /**
   * Checks if the move direction is valid for the piece.
   * 
   * @param move The move to check.
   * @param piece The piece making the move.
   * @return True if the direction is valid, false otherwise.
   */
  private def isValidDirection(move: Move, piece: Piece): Boolean = {
    val dy = move.to.y - move.from.y
    piece match {
      case Piece(White, false) => dy < 0 // เดินขึ้นเท่านั้น
      case Piece(Black, false) => dy > 0 // เดินลงเท่านั้น
      case Piece(_, true) => true // King เดินได้ทุกทิศทาง
    }
  }

  /**
   * Checks if a normal move (one square) is valid.
   * 
   * @param move The move to check.
   * @return True if the move is valid, false otherwise.
   */
  private def canMove(move: Move): Boolean = {
    apply(move.to).fold(
      _ => false,
      _.isEmpty
    ) && (move.from.x - move.to.x).abs == 1
  }

  /**
   * Checks if a jump move (two squares) is valid.
   * 
   * @param move The move to check.
   * @param currentPlayer The current player making the move.
   * @return True if the move is valid, false otherwise.
   */
  private def canJump(move: Move, currentPlayer: Color): Boolean = {
    move.capturedPosition.exists { pos =>
      (for {
        targetSquare <- apply(move.to)
        capturedPiece <- apply(pos)
      } yield {
        targetSquare.isEmpty && // ปลายทางต้องว่าง
          capturedPiece.exists(_.color != currentPlayer) // มีหมากฝ่ายตรงข้ามให้กิน
      }).getOrElse(false)
    }
  }

  /**
   * Executes the move and returns the updated board.
   * 
   * @param move The move to execute.
   * @param currentPlayer The current player making the move.
   * @return Either a BoardError or the updated Board.
   */
  def makeMove(move: Move, currentPlayer: Color): Either[BoardError, Board] = {
    def execute(piece: Piece) = for {
      _ <- validateMove(move, piece, currentPlayer)
      emptyBoard <- updated(move.from, None)
      promotedPiece = shouldPromote(move.to, piece)
      updatedBoard <- emptyBoard.updated(move.to, Some(promotedPiece))
      finalBoard <- move.capturedPosition.fold[Either[BoardError, Board]](Right(updatedBoard)) { pos =>
        updatedBoard.updated(pos, None)
      }
    } yield finalBoard

    for {
      currentPiece <- apply(move.from)
      piece <- currentPiece.toRight(InvalidMove(move))
      result <- execute(piece)
    } yield result
  }

  /**
   * Validates the move according to the game rules.
   *
   * @param move          The move to validate.
   * @param piece         The piece making the move.
   * @param currentPlayer The current player making the move.
   * @return Either a BoardError or Unit if the move is valid.
   */
  private def validateMove(move: Move, piece: Piece, currentPlayer: Color): Either[BoardError, Unit] = {
    if (!move.isValid) Left(InvalidMove(move))
    else if (piece.color != currentPlayer) Left(WrongPlayer(piece.color))
    else if (!isValidDirection(move, piece)) Left(InvalidMove(move))
    else if (move.isJump && !canJump(move, currentPlayer)) Left(InvalidMove(move))
    else if (!move.isJump && !canMove(move)) Left(InvalidMove(move))
    else Right(())
  }

  /**
   * Checks if the piece should be promoted to a king.
   *
   * @param pos   The position of the piece.
   * @param piece The piece to check.
   * @return The piece, promoted if necessary.
   */
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
  /**
   * Creates the initial board setup.
   *
   * @return The initial board.
   */
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