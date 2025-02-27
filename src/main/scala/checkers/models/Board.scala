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
   * Checks if the diagonal path is clear for the move.
   *
   * @param move The move to check.
   * @return True if the path is clear, false otherwise.
   */
  def isDiagonalPathClear(move: Move): Boolean = {
    val dx = move.to.x - move.from.x
    val dy = move.to.y - move.from.y
    val piece = apply(move.from).toOption.flatten.get
    
    if (!move.isValid && !isValidDirection(move, piece)) false
    else {
      val stepX = dx / dx.abs
      val stepY = dy / dy.abs
      val steps = dx.abs - 1
      
      (1 to steps).forall { i =>
        val checkPos = Position(
          move.from.x + (stepX * i),
          move.from.y + (stepY * i)
        )
        apply(checkPos).fold(
          _ => false,
          _.isEmpty
        )
      }
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

  private def findCapturedPiece(move: Move, currentPlayer: Color): List[Position] = {
    def isCapturable(pos: Position): Boolean =
      apply(pos).toOption.flatten.exists(_.color != currentPlayer)

    move.getJumpType match {
      case NormalJump =>
        // ตรวจสอบตำแหน่งกลาง (กระโดด 2 ช่อง)
        val midX = (move.from.x + move.to.x) / 2
        val midY = (move.from.y + move.to.y) / 2
        val midPos = Position(midX, midY)
        if (isCapturable(midPos)) List(midPos) else Nil

      case LongRangeJump =>
        // สำหรับ King: ตรวจสอบทุกตำแหน่งในเส้นทาง
        val dx = move.to.x - move.from.x
        val dy = move.to.y - move.from.y
        val stepX = dx.sign
        val stepY = dy.sign

        (1 until dx.abs)
          .flatMap { i =>
            val checkPos = Position(move.from.x + (stepX * i), move.from.y + (stepY * i))
            Some(checkPos).filter(isCapturable)
          }
          .toList

      case NoJump => Nil
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
    for {
      piece <- validateMove(move, currentPlayer)
      emptyBoard <- updated(move.from, None)
      updatedPiece = shouldPromote(move.to, piece)
      placedBoard <- emptyBoard.updated(move.to, Some(updatedPiece))
      finalBoard <- captureIfNeeded(move, currentPlayer, placedBoard)
    } yield finalBoard
  }

  /**
   * Validates the move according to the game rules.
   *
   * @param move          The move to validate.
   * @param currentPlayer The current player making the move.
   * @return Either a BoardError or Unit if the move is valid.
   */
  private def validateMove(move: Move, currentPlayer: Color): Either[BoardError, Piece] = {
    if (!move.isValid) Left(InvalidMove(move))
    else {
      apply(move.from) match {
        case Right(Some(piece)) if piece.color == currentPlayer => Right(piece)
        case Right(Some(piece)) => Left(WrongPlayer(piece.color))
        case _ => Left(InvalidMove(move))
      }
    }
  }

  private def captureIfNeeded(move: Move, currentPlayer: Color, board: Board): Either[BoardError, Board] =
    move.capturedPosition.flatMap(pos => board.apply(pos).toOption.flatten) match {
      case Some(piece) if piece.color != currentPlayer =>
        move.capturedPosition.fold[Either[BoardError, Board]](Right(board)) { cp =>
          board.updated(cp, None)
        }
      case _ => Right(board)
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