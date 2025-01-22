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
  
  private def isDiagonalPathClear(move: Move): Boolean = {
    val dx = move.to.x - move.from.x
    val dy = move.to.y - move.from.y
    
    if (dx.abs != dy.abs) false  // ต้องเป็นแนวทแยงเท่านั้น
    else {
      val stepX = dx / dx.abs
      val stepY = dy / dy.abs
      val steps = dx.abs - 1

      // ตรวจสอบทุกช่องระหว่างจุดเริ่มต้นและจุดสิ้นสุด
      (1 to steps).forall { i =>
        val checkPos = Position(
          move.from.x + (stepX * i),
          move.from.y + (stepY * i)
        )
        apply(checkPos).fold(
          _ => false,
          _.isEmpty  // ต้องเป็นช่องว่างทั้งหมด
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

  private def findCapturedPiece(move: Move, currentPlayer: Color): Option[Position] = {
    def isCapturable(pos: Position): Boolean = 
      apply(pos).toOption.flatten.exists(_.color != currentPlayer)

    move.getJumpType match {
      case NormalJump =>
        // Normal 2-square jump
        val midX = (move.from.x + move.to.x) / 2
        val midY = (move.from.y + move.to.y) / 2
        val midPos = Position(midX, midY)
        Some(midPos).filter(isCapturable)
        
      case LongRangeJump =>
        // King's jump - find first enemy piece in path
        val dx = move.to.x - move.from.x
        val dy = move.to.y - move.from.y
        val stepX = if (dx > 0) 1 else -1
        val stepY = if (dy > 0) 1 else -1
        
        (1 until dx.abs)
          .map(i => Position(move.from.x + (stepX * i), move.from.y + (stepY * i)))
          .find(isCapturable)
          
      case NoJump => None
    }
  }

  /**
   * Checks if a jump move (two squares) is valid.
   * 
   * @param move The move to check.
   * @param currentPlayer The current player making the move.
   * @return True if the move is valid, false otherwise.
   */
  private def canJump(move: Move, currentPlayer: Color): Boolean = {
    findCapturedPiece(move, currentPlayer).exists { pos =>
      apply(move.to).toOption.flatten.isEmpty &&
      apply(pos).toOption.flatten.exists(_.color != currentPlayer)
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
    def processMove(piece: Piece, board: Board): Either[BoardError, Board] = {
      for {
        _ <- validateMove(move, piece, currentPlayer)
        emptyBoard <- board.updated(move.from, None)
        promotedPiece = shouldPromote(move.to, piece)
        updatedBoard <- emptyBoard.updated(move.to, Some(promotedPiece))
        capturedBoard <- findCapturedPiece(move, currentPlayer).fold[Either[BoardError, Board]](
          Right(updatedBoard)
        )(pos => updatedBoard.updated(pos, None))
      } yield capturedBoard
    }

    for {
      piece <- getPieceAt(move.from)
      result <- processMove(piece, this)
    } yield result
  }

  private def getPieceAt(pos: Position): Either[BoardError, Piece] = 
    apply(pos).flatMap(_.toRight(InvalidMove(Move(pos, pos))))

  /**
   * Validates the move according to the game rules.
   *
   * @param move          The move to validate.
   * @param piece         The piece making the move.
   * @param currentPlayer The current player making the move.
   * @return Either a BoardError or Unit if the move is valid.
   */
  private def validateMove(move: Move, piece: Piece, currentPlayer: Color): Either[BoardError, Unit] = 
    (move.isValid, piece.color == currentPlayer) match {
      case (false, _) => Left(InvalidMove(move))
      case (_, false) => Left(WrongPlayer(piece.color))
      case _ => Right(())
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