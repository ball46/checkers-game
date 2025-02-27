package checkers.models

sealed trait JumpType
case object NormalJump extends JumpType     // 2-square jump
case object LongRangeJump extends JumpType  // king's jump
case object NoJump extends JumpType         // regular move

/**
 * Represents a move request with the starting and ending positions.
 *
 * @param from The starting position of the move.
 * @param to   The ending position of the move.
 */
case class MoveRequest(from: Position, to: Position)

case class Direction(x: Int, y: Int)

/**
 * Represents a move in the game.
 *
 * @param from The starting position of the move.
 * @param to   The ending position of the move.
 */
case class Move(from: Position, to: Position) {
  /**
   * Checks if the move is valid according to the basic rules.
   *
   * @return True if the move is valid, false otherwise.
   */
  def isValid: Boolean = from.isValid && to.isValid && isDiagonal

  /**
   * Checks if the move is diagonal.
   *
   * @return True if the move is diagonal, false otherwise.
   */
  private def isDiagonal: Boolean = {
    val dx = (from.x - to.x).abs
    val dy = (from.y - to.y).abs
    dx == dy
  }

  /**
   * Determines the type of jump for the move.
   *
   * @return The type of jump.
   */
  def getJumpType: JumpType = {
    val dx = (from.x - to.x).abs
    dx match {
      case 2 => NormalJump
      case n if n > 2 => LongRangeJump
      case _ => NoJump
    }
  }

  /**
   * Checks if the move is a jump move.
   *
   * @return True if the move is a jump move, false otherwise.
   */
  def isJumpMove: Boolean = getJumpType != NoJump
  
  /**
   * Finds the position of the piece to be captured (if it is a jump move).
   *
   * @return An Option containing the position of the captured piece, or None if it is not a jump move.
   */
  def capturedPosition: Option[Position] = {
    if (!isJumpMove) None
    else {
      val dir = direction
      Some(Position(from.x + dir.x, from.y + dir.y))
    }
  }

  private def direction: Direction = {
    val dx = to.x - from.x
    val dy = to.y - from.y
    Direction(
      x = if (dx > 0) 1 else -1,
      y = if (dy > 0) 1 else -1
    )
  }
}

object Move {
  def fromRequest(req: MoveRequest): Move = Move(req.from, req.to)
}
