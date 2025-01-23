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
    if (dx == 2) NormalJump
    else if (dx > 2) LongRangeJump
    else NoJump
  }

  /**
   * Checks if the move is a jump move.
   *
   * @return True if the move is a jump move, false otherwise.
   */
  def isJumpMove: Boolean = getJumpType != NoJump

  private def direction: (Int, Int) = {
    val dx = to.x - from.x
    val dy = to.y - from.y
    val stepX = if (dx > 0) 1 else -1
    val stepY = if (dy > 0) 1 else -1
    (stepX, stepY)
  }

  /**
   * Finds the position of the piece to be captured (if it is a jump move).
   *
   * @return An Option containing the position of the captured piece, or None if it is not a jump move.
   */
  def capturedPosition: Option[Position] = {
    if (!isJumpMove) None
    else Some(Position(
      from.x + direction._1,
      from.y + direction._2
    ))
  }
}
