package checkers.models

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
  def isValid: Boolean = {
    from.isValid && to.isValid && isDiagonal
  }

  /**
   * Checks if the move is diagonal.
   *
   * @return True if the move is diagonal, false otherwise.
   */
  def isDiagonal: Boolean = {
    val dx = (from.x - to.x).abs
    val dy = (from.y - to.y).abs
    dx == dy && dx <= 2  // Allow only 1 or 2 square diagonal moves
  }

  /**
   * Checks if the move is a jump (i.e., moves two squares).
   *
   * @return True if the move is a jump, false otherwise.
   */
  def isJump: Boolean = (from.x - to.x).abs == 2

  /**
   * Finds the position of the piece to be captured (if it is a jump move).
   *
   * @return An Option containing the position of the captured piece, or None if it is not a jump move.
   */
  def capturedPosition: Option[Position] = {
    if (!isJump) None
    else Some(Position(
      (from.x + to.x) / 2,
      (from.y + to.y) / 2
    ))
  }
}
