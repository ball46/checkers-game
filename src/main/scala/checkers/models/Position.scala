package checkers.models

/**
 * Represents a position on the board.
 *
 * @param x The x-coordinate of the position.
 * @param y The y-coordinate of the position.
 */
case class Position(x: Int, y: Int) {
  /**
   * Checks if the position is valid within the board boundaries.
   *
   * @return True if the position is valid, false otherwise.
   */
  def isValid: Boolean = x >= 0 && x < 8 && y >= 0 && y < 8
}