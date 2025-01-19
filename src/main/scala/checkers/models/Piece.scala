package checkers.models

/**
 * Represents the color of a piece.
 */
sealed trait Color
case object Black extends Color
case object White extends Color

/**
 * Represents a piece in the game.
 *
 * @param color The color of the piece.
 * @param isKing Indicates if the piece is a king.
 */
case class Piece(color: Color, isKing: Boolean = false) {
  /**
   * Promotes the piece to a king.
   *
   * @return The promoted piece.
   */
  def promote: Piece = copy(isKing = true)
}