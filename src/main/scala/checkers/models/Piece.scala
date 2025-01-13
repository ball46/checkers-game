package checkers.models

sealed trait Color
case object Black extends Color
case object White extends Color

case class Piece(color: Color, isKing: Boolean = false) {
  def promote: Piece = copy(isKing = true)
}
