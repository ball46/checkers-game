package checkers.models

case class Position(x: Int, y: Int) {
  def isValid: Boolean = x >= 0 && x < 8 && y >= 0 && y < 8
}