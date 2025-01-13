package checkers.models

case class Move(from: Position, to: Position) {
  // ตรวจสอบว่าการเดินถูกต้องตามกติกาพื้นฐาน
  def isValid: Boolean = {
    from.isValid && to.isValid &&
      (from.x - to.x).abs <= 2 &&
      (from.y - to.y).abs <= 2
  }

  // ตรวจสอบว่าเป็นการกินหมากหรือไม่ (ถ้าเดิน 2 ช่อง = กินหมาก)
  def isJump: Boolean = (from.x - to.x).abs == 2

  // หาตำแหน่งของหมากที่จะถูกกิน (ถ้าเป็นการกิน)
  def capturedPosition: Option[Position] = {
    if (!isJump) None
    else Some(Position(
      (from.x + to.x) / 2,
      (from.y + to.y) / 2
    ))
  }
}
