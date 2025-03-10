import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import checkers.models._

class LogicGameSpec extends AnyFunSuite with Matchers {

  // 1. ทดสอบการตั้งค่าเริ่มต้นของเกม
  test("initial board setup should have correct pieces and positions") {
    val game = Game.initialTuneBasePlayer("TestGame")

    // ตรวจสอบผู้เล่นเริ่มต้นและสถานะ
    assert(game.currentPlayer == White)
    assert(game.status == InProgress)

    val board = game.board

    // ตรวจสอบหมากดำ (12 ชิ้น) ในแถวบน
    (0 to 2).foreach { y =>
      (0 to 7).foreach { x =>
        if ((x + y) % 2 == 1) {
          board(Position(x, y)).toOption.flatten shouldBe Some(Piece(Black))
        }
      }
    }

    // ตรวจสอบหมากขาว (12 ชิ้น) ในแถวล่าง
    (5 to 7).foreach { y =>
      (0 to 7).foreach { x =>
        if ((x + y) % 2 == 1) {
          board(Position(x, y)).toOption.flatten shouldBe Some(Piece(White))
        }
      }
    }

    // ตรวจสอบช่องว่างตรงกลางกระดาน
    (3 to 4).foreach { y =>
      (0 to 7).foreach { x =>
        board(Position(x, y)).toOption.flatten shouldBe None
      }
    }
  }

  // 2. ทดสอบการเคลื่อนที่พื้นฐาน
  test("basic movement rules should be enforced") {
    val game = Game.initialTuneBasePlayer("TestGame")

    // การเคลื่อนที่ที่ถูกต้องของหมากขาว (ขึ้นแนวทแยง)
    val validWhiteMove = Move(Position(0, 5), Position(1, 4))
    game.makeMove(validWhiteMove).isRight shouldBe true

    // การเคลื่อนที่ที่ไม่ถูกต้อง - ไม่ใช่แนวทแยง
    val invalidDiagonalMove = Move(Position(2, 5), Position(2, 4))
    game.makeMove(invalidDiagonalMove).isLeft shouldBe true

    // การเคลื่อนที่ที่ไม่ถูกต้อง - เคลื่อนที่ไกลเกินไป (ไม่ใช่การกระโดด)
    val invalidDistanceMove = Move(Position(2, 5), Position(5, 3))
    game.makeMove(invalidDistanceMove).isLeft shouldBe true
  }

  test("pieces should only move in their allowed directions") {
    val game = Game.initialTuneBasePlayer("TestGame")

    // ขาวเคลื่อนที่ขึ้น (ถูกต้อง)
    val whiteUpMove = Move(Position(2, 5), Position(1, 4))
    game.makeMove(whiteUpMove).isRight shouldBe true

    // สร้างเกมใหม่เพื่อทดสอบกรณีอื่น
    val game2 = Game.initialTuneBasePlayer("TestGame2")

    // ขาวเคลื่อนที่ลง (ไม่ถูกต้อง)
    val whiteDownMove = Move(Position(2, 5), Position(1, 6))
    game2.makeMove(whiteDownMove).isLeft shouldBe true

    // ปรับกระดานเพื่อทดสอบการเคลื่อนที่ของหมากดำ
    val pieces = Vector.fill(8)(Vector.fill(8)(None: Option[Piece]))
    val blackBoard = Board(
      pieces.updated(3, pieces(3).updated(3, Some(Piece(Black))))
    )
    val blackGame = Game("BlackMoveTest", blackBoard, Black)

    // ดำเคลื่อนที่ลง (ถูกต้อง)
    val blackDownMove = Move(Position(3, 3), Position(2, 4))
    blackGame.makeMove(blackDownMove).isRight shouldBe true

    // ดำเคลื่อนที่ขึ้น (ไม่ถูกต้อง)
    val blackGame2 = Game("BlackMoveTest2", blackBoard, Black)
    val blackUpMove = Move(Position(3, 3), Position(2, 2))
    blackGame2.makeMove(blackUpMove).isLeft shouldBe true
  }

  // 3. ทดสอบการกระโดดและการกินหมาก
  test("jump moves should capture opponent pieces") {
    val pieces = Vector.fill(8)(Vector.fill(8)(None: Option[Piece]))
    val jumpBoard = Board(
      pieces
        .updated(4, pieces(4).updated(1, Some(Piece(Black))))
        .updated(5, pieces(5).updated(0, Some(Piece(White))))
    )

    val game = Game("JumpTest", jumpBoard, White)
    val jumpMove = Move(Position(0, 5), Position(2, 3))

    val result = game.makeMove(jumpMove)
    result.isRight shouldBe true

    val newBoard = result.getOrElse(fail("Move should succeed")).board

    // ตรวจสอบว่าหมากถูกกินไปแล้ว
    newBoard(Position(1, 4)).toOption.flatten shouldBe None
    // ตรวจสอบว่าหมากขาวเคลื่อนที่ไปยังตำแหน่งใหม่
    newBoard(Position(2, 3)).toOption.flatten shouldBe Some(Piece(White))
    // ตรวจสอบว่าตำแหน่งเดิมว่าง
    newBoard(Position(0, 5)).toOption.flatten shouldBe None
  }

  test("player should be able to make multiple jumps in one turn") {
    val pieces = Vector.fill(8)(Vector.fill(8)(None: Option[Piece]))
    val multiJumpBoard = Board(
      pieces
        .updated(2, pieces(2).updated(3, Some(Piece(Black))))
        .updated(4, pieces(4).updated(1, Some(Piece(Black))))
        .updated(5, pieces(5).updated(0, Some(Piece(White))))
    )

    val game = Game("MultiJumpTest", multiJumpBoard, White)

    // กระโดดครั้งแรก
    val firstJump = Move(Position(0, 5), Position(2, 3))
    val afterFirstJump = game.makeMove(firstJump)
    afterFirstJump.isRight shouldBe true

    val midGame = afterFirstJump.getOrElse(fail("First jump should succeed"))

    // ตรวจสอบว่ายังเป็นตาของผู้เล่นเดิม
    midGame.currentPlayer shouldBe White
    // ตรวจสอบว่าอยู่ในโหมดต่อเนื่อง
    midGame.isContinuation shouldBe true

    // กระโดดครั้งที่สอง
    val secondJump = Move(Position(2, 3), Position(4, 1))
    val afterSecondJump = midGame.makeMove(secondJump)
    afterSecondJump.isRight shouldBe true

    val finalGame = afterSecondJump.getOrElse(fail("Second jump should succeed"))
    val finalBoard = finalGame.board

    // ตรวจสอบว่าหมากทั้งสองถูกกินไป
    finalBoard(Position(3, 2)).toOption.flatten shouldBe None
    finalBoard(Position(1, 4)).toOption.flatten shouldBe None

    // ตรวจสอบว่าหมากอยู่ในตำแหน่งสุดท้าย
    finalBoard(Position(4, 1)).toOption.flatten shouldBe Some(Piece(White))

    // ตรวจสอบว่าเปลี่ยนเป็นตาของอีกฝ่าย
    finalGame.currentPlayer shouldBe Black
    finalGame.isContinuation shouldBe false
  }

  // 4. ทดสอบการเลื่อนขั้นเป็นคิง
  test("pieces should be promoted to kings when reaching the opposite end") {
    // ทดสอบหมากขาว
    val whitePieces = Vector.fill(8)(Vector.fill(8)(None: Option[Piece]))
    val whitePromotionBoard = Board(
      whitePieces.updated(1, whitePieces(1).updated(0, Some(Piece(White))))
    )

    val whiteGame = Game("WhitePromotion", whitePromotionBoard, White)
    val whiteMove = Move(Position(0, 1), Position(1, 0))

    val whiteResult = whiteGame.makeMove(whiteMove)
    whiteResult.isRight shouldBe true

    val afterWhiteMove = whiteResult.getOrElse(fail("White promotion move should succeed"))
    val whitePiece = afterWhiteMove.board(Position(1, 0)).toOption.flatten.getOrElse(fail("Piece should exist"))
    whitePiece.isKing shouldBe true

    // ทดสอบหมากดำ
    val blackPieces = Vector.fill(8)(Vector.fill(8)(None: Option[Piece]))
    val blackPromotionBoard = Board(
      blackPieces.updated(6, blackPieces(6).updated(7, Some(Piece(Black))))
    )

    val blackGame = Game("BlackPromotion", blackPromotionBoard, Black)
    val blackMove = Move(Position(7, 6), Position(6, 7))

    val blackResult = blackGame.makeMove(blackMove)
    blackResult.isRight shouldBe true

    val afterBlackMove = blackResult.getOrElse(fail("Black promotion move should succeed"))
    val blackPiece = afterBlackMove.board(Position(6, 7)).toOption.flatten.getOrElse(fail("Piece should exist"))
    blackPiece.isKing shouldBe true
  }

  test("king pieces should move in all diagonal directions") {
    val pieces = Vector.fill(8)(Vector.fill(8)(None: Option[Piece]))
    val kingBoard = Board(
      pieces.updated(4, pieces(4).updated(3, Some(Piece(White, isKing = true))))
    )

    val game = Game("KingTest", kingBoard, White)

    // คิงเคลื่อนที่ขึ้นซ้าย
    val upLeftMove = Move(Position(3, 4), Position(1, 2))
    game.makeMove(upLeftMove).isRight shouldBe true

    // คิงเคลื่อนที่ขึ้นขวา
    val upRightMove = Move(Position(3, 4), Position(5, 2))
    game.makeMove(upRightMove).isRight shouldBe true

    // คิงเคลื่อนที่ลงซ้าย
    val downLeftMove = Move(Position(3, 4), Position(1, 6))
    game.makeMove(downLeftMove).isRight shouldBe true

    // คิงเคลื่อนที่ลงขวา
    val downRightMove = Move(Position(3, 4), Position(5, 6))
    game.makeMove(downRightMove).isRight shouldBe true
  }

  // 5. ทดสอบสถานะเกม
  test("game should end properly with correct winner declared") {
    // สร้างสถานการณ์ที่ Black มีหมากเหลือตัวเดียวและกำลังจะถูกกิน
    val pieces = Vector.fill(8)(Vector.fill(8)(None: Option[Piece]))
    val endgameBoard = Board(
      pieces
        .updated(4, pieces(4).updated(1, Some(Piece(Black))))
        .updated(5, pieces(5).updated(0, Some(Piece(White))))
    )

    val game = Game("EndgameTest", endgameBoard, White)
    val jumpMove = Move(Position(0, 5), Position(2, 3))

    val result = game.makeMove(jumpMove)
    result.isRight shouldBe true

    val endedGame = result.getOrElse(fail("Move should succeed"))
    endedGame.status shouldBe GameOver(Some(White))
  }

  test("getValidMovesForPlayer should return correct moves") {
    val game = Game.initialTuneBasePlayer("TestGame")

    // ตรวจสอบการเคลื่อนที่ที่ถูกต้องของ White
    val whiteMoves = game.getValidMovesForPlayer(White)
    whiteMoves.nonEmpty shouldBe true

    // ตรวจสอบว่ามีการเคลื่อนที่จากตำแหน่ง (0, 5)
    whiteMoves.contains(Position(0, 5)) shouldBe true

    // ตรวจสอบว่าไม่มีการเคลื่อนที่สำหรับตำแหน่งที่ไม่มีหมาก
    whiteMoves.contains(Position(0, 0)) shouldBe false
  }

  test("continuation jumps should be enforced when possible") {
    val pieces = Vector.fill(8)(Vector.fill(8)(None: Option[Piece]))
    val continuationBoard = Board(
      pieces
        .updated(2, pieces(2).updated(3, Some(Piece(Black))))
        .updated(4, pieces(4).updated(1, Some(Piece(Black))))
        .updated(5, pieces(5).updated(0, Some(Piece(White))))
    )

    val game = Game("ContinuationTest", continuationBoard, White)

    // กระโดดครั้งแรก
    val firstJump = Move(Position(0, 5), Position(2, 3))
    val afterFirstJump = game.makeMove(firstJump).getOrElse(fail("First jump should succeed"))

    // ตรวจสอบว่าอยู่ในโหมดต่อเนื่อง
    afterFirstJump.isContinuation shouldBe true

    // พยายามเคลื่อนที่โดยไม่กระโดดต่อ (ควรล้มเหลว)
    val invalidMove = Move(Position(0, 2), Position(1, 3))
    afterFirstJump.makeMove(invalidMove).isLeft shouldBe true

    // ตรวจสอบว่ามีเฉพาะการเคลื่อนที่ที่ถูกต้องสำหรับหมากที่เพิ่งเคลื่อนที่
    val validMoves = afterFirstJump.getValidMovesForPlayer(White)
    validMoves.size shouldBe 1
    validMoves.contains(Position(2, 3)) shouldBe true
  }
}