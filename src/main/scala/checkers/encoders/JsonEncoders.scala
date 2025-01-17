package checkers.encoders

import checkers.models.*
import io.circe.syntax.*
import io.circe.{Encoder, Json, KeyEncoder}

object JsonEncoders {

  implicit val colorEncoder: Encoder[Color] = Encoder.instance {
    case White => Json.fromString("White")
    case Black => Json.fromString("Black")
  }

  implicit val gameStatusEncoder: Encoder[GameStatus] = Encoder.instance {
    case InProgress => Json.fromString("InProgress")
    case GameOver(winner) => Json.fromString(winner.fold("Draw")(_.toString + " Won"))
  }

  implicit val positionKeyEncoder: KeyEncoder[Position] = KeyEncoder.instance { pos =>
    s"${pos.x},${pos.y}"
  }

  implicit val moveEncoder: Encoder[Move] = Encoder.instance { move =>
    Json.obj(
      "from" -> Json.obj("x" -> Json.fromInt(move.from.x), "y" -> Json.fromInt(move.from.y)),
      "to" -> Json.obj("x" -> Json.fromInt(move.to.x), "y" -> Json.fromInt(move.to.y))
    )
  }

  implicit val pieceEncoder: Encoder[Piece] = Encoder.instance { piece =>
    Json.obj(
      "color" -> piece.color.asJson,
      "isKing" -> Json.fromBoolean(piece.isKing)
    )
  }
  
  implicit val boardEncoder: Encoder[Board] = Encoder.instance { board =>
    Json.arr(
      board.pieces.zipWithIndex.flatMap { case (row, y) =>
        row.zipWithIndex.map { case (pieceOpt, x) =>
          Json.obj(
            "x" -> Json.fromInt(x),
            "y" -> Json.fromInt(y),
            "piece" -> pieceOpt.asJson
          )
        }
      }: _*
    )
  }

  implicit val positionMoveListEncoder: Encoder[Map[Position, List[Move]]] = Encoder.encodeMap[Position, List[Move]]
}