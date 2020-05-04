package maak.chess

case class ChessGameTag(tag: String, info: String)
case class ChessMove(number: Int, whiteMove: Option[String], blackMove: Option[String], comment: Option[String])

case class ChessGame (
  tags: List[ChessGameTag],
  moves: List[ChessMove],
  result: String
)
