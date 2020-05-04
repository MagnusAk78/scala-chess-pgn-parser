package maak.chess.pgn

import maak.chess.ChessGame

object PgnPrinter {
  def cratePgnFromGame(game: ChessGame): String = {
    val stringBuffer: StringBuffer = new StringBuffer()
    game.tags.foreach( tag => {
      stringBuffer.append('[')
      stringBuffer.append(tag.tag)
      stringBuffer.append(" \"")
      stringBuffer.append(tag.info)
      stringBuffer.append("\"")
      stringBuffer.append("]\n")
    })
    stringBuffer.append("\n")

    game.moves.foreach( move => {
      stringBuffer.append(move.number)
      stringBuffer.append(". ")
      if(move.whiteMove.nonEmpty) {
        stringBuffer.append(move.whiteMove.get)
        stringBuffer.append(" ")
      }
      if(move.blackMove.nonEmpty) {
        stringBuffer.append(move.blackMove.get)
        stringBuffer.append(" ")
      }
      if(move.comment.nonEmpty) {
        stringBuffer.append("{")
        stringBuffer.append(move.comment.get)
        stringBuffer.append("} ")
      }
    })

    stringBuffer.append(game.result)
    stringBuffer.append("\n")
    stringBuffer.toString
  }
}
