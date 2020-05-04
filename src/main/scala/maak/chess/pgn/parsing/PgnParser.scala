package maak.chess.pgn.parsing

import maak.chess.{ChessGame, ChessGameTag, ChessMove}

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}

case class PgnParserError(location: Location, msg: String)

object PgnParser extends RegexParsers {
  override def skipWhitespace = true

  override val whiteSpace: Regex = """[\s\t\r\f\n]+""".r

  private sealed trait PgnToken
  private case class StringToken(string: String) extends PgnToken
  private case class IntegerToken(integer: Int) extends PgnToken
  private case object PeriodCharacterToken extends PgnToken
  private case object AsteriskCharacterToken extends PgnToken
  private case object LeftBracketToken extends PgnToken
  private case object RightBracketToken extends PgnToken
  private case object LeftParenthesisToken extends PgnToken
  private case object RightParenthesisToken extends PgnToken
  private case object LeftAngleBracketToken extends PgnToken
  private case object RightAngleBracketToken extends PgnToken
  private case class NumericAnnotationGlyphToken(nag: Int) extends PgnToken
  private case class SymbolToken(string: String) extends PgnToken

  sealed trait PgnSection
  case class TagPair(symbolToken: String, stringToken: String) extends PgnSection
  case class MoveText(number: Int, whiteMove: Option[String], blackMove: Option[String]) extends PgnSection
  case class GameTerminationMarker(string: String) extends PgnSection
  case class Comment(string: String) extends PgnSection

  def apply(pgnString: String): Either[PgnParserError, ChessGame] = {
    parse(parseChessGame, pgnString) match {
      case NoSuccess(msg, next) => Left(PgnParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, _) => Right(result)
    }
  }

  private def stringToken: Parser[StringToken] =
    """"[^"]*"""".r ^^ { str =>
      StringToken(str.drop(1).dropRight(1))
    }

  private def integerToken: Parser[IntegerToken] =
    """[0-9]+""".r ^^ { str =>
      IntegerToken(str.toInt)
    }

  private def periodCharacterToken: Parser[PgnToken] =
    """\.""".r ^^ { str =>
      PeriodCharacterToken
    }

  private def asteriskCharacterToken: Parser[PgnToken] =
    """\*""".r ^^ { str =>
      AsteriskCharacterToken
    }

  private def leftBracketToken: Parser[PgnToken] =
    """\[""".r ^^ { str =>
      LeftBracketToken
    }

  private def rightBracketToken: Parser[PgnToken] =
    """\]""".r ^^ { str =>
      RightBracketToken
    }

  private def leftParenthesisToken: Parser[PgnToken] =
    """\(""".r ^^ { str =>
      LeftParenthesisToken
    }

  private def rightParenthesisToken: Parser[PgnToken] =
    """\)""".r ^^ { str =>
      RightParenthesisToken
    }

  private def leftAngleBracketToken: Parser[PgnToken] =
    """\<""".r ^^ { str =>
      LeftAngleBracketToken
    }

  private def rightAngleBracketToken: Parser[PgnToken] =
    """\>""".r ^^ { str =>
      RightAngleBracketToken
    }

  private def numericAnnotationGlyphToken: Parser[NumericAnnotationGlyphToken] =
    """\$[0-9]+]""".r ^^ { str =>
      NumericAnnotationGlyphToken(2)
    }

  /* The symbol token must not parse any of the four results "1-0", "0-1", "1/2-1/2", or "*". This is because of a
  flaw (imo) in the definitions that doesn't define the draw result ("1/2-1/2") as a valid symbol.
  Standard: http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm */
  private def symbolToken: Parser[SymbolToken] =
    """(?!1/2-1/2)(?!1-0)(?!0-1)[a-zA-Z0-9_\+#\=:-]+""".r ^^ { str =>
      SymbolToken(str)
    }

  def tagPair: Parser[TagPair] = leftBracketToken ~ symbolToken ~ stringToken ~ rightBracketToken ^^ {
    case _ ~ symbol ~ string ~ _ => TagPair(symbol.string, string.string)
  }

  def moveText: Parser[MoveText] = integerToken ~ periodCharacterToken.+ ~ symbolToken.? ~ symbolToken.? ^^ {
    case number ~ List(_) ~ optionWhite ~ optionBlack =>
      MoveText(number.integer, optionWhite.map(_.string), optionBlack.map(_.string))
  }

  private def gtmDraw: Parser[GameTerminationMarker] =
    """1/2-1/2""".r ^^ { str =>
      GameTerminationMarker(str)
    }

  private def whiteWins: Parser[GameTerminationMarker] =
    """1-0""".r ^^ { str =>
      GameTerminationMarker(str)
    }

  private def blackWins: Parser[GameTerminationMarker] =
    """0-1""".r ^^ { str =>
      GameTerminationMarker(str)
    }

  private def unknownGTM: Parser[GameTerminationMarker] = asteriskCharacterToken ^^ {
    case a => GameTerminationMarker("*")
  }

  private def gameTerminationMarker: Parser[GameTerminationMarker] = whiteWins | blackWins | gtmDraw | unknownGTM

  def commentSemicolon: Parser[Comment] = {
    """;[^\n]*""".r ^^ { str =>
      Comment(str.trim.drop(1))
    }
  }

  def commentBraces: Parser[Comment] = {
    """\{[^\}]*\}""".r ^^ { str =>
      Comment(str.trim.drop(1).dropRight(1))
    }
  }

  def parseChessGame: Parser[ChessGame] = {
    phrase(rep1(gameTerminationMarker | moveText | tagPair | commentBraces | commentSemicolon)) ^^ {
      case pgnSectionList: List[PgnSection] => {
        val tags: ListBuffer[ChessGameTag] = ListBuffer()
        val moves: ListBuffer[ChessMove] = ListBuffer()
        var result: String = ""
        pgnSectionList.foreach(_ match {
          case gtm: GameTerminationMarker => {
            println("Result found!!! = " + gtm.string)
            result = gtm.string
          }
          case tp: TagPair => tags.append(ChessGameTag(tp.symbolToken, tp.stringToken))
          case move: MoveText => moves.append(ChessMove(move.number, move.whiteMove, move.blackMove, None))
          case comment: Comment => {
            if (moves.nonEmpty) {
              val lastMove = moves.last
              moves.remove(moves.length - 1)
              moves.append(lastMove.copy(comment = Some(comment.string)))
            }
          }
        })
        ChessGame(tags.toList, moves.toList, result)
      }
    }
  }
}