package chess

import Pos.posAt

sealed trait Role {
  val forsyth: Char
  lazy val forsythUpper: Char = forsyth.toUpper
  lazy val pgn: Char          = forsythUpper
  lazy val name               = toString.toLowerCase
  val projection: Boolean
  val dirs: Directions
  val dirsOpposite: Directions
  def dir(from: Pos, to: Pos): Option[Direction] //todel?
}
sealed trait PromotableRole extends Role {
  val promotesTo: PromotedRole
}
sealed trait PromotedRole extends Role {
  val promotedFrom: PromotableRole
}

case object King extends Role {
  val forsyth                  = 'k'
  val dirs: Directions         = Rook.dirs ::: Bishop.dirs
  val dirsOpposite: Directions = dirs
  def dir(from: Pos, to: Pos)  = None
  val projection               = false
}
case object Rook extends PromotableRole {
  val forsyth                  = 'r'
  val dirs: Directions         = List(_.up, _.down, _.left, _.right)
  val dirsOpposite: Directions = dirs
  def dir(from: Pos, to: Pos) =
    if (to ?| from)
      Some(
        if (to ?^ from) (_.up) else (_.down)
      )
    else if (to ?- from)
      Some(
        if (to ?< from) (_.left) else (_.right)
      )
    else None
  val projection               = true
  val promotesTo: PromotedRole = Dragon
}
case object Bishop extends PromotableRole {
  val forsyth                  = 'b'
  val dirs: Directions         = List(_.upLeft, _.upRight, _.downLeft, _.downRight)
  val dirsOpposite: Directions = dirs
  def dir(from: Pos, to: Pos) =
    if (to onSameDiagonal from)
      Some(
        if (to ?^ from) {
          if (to ?< from) (_.upLeft) else (_.upRight)
        } else {
          if (to ?< from) (_.downLeft) else (_.downRight)
        }
      )
    else None
  val projection               = true
  val promotesTo: PromotedRole = Horse
}
case object Knight extends PromotableRole {
  val forsyth = 'n'
  val dirs: Directions = List(
    p => posAt(p.x - 1, p.y + 2),
    p => posAt(p.x + 1, p.y + 2)
  )
  val dirsOpposite: Directions = List(
    p => posAt(p.x - 1, p.y - 2),
    p => posAt(p.x + 1, p.y - 2)
  )
  def dir(from: Pos, to: Pos)  = None
  val projection               = false
  val promotesTo: PromotedRole = PromotedKnight
}
case object Pawn extends PromotableRole {
  val forsyth                  = 'p'
  val dirs: Directions         = List(_.up)
  val dirsOpposite: Directions = List(_.down)
  def dir(from: Pos, to: Pos)  = None
  val projection               = false
  val promotesTo: PromotedRole = Tokin
}
case object Gold extends Role {
  val forsyth                  = 'g'
  val dirs: Directions         = List(_.up, _.down, _.left, _.right, _.upLeft, _.upRight)
  val dirsOpposite: Directions = List(_.up, _.down, _.left, _.right, _.downLeft, _.downRight)
  def dir(from: Pos, to: Pos)  = None
  val projection               = false
}
case object Silver extends PromotableRole {
  val forsyth                  = 's'
  val dirs: Directions         = List(_.up, _.upLeft, _.upRight, _.downLeft, _.downRight)
  val dirsOpposite: Directions = List(_.down, _.upLeft, _.upRight, _.downLeft, _.downRight)
  def dir(from: Pos, to: Pos)  = None
  val projection               = false
  val promotesTo: PromotedRole = PromotedSilver
}
case object Lance extends PromotableRole {
  val forsyth                  = 'l'
  val dirs: Directions         = List(_.up)
  val dirsOpposite: Directions = List(_.down)
  def dir(from: Pos, to: Pos)  = None //todo?
  val projection               = true
  val promotesTo: PromotedRole = PromotedLance
}
case object Tokin extends PromotedRole {
  val forsyth                                    = 't' // +p
  val dirs: Directions                           = Gold.dirs
  val dirsOpposite: Directions                   = Gold.dirsOpposite
  def dir(from: Pos, to: Pos): Option[Direction] = None
  val projection: Boolean                        = false
  val promotedFrom: PromotableRole               = Pawn
}
case object PromotedSilver extends PromotedRole {
  val forsyth: Char                              = 'a' // +s
  val dirs: Directions                           = Gold.dirs
  val dirsOpposite: Directions                   = Gold.dirsOpposite
  def dir(from: Pos, to: Pos): Option[Direction] = None
  val projection: Boolean                        = false
  val promotedFrom: PromotableRole               = Silver
}
case object PromotedKnight extends PromotedRole {
  val forsyth: Char                              = 'm' // +n
  val dirs: Directions                           = Gold.dirs
  val dirsOpposite: Directions                   = Gold.dirsOpposite
  def dir(from: Pos, to: Pos): Option[Direction] = None
  val projection: Boolean                        = false
  val promotedFrom: PromotableRole               = Knight
}
case object PromotedLance extends PromotedRole {
  val forsyth: Char                              = 'u' // +l
  val dirs: Directions                           = Gold.dirs
  val dirsOpposite: Directions                   = Gold.dirsOpposite
  def dir(from: Pos, to: Pos): Option[Direction] = None
  val projection: Boolean                        = false
  val promotedFrom: PromotableRole               = Lance
}
case object Horse extends PromotedRole {
  val forsyth: Char                = 'h'         // +b
  val dirs: Directions             = Bishop.dirs //todo
  val dirsOpposite: Directions     = Bishop.dirsOpposite
  def dir(from: Pos, to: Pos)      = Bishop.dir(from, to)
  val projection                   = true
  val promotedFrom: PromotableRole = Bishop
}
case object Dragon extends PromotedRole {
  val forsyth: Char                              = 'd'
  val dirs: Directions                           = Rook.dirs //todo
  val dirsOpposite: Directions                   = dirs
  def dir(from: Pos, to: Pos): Option[Direction] = Rook.dir(from, to)
  val projection: Boolean                        = true
  val promotedFrom: PromotableRole               = Rook
}

object Role {

  val all: List[Role] = List(
    King,
    Rook,
    Bishop,
    Knight,
    Pawn,
    Gold,
    Silver,
    Lance,
    Tokin,
    Horse,
    PromotedSilver,
    PromotedKnight,
    PromotedLance,
    Dragon
  )
  val allPromotable: List[PromotableRole] = List(Rook, Bishop, Knight, Lance, Silver, Pawn)
  val allPromoted: List[PromotedRole] =
    List(Dragon, Horse, PromotedKnight, PromotedLance, PromotedSilver, Tokin)
  val allByForsyth: Map[Char, Role] = all map { r =>
    (r.forsyth, r)
  } toMap
  val allByPgn: Map[Char, Role] = all map { r =>
    (r.pgn, r)
  } toMap
  val allByName: Map[String, Role] = all map { r =>
    (r.name, r)
  } toMap
  val allPromotableByName: Map[String, PromotableRole] =
    allPromotable map { r =>
      (r.toString, r)
    } toMap
  val allPromotableByForsyth: Map[Char, PromotableRole] =
    allPromotable map { r =>
      (r.forsyth, r)
    } toMap
  val allPromotableByPgn: Map[Char, PromotableRole] =
    allPromotable map { r =>
      (r.pgn, r)
    } toMap

  def forsyth(c: Char): Option[Role] = allByForsyth get c

  def promotable(c: Char): Option[PromotableRole] =
    allPromotableByForsyth get c

  def promotable(name: String): Option[PromotableRole] =
    allPromotableByName get name.capitalize

  def promotable(name: Option[String]): Option[PromotableRole] =
    name flatMap promotable

  def valueOf(r: Role): Option[Int] =
    r match {
      case Pawn                                                   => Some(1)
      case Lance                                                  => Some(3)
      case Knight                                                 => Some(4)
      case Silver                                                 => Some(5)
      case Gold | PromotedSilver | PromotedLance | PromotedKnight => Some(6)
      case Tokin                                                  => Some(7)
      case Bishop                                                 => Some(8)
      case Rook                                                   => Some(10)
      case Horse                                                  => Some(10)
      case Dragon                                                 => Some(12)
      case King                                                   => None
    }
}
