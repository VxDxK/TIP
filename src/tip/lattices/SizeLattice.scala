package tip.lattices

object SizeElement extends Enumeration {
  val Bool, Byte, Char, Int, BigInt = Value
}

object SizeLattice extends FlatLattice[SizeElement.Value] with LatticeWithOps {

  import SizeElement._

  private val chain: List[SizeElement.Value] = List(Bool, Byte, Char, Int, BigInt)

  private def maxSize(a: SizeElement.Value, b: SizeElement.Value): SizeElement.Value =
    chain(chain.indexOf(a).max(chain.indexOf(b)))

  private def join(a: Element, b: Element): Element = (a, b) match {
    case (Bot, x) => x
    case (x, Bot) => x
    case (Top, _) => Top
    case (_, Top) => Top
    case (FlatEl(x), FlatEl(y)) => FlatEl(maxSize(x, y))
  }

  def num(i: Int): Element =
    if (i == 0 || i == 1)
      FlatEl(Bool)
    else if (i >= -128 && i <= 127)
      FlatEl(Byte)
    else if (i >= 0 && i <= 65535)
      FlatEl(Char)
    else if (i.toLong >= scala.Int.MinValue && i.toLong <= scala.Int.MaxValue)
      FlatEl(Int)
    else
      FlatEl(BigInt)

  private def arithmetic(a: Element, b: Element): Element =
    join(a, b) match {
      case Bot => Bot
      case Top => Top
      case FlatEl(Bool) | FlatEl(Byte) | FlatEl(Char) => Top
      case FlatEl(Int) => Top
      case FlatEl(BigInt) => FlatEl(BigInt)
      case FlatEl(_) => Top
    }

  def plus(a: Element, b: Element): Element = arithmetic(a, b)

  def minus(a: Element, b: Element): Element = arithmetic(a, b)

  def times(a: Element, b: Element): Element = arithmetic(a, b)

  def div(a: Element, b: Element): Element = arithmetic(a, b)

  def eqq(a: Element, b: Element): Element =
    join(a, b) match {
      case Bot => Bot
      case Top => Top
      case _ => FlatEl(Bool)
    }

  def gt(a: Element, b: Element): Element =
    join(a, b) match {
      case Bot => Bot
      case Top => Top
      case _ => FlatEl(Bool)
    }
}
