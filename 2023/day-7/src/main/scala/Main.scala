def getCardValue(c: String): Int = "23456789TJQKA".indexOf(c) + 2

def getCardValue2(c: String): Int =
  c match {
    case "J" => 1
    case otherwise => getCardValue(otherwise)
  }

def getKindValue(head: Int, second: Int) : Int =  (head, second) match {
    case (5, _) => 7
    case (4, _) => 6
    case (3, 2) => 5
    case (3, 1) => 4
    case (2, 2) => 3
    case (2, 1) => 2
    case (1, 1) => 1
  }

def getKind(cards: Array[String]): Int = {
  val values = cards.groupBy(identity).view.mapValues(_.length).values.toSeq.sorted.reverse
  getKindValue(values.head, values.lift(1).getOrElse(0))
}
def getKind2(cards: Array[String]): Int = {
  val cardMap = cards.groupBy(identity).view.mapValues(_.length)
  val joker = cardMap.getOrElse("J", 0)
  val values = cardMap.filterKeys(_ != "J").values.toSeq.sorted.reverse
  if (values.isEmpty) {
    return 7
  }
  getKindValue(values.head+joker, values.lift(1).getOrElse(0))
}

case class CamelCardsHand(cards: Array[String], bid: Int, kindGetter: Array[String] => Int, valueGetter: String => Int) extends Ordered[CamelCardsHand] {
  def compare(that: CamelCardsHand): Int = {
    val kindComparison = kindGetter(this.cards).compareTo(kindGetter(that.cards))

    if (kindComparison != 0) {
      kindComparison
    } else {
      this.cards.indices.view
        .map(i => valueGetter(this.cards(i)).compareTo(valueGetter(that.cards(i))))
        .find(_ != 0)
        .getOrElse(0)
    }
  }
}

def parse(file: os.Path, kindGetter: Array[String] => Int, valueGetter: String => Int): List[CamelCardsHand] = {
  os.read.lines(file)
    .map(_.split(" "))
    .map(a => CamelCardsHand(a(0).split(""), a(1).toInt, kindGetter, valueGetter))
    .toList
}

def inner_solve(filename: os.Path, kindGetter: Array[String] => Int, valueGetter: String => Int): Int = {
  parse(filename, kindGetter, valueGetter)
    .sorted
    .zipWithIndex.map { (value, index) => (index + 1) * value.bid }.sum
}

def solve1(filename: os.Path): Int = {
  inner_solve(filename, getKind, getCardValue)
}

def solve2(filename: os.Path): Int = {
  inner_solve(filename, getKind2, getCardValue2)
}