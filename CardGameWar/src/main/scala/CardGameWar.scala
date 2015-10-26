import scala.util.Random

case class Deck(cards: List[Card])

case class Player(name: String, deck: Deck)

sealed abstract class Suit(val intValue: Int)

case object Spade extends Suit(0)
case object Club extends Suit(1)
case object Diamond extends Suit(2)
case object Heart extends Suit(3)

case class Rank(rank: String) {
  val validCourtMembers = Seq("Jack", "Queen", "King", "Ace")

  def isNumeric(input: String): Boolean = input.forall(_.isDigit)

  def intValue: Int = {
    if(isNumeric(rank)) rank.toInt
    else {
      rank.toLowerCase match {
        case "jack" => 11
        case "queen" => 12
        case "king" => 13
        case "ace" => 14
      }
    }
  }

  def verifyRanksForInt: String => Unit = i => {
    val lessThenOne = i.toInt <= 1
    val overTen = i.toInt > 10
    if (lessThenOne || overTen) throw new Exception("invalid rank!")
  }

  def verifyRanksForString: String => Unit = court => {
    val courtMember = validCourtMembers.find(_.toLowerCase == court.toLowerCase)
    if (courtMember.isEmpty) throw new Exception("invalid court member! off with thar heads")
  }

  def apply(court: String) = {
    if(isNumeric(court)) verifyRanksForInt(court)
    else verifyRanksForString(court)
  }
}

object RealCard{
  private def suitFromString(suit: String): Suit =
    suit.toLowerCase match {
      case "spade" => Spade
      case "club" => Club
      case "diamond" => Diamond
      case "heart" => Heart
      case _ => throw new Exception("!!!! unknown: "+suit)
    }

  private def rankFromString(rank: String): Rank = Rank(rank)

  def fromString(suit:String, rank:String) = {
    val s: Suit = suitFromString(suit)
    val r: Rank = rankFromString(rank)
    RealCard(s, r)
  }
}

case class Card(suit: String, rank: String){
  val realCard: RealCard = RealCard.fromString(suit, rank)
}

case class RealCard(suit: Suit, rank: Rank)

object CardGameWar {
  // Feel free to use these cards or use your own data structure
  val suits = List("Spade", "Club", "Diamond", "Heart")
  val ranks = List("2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King", "Ace")

  // Creates a shuffled deck of cards
  def createDecks: (Deck, Deck) = {
  val allCards =
    new Random shuffle (for {
      suit <- suits
      rank <- ranks
    } yield Card(suit, rank.toString))

    if(allCards.length != 52) throw new Exception("did you add jokers?")
    if(allCards.length % 2 == 1) throw new Exception("its an odd deck...")

    val List(d1, d2) = allCards.grouped(allCards.length / 2).toList
    (Deck(d1), Deck(d2))
  }

  def playRound(player1: Card, player2: Card): Card = {
    val p1rank = player1.realCard.rank.intValue
    val p1suit = player1.realCard.suit.intValue
    val p2rank = player2.realCard.rank.intValue
    val p2suit = player2.realCard.suit.intValue

    //println(s"p1 -> ($p1rank,$p1suit); p2 -> ($p2rank,$p2suit)")

    if(p1rank == p2rank){
      if(p1suit > p2suit) player1 else player2
    }else{
      if(p1rank > p2rank) player1
      else player2
    }
  }

  def checkDecks(p1: Player, p2:Player): Option[Player] = {
    if(p1.deck.cards.length == 0) Some(p1)
    else if(p2.deck.cards.length == 0) Some(p2)
    else None
  }

  def playAndRetDecks(p1: Player, p2: Player): (Player, Player) = {
    val p1card :: p1rem = p1.deck.cards
    val p2card :: p2rem = p2.deck.cards
    val cardWon = playRound(p1card, p2card)

    val newDecks =
      if(cardWon == p1card){
        val p1newDeck =  p1rem ++ List(p1card, cardWon)
        val p2newDeck = p2rem
        (Deck(p1newDeck), Deck(p2newDeck))
      } else {
        val p2newDeck =  p2rem ++ List(p2card, cardWon)
        val p1newDeck = p1rem
        (Deck(p1newDeck), Deck(p2newDeck))
      }

    (Player(p1.name, newDecks._1), Player(p2.name, newDecks._2))
  }

  def playGame(player1: Player, player2: Player): String = {
    var noWinner = true
    var winner: Option[Player] = None

    // stateful vars because im to lazy to use the state monad atm...
    var sPlayer1 = player1
    var sPlayer2 = player2

    while(noWinner){
      val (p1, p2) = playAndRetDecks(sPlayer1, sPlayer2)
      //println(s"${p1.deck.cards.length} vs ${p2.deck.cards.length}")

      val win = checkDecks(p1, p2)
      if(win.isDefined) {
        winner = win
        noWinner = false
      }

      //update that state
      sPlayer1 = p1
      sPlayer2 = p2
    }

    val winnerFound = winner.getOrElse(throw new Exception("no winner?"))
    winnerFound.name
  }

}

