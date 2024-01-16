package objsets

class TweetSetSuite extends munit.FunSuite:
  trait TestSets:
    val set0 = Empty()
    val set1 = set0.incl(Tweet("a", "a body", 20))
    val set2 = set1.incl(Tweet("b", "b body", 30))
    val c = Tweet("c", "c body", 10)
    val d = Tweet("d", "d body", 20)
    val set3c = set2.incl(c)
    val set3d = set2.incl(d)
    val set4 = set3c.incl(d)

  def asSet(tweets: TweetSet): Set[Tweet] =
    var res = Set[Tweet]()
    // tweets.foreach(res += _)
    // or more understandably:
    tweets.foreach(tweet => res = res + tweet)
    res

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set0") {
    new TestSets:
      assertEquals(size(set0.filter(tw => tw.user == "a")), 0)
      
      // this is more to the point in terms of the intention of the check
      // although isInstanceOf method is decouraged in Scala
      assert(set0.isInstanceOf[Empty])
  }

  test("filter: a on set1") {
    new TestSets:
      assertEquals(size(set1.filter(tw => tw.user == "a")), 1)
      assert(set1.isInstanceOf[NonEmpty])
  }

  test("filter: twenty on set1") {
    new TestSets:
      assertEquals(size(set1.filter(tw => tw.retweets == 20)), 1)
  }

  test("filter: twenty on set2") {
    new TestSets:
      assertEquals(size(set2), 2)
      assertEquals(size(set2.filter(tw => tw.retweets == 20)), 1)
  }  

  test("filter: twenty on set4") {
    new TestSets:
      assertEquals(size(set4), 4)
      assertEquals(size(set4.filter(tw => tw.retweets == 20)), 2)
  }

  test("union: set3c and set3d") {
    new TestSets:
      assertEquals(size(set3c.union(set3d)), 4)
  }

  test("union: set4 with empty set0") {
    new TestSets:
      assertEquals(size(set4.union(set0)), 4)
  }

  test("union: empty set0 with empty set4") {
    new TestSets:
      assertEquals(size(set0.union(set4)), 4)
  }

  test("mostRetweeted: with set1") {
    new TestSets:
      assertEquals(set1.mostRetweeted.retweets, 20)
  }

  test("mostRetweeted: with set2") {
    new TestSets:
      assertEquals(set2.mostRetweeted.retweets, 30)
  }

  test("mostRetweeted: with set4") {
    new TestSets:
      assertEquals(set4.mostRetweeted.retweets, 30)
  }

  test("descending: set4") {
    new TestSets:
      val trends = set4.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.retweets == 30)
      assert(trends.tail.head.retweets == 20)
      assert(trends.tail.tail.head.retweets == 20)
      assert(trends.tail.tail.tail.head.retweets == 10)
      assert(trends.tail.tail.tail.tail.isEmpty == true)    
  }


  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
