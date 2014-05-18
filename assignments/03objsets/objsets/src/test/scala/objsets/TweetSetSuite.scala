package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    val set6 = set1.incl(new Tweet("a", "a body", 20))
    val set7 = set6.incl(new Tweet("b", "b body", 21))
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      var result: TweetSet = set5.filter(tw => tw.user == "a")
      assert(size(result) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("filter: 321 on tweet data") {
    new TestSets {
      assert(size(TweetReader.allTweets.filter(tw => tw.retweets == 321)) === 1)
    }
  }

  test("filter: 205 on tweet data") {
    new TestSets {
      assert(size(TweetReader.allTweets.filter(tw => tw.retweets == 205)) === 1)
    }
  }

  test("filter: 205 or 321 on tweet data") {
    new TestSets {
      assert(size(TweetReader.allTweets.filter(tw => tw.retweets == 205 || tw.retweets == 321)) === 2)
    }
  }

  test("filter: trending apple and google then 205 or 321 on tweet data") {
    new TestSets {

      val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
      val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

      lazy val googleTweets: TweetSet = TweetReader.allTweets.filter(t => google.exists(s => t.text.contains(s)))
      lazy val appleTweets: TweetSet = TweetReader.allTweets.filter(t => apple.exists(s => t.text.contains(s)))

      assert(size((appleTweets union googleTweets).filter(tw => tw.retweets == 205 || tw.retweets == 321)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("mostRetweeted: empty set") {
    new TestSets {

      intercept[NoSuchElementException] {
        var mr = set1.mostRetweeted
      }
    }
  }

  test("mostRetweeted: set5") {
    new TestSets {
      val mr = set5.mostRetweeted
      assert(mr.retweets == 20)
    }
  }

  test("mostRetweeted: set7") {
    new TestSets {
      val mr = set7.mostRetweeted
      assert(mr.retweets == 21)
    }
  }

  test("descending: empty set") {
    new TestSets {
      val trends = set1.descendingByRetweet
      assert(trends.isEmpty)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

  test("descending: set7") {
    new TestSets {
      val trends = set7.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "b")
    }
  }

  test("google vs apple: count tweets") {
    assert(size(GoogleVsApple.googleTweets) === 35)
    assert(size(GoogleVsApple.appleTweets) === 140)
  }
}
