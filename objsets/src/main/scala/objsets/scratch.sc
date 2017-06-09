import objsets._

object tweetsets {

  val set1 = new Empty
  val set2 = set1.incl(new Tweet("a", "a body", 21))
  val set3 = set2.incl(new Tweet("b", "b body", 20))
  val c = new Tweet("c", "c body", 7)
  val d = new Tweet("d", "d body", 99)
  val set4c = set3.incl(c)
  val set4d = set3.incl(d)
  val set5 = set4c.incl(d)

  println("descendingByRetweet")

  //val trends = set5.descendingByRetweet
  //trends.head.user

  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  google.exists(s => s.contains("not there"))
  //lazy val googleTweets: TweetSet = TweetReader.allTweets.filter(p => google.exists(s => s.contains(p.text)))
  // val appleTweets: TweetSet = TweetReader.allTweets.filter(p => apple.exists(s => s.contains(p.text)))

}