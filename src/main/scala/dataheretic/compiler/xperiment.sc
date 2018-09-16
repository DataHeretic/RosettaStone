import scala.collection.mutable

val header = Seq("h1", "h2", "h3")

val rows =
  Seq(
    Seq("a", "b", "c"),
    Seq("1", "2"),
    Seq("!", "@", "#")
  )

rows.transpose

//rows map { _.map (Seq.apply(_)) }

//
//rows map { _.zipWithIndex.groupBy(_._2).mapValues(_.map(_._2)) }
//

//
//rows.foldLeft(Map.empty[Int, Seq[String]]) { case (accum, nxt) =>
//  val nxtMap = (nxt zipWithIndex) map { case (a,b)=>(b,a) } toMap
//
//
//}