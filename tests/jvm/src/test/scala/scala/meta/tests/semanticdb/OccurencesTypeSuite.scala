package scala.meta.tests.semanticdb

import scala.meta.internal.semanticdb.scalac.OccurrenceMode

class OccurencesTypeSuite extends SemanticdbSuite(occurences = OccurrenceMode.SymbolAndType) {


//  occurrences(
//    """object L2 {
//	    |  val list = List(1, 2, 3)
//	    |  list
//			|  list.head
//	    |}""".stripMargin
//	  ,
//    """[0:7..0:9): L2 <= _empty_.L2.
//	    |[1:6..1:10): list <= _empty_.L2.list().
//	    |[1:13..1:17): List => scala.collection.immutable.List. ((xs: Int*): List[Int])
//	    |[2:2..2:6): list => _empty_.L2.list(). (List[Int])
//	    |[3:2..3:6): list => _empty_.L2.list(). (List[Int])
//	    |[3:7..3:11): head => scala.collection.IterableLike#head(). (Int)""".stripMargin
//  )

	synthetics(
		"""object L2 { val head = Array(1, 2, 3)(0) }"""
		,
		"""[0:28..0:28):  => *.apply
			|[0:0..0:1): * => _star_.
			|[0:2..0:7): apply => scala.Array.apply(Int,Int*). ((x: Int, xs: Int*): Array[Int])
			|[0:37..0:37):  => *.apply
			|[0:0..0:1): * => _star_.
			|[0:2..0:7): apply => scala.Array#apply(Int). (Int: Int)""".stripMargin
	)

//	occurrences(
//		"""object L2 {
//			|  var foo = List(1,2)
//			|  foo = List(1,2,3)
//			|  foo ::= 3
//			|}
//		""".stripMargin
//  ,
//		"""""".stripMargin
//	)
//	)
//
//	occurrences(
//		"""object L3 { def param(a: Seq[Int]) = a.head } """
//	,
//		"""[0:25..0:28) scala.package.Seq#
//			|[0:29..0:32) scala.Int#
//			|[0:37..0:38) scala.package.Seq#scala.Int#
//			|[0:39..0:43) scala.Int#""".stripMargin
//	)
//
//	occurrences(
//		"""object L4 { val a = List.empty[Int].map(_ + 1) } """
//	,
//		"""[0:20..0:24) scala.collection.immutable.List.
//			|[0:25..0:30) scala.collection.immutable.List#scala.Int#
//			|[0:31..0:34) scala.Int#
//			|[0:36..0:39) scala.collection.immutable.List#map(Function1,CanBuildFrom).(f)scala.collection.immutable.List#map(Function1,CanBuildFrom).(bf)scala.collection.immutable.List#scala.Int#
//			|[0:39..0:39) ~> [0:12..0:15) scala.collection.immutable.List#map(Function1,CanBuildFrom).(f)scala.collection.immutable.List#map(Function1,CanBuildFrom).(bf)scala.collection.immutable.List#scala.Int#
//			|[0:39..0:39) ~> [0:2..0:5) scala.collection.immutable.List#map(Function1,CanBuildFrom).(f)scala.collection.immutable.List#map(Function1,CanBuildFrom).(bf)scala.collection.immutable.List#scala.Int#
//			|[0:39..0:39) ~> [0:7..0:11) scala.collection.immutable.List#map(Function1,CanBuildFrom).(f)scala.collection.immutable.List#map(Function1,CanBuildFrom).(bf)scala.collection.immutable.List#scala.Int#
//			|[0:42..0:43) scala.Int#`+`(Int).(x)scala.Int#
//			|[0:46..0:46) ~> [0:34..0:46) scala.collection.immutable.List#scala.Int#
//			|[0:46..0:46) ~> [0:47..0:50) scala.collection.immutable.List#scala.Int#""".stripMargin
//	)
}
