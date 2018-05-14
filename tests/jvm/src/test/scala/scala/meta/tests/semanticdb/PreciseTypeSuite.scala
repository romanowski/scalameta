package scala.meta.tests.semanticdb

import scala.meta.internal.semanticdb3.Print
import scala.meta.internal.{semanticdb3 => s}

class PreciseTypeSuite extends SemanticdbSuite() {

  def testActualTypes(code: String)(occurencesTypes: String) = test(code){
    val db = computeDatabaseFromSnippet(code)
    val allOccurences = db.occurrences ++ db.synthetics.flatMap(_.text).flatMap(_.occurrences)

    def printOccurence(occ: s.SymbolOccurrence) = occ.actualTpe.map( tpe =>
          s"${Print.range(occ.range.get)} ${Print.tpe(db, tpe)}"
      )

    val occurencesStrings = for {
      syn <- db.synthetics
      text = syn.text.get
      occ <- text.occurrences
    } yield printOccurence(occ).map(t => s"${Print.range(syn.range.get)} ~> $t")

    val typeLines = db.occurrences.map(printOccurence) ++ occurencesStrings
    val typeString = typeLines.flatten.sorted.mkString("\n")
    assert(typeString == occurencesTypes)
  }

  testActualTypes(
    "object L1 { val lambda = () => 123 }"
  )(
    "[0:25..0:34) scala.Function0#scala.Int#"
  )

  testActualTypes(
    "object L2 { val head = Array(1, 2, 3)(0) }"
  )(
    """[0:23..0:28) scala.Array.
      |[0:28..0:28) ~> [0:2..0:7) scala.Array.apply(Int,Int*).(x)scala.Array.apply(Int,Int*).(xs)scala.Array#scala.Int#
      |[0:37..0:37) ~> [0:2..0:7) scala.Array#apply(Int).(i)scala.Int#""".stripMargin
  )
}
