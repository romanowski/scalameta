package scala.meta.tests.tokenizers

import scala.meta._

class TokenizerCoverageSuite() extends BaseTokenizerCoverageSuite {

  // Term
  check[Term.Annotate]("→(a)←: →@A←")
  check[Term.Apply]("→(f)←(→(((a)))←)")
  check[Term.Apply]("→(f)←(→(a)←)")
  check[Term.Apply]("→(f)←(→{ case a => a }←)")
  check[Term.Apply]("→(f)←(→{ x }←)")
  check[Term.ApplyInfix]("→(a)← →op← →(b)←")
  check[Term.ApplyInfix]("→(a, b)← →op← (→c←, →d←)")
  check[Term.ApplyInfix]("→(a, b)← →op← →((c, d))←")
  check[Term.ApplyInfix]("→1← →+← →1←")
  check[Term.ApplyInfix]("→a← →f← →(b)←")
  check[Term.ApplyType]("→(f)← [→A←,→B←]")
  check[Term.ApplyType]("→(f)← [→A←]")
  check[Term.ApplyUnary]("→-← →(a)←")
  check[Term.Ascribe]("→(a)←: →(A)←")
  check[Term.Assign]("→(a)← = →(b)←")
  check[Term.Block]("{ →(a)←; →(b)← }")
  check[Term.Do]("do →{d}← while (→p←)")
  check[Term.Eta]("→(f)← _")
  check[Term.For]("for { →x <- xs← } →(f)←")
  check[Term.ForYield]("for { →x <- xs← } yield →(f)←")
  check[Term.Function]("(→(a)←, →(b)←) => →(a)←")
  check[Term.If]("if (→p←) →(t)← else →(f)←")
  check[Term.If]("if (→p←) →(t)←")
  check[Term.If]("if (→p←) →if (p2) t←")
  check[Term.If]("if (→p←) →{}←")
  check[Term.Interpolate](""" →s←"→start ←${→(a)←}→ end←" """)
  check[Term.Match]("→(a)← match { →case x => x← }")
  check[Term.New]("new →(A)←")
  check[Term.NewAnonymous]("new →(A){}←")
  check[Term.Param, Decl.Def]("def f(→a←: →A← = →(da)←): A")
  check[Term.PartialFunction]("{ →case x => x;← →case y => y← }")
  check[Term.Repeated, Term.Apply]("f(→(x)←: _*)")
  check[Term.Return]("return →(a)←")
  check[Term.Select]("→(a)←.→b←")
  check[Term.Super]("→a.super[B]←.→c←")
  check[Term.This]("→a←.this")
  check[Term.Throw]("throw →(e)←")
  check[Term.Try]("try (→f←) catch { →case x => x;← →case y => y← } finally →{ }←")
  check[Term.TryWithHandler]("try (→f←) catch →(h)← finally →{ }←")
  check[Term.Tuple]("(→(a)←, →(b)←)")
  check[Term.While]("while (→p←) →{d}←")
  check[Term.Xml]("→<a>b←{→c←}→d</a>←")
  checkNone[Term.Name]("(x)")
  checkNone[Term.Placeholder]("(_)")

  // Import
  check[Import]("import →a.b←")
  check[Import]("import →a.b←, →c.d←") // 
  check[Importer, Import]("import →a←.→_←")        // Wildcard
  check[Importer, Import]("import →a←.{ →b←, →c← }") // Name
  check[Importer, Import]("import →a←.{ →b => c← }") // Rename
  check[Importer, Import]("import →a←.{ →b => _← }") // Unimport  
}