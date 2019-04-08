package scala.meta.internal.semanticdb.javac.semantics

import com.sun.source.tree.ClassTree
import com.sun.source.tree.CompilationUnitTree
import com.sun.source.tree.IdentifierTree
import com.sun.source.tree.MemberReferenceTree
import com.sun.source.tree.MemberSelectTree
import com.sun.source.tree.MethodTree
import com.sun.source.tree.Tree
import com.sun.source.tree.TypeParameterTree
import com.sun.source.tree.VariableTree
import com.sun.source.util.TreeScanner
import com.sun.tools.javac.tree.JCTree
import com.sun.tools.javac.tree.JCTree.JCCompilationUnit
import com.sun.tools.javac.tree.TreeInfo
import javax.lang.model.element.Name

import scala.meta.internal.semanticdb.SymbolOccurrence
import scala.meta.internal.semanticdb.Range

object SymbolOccurrencesGenerator {

  def generateOccurrences(cu: CompilationUnitTree): Seq[SymbolOccurrence] =
   cu.accept(Visitor(cu.asInstanceOf[JCCompilationUnit]), Nil)

  case class Visitor(cu: JCCompilationUnit) extends TreeScanner[Seq[SymbolOccurrence], Seq[SymbolOccurrence]]() {

    def rangeFromTree(tree: Tree): Option[Range] = {
      val start = TreeInfo.getStartPos(tree.asInstanceOf[JCTree])
      val end = TreeInfo.getEndPos(tree.asInstanceOf[JCTree], cu.endPositions)

      if (end < 0) None else {
        val lm = cu.getLineMap
        Some(Range.apply(
          // In scala meta we count lines from 0 not 1
          lm.getLineNumber(start) - 1, lm.getColumnNumber(start), lm.getLineNumber(end) - 1, lm.getColumnNumber(end)))
      }
    }

    override def reduce(r1: Seq[SymbolOccurrence], r2: Seq[SymbolOccurrence]): Seq[SymbolOccurrence] =
      nullToNil(r1) ++ nullToNil(r2)


    private def nullToNil(from: Seq[SymbolOccurrence]): Seq[SymbolOccurrence] = from match {
      case null => Nil
      case _ => from
    }

    def visitTree(t: Tree, name: Name, definition: Boolean = false): Seq[SymbolOccurrence] = {
      val pos = rangeFromTree(t)
      if (pos.isEmpty) Nil else {
        val sym = TreeInfo.symbol(t.asInstanceOf[JCTree])
        if (sym == null){
          Nil
        } else {
          val occ = SymbolOccurrence(
            range = pos,
            symbol = sym.sym,
            role = if (definition) SymbolOccurrence.Role.DEFINITION else SymbolOccurrence.Role.REFERENCE,
          )
          Seq(occ)
        }
      }
    }

    override def visitIdentifier(identifier: IdentifierTree, result: Seq[SymbolOccurrence]): Seq[SymbolOccurrence] =
      visitTree(identifier, identifier.getName)

    override def visitClass(classTree: ClassTree, results: Seq[SymbolOccurrence]): Seq[SymbolOccurrence] =
      nullToNil(super.visitClass(classTree, results)) ++ visitTree(classTree, classTree.getSimpleName, definition = true)

    // TODO consider other visit methods
    // labels?
    // arrayApply - as occurence?

    override def visitMethod(methodTree: MethodTree, results: Seq[SymbolOccurrence]): Seq[SymbolOccurrence] =
      nullToNil(super.visitMethod(methodTree, results)) ++ visitTree(methodTree, methodTree.getName, definition = true)

    override def visitMemberSelect(memberSelectTree: MemberSelectTree, p: Seq[SymbolOccurrence]): Seq[SymbolOccurrence] =
      nullToNil(super.visitMemberSelect(memberSelectTree, p)) ++
        visitTree(memberSelectTree, memberSelectTree.getIdentifier)

    override def visitMemberReference(memberReferenceTree: MemberReferenceTree, p: Seq[SymbolOccurrence]): Seq[SymbolOccurrence] =
      nullToNil(super.visitMemberReference(memberReferenceTree, p)) ++
        visitTree(memberReferenceTree, memberReferenceTree.getName)


    override def visitTypeParameter(typeParameterTree: TypeParameterTree, p: Seq[SymbolOccurrence]): Seq[SymbolOccurrence] =
      nullToNil(super.visitTypeParameter(typeParameterTree, p)) ++
        visitTree(typeParameterTree, typeParameterTree.getName)

    override def visitVariable(variableTree: VariableTree, p: Seq[SymbolOccurrence]): Seq[SymbolOccurrence] =
      nullToNil(super.visitVariable(variableTree, p)) ++
        visitTree(variableTree, variableTree.getName, definition = true)
  }
}
