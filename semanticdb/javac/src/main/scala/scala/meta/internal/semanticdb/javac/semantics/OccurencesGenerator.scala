package scala.meta.internal.semanticdb.javac.semantics

import com.sun.source.tree.ClassTree
import com.sun.source.tree.CompilationUnitTree
import com.sun.source.tree.IdentifierTree
import com.sun.source.tree.MemberReferenceTree
import com.sun.source.tree.MemberSelectTree
import com.sun.source.tree.MethodInvocationTree
import com.sun.source.tree.MethodTree
import com.sun.source.tree.Tree
import com.sun.source.tree.TypeParameterTree
import com.sun.source.tree.VariableTree
import com.sun.source.util.TreeScanner
import com.sun.tools.javac.tree.JCTree
import javax.lang.model.element.Name
import com.sun.tools.javac.tree.JCTree.JCCompilationUnit
import com.sun.tools.javac.tree.TreeInfo
import com.sun.tools.javac.{code => js}

import scala.meta.internal.semanticdb.SymbolOccurrence
import scala.meta.internal.{semanticdb => s}


object OccurencesGenerator {

  def generateOccurences(cu: CompilationUnitTree): Seq[s.SymbolOccurrence] = {
    val res = cu.accept(Visitor(cu.asInstanceOf[JCCompilationUnit]), Nil)
    res

  }

  case class Visitor(cu: JCCompilationUnit) extends TreeScanner[Seq[s.SymbolOccurrence], Seq[s.SymbolOccurrence]]() {

    def rageFromTree(tree: Tree): Option[s.Range] = {
      val start = TreeInfo.getStartPos(tree.asInstanceOf[JCTree])
      val end = TreeInfo.getEndPos(tree.asInstanceOf[JCTree], cu.endPositions)

      if (end < 0) None else {
        val lm = cu.getLineMap
        Some(s.Range.apply(
          // In scala meta we count lines from 0 not 1
          lm.getLineNumber(start) - 1, lm.getColumnNumber(start), lm.getLineNumber(end) - 1, lm.getColumnNumber(end)))
      }
    }

    def findOwnerPath(sym: js.Symbol): String = sym match {
      case ps: js.Symbol.PackageSymbol =>
        // TODO is it possible?
        ""
      case cs: js.Symbol.ClassSymbol =>
        Option(cs.classfile).orElse(Option(cs.sourcefile)).fold("")(_.toUri.toString)
      case _ =>
        findOwnerPath(sym.owner)
    }

    override def reduce(r1: Seq[SymbolOccurrence], r2: Seq[SymbolOccurrence]): Seq[SymbolOccurrence] =
      Option(r1).toSeq.flatten ++ Option(r2).toSet.flatten


    def nullToNil(from: Seq[SymbolOccurrence]): Seq[SymbolOccurrence] = from match {
      case null => Nil
      case _ => from
    }

    def visitTree(t: Tree, name: Name, definition: Boolean = false): Seq[SymbolOccurrence] = {
      // TODO positions since now we map way, way too much...
      val pos = rageFromTree(t)
      if ( pos.isEmpty) Nil else {
        val sym = TreeInfo.symbol(t.asInstanceOf[JCTree])
        if (sym == null){
          Nil
        } else {
          val occ = s.SymbolOccurrence(
            range = pos,
            symbol = sym.sym,
            role = if(definition) SymbolOccurrence.Role.DEFINITION else SymbolOccurrence.Role.REFERENCE,
            source = findOwnerPath(sym),
            token = name.toString
          )
          Seq(occ)
        }
      }
    }

    override def visitIdentifier(identifierTree: IdentifierTree, p: Seq[SymbolOccurrence]): Seq[SymbolOccurrence] = {
      visitTree(identifierTree, identifierTree.getName)
    }

    override def visitClass(classTree: ClassTree, p: Seq[SymbolOccurrence]): Seq[SymbolOccurrence] =
      nullToNil(super.visitClass(classTree, p)) ++ visitTree(classTree, classTree.getSimpleName, definition = true)
    // TODO
    // labels?
    // arrayApply - as occurence?


    override def visitMethod(methodTree: MethodTree, p: Seq[SymbolOccurrence]): Seq[SymbolOccurrence] =
      nullToNil(super.visitMethod(methodTree, p)) ++ visitTree(methodTree, methodTree.getName, definition = true)


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
