package com.github.sbroadhead.examples

import com.github.sbroadhead.codegraph._

/**
 * Example of using `CodeGraph` for a simple expression library.
 */
object Expressions {
  import Exceptions._

  /** Node labels for expression graphs. */
  object NodeLabels {
    /** Base class for node labels in the expression graph. */
    sealed abstract class ExprNodeLabel
    /** Integer-valued graph-node. */
    case class INT() extends ExprNodeLabel
    /** Boolean-valued graph-node. */
    case class BOOL() extends ExprNodeLabel
  }

  /** Edge labels for expression graphs. */
  object EdgeLabels {
    import NodeLabels._

    /** Base class for edge labels in the expression graph. */
    sealed abstract class ExprEdgeLabel
    /** Load a constant `Int`. */
    case class ConstInt(value: Int) extends ExprEdgeLabel with EdgeLabel.Nullary[INT]
    /** Load a constant `Boolean`. */
    case class ConstBool(value: Boolean) extends ExprEdgeLabel with EdgeLabel.Nullary[BOOL]
    /** Integer addition. */
    case object `add` extends ExprEdgeLabel with EdgeLabel.Binary[INT, INT, INT]
    /** Integer subtraction. */
    case object `sub` extends ExprEdgeLabel with EdgeLabel.Binary[INT, INT, INT]
    /** Integer multiplication. */
    case object `mul` extends ExprEdgeLabel with EdgeLabel.Binary[INT, INT, INT]
    /** Integer division. */
    case object `div` extends ExprEdgeLabel with EdgeLabel.Binary[INT, INT, INT]
    /** Integer compares less than. */
    case object `lt` extends ExprEdgeLabel with EdgeLabel.Binary[INT, INT, BOOL]
    /** Integer compares greater than. */
    case object `gt` extends ExprEdgeLabel with EdgeLabel.Binary[INT, INT, BOOL]
    /** Integer compares equal. */
    case object `equ` extends ExprEdgeLabel with EdgeLabel.Binary[INT, INT, BOOL]
    /** Logical AND. */
    case object `and` extends ExprEdgeLabel with EdgeLabel.Binary[BOOL, BOOL, BOOL]
    /** Logical OR. */
    case object `or` extends ExprEdgeLabel with EdgeLabel.Binary[BOOL, BOOL, BOOL]
  }

  import NodeLabels._
  import EdgeLabels._

  /**
   * `CodeGraph` type representing expressions.
   */
  type ExpressionCodeGraph = CodeGraph[ExprNodeLabel, ExprEdgeLabel]

  /**
   * Type-class supporting the promotion of constant literals to well-typed node names generated
   * by nullary (zero-input) edges.
   *
   * The type `T` should be a primitive Scala type (i.e., `Int`),
   * `NodeType` should be the corresponding node type (i.e., `INT`), and `EdgeType` should
   * be the type of a nullary edge label that generates nodes of that type.
   */
  trait ConstEdge[T, NodeType <: ExprNodeLabel with Product, EdgeType <: ExprEdgeLabel with EdgeLabel.Nullary[NodeType]] {
    def promote(value: T): EdgeType
    def newNode: NodeName[NodeType]
  }

  implicit def constIntEdge: ConstEdge[Int, INT, ConstInt] =
    new ConstEdge[Int, INT, ConstInt] {
      override def promote(value: Int): ConstInt = ConstInt(value)
      override def newNode: NodeName[INT] = NodeName.fresh[INT](INT())
    }

  implicit def constBoolEdge: ConstEdge[Boolean, BOOL, ConstBool] =
    new ConstEdge[Boolean, BOOL, ConstBool] {
      override def promote(value: Boolean): ConstBool = ConstBool(value)
      override def newNode: NodeName[BOOL] = NodeName.fresh[BOOL](BOOL())
    }

  /**
   * A simple but helpful `CodeGraph` mutator that takes a constant of an arbitrary type
   * and uses the [[ConstEdge]] typeclass to turn it into a well-typed CodeGraph node.
   *
   * This way we don't have to type a different constructor name (e.g., `ConstInt`) for
   * every type of constant we wish to use.
   */
  def const[T, U <: ExprNodeLabel with Product, V <: ExprEdgeLabel with EdgeLabel.Nullary[U]](x: T)
    (implicit pc: ConstEdge[T, U, V], builder: CodeGraphBuilder[ExprNodeLabel, ExprEdgeLabel]): NodeName[U] = {
      import builder.mutators._
      val edgeLabel = pc.promote(x)
      val node = pc.newNode
      addNode(node)
      addEdge(new CodeGraph.Edge[ExprEdgeLabel](edgeLabel, Seq(), Seq(node.key)))
      node
    }

  /**
   * Fully evaluate an [[ExpressionCodeGraph]] and return the computed results.
   * @param cg The [[ExpressionCodeGraph]] to evaluate.
   * @param inputs The inputs to the CodeGraph.
   * @return The computed results.
   */
  def evaluate(cg: ExpressionCodeGraph, inputs: Seq[Any]): Seq[Any] = {
    import CodeGraphOps._
    var env: Map[CodeGraph.NodeKey, Any] = cg.inputs.zip(inputs).toMap
    for (edgeKey <- cg.topSort) {
      val edge = cg.edge(edgeKey).head
      def args = edge.args.map(x => env.getOrElse(x, throw new RuntimeException(s"Node not evaluated: $x")))

      def arg0[T] = args(0).asInstanceOf[T]
      def arg1[T] = args(1).asInstanceOf[T]

      def results = edge.label match {
        case ConstInt(x) => Seq(x)
        case ConstBool(x) => Seq(x)
        case `add` => Seq(arg0[Integer] + arg1[Integer])
        case `sub` => Seq(arg0[Integer] - arg1[Integer])
        case `mul` => Seq(arg0[Integer] * arg1[Integer])
        case `div` => Seq(arg0[Integer] / arg1[Integer])
        case `lt` => Seq(arg0[Integer] < arg1[Integer])
        case `gt` => Seq(arg0[Integer] > arg1[Integer])
        case `equ` => Seq(arg0[Integer] == arg1[Integer])
        case `and` => Seq(arg0[Boolean] && arg1[Boolean])
        case `or` => Seq(arg0[Boolean] || arg1[Boolean])
      }
      env = env ++ edge.results.zip(results).toMap
    }
    cg.outputs.map(x => env.getOrElse(x, throw InvalidNodeKeyException(x)))
  }

  /**
   * Specialized [[CodeGraphBuilder]] supporting extended syntax for expressions.
   */
  class ExpressionGraphBuilder extends CodeGraphBuilder[ExprNodeLabel, ExprEdgeLabel] {
    import mutators._

    implicit class IntegerOperations(n: NodeName[INT]) {
      def +(m: NodeName[INT]): NodeName[INT] = add $(n, m)
      def -(m: NodeName[INT]): NodeName[INT] = sub $(n, m)
      def *(m: NodeName[INT]): NodeName[INT] = mul $(n, m)
      def /(m: NodeName[INT]): NodeName[INT] = div $(n, m)
      def <(m: NodeName[INT]): NodeName[BOOL] = lt $(n, m)
      def >(m: NodeName[INT]): NodeName[BOOL] = gt $(n, m)
      def ===(m: NodeName[INT]): NodeName[BOOL] = equ $(n, m)
    }

    implicit class BooleanOperations(n: NodeName[BOOL]) {
      def &&(m: NodeName[BOOL]): NodeName[BOOL] = and $(n, m)
      def ||(m: NodeName[BOOL]): NodeName[BOOL] = or $(n, m)
    }

    // Automatic constant promotion
    implicit def toConst[T, U <: ExprNodeLabel with Product, V <: ExprEdgeLabel with EdgeLabel.Nullary[U]]
      (value: T)(implicit ct: ConstEdge[T, U, V]): NodeName[U] =
        const(value)
  }

  // Simple expression
  object simpleExprCodeGraph extends ExpressionGraphBuilder {
    type Input = (INT, INT)
    type Output = (INT, BOOL)

    import mutators._

    val (x, y) = input
    val sum = x + y
    val prod = x * y
    val prodMinusSum = prod - sum
    val resultGt100 = prodMinusSum > 100

    output(prodMinusSum, resultGt100)
  }

  // Evaluate a polynomial
  case class PolynomialGraph(coeffs: Seq[Int]) extends ExpressionGraphBuilder {
    type Input = Tuple1[INT]
    type Output = Tuple1[INT]

    import mutators._

    val x = input
    var r = const(coeffs.head)
    for (a <- coeffs.tail) {
      r = (r * x) + a
    }
    output(r)
  }

  // Compose CodeGraphs using splicing
  object spliceCodeGraph extends ExpressionGraphBuilder {
    type Input = Tuple2[INT, INT]
    type Output = Tuple1[INT]

    import mutators._

    val (x, y) = input
    val (prodMinusSum, resultGt100) = splice(simpleExprCodeGraph, x, y)
    val answer = splice(PolynomialGraph(Seq(2, 4, 6, 8)), prodMinusSum)
    output(answer)
  }
}
