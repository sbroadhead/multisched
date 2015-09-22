package com.github.sbroadhead.codegraph

import com.github.sbroadhead._
import com.github.sbroadhead.codegraph.CodeGraph.NodeKey

import scala.util._
import scala.language.experimental.macros
import scala.collection.mutable.{Seq => MutableSeq}
import scala.reflect.runtime.universe.{Try => _, _}
import shapeless._
import shapeless.syntax.std.traversable._
import shapeless.ops.{hlist => hl, tuple => tp, traversable => tv}
import shapeless.nat._

/**
 * Trait supporting mutable [[CodeGraph]] operations.
 * @tparam N The node label type.
 * @tparam E The edge label type.
 */
trait CodeGraphBuilder[N, E] extends CodeGraphInterface { self =>
  /**
   * The [[MutableCodeGraph]] underlying this instance.
   */
  implicit protected val cg: MutableCodeGraph.Aux[N, E, Input, Output] = new MutableCodeGraph[N, E] {
    type Input = self.Input
    type Output = self.Output
  }

  implicit val thisImplicit: CodeGraphBuilder[N, E] = this

  def nodes = cg.nodes.toMap
  def edges = cg.edges.toMap
  def inputs = cg.inputs.toSeq
  def outputs = cg.outputs.toSeq

  /**
   * Helper object to splice a typed CodeGraph into `cg` given typed arguments,
   * and producing typed output nodes.
   */
  implicit class Splicer[Input0, Output0, S <: Nat, InputNN <: Product,
      OutputNN <: Product, OutputGen <: Product, Gen <: HList, GenN <: HList, Final]
    (subcg: CodeGraph[N, E] with CodeGraphInterface.Aux[Input0, Output0])
    (implicit
      nntmi: NodeNameTupleMapped.Aux[Input0, InputNN],
      nntmo: NodeNameTupleMapped.Aux[Output0, OutputNN],
      gen: Generic.Aux[OutputNN, Gen],
      sz: hl.Length.Aux[Gen, S],
      fll: hl.Fill.Aux[S, NodeName[N], GenN],
      tup: hl.Tupler.Aux[GenN, OutputGen],
      ft: tv.FromTraversable[GenN],
      tuw: TupleUnwrapper.Aux[OutputNN, Final])
  {
    def apply(args: InputNN): Final = {
      import CodeGraphOps._

      val newcg = MutableCodeGraph(subcg.duplicate)
      val argSeq = args.productIterator.map { case x: NodeName[N] => x.key }.toSeq
      val renameMap = newcg.inputs.zip(argSeq).toMap

      cg.nodes ++= newcg.nodes -- newcg.inputs
      for (e <- newcg.edges.values) {
        addEdge(new CodeGraph.Edge[E](
          e.label,
          e.args.map(x => renameMap.getOrElse(x, x)),
          e.results // inputs can't be in the results
        ))
      }
      tuw(seqToOutput(newcg.outputs))
    }

    def apply(args: Any*): Any = macro CodeGraphMacros.spliceSyntaxImpl

    private def seqToOutput(keys: Seq[CodeGraph.NodeKey]): OutputNN = {
      val nodeNames = keys.map(x =>
        NodeName[N](cg.nodes.getOrElse(x, throw Exceptions.InvalidNodeKeyException(x)), x))
      val hl = nodeNames.toList.toHList[GenN]
      hl.map(h => h.tupled).getOrElse {
        throw new RuntimeException("Output nodes did not match up with expected type")
      }.asInstanceOf[OutputNN]
    }
  }

  /**
   * Create and return the input nodes for this graph, mutating the CodeGraph to incorporate
   * them in the process.
   * @return the tuple of strongly-typed input nodes.
   */
  def input[T <: Product]
    (implicit
      nni: NodeNameInstantiator.Aux[Input, T],
      tuw: TupleUnwrapper[T]): tuw.Out = {
    if (_inputs != null) {
      return _inputs.asInstanceOf[tuw.Out]
    }
    val inputs = nni.apply()
    val nodes = inputs.productIterator.collect { case x: NodeName[N] => x }.toSeq
    for (node <- nodes) addNode(node)
    cg.inputs = MutableSeq(nodes.map(_.key) : _*)
    val unwrapped = tuw(inputs)
    _inputs = unwrapped
    unwrapped
  }

  /**
   * Helper class to consume a tuple of output nodes and mutate the CodeGraph accordingly.
   */
  case class OutputConsumer[T <: Product](implicit nni: NodeNameInstantiator.Aux[Output, T]) {
    def apply(args: T) {
      if (_outputNodesSet)
        throw new RuntimeException("Output nodes have already been set for this CodeGraph")
      val nodes = args.productIterator.collect { case x: NodeName[N] => x }
      cg.outputs = MutableSeq(nodes.map(_.key).toSeq : _*)
      _outputNodesSet = true
    }
  }

  /**
   * Macro wrapper around clumsy `OutputConsumer` syntax. Simply call output(node1, node2, ...)
   * and it will be transformed into an invocation of `OutputConsumer().apply((node1, node2, ...))`.
   */
  def output(args: Any*): Unit = macro CodeGraphMacros.outputImpl


  /**
   * Helper class for applying arguments to a strongly-typed edge label (that has
   * the [[EdgeLabel]] trait) in a strongly-typed fashion.
   */
  implicit class EdgeFactory[Args <: Product, Results <: Product, ATup <: Product, RTup <: Product, Final]
    (label: E with EdgeLabel[Args, Results])
    (implicit
      nntm: NodeNameTupleMapped.Aux[Args, ATup],
      inst: NodeNameInstantiator.Aux[Results, RTup],
      tuw: TupleUnwrapper.Aux[RTup, Final])
  {
    /**
     * Apply the given arguments to this edge label and add the resulting edge
     * to the underlying [[CodeGraph]].
     * @param args the arguments to this edge.
     * @return the result nodes of this edge.
     */
    def withArgs(args: ATup): Final = {
      val argKeys = args.productIterator.map { case NodeName(_, key) => key }.toSeq
      if (_cache.contains((label, argKeys))) {
        return _cache((label, argKeys)).asInstanceOf[Final]
      }
      val resultNodes = inst.apply()
      val resKeys = resultNodes.productIterator.map { case NodeName(_, key) => key }
      val resultsTyped = resultNodes.productIterator.map { x => x.asInstanceOf[NodeName[N]] }.toSeq
      for (node <- resultsTyped) addNode(node)
      val edge = new CodeGraph.Edge[E](label, argKeys.toSeq, resKeys.toSeq)
      addEdge(edge)
      val resultNodesUnwrapped = tuw(resultNodes)
      _cache += (label, argKeys) -> resultNodesUnwrapped
      resultNodesUnwrapped
    }

    /**
     * Helper macro to mimic function application to produce edges; wraps `withArgs`.
     */
    def apply(args: Any*): Final = macro CodeGraphMacros.edgeApplyImpl
  }

  /**
   * Add a node to this CodeGraph and return the key.
   * @param node The node to add.
   * @return The key of the newly added node.
   */
  def addNode(node: NodeName[N]): CodeGraph.NodeKey = {
    cg.nodes += node.key -> node.label
    node.key
  }

  /**
   * Add an edge to this CodeGraph and return the key.
   * @param edge The edge to add.
   * @return The key of the newly added edge.
   */
  def addEdge(edge: CodeGraph.Edge[E]): CodeGraph.EdgeKey = {
    val newKey = 1 + cg.edges.keys.reduceOption(_ max _).getOrElse(0: CodeGraph.EdgeKey)
    cg.edges(newKey) = edge
    newKey
  }

  /**
   * Associate a name with a given node.
   * @param node The node to name.
   * @param name The name.
   */
  def giveName[M <: N](node: NodeName[M], name: String): NodeName[M] = {
    _namedNodes += node.key -> name
    node
  }

  implicit class NodeOps[M <: N](node: NodeName[M]) {
    def named(name: String): NodeName[M] = giveName(node, name)
  }

  implicit class NodeTupleOps[M <: N](node: Tuple1[NodeName[M]]) {
    def named(name: String): NodeName[M] = giveName(node._1, name)
  }

  def getResult: CodeGraph[N, E] = CodeGraph[N, E](nodes, edges, inputs, outputs)

  /**
   * Returns a map from node keys to friendly names. Useful for debugging.
   */
  def nodeNames(implicit ntt: TypeTag[N]) : Map[NodeKey, String] = {
    val rm = scala.reflect.runtime.currentMirror
    rm.classSymbol(this.getClass).toType.members.map {
      case m: TermSymbol =>
        if (m.isMethod) { None }
        else {
          m.getter match {
            case getter: MethodSymbol =>
              val instanceMirror = rm.reflect(this)
              val value = instanceMirror.reflectMethod(getter).apply()
              val node = value match {
                case x: Tuple1[_] => Try(x._1.asInstanceOf[NodeName[N]]).toOption
                case x => Try(x.asInstanceOf[NodeName[N]]).toOption
              }
              node.map { nd => nd.key -> m.name.toString }
            case _ => None
          }
        }
      case _ => None
    }.collect { case Some(x) => x }.toMap ++ _namedNodes
  }

  private var _inputNodes: Product = <>
  private var _outputNodes: Product = <>
  private var _outputNodesSet: Boolean = false
  private var _cache: Map[(E, Seq[CodeGraph.NodeKey]), Any] = Map()
  private var _namedNodes: Map[CodeGraph.NodeKey, String] = Map()
  private var _inputs: Any = null
}