package com.github.sbroadhead.codegraph.dot

/**
 * Builders for DOT graph strings.
 */
object Dot {
  type Builder[T] = (T => Unit) => Unit

  def runBuilder[T](builder: Builder[T]): Seq[T] = {
    var results: List[T] = List()
    builder(x => results = results :+ x)
    results.toSeq
  }

  def escape(x: String) = x.replaceAll("\\n", "\\\\n")

  def digraph(id: String)(body: Builder[String]): String =
    s"digraph \042$id\042 { ${runBuilder(body).mkString(";\n")} }"

  def edge(parts: Seq[String])(body: Builder[(String, String)]): String =
    s"${parts.mkString("->")} [\n${runBuilder(body).map { case (a, b) => s"$a=\042${escape(b)}\042" }.mkString("\n")}]"

  def node(id: String)(body: Builder[(String, String)]): String =
    s"$id [\n${runBuilder(body).map { case (a, b) => s"$a=\042${escape(b)}\042" }.mkString("\n")}]"

  def subgraph(id: String)(body: Builder[String]): String =
    s"subgraph \042$id\042 { ${runBuilder(body).mkString(";\n")} }"
}
