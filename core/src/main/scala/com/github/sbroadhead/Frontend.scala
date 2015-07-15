package com.github.sbroadhead

import org.reflections._
import org.reflections.scanners.SubTypesScanner
import org.reflections.util._
import shapeless.Nat._0

/**
 * Application front-end
 */
object Frontend {
  def main(args: Array[String]): Unit = {
    def isOpt(x: String) = x.head == '-'
    def parseArg(cur: Map[String, Any], args: List[String]): Map[String, Any] = {
      args match {
        case Nil => cur
        case "--list-demos" :: tail => parseArg(cur ++ Map("list-demos" -> ()), tail)
        case "--demo" :: name :: tail => cur ++ Map("demo" -> (name, tail))
        case x :: tail => {
          println(s"Unknown argument: $x")
          sys.exit(1)
        }
      }
    }

    val argMap = parseArg(Map(), args.toList)

    if (argMap.contains("list-demos")) {
      showDemos()
      sys.exit(0)
    } else if (argMap.contains("demo")) {
      val (demo, demoArgs) = argMap.getOrElse("demo", sys.error("impossible")).asInstanceOf[(String, Seq[String])]
      runDemo(demo, demoArgs)
      sys.exit(0)
    }
  }

  def getDemos: Set[Class[_ <: Demo]] = {
    import scala.collection.JavaConversions._
    val refl = new Reflections(ClasspathHelper.forPackage("com.github.sbroadhead"), new SubTypesScanner())
    val iter = for (s <- refl.getSubTypesOf(classOf[Demo])) yield s
    iter.toSet
  }

  def showDemos(): Unit = {
    println("Available demos:\n")
    for (demo <- getDemos) {
      println(s" * ${demo.getSimpleName}")
    }
  }

  def runDemo(name: String, args: Seq[String]): Unit = {
    val demo = getDemos.find(x => x.getSimpleName == name)
    if (demo.isDefined) {
      demo.get.newInstance.run(args)
    }
  }

  class DummyDemo extends Demo {
    override def run(args: Seq[String]): Unit = println(s"Hello ${args.headOption.getOrElse("(none)")}.")
  }
}
