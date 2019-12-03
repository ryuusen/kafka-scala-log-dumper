import java.io.File
import java.nio.file.Path

import scala.meta._
import scala.util.matching.Regex

object main {

  def recursiveListFiles(f: File, r: Regex): Array[File] = {
    val these = f.listFiles
    val good = these.filter(f => r.findFirstIn(f.getName).isDefined)
    good ++ these.filter(_.isDirectory).flatMap(recursiveListFiles(_, r))
  }

  def getBaseFunName(node: Tree): String = {
    node match {
      case nameNode: Term.Name => return nameNode.value
      case selectNode: Term.Select => return selectNode.name.value
      case applyType: Term.ApplyType => return "applyType"
      case applyType: Term.Apply => return "apply"
      case placeholderNode: Term.Placeholder => return "placeholder"
      case _ =>
        println(node.parent.get)
        println(s"WEIRD NODE: ${node.productPrefix} ${node}")
        return node.productPrefix
    }
  }

  def main(args: Array[String]): Unit = {
    val kafkaBasePath = ""
    val files = recursiveListFiles(new File(kafkaBasePath + "/kafka/core/src/main"), new Regex(""".*\.scala$"""))
    for (file <- files) {
      process(file.toPath)
    }
  }

  def process(path: Path): Unit = {
    val bytes = java.nio.file.Files.readAllBytes(path)
    val text = new String(bytes, "UTF-8")
    val input = Input.VirtualFile(path.toString, text)
    val exampleTree = input.parse[Source].get

    val logFuns = Array("trace", "debug", "info", "warn", "error", "fatal")

    var packageName = "";
    var className = "";

    var variables: Map[String, String] = Map()

    exampleTree.traverse {
      case node: Pkg =>
        packageName = node.ref.syntax
      case node: Defn.Object =>
        className = node.name.value
      case node: Defn.Class =>
        className = node.name.value
      case node: Defn.Trait =>
        className = node.name.value
      case node: Defn.Val =>

        def handlePat(p: Pat): Unit = {
          p match {
            case p: Pat.Var => variables += (p.name.toString() -> handleArg(node.rhs, variables))
            case p : Pat.Wildcard =>
            case p : Pat.Extract =>
              for (a <- p.args) {
                handlePat(a)
              }
            case p: Pat.Tuple =>
              for (a <- p.args) {
                handlePat(a)
              }
            case _ => println(s"${path}:${node.pos.startLine}"); println(p.productPrefix); println(p.syntax)
          }
        }

        for (pat <- node.pats) {
          if(node.rhs.isInstanceOf[Lit.String]
            || node.rhs.isInstanceOf[Term.Interpolate]
            || node.rhs.isInstanceOf[Term.ApplyInfix])
            handlePat(pat)
        }
      case node: Term.Apply =>
        if (node.args.size > 0 && logFuns.contains(getBaseFunName(node.fun))) {
          print(s"${path}:${node.pos.startLine}")
          print(s"\t${packageName}.${className}\t${node.fun}".replaceAll("\n", ""))


          for (arg <- node.args) {
            print(s"\t${handleArg(arg, variables)}".replaceAll("\n", ""))
          }
          println()
        }
    }

    def handleArg(node: Tree, variables: Map[String, String]): String = {
      node match {
        //case nameNode: Term.Name => variables.getOrElse(nameNode.value, s"{${nameNode.value}}")
        case nameNode: Term.Name => s"{${nameNode.value}}"
        case applyNode: Term.Apply =>
          if (getBaseFunName(applyNode.fun).equals("format")) {
            if (applyNode.fun.isInstanceOf[Term.Select]) {
              var baseString = handleArg(applyNode.fun.asInstanceOf[Term.Select].qual, variables)
              baseString = variables.getOrElse(baseString, baseString)
              baseString = baseString.replaceAll("%[-#+0,(\\s]*[0-9]*[.]*[0-9]*[a-mo-zA-Z]+", "%s")
              //println(baseString)
              //println(applyNode.args.size)
              try {
                baseString = baseString.format(applyNode.args.map(f => s"{${f.toString()}}"): _*)
                return baseString
              } catch {
                case x: Exception => return " CORRUPTED CALL "
              }
            }
          } else return s"{${applyNode.syntax}}"
          s"{${applyNode.productPrefix}}"
        case stringNode: Lit.String => stringNode.value
        case infixNode: Term.ApplyInfix => handleArg(infixNode.lhs, variables) + handleArg(infixNode.args(0), variables)
        case interpolateNode: Term.Interpolate =>
          val strBuilder = new StringBuilder
          val it1 = interpolateNode.parts.iterator
          val it2 = interpolateNode.args.iterator

          while (it1.hasNext && it2.hasNext) {
            strBuilder.append(it1.next())
            strBuilder.append(s"{${it2.next()}}")
          }

          while (it1.hasNext) {
            strBuilder.append(it1.next())
          }

          while (it2.hasNext) {
            strBuilder.append(s"{${it2.next()}}")
          }

          strBuilder.toString()
        case _ => s"{${node.syntax}}"
      }
    }
  }
}
