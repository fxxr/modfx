package modfx

import better.files._
import File._
import java.io.{File => JFile}

import modfx.javamodel.JavaModelSync
import modfx.model.Obj

/**
  * Produces boilerplate code from mdx files
  */
object ModFx extends App {
  args.toList match {
    case projectRoot :: Nil => sync(projectRoot)
    case _ => println("Use ModFX <modulename>")
  }

  def sync(projectRootPath: String): Unit = {
    val projectRoot = projectRootPath.toFile
    val modelFiles = projectRoot.glob("*.mdx").toSeq
    modelFiles match {
      case Nil => println("Error: " + modelsNotFound(projectRoot))
      case _ =>
        parse(modelFiles).foreach { case (moduleRoot, parseResult) =>
          parseResult match {
            case Left(err) => println("Parse error: " + err)
            case Right(objs) => new JavaModelSync(moduleRoot, objs).run()
          }
        }
    }
  }

  private def modelsNotFound(projectRoot: File) = "Model files not found in " + projectRoot.pathAsString

  type ParseResult = Either[String, Seq[Obj]]
  type ParseResults = Seq[(File, ParseResult)]

  private def parse(modelFiles: Seq[File]): ParseResults = modelFiles map (modelFile => (modelFile.parent, parse(modelFile)))

  private def parse(modelFile: File): ParseResult = MdxParser.parse(modelFile.contentAsString, modelFile.parent.name)
}