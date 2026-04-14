package fuse

import cats.effect.IO
import cats.implicits.*
import code.Grin
import core.Bindings.*
import core.Context.*
import core.*
import org.parboiled2.*
import parser.FuseParser
import parser.FuseParser.*
import parser.ParserErrorFormatter

import java.io.*
import java.nio.file.{Files, Path, Paths}
import scala.util.Either
import scala.util.Failure
import scala.util.Success
import code.Monomorphization

object Compiler {
  def run(
      command: Command,
      fileName: String,
      origin: InputStream,
      destination: OutputStream
  ): IO[Option[String]] =
    for {
      code <- IO.blocking(origin.readAllBytes.map(_.toChar).mkString)
      stdlibResult <- command.includeStdlib match {
        case true  => loadStdlib().map(_.map(Some(_)))
        case false => IO.pure(Right(None): Either[Error, Option[String]])
      }
      result = stdlibResult.flatMap(stdlib =>
        compile(command, code, fileName, stdlib)
      )
      value <- result match {
        case Right(compiledCode) =>
          val fullCode = command match {
            case BuildFile(_, _) =>
              val prelude = Grin.generatePrelude(compiledCode)
              prelude.isEmpty match {
                case true  => compiledCode
                case false => s"$prelude\n\n$compiledCode"
              }
            case _ => compiledCode
          }
          IO.blocking(destination.write(fullCode.getBytes)).map(_ => None)
        case Left(error) => IO(Some(error))
      }
    } yield value

  def loadStdlib(): IO[Either[Error, String]] = {
    val stdlibDir = Paths.get(sys.env.getOrElse("FUSE_STDLIB", "stdlib"))
    IO.blocking(Files.isDirectory(stdlibDir)).flatMap {
      case false =>
        IO.pure(Left(s"stdlib directory not found: $stdlibDir"))
      case true =>
        readStdlibFiles(stdlibDir).map { entries =>
          orderStdlibEntries(entries).map(_.map(_._2).mkString("\n"))
        }
    }
  }

  /** Read every `*.fuse` file under `dir` and return `(path, content)` pairs.
    */
  def readStdlibFiles(dir: Path): IO[List[(Path, String)]] = IO.blocking {
    import scala.jdk.CollectionConverters.*
    val stream = Files.newDirectoryStream(dir, "*.fuse")
    try
      stream.asScala.toList.map(p =>
        p -> new String(Files.readAllBytes(p)).trim
      )
    finally stream.close()
  }

  /** Order stdlib entries so every file's declared traits and types load before
    * any file that impls against them. Uses a real topological sort over parsed
    * decls rather than a substring grep for `trait ` — a file containing both a
    * trait and an unrelated impl no longer misroutes, and a dependency cycle
    * becomes a clean error instead of silent misordering.
    *
    * Ties within a dependency layer sort alphabetically for determinism.
    */
  def orderStdlibEntries(
      entries: List[(Path, String)]
  ): Either[Error, List[(Path, String)]] =
    entries
      .traverse { case (path, content) =>
        parse(content, path.getFileName.toString)
          .map(decls => (path, content, extractStdlibFileDeps(decls.toList)))
      }
      .flatMap(topologicalSortStdlib)

  case class StdlibFileDeps(provides: Set[String], requires: Set[String])

  /** Extract the names a single stdlib file defines (`provides`) and the trait
    * names it impls against (`requires`). Only trait-ish refs participate in
    * ordering — value-level references are resolved later by the type checker
    * once all binds are in context.
    */
  def extractStdlibFileDeps(decls: List[FDecl]): StdlibFileDeps = {
    val provides = decls.collect {
      case FTraitDecl(_, i, _, _)       => i.value
      case FVariantTypeDecl(_, i, _, _) => i.value
      case FRecordTypeDecl(_, i, _, _)  => i.value
      case FTupleTypeDecl(_, i, _, _)   => i.value
      case FTypeAlias(_, i, _, _)       => i.value
    }.toSet
    val requires = decls.collect {
      case FTraitInstance(_, traitId, _, _, _, _) =>
        traitId.value
    }.toSet
    StdlibFileDeps(provides, requires)
  }

  /** Kahn-style topological sort. Each pass emits every file whose `requires`
    * is satisfied by files already emitted; if no file is ready and the
    * frontier is non-empty, the graph has a cycle (error). A `requires` entry
    * that is not provided by any stdlib file is ignored — those are built-in or
    * user-level names and do not constrain stdlib ordering.
    */
  def topologicalSortStdlib(
      items: List[(Path, String, StdlibFileDeps)]
  ): Either[Error, List[(Path, String)]] = {
    val allProvides = items.flatMap { case (_, _, d) => d.provides }.toSet
    val filtered = items.map { case (p, c, d) =>
      (p, c, StdlibFileDeps(d.provides, d.requires.intersect(allProvides)))
    }
    @scala.annotation.tailrec
    def loop(
        remaining: List[(Path, String, StdlibFileDeps)],
        satisfied: Set[String],
        out: List[(Path, String)]
    ): Either[Error, List[(Path, String)]] = remaining match {
      case Nil => Right(out.reverse)
      case _   =>
        val (ready, blocked) = remaining.partition { case (_, _, d) =>
          d.requires.subsetOf(satisfied)
        }
        ready match {
          case Nil =>
            Left(
              s"stdlib dependency cycle or unresolved reference in: " +
                blocked.map(_._1.getFileName.toString).sorted.mkString(", ")
            )
          case _ =>
            val layer = ready.sortBy(_._1.getFileName.toString)
            val layerProvides =
              layer.flatMap { case (_, _, d) => d.provides }.toSet
            val newOut =
              layer.foldLeft(out) { case (acc, (p, c, _)) => (p, c) :: acc }
            loop(blocked, satisfied ++ layerProvides, newOut)
        }
    }
    loop(filtered, Set.empty, Nil)
  }

  def compile(
      command: Command,
      code: String,
      fileName: String,
      stdlibSource: Option[String] = None
  ): Either[Error, String] = for {
    v <- parse(code, fileName)
    c1 = BuiltIn.Binds.map(b => (b.i, NameBind))
    stdlibBinds <- stdlibSource match {
      case Some(source) =>
        for {
          stdlibDecls <- parse(source, "stdlib.fuse")
          // NOTE: The built-in functions are reversed in order to initialize
          // the context in the correct order.
          binds <- Desugar.run(stdlibDecls.toList, (c1.reverse, 0))
        } yield binds
      case None => Right(List.empty[Bind])
    }
    c2 = c1 ++ stdlibBinds.map(b => (b.i, NameBind))
    d <- Desugar.run(v.toList, (c2.reverse, 0))
    b2 = BuiltIn.Binds ++ stdlibBinds ++ d
    bindings <- TypeChecker.run(b2)
    out <- command match {
      case BuildFile(_, _) =>
        Right(Grin.generate(Monomorphization.replace(bindings)))
      case CheckFile(_, _) =>
        Representation.typeRepresentation(bindings).map(_.mkString("\n"))
    }
  } yield out

  def parse(code: String, fileName: String): Either[Error, Seq[FDecl]] = {
    val parser = new FuseParser(code, fileName)
    parser.Module.run() match {
      case Success(result)        => Right(result)
      case Failure(e: ParseError) =>
        Left(parser.formatError(e, new ParserErrorFormatter(fileName)))
      case Failure(e) => Left(e.toString)
    }
  }
}
