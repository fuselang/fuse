package fuse

import cats.data.EitherT
import cats.effect.{IO, ExitCode}
import cats.implicits.*
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import core.Context.Error

import java.io.{
  File,
  FileInputStream,
  FileOutputStream,
  InputStream,
  OutputStream
}
import java.nio.file.{Files, Path, Paths}
import parser.Info.UnknownInfo

sealed trait Command {
  val includeStdlib: Boolean
}
case class BuildFile(file: String, includeStdlib: Boolean = true)
    extends Command
case class CheckFile(file: String, includeStdlib: Boolean = true)
    extends Command

/** Build pipeline errors. */
sealed trait BuildError
case class FuseCompileError(error: Error) extends BuildError
case class GrinCompileError(exitCode: Int, output: String) extends BuildError

object Fuse
    extends CommandIOApp(
      name = "fuse",
      header = "Fuse is a tool for managing Fuse source code.",
      version = "0.3.4" // x-release-please-version
    ) {

  // File extensions (public for test access)
  val FuseFileExtension = "fuse"
  val FuseGrinExtension = "grin"
  val FuseOutputExtension = "out"

  /** Paths for build artifacts derived from source file. */
  case class BuildPaths(
      source: Path,
      grin: Path,
      output: Path
  )

  object BuildPaths {
    def fromSource(sourcePath: String): BuildPaths = {
      val base = sourcePath.stripSuffix(FuseFileExtension)
      BuildPaths(
        source = Paths.get(sourcePath),
        grin = Paths.get(base + FuseGrinExtension),
        output = Paths.get(base + FuseOutputExtension)
      )
    }
  }

  val fileOpts: Opts[String] = Opts.argument[String](metavar = "file")

  val buildCommand: Opts[BuildFile] =
    Opts.subcommand("build", "Compile fuse source code file.") {
      fileOpts.map(f => BuildFile(f))
    }

  val checkCommand: Opts[CheckFile] =
    Opts.subcommand("check", "Checks fuse source code file for type errors.") {
      fileOpts.map(f => CheckFile(f))
    }

  val compilerCommand: Opts[Command] = buildCommand `orElse` checkCommand

  override def main: Opts[IO[ExitCode]] =
    compilerCommand.map {
      case c: BuildFile => build(c)
      case c: CheckFile => check(c)
    }

  /** Build pipeline using EitherT for short-circuit error handling. */
  def build(command: BuildFile): IO[ExitCode] = {
    val paths = BuildPaths.fromSource(command.file)

    val pipeline: EitherT[IO, BuildError, Unit] = for {
      // Phase 1: Fuse -> GRIN
      _ <- compileFuseToGrin(command, paths)
      // Phase 2: GRIN -> Executable with native GC
      _ <- compileGrinWithGC(paths)
      // Phase 3: Cleanup GRIN intermediate file
      _ <- EitherT.right[BuildError](
        cleanupIntermediateFiles(List(paths.grin))
      )
    } yield ()

    pipeline.value.flatMap {
      case Right(_)  => IO.pure(ExitCode.Success)
      case Left(err) =>
        IO.println(formatBuildError(err)).as(ExitCode.Error)
    }
  }

  /** Format build error for display. */
  def formatBuildError(error: BuildError): String = error match {
    case FuseCompileError(e)            => e.toString
    case GrinCompileError(code, output) =>
      val details = output.trim match {
        case "" => ""
        case s  => s"\n$s"
      }
      s"GRIN compilation failed (exit code: $code)$details"
  }

  /** Phase 1: Compile Fuse source to GRIN. */
  def compileFuseToGrin(
      command: BuildFile,
      paths: BuildPaths
  ): EitherT[IO, BuildError, Unit] =
    EitherT(
      compileFile(command, paths.source.toFile, paths.grin.toFile).map {
        case Some(error) => Left(FuseCompileError(error))
        case None        => Right(())
      }
    )

  /** Phase 2: Compile GRIN to executable with native Boehm GC support. */
  def compileGrinWithGC(
      paths: BuildPaths
  ): EitherT[IO, BuildError, Unit] = {
    val runtime = sys.env.getOrElse("FUSE_RUNTIME", "grin")
    val grinCommand = Seq(
      "grin",
      paths.grin.toString,
      "--no-prelude",
      "--gc=boehm",
      "--optimize",
      "-o",
      paths.output.toString,
      "-q",
      "-C",
      s"$runtime/runtime.c",
      "-C",
      s"$runtime/prim_ops.c"
    ).mkString(" ")
    EitherT(executeCommand(grinCommand).map { case (exitCode, output) =>
      Either.cond(exitCode == 0, (), GrinCompileError(exitCode, output))
    })
  }

  /** Clean up intermediate files (best effort, ignores errors). */
  def cleanupIntermediateFiles(files: List[Path]): IO[Unit] =
    files.traverse_ { path =>
      IO.blocking(Files.deleteIfExists(path)).void
    }

  /** Type check command. */
  def check(command: CheckFile): IO[ExitCode] = {
    val program = new File(command.file)
    val output = new File(
      command.file.stripSuffix(FuseFileExtension) + FuseOutputExtension
    )
    compileFile(command, program, output).flatMap {
      case Some(error) => IO.println(error).as(ExitCode.Error)
      case None        => IO.pure(ExitCode.Success)
    }
  }

  /** Compile a Fuse file using bracket for resource safety. */
  def compileFile(
      command: Command,
      origin: File,
      destination: File
  ): IO[Option[Error]] =
    validateSourceFile(origin).flatMap {
      case Some(error) => IO.pure(Some(error))
      case None        =>
        val acquireStreams: IO[(InputStream, OutputStream)] =
          (
            IO(new FileInputStream(origin)),
            IO(new FileOutputStream(destination))
          ).tupled
        val releaseStreams: ((InputStream, OutputStream)) => IO[Unit] = {
          case (in, out) =>
            (IO(in.close()), IO(out.close())).tupled
              .handleErrorWith(_ => IO.unit)
              .void
        }
        acquireStreams
          .bracket { case (in, out) =>
            Compiler.run(command, origin.getPath, in, out)
          }(releaseStreams)
          .handleErrorWith { e =>
            IO.pure(
              Some(
                Utils.consoleError(
                  s"I/O error: ${e.getMessage}",
                  UnknownInfo
                )
              )
            )
          }
    }

  /** Validate that a source file exists, is a regular file, and is readable. */
  def validateSourceFile(file: File): IO[Option[Error]] = IO {
    (file.exists, file.isFile, file.canRead) match {
      case (false, _, _) =>
        Some(
          Utils.consoleError(
            s"file not found: ${file.getPath}",
            UnknownInfo
          )
        )
      case (_, false, _) =>
        Some(
          Utils.consoleError(
            s"not a file: ${file.getPath}",
            UnknownInfo
          )
        )
      case (_, _, false) =>
        Some(
          Utils.consoleError(
            s"file not readable: ${file.getPath}",
            UnknownInfo
          )
        )
      case _ => None
    }
  }

  /** Execute a shell command and return the exit code with combined output. */
  def executeCommand(command: String): IO[(Int, String)] =
    IO.blocking {
      val process = new ProcessBuilder("sh", "-c", command)
        .redirectErrorStream(true)
        .start()
      val output = new String(process.getInputStream.readAllBytes())
      val exitCode = process.waitFor()
      (exitCode, output)
    }
}
