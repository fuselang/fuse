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

sealed trait Command
case class BuildFile(file: String) extends Command
case class CheckFile(file: String) extends Command

/** Build pipeline errors. */
sealed trait BuildError
case class FuseCompileError(error: Error) extends BuildError
case class GrinCompileError(exitCode: Int) extends BuildError

object Fuse
    extends CommandIOApp(
      name = "fuse",
      header = "Fuse is a tool for managing Fuse source code.",
      version = "0.1"
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
      fileOpts.map(BuildFile.apply)
    }

  val checkCommand: Opts[CheckFile] =
    Opts.subcommand("check", "Checks fuse source code file for type errors.") {
      fileOpts.map(CheckFile.apply)
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
    case FuseCompileError(e)    => e.toString
    case GrinCompileError(code) => s"GRIN compilation failed (exit code: $code)"
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
    EitherT(executeCommand(grinCommand).map { exitCode =>
      Either.cond(exitCode == 0, (), GrinCompileError(exitCode))
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
  ): IO[Option[Error]] = {
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

    acquireStreams.bracket { case (in, out) =>
      Compiler.run(command, origin.getPath, in, out)
    }(releaseStreams)
  }

  /** Execute a shell command and return the exit code. */
  def executeCommand(command: String): IO[Int] =
    IO.blocking {
      val process = new ProcessBuilder("sh", "-c", command).start()
      process.waitFor()
    }
}
