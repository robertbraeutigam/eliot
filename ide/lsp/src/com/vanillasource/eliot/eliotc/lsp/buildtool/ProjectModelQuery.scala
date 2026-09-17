package com.vanillasource.eliot.eliotc.lsp.buildtool

import cats.effect.{IO, Resource}

import java.io.IOException
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/** Asks a project's build tool for its [[ProjectModel]]: `./eliotw --project-model`, run in the project's directory.
  *
  * The language server never reads `eliot.pkg` itself. Resolving it means MVS over the whole closure plus the cache's
  * checkout paths, and a second resolver here would drift from the build's — the editor would then report diagnostics
  * against roots no build compiles. So the server runs the project's own pinned wrapper, which runs the pinned launcher,
  * and reads what it prints. The query does not fetch: a package whose dependencies were never fetched comes back
  * unresolved, and building it once fixes that.
  *
  * The wrapper's output goes to temporary files rather than pipes, so a chatty or stuck process can neither fill a pipe
  * nor outlive [[timeout]].
  */
object ProjectModelQuery {

  /** The committed wrapper a project is run through. */
  val wrapperName: String = "eliotw"

  /** The descriptor that makes a directory a project. */
  val descriptorName: String = "eliot.pkg"

  /** The build tool's option asking for the model. */
  val modelOption: String = "--project-model"

  /** Long enough for a first run, which downloads the pinned launcher before answering. */
  val timeout: FiniteDuration = 2.minutes

  /** Whether `directory` is a build-tool project: it has both a descriptor and the wrapper that reads it. */
  def isProject(directory: Path): Boolean =
    Files.isRegularFile(directory.resolve(descriptorName)) && Files.isRegularFile(directory.resolve(wrapperName))

  /** The model of the project at `directory`, or why the tool gave none. */
  def query(directory: Path): IO[Either[String, ProjectModel]] =
    run(command(directory), directory, timeout).map(_.flatMap(ProjectModel.parse))

  /** The command line asking `directory`'s wrapper for the model; through `sh` when the wrapper lost its executable bit
    * (a checkout on a filesystem that does not keep one).
    */
  def command(directory: Path): Seq[String] = {
    val wrapper = directory.resolve(wrapperName)
    if (Files.isExecutable(wrapper)) Seq(wrapper.toString, modelOption) else Seq("sh", wrapper.toString, modelOption)
  }

  /** Run `command` in `directory` and answer its standard output, or a message naming the command and what went wrong:
    * it would not start, it did not finish within `limit`, or it exited non-zero (with what it printed).
    */
  def run(command: Seq[String], directory: Path, limit: FiniteDuration): IO[Either[String, String]] =
    outputFiles.use { (output, errors) =>
      IO.blocking {
        val process = new ProcessBuilder(command.asJava)
          .directory(directory.toFile)
          .redirectOutput(output.toFile)
          .redirectError(errors.toFile)
          .start()
        process.getOutputStream.close()
        if (!process.waitFor(limit.toMillis, TimeUnit.MILLISECONDS)) {
          process.destroyForcibly()
          Left(s"'${command.mkString(" ")}' did not answer within $limit")
        } else if (process.exitValue() != 0) {
          Left(s"'${command.mkString(" ")}' exited with ${process.exitValue()}: ${printed(errors, output)}")
        } else {
          Right(Files.readString(output))
        }
      }.handleError {
        case error: IOException => Left(s"'${command.mkString(" ")}' could not be run: ${error.getMessage}")
        case error              => Left(s"'${command.mkString(" ")}' failed: $error")
      }
    }

  /** What a failed run said: its error stream, or — a launcher that predates the option prints its usage — the first
    * lines of its output.
    */
  private def printed(errors: Path, output: Path): String = {
    val said = Files.readString(errors).trim
    if (said.nonEmpty) said else Files.readAllLines(output).asScala.take(4).mkString(" / ")
  }

  private def outputFiles: Resource[IO, (Path, Path)] =
    Resource.make(
      IO.blocking((Files.createTempFile("eliot-model", ".out"), Files.createTempFile("eliot-model", ".err")))
    )((output, errors) => IO.blocking { Files.deleteIfExists(output); Files.deleteIfExists(errors) }.void)
}
