package com.vanillasource.eliot.eliotc.main

import cats.{Functor, Monad}
import cats.data.OptionT
import cats.effect.{Async, Sync}
import cats.effect.std.Console
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.{Logging, User}
import com.vanillasource.eliot.eliotc.plugin.Configuration.namedKey
import com.vanillasource.eliot.eliotc.plugin.{CompilerPlugin, Configuration}
import com.vanillasource.eliot.eliotc.processor.NullProcessor
import com.vanillasource.eliot.eliotc.util.CatsOps.*
import scopt.{DefaultOEffectSetup, OParser, OParserBuilder}

import java.nio.file.Path
import java.util.ServiceLoader
import scala.jdk.CollectionConverters.*

object Compiler extends Logging {
  val targetPathKey: Configuration.Key[Path] = namedKey[Path]("targetPath")

  def runCompiler[F[_]: Async: Console](args: List[String]): OptionT[F, Unit] = {
    type OPTF[T] = OptionT[F, T]

    for {
      plugins          <- allLayers[OPTF]()
      // Run command line parsing with all options from all layers
      configuration    <- parserCommandLine(args, plugins.map(_.commandLineParser()))
      // Select active plugins
      targetPlugin     <- OptionT
                            .fromOption[F](plugins.find(_.isSelectedBy(configuration)))
                            .orElseF(User.compilerGlobalError("No target plugin selected.").as(None))
      _                <- debug(s"Selected target plugin: ${targetPlugin.getClass.getSimpleName}")
      activatedPlugins  = collectActivatedPlugins(targetPlugin, configuration, plugins)
      _                <- debug(s"Selected active plugins: ${activatedPlugins.map(_.getClass.getSimpleName).mkString(", ")}")
      // Give plugins a chance to configure each other
      newConfiguration <- activatedPlugins.traverse_(_.configure()).runS(configuration).liftOptionT
      // Collect all processors
      processor        <- activatedPlugins.traverse_(_.initialize(newConfiguration)).runS(NullProcessor()).liftOptionT
      // Run fact generator / compiler
      _                <- debug("Compiler starting...")
      generator        <- FactGenerator.create[F](processor).liftOptionT
      _                <- targetPlugin.run[F](newConfiguration, generator).liftOptionT
      _                <- debug("Compiler exiting normally.")
    } yield ()
  }

  private def collectActivatedPlugins(
      initialPlugin: CompilerPlugin,
      configuration: Configuration,
      all: Seq[CompilerPlugin]
  ): Seq[CompilerPlugin] =
    LazyList.unfold(Seq(initialPlugin))(ps =>
      ps.headOption.map(plugin => plugin -> (ps.tail ++ resolvePlugins(plugin.pluginDependencies(configuration), all)))
    )

  private def resolvePlugins(classes: Seq[Class[_ <: CompilerPlugin]], all: Seq[CompilerPlugin]): Seq[CompilerPlugin] =
    all.filter(p => classes.contains(p.getClass))

  private def allLayers[F[_]: Sync](): F[Seq[CompilerPlugin]] = Sync[F].blocking {
    ServiceLoader
      .load(classOf[CompilerPlugin])
      .iterator()
      .asScala
      .toSeq
  }

  private def baseOptions() = {
    val cmdLineBuilder: OParserBuilder[Configuration] = OParser.builder[Configuration]
    import cmdLineBuilder.*

    OParser.sequence(
      programName("eliotc"),
      help("help").text("prints this help text"),
      opt[Path]('o', "output-dir")
        .text("the directory any output should be written")
        .action((path, config) => config.set(targetPathKey, path))
    )
  }

  private def parserCommandLine[F[_]: Sync](
      args: Seq[String],
      options: Seq[OParser[_, Configuration]]
  ): OptionT[F, Configuration] = Sync[F].blocking {
    val (result, effects) = OParser.runParser(
      OParser.sequence(baseOptions(), options: _*),
      args,
      Configuration().set(targetPathKey, Path.of("target"))
    )

    var terminateState: Option[Unit] = Some(())

    OParser.runEffects(
      effects,
      new DefaultOEffectSetup {
        override def terminate(exitState: Either[String, Unit]): Unit = {
          terminateState = None
        }
      }
    )

    terminateState.flatMap(_ => result)
  }.toOptionT

}
