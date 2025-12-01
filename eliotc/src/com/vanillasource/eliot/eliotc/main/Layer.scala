package com.vanillasource.eliot.eliotc.main

import cats.effect.IO

import java.util.ServiceLoader
import scala.jdk.CollectionConverters.*

trait Layer {}

object Layer {
  def allLayers(): IO[Seq[Layer]] = IO.blocking {
    ServiceLoader
      .load(classOf[Layer])
      .iterator()
      .asScala
      .toSeq
  }
}
