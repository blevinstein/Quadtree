package com.blevinstein.util

import java.io.IOException
import java.nio.file._

class Config(pathPrefix: String = "config") {
  if (!Files.exists(Paths.get(pathPrefix))) {
    Files.createDirectory(Paths.get(pathPrefix))
  }

  def get(filename: String): Option[String] = {
    try {
      Some(new String(Files.readAllBytes(Paths.get(pathPrefix, filename))))
    } catch {
      case e: IOException => None
    }
  }

  def set(filename: String, value: String): Unit = {
    Files.write(Paths.get(pathPrefix, filename), value.getBytes)
  }
}
