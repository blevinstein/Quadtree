package com.blevinstein.qt.sim

import com.blevinstein.qt.QuadTree
import com.blevinstein.geom.Point

import java.awt.Color
import java.awt.image.BufferedImage

object ImageHelper {
  val colorToMaterial: QuadTree[Color] => QuadTree[Option[Material]] =
      QuadTree.transform((color: Color) =>
          if (color.getAlpha() > 0) {
            Some(new Material(color, color.getAlpha() / 255f))
          } else {
            None
          })

  def createColor(argb: Int): Color = {
    new Color(
        argb >> 16 & 0xFF,
        argb >> 8 & 0xFF,
        argb & 0xFF,
        argb >> 24 & 0xFF)
  }

  def createTreeFromImage(image: BufferedImage): QuadTree[Option[Material]] = {
    require(image.getWidth() == image.getHeight(), "image is not square")
    require((image.getWidth() & (image.getWidth() - 1)) == 0,
        "image size is not a power of 2")

    val depth: Int = (math.log(image.getWidth()) / math.log(2)).round.toInt

    colorToMaterial(QuadTree.approx(
        depth,
        (point: Point) =>
            createColor(image.getRGB(
                (point.x * image.getWidth()).floor.toInt,
                ((1 - point.y) * image.getHeight()).floor.toInt))))
  }
}
