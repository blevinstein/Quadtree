package com.blevinstein.qt.sim;

import com.blevinstein.qt.QuadTree;

object Operators {
  // Given two "partial QuadTrees" (i.e. QuadTree[Option[Any]]) returns a
  // QuadTree which is [true] in regions where they overlap ("collide").
  val collideOp:
      (QuadTree[Option[Any]], QuadTree[Option[Any]]) => QuadTree[Boolean] =
          QuadTree.merge((m1: Option[Any], m2: Option[Any]) =>
              (m1, m2) match {
                case (Some(_), Some(_)) => true
                case _ => false
              }) _
  // Collapses a QuadTree of Boolean, returns true if any region is true.
  val anyOp: QuadTree[Boolean] => Boolean =
      QuadTree.reduce((bs: List[Boolean]) => {
        bs.exists((b) => b)
      }) _
  // Collapses a QuadTree of Float, returns the average weighted by area.
  val avgOp: QuadTree[Float] => Float =
      QuadTree.reduce((xs: List[Float]) => xs.sum / xs.length) _
}
