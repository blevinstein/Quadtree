package com.blevinstein.qt.sim;

import com.blevinstein.qt.QuadTree;

object Operators {
  val addOp:
      (QuadTree[Option[Material]], QuadTree[Option[Material]])
      => QuadTree[Option[Material]] =
          QuadTree.merge((o1: Option[Material], o2: Option[Material]) =>
              (o1, o2) match {
                  case (_, Some(m2)) => Some(m2)
                  case (Some(m1), _) => Some(m1)
                  case (None, None) => None
              }) _
  // Collapses a QuadTree of Boolean, returns true if any region is true.
  val anyOp: QuadTree[Boolean] => Boolean =
      QuadTree.reduce((bs: List[Boolean]) => {
        bs.exists((b) => b)
      }) _
  // Collapses a QuadTree of Float, returns the average weighted by area.
  val avgOp: QuadTree[Float] => Float =
      QuadTree.reduce((xs: List[Float]) => xs.sum / xs.length) _
  // Given two "partial QuadTrees" (i.e. QuadTree[Option[Any]]) returns a
  // QuadTree which is [true] in regions where they overlap ("collide").
  val collideOp:
      (QuadTree[Option[Any]], QuadTree[Option[Any]]) => QuadTree[Boolean] =
          QuadTree.merge((o1: Option[Any], o2: Option[Any]) =>
              (o1, o2) match {
                case (Some(_), Some(_)) => true
                case _ => false
              }) _
}
