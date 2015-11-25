package com.blevinstein.qt.sim;

import com.blevinstein.qt.QuadTree;

object Operators {
  val collideOp:
      (QuadTree[Option[Any]], QuadTree[Option[Any]]) => QuadTree[Boolean]
      = QuadTree.merge((m1: Option[Any], m2: Option[Any]) =>
          (m1, m2) match {
            case (Some(_), Some(_)) => true
            case _ => false
          }) _
  val anyOp: QuadTree[Boolean] => Boolean =
      QuadTree.reduce((bs: List[Boolean]) => {
        bs.exists((b) => b)
      }) _
}
