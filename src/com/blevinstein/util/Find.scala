package com.blevinstein.util

object Find {
  def findMap[X,Y](iter: Iterable[X], p: X => Option[Y]): Option[Y] =
      iter.foldLeft[Option[Y]](None)((accum: Option[Y], elem: X) =>
          accum match {
            case Some(value) => Some(value)
            case None => p(elem)
          })
}
