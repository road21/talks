package dh.lang

import dh.lang.core.RedYellowGreen

object prelude extends boolean, control, date, datetime, decimal, ryg

trait boolean:
  def not(arg: Boolean): Boolean = !arg

  extension (arg: Boolean)
    def and(other: => Boolean): Boolean = arg && other
    def or(other: => Boolean): Boolean  = arg || other

trait ryg:
  extension (r: RedYellowGreen)
    def req: RedYellowGreen = r

trait control extends iifs

