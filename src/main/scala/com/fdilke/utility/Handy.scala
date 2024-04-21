package com.fdilke.utility

object Handy:

  def stackDepth(): Int =
    Thread.currentThread().getStackTrace.length

