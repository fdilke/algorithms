package com.fdilke.utility

object Handy:

  def stackDepth(): Int =
    Thread.currentThread().getStackTrace.length
    
  def timeMsec[T](block: => T): (T, Long) =
    val start: Long = 
      System.currentTimeMillis()
    val result: T =
      block
    val finish: Long = 
      System.currentTimeMillis()
    (result, finish - start)
    

