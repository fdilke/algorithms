package com.fdilke.utility

import scala.collection.mutable

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

//  def memoize[I, O](f: I => O): I => O = 
//    new mutable.HashMap[I, O]().withDefault(f)
//    
//  def memoize[I, J, O](f: (I, J) => O): (I, J) => O =
//    Function.untupled:
//      memoize[(I, J), O]:
//        Function.tupled(f)
        
//    new mutable.HashMap[I, O]().withDefault:
//      (key: I) => getOrElseUpdate(key, f(key))
//  new mutable.HashMap[I, O]():
//      override def apply(key: I) = getOrElseUpdate(key, f(key))
