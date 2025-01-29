package com.fdilke.algebra.permutation

import GroupSugar._

object GroupVerifier:
  def checkGroupOf[T](
    group: Group[T]
  ): Unit =
    given Group[T] = group
    def containsTheUnit(): Unit =
      if !group.elements.contains(group.unit) then
        throw new IllegalArgumentException("does not contain the unit")

    def closedUnderMultiplication(): Unit =
      if !
        group.elements.forall: x =>
          group.elements.forall: y =>
            group.elements.contains(x * y)
        then
          throw new IllegalArgumentException("is not closed under multiplication")

    def closedUnderInversion(): Unit =
      if !
        group.elements.forall: x =>
          group.elements.contains(~x)
      then
        throw new IllegalArgumentException("is not closed under inversion")

    def obeyTheUnitLaw(): Unit =
      val unit: T =
        group.unit
      if !
        group.elements.forall: x =>
          (x * unit == x) &&
            (unit * x == x)
      then
        throw new IllegalArgumentException("does not obey the unit law")

    def associative(): Unit =
      if !
        group.elements.forall: x =>
          group.elements.forall: y =>
            group.elements.forall: z =>
              (x * y) * z == x * (y * z)
      then
        throw new IllegalArgumentException("is not associative")

    def existInverses(): Unit =
      if !
        group.elements.forall: x =>
          (x * ~x == group.unit) &&
            (~x * x == group.unit)
      then
        throw new IllegalArgumentException("does not have inverses")

    containsTheUnit()
    closedUnderMultiplication()
    closedUnderInversion()
    obeyTheUnitLaw()
    associative()
    existInverses()

