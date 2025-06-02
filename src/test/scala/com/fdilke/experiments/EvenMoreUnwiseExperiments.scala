package com.fdilke.experiments

object EvenMoreUnwiseExperiments extends App:
  //  def noddy4(
  //              block: [E] => Sets.Dot[E] ?=> (monoid: Sets.Monoid[E]) ?=> monoid.Action[Int] ?=> Unit
  //            ): Unit = ()
  //  noddy4 {
  //    [E] => (_ : Sets.Dot[E]) ?=> (monoid: Sets.Monoid[E]) ?=> (action: monoid.Action[Int]) ?=>
  //      ()
  //  }
  def noddy4(
              block: [E] => List[E] ?=> Int
            ): Unit =
    given List[String] =
      List("xx", "yy", "zz")
    val measure = block[String]
    println(s"measure = $measure")

  noddy4:
    [E] => (list: List[E]) ?=>
      list.size

  def withGroupOfDisunity[M : Set, RESULT](
   block: [G] => Set[G] ?=> List[G] ?=> (embed: G => M) => RESULT
  )(
   implicit monoid: Array[M]
  ): RESULT =
    val theSet: Set[M] = summon
    given List[M] = theSet.toList
    val id: M => M = identity
    block[M](id)

  val groupMessage: String = {
    given Array[Int] = Array(3,2,1)
    given Set[Int] = Set(4, 5)
    withGroupOfDisunity[Int, String]:
      [G] => (set: Set[G]) ?=> (list: List[G]) ?=> (embed: G => Int) =>
      s"set: ${set.size} and list: ${ list.size }"
  }
  println(s"groupMessage = $groupMessage")

  case class MrSnoid[E](e: E):
    case class LarkClark[X](x: X)
  
  def withEndorphinMonoid[X : Set, RESULT](
    block: [E] => Set[E] ?=> (snoid: MrSnoid[E]) ?=> snoid.LarkClark[X] => RESULT
  ): RESULT =
    given Set[Int] = Set(1,2,3)
    implicit val snoid: MrSnoid[Int] = MrSnoid(0)
    block[Int]:
      snoid.LarkClark:
        summon[Set[X]].head
  
  val endMessage: String = {
    given Set[Boolean] = Set(true, false)
    withEndorphinMonoid[Boolean, String]:
      [E] => (_: Set[E]) ?=> (snoid: MrSnoid[E]) ?=> (lark: snoid.LarkClark[Boolean]) =>
        s"heeee"
  }

