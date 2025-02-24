object NoddyIterate extends App:
  val it: Iterable[Int] = (1 to 15).toSeq
  def funnyFunc(i: Int): Boolean =
    println(s"funnyFunc($i)")
    (i % 4) == 3
  val found = it.find { funnyFunc }
  println(s"found = $found")


object NoddySearch extends App:
    val seqValues: Iterable[Boolean] = Iterable(true, false)
    val explorations: AtomicReference[Seq[Seq[Boolean]]] = AtomicReference[Seq[Seq[Boolean]]](Seq.empty)
    class SearchNode(prefix: Seq[Boolean]) extends Node[SearchNode, Seq[Boolean]]:
      override def explore: NodeStatus =
        println("Exploring: " + prefix)
        explorations.set(explorations.get() :+ prefix)
        if (prefix.length == 3)
          NodeGood(prefix)
        else
          NodeContinue(
            seqValues.map { v => SearchNode(prefix :+ v) }
          )
      override def toString: String =
        "NODE<" + prefix + ">"
    val initialNode = SearchNode(Seq.empty)
    println("initial! explorations.get() = " + explorations.get)
    // initialNode.solve is Some(
    //   Seq(true, true, true)
    // )
    // initialNode.explore
    // println("explored: explorations.get() = " + explorations.get)
    // initialNode.allSolutions 
    // println("solved all: explorations.get.size() = " + explorations.get.size) // 15
    initialNode.solve
    println("solved one: explorations.get.size() = " + explorations.get.size) // #??
    println("" + explorations.get().size + " explorations")

  private def checkMinColoring(
     numColors: Int,
     coloring: Seq[Int],
     graph: Graph,
   ): Unit =
      for
        i <- coloring.indices
        j <- 0 until i
      do
        if graph.adjacencyTable(i)(j) && (coloring(i) == coloring(j)) then
          throw new IllegalArgumentException("adjacent vertices have same color")
      if coloring.distinct.size > numColors then
        throw new IllegalArgumentException("too many colors")
      if PartialColoring.fromColorsAndGraph(
        coloring,
        graph
      ).amalgamations.nonEmpty then
        throw new IllegalArgumentException("not a minimal coloring: an amalgamation is possible")
