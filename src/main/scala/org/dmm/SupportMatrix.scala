package org.dmm

class SupportMatrix(val m: Int, val n: Int) {
  val matrix: Array[Array[Int]] = Array.ofDim[Int](n + m, n + 1)

  def apply(i: Int) = matrix(i)
  def minInRow(r: Int): Int = this(r).map(Math.abs).zipWithIndex.slice(r, n).filter(nonZero).min._2
  def countNonZeroInRow(r: Int): Int = this(r).slice(r, n + 1).count(nonZero)
  def changeColumn(r: Int, c: Int, min: Int): Unit = {
    for (i <- r until m + n) {
      val swap = this(i)(c)
      this(i)(c) = this(i)(min)
      this(i)(min) = swap
    }
  }

  private def nonZero(value: Int) = value != 0
  private def nonZero(value: (Int, Int)) = value._1 != 0

  def print(k: Int): Unit = {
    println("result:")
    println(k)
    for (i <- m until n + m) {
      var line = this(i)(n).toString
      for (j <- n - k until n) line += s" ${this(i)(j)}"
      println(line)
    }
  }
  def last(r: Int): Int = this(r)(n)

  override def toString: String = matrix.foldLeft("") { case (prev, next) => prev + next.foldLeft("") { case (line, element) => line + element.toString + " " } + "\n" }
}

object SupportMatrix {
  def fromFile(fileName: String): SupportMatrix = {
    val b = new FileBuffer(fileName)
    val a = new SupportMatrix(b.next(), b.next())

    for (i <- 0 until a.m) {
      for (j <- 0 until a.n) a(i)(j) = b.next()
      a(i)(a.n) = -b.next()
    }
    Range(a.m, a.n + a.m).zipWithIndex.foreach { case (i, j) => a(i)(j) = 1 }
    println(a)
    a
  }
}