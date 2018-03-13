package org.dmm

import java.io.File

object Dmm {
  object NoAnswerException extends Throwable

  var step = 1
  def main(args: Array[String]): Unit = {
    val tests = if (args.length > 1) args(0) match {
      case "-dir" =>
        args.slice(1, args.length).foldLeft(Array.empty[String]) { case(res, next) => val d = new File(next)
          if (d.exists && d.isDirectory) {
            res ++ d.listFiles.filter(_.isDirectory).foldLeft(Array.empty[String]) { case (r, n) =>
              if (n.isDirectory) r ++ n.listFiles().filter(_.isFile).filter(_.getName == "stdin.txt").map(_.getAbsolutePath)
              else r
            }
          }
          else res
        }
      case _ => args
    } else args
    tests.foreach(computeFile)
  }

  def step(a: SupportMatrix, r: Int, min: Int): Unit = {
    for (i <- r until a.n + 1; if i != min) {
      val k = a(r)(i) / a(r)(min)
      for (j <- r until a.m + a.n) a(j)(i) -= k * a(j)(min)
    }

//    println(s"step: $step")
//    println(a)
    step += 1
  }

  def computeFile(fileName: String): Unit = { println(fileName)
    step = 1
    val a = SupportMatrix.fromFile(fileName)
    var (min, r, c) = (0, 0, 0)

    try {
      while (r < a.m && c < a.n) {
        a.countNonZeroInRow(r) match {
          case 0 => r += 1
          case 1 if a.last(r) == 0 =>
            if (c != min) a.changeColumn(r, c, min)
            r += 1; c += 1
          case 2 if a.last(r) != 0 && r < a.m && a.last(r) % a(r)(a.minInRow(r)) != 0 => throw NoAnswerException
          case _ => min = a.minInRow(r)
            if (min == a.n) throw NoAnswerException
            step(a, r, min)
        }
      }
      if (a.n < a.m) for (i <- r until a.m; if a(i)(a.n) != 0) throw NoAnswerException
      a.print(a.n - c)
    } catch {
      case NoAnswerException => println("no")
      case e: UnsupportedOperationException => println("can't find min in row (zeros)")
    }
  }
}