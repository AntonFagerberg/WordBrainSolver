import dk.dren.hunspell.Hunspell

import scala.io.StdIn

class WordBrainHelper() {
  val dict = Hunspell.getInstance().getDictionary("/Users/anton/Downloads/SV_se/SV_se")

  def recInit(w: List[List[Char]], s: Int): Unit = {
    for {
      (l, i) <- w.zipWithIndex
      (c, j) <- l.zipWithIndex
      if w(i)(j) != '0'
    } {
      rec(
        w.updated(i, w(i).updated(j, '0')),
        s"$c",
        List(j -> i),
        i,
        j,
        s
      )
    }
  }

  def rec(w: List[List[Char]], s: String, p: List[(Int, Int)], i: Int, j: Int, length: Int): Unit = {
    if (s.length == length) {
      if (!dict.misspelled(s))
        println(s"$s ${p.map(t => s"${t._1},${t._2}").mkString(" ")}")
    } else {
      for {
        ii <- i - 1 to i + 1
        jj <- j - 1 to j + 1
        if ii >= 0 && ii < 4
        if jj >= 0 && jj < 4
        if w(ii)(jj) != '0'
      } {
        rec(
          w.updated(ii, w(ii).updated(jj, '0')),
          s"$s${w(ii)(jj)}",
          p ++ List(jj -> ii),
          ii,
          jj,
          length
        )
      }
    }
  }

  def transform(wm: List[List[Char]], moves: List[(Int, Int)]): List[List[Char]] = {
    wm.zipWithIndex.map { case(ly, y) =>
      ly.zipWithIndex.filter { case (_, x) =>
          !moves.contains(x -> y)
      }
    }.map(_.map(_._1)).map { l =>
      List.fill(4 - l.length)('0') ++ l
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val wbh = new WordBrainHelper()

    print("Input: ")
    val input = StdIn.readLine()
    println()

    var wm = List.fill(4,4)(' ').zipWithIndex.map { case (l, i) =>
      l.zipWithIndex.map { case (_, j) =>
        input.charAt(i + j * 4)
      }
    }

    while (true) {
      print("Nr: ")
      val n = StdIn.readInt()
      println()
      wbh.recInit(wm, n)
      print("Pattern: ")
      val p = StdIn.readLine().split(" ").map(_.split(",")).map(i => i(0).toInt -> i(1).toInt).toList
      println()
      wm = wbh.transform(wm, p)
    }
  }
}
