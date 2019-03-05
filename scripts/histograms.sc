import better.files._
import java.io.{File => JFile}
import better.files.Dsl._

import java.io.PrintWriter

def histogram(thresh: Int = 999999999, file : String) : Unit = {
  import java.io._
  import java.lang._
  val corpus = scala.io.Source.fromFile("editions/" + file).mkString
  val corpusnospace = corpus.replaceAll("\n"," ").trim
  val corpussplit = corpusnospace.replaceAll("\\p{Punct}|\\d","").toLowerCase.split("\\s+")
  val hist = corpussplit.groupBy(e => e).map(e => e._1 -> e._2.length)
  import scala.collection.immutable.ListMap
  val histo = ListMap(hist.toSeq.sortWith(_._2 > _._2):_*).take(thresh).toString
  val histog = histo.replaceAll("[()]","").trim.replaceAll("ListMap","").trim.replaceAll(" -> ","#")
  val histogr = histog.split(", ").toArray
  val hd = new BufferedWriter(new FileWriter(new File("results/" + "hist" + thresh + "_" + file)))
  for (diff <- histogr) hd.write(diff + "\n")
  hd.close()
  println("created histogram with threshold in RESULTS directory")
}

def nounHist(thresh : Int = 999999999, file: String): Unit = {
  import java.io._
  import java.lang._
  val corpus = scala.io.Source.fromFile("results/" + "fp_" + file).mkString
  val co = corpus.split("> ").toArray.filter(_.contains("noun>"))
  val cor = co.mkString.split("\n")
  val corp = cor.toArray.filterNot(_.contains("<u>")).filterNot(_.contains("no result"))
  val hist = corp.groupBy(e => e).map(e => e._1 -> e._2.length)
  import scala.collection.immutable.ListMap
  val histo = ListMap(hist.toSeq.sortWith(_._2 > _._2):_*).take(thresh).toString
  val histog = histo.replaceAll("[()]","").trim.replaceAll("ListMap","").trim.replaceAll(" -> ","#")
  val histogr = histog.split(", ").toArray
  val hd = new BufferedWriter(new FileWriter(new File("results/" + "nouns_" + file)))
  for (diff <- histogr) hd.write(diff + "\n")
  hd.close()
  println("created list of nouns in RESULTS directory")
}

def verbHist(thresh : Int = 999999999, file: String): Unit = {
  import java.io._
  import java.lang._
  val corpus = scala.io.Source.fromFile("results/" + "fp_" + file).mkString
  val co = corpus.split("> ").toArray.filter(_.contains("verb>"))
  val cor = co.mkString.split("\n")
  val corp = cor.toArray.filterNot(_.contains("<u>")).filterNot(_.contains("no result"))
  val hist = corp.groupBy(e => e).map(e => e._1 -> e._2.length)
  import scala.collection.immutable.ListMap
  val histo = ListMap(hist.toSeq.sortWith(_._2 > _._2):_*).take(thresh).toString
  val histog = histo.replaceAll("[()]","").trim.replaceAll("ListMap","").trim.replaceAll(" -> ","#")
  val histogr = histog.split(", ").toArray
  val hd = new BufferedWriter(new FileWriter(new File("results/" + "verbs_" + file)))
  for (diff <- histogr) hd.write(diff + "\n")
  hd.close()
  println("created list of verbs in RESULTS directory")
}


def ptcplHist(thresh : Int = 999999999, file: String): Unit = {
  import java.io._
  import java.lang._
  val corpus = scala.io.Source.fromFile("results/" + "fp_" + file).mkString
  val co = corpus.split("> ").toArray.filter(_.contains("ptcpl>"))
  val cor = co.mkString.split("\n")
  val corp = cor.toArray.filterNot(_.contains("<u>")).filterNot(_.contains("no result"))
  val hist = corp.groupBy(e => e).map(e => e._1 -> e._2.length)
  import scala.collection.immutable.ListMap
  val histo = ListMap(hist.toSeq.sortWith(_._2 > _._2):_*).take(thresh).toString
  val histog = histo.replaceAll("[()]","").trim.replaceAll("ListMap","").trim.replaceAll(" -> ","#")
  val histogr = histog.split(", ").toArray
  val hd = new BufferedWriter(new FileWriter(new File("results/" + "ptcpls_" + file)))
  for (diff <- histogr) hd.write(diff + "\n")
  hd.close()
  println("created list of participles in RESULTS directory")
}


def adjHist(thresh : Int = 999999999, file: String): Unit = {
  import java.io._
  import java.lang._
  val corpus = scala.io.Source.fromFile("results/" + "fp_" + file).mkString
  val co = corpus.split("> ").toArray.filter(_.contains("adj>"))
  val cor = co.mkString.split("\n")
  val corp = cor.toArray.filterNot(_.contains("<u>")).filterNot(_.contains("no result"))
  val hist = corp.groupBy(e => e).map(e => e._1 -> e._2.length)
  import scala.collection.immutable.ListMap
  val histo = ListMap(hist.toSeq.sortWith(_._2 > _._2):_*).take(thresh).toString
  val histog = histo.replaceAll("[()]","").trim.replaceAll("ListMap","").trim.replaceAll(" -> ","#")
  val histogr = histog.split(", ").toArray
  val hd = new BufferedWriter(new FileWriter(new File("results/" + "adjs_" + file)))
  for (diff <- histogr) hd.write(diff + "\n")
  hd.close()
  println("created list of adjectives in RESULTS directory")
}

def advHist(thresh : Int = 999999999, file: String): Unit = {
  import java.io._
  import java.lang._
  val corpus = scala.io.Source.fromFile("results/" + "fp_" + file).mkString
  val co = corpus.split("> ").toArray.filter(_.contains("adv>"))
  val cor = co.mkString.split("\n")
  val corp = cor.toArray.filterNot(_.contains("<u>")).filterNot(_.contains("no result"))
  val hist = corp.groupBy(e => e).map(e => e._1 -> e._2.length)
  import scala.collection.immutable.ListMap
  val histo = ListMap(hist.toSeq.sortWith(_._2 > _._2):_*).take(thresh).toString
  val histog = histo.replaceAll("[()]","").trim.replaceAll("ListMap","").trim.replaceAll(" -> ","#")
  val histogr = histog.split(", ").toArray
  val hd = new BufferedWriter(new FileWriter(new File("results/" + "adverbs_" + file)))
  for (diff <- histogr) hd.write(diff + "\n")
  hd.close()
  println("created list of adverbs in RESULTS directory")
}


def indeclHist(thresh : Int = 999999999, file: String): Unit = {
  import java.io._
  import java.lang._
  val corpus = scala.io.Source.fromFile("results/" + "fp_" + file).mkString
  val co = corpus.split("> ").toArray.filter(_.contains("<indecl"))
  val cor = co.mkString.split("\n")
  val corp = cor.toArray.filterNot(_.contains("<u>")).filterNot(_.contains("no result"))
  val hist = corp.groupBy(e => e).map(e => e._1 -> e._2.length)
  import scala.collection.immutable.ListMap
  val histo = ListMap(hist.toSeq.sortWith(_._2 > _._2):_*).take(thresh).toString
  val histog = histo.replaceAll("[()]","").trim.replaceAll("ListMap","").trim.replaceAll(" -> ","#")
  val histogr = histog.split(", ").toArray
  val hd = new BufferedWriter(new FileWriter(new File("results/" + "indecls_" + file)))
  for (diff <- histogr) hd.write(diff + "\n")
  hd.close()
  println("created list of indeclinables in RESULTS directory")
}

def pronHist(thresh : Int = 999999999, file: String): Unit = {
  import java.io._
  import java.lang._
  val corpus = scala.io.Source.fromFile("results/" + "fp_" + file).mkString
  val co = corpus.split("> ").toArray.filter(_.contains("pron>"))
  val cor = co.mkString.split("\n")
  val corp = cor.toArray.filterNot(_.contains("<u>")).filterNot(_.contains("no result"))
  val hist = corp.groupBy(e => e).map(e => e._1 -> e._2.length)
  import scala.collection.immutable.ListMap
  val histo = ListMap(hist.toSeq.sortWith(_._2 > _._2):_*).take(thresh).toString
  val histog = histo.replaceAll("[()]","").trim.replaceAll("ListMap","").trim.replaceAll(" -> ","#")
  val histogr = histog.split(", ").toArray
  val hd = new BufferedWriter(new FileWriter(new File("results/" + "pronouns_" + file)))
  for (diff <- histogr) hd.write(diff + "\n")
  hd.close()
  println("created list of pronouns in RESULTS directory")
}

def gndHist(thresh : Int = 999999999, file: String): Unit = {
  import java.io._
  import java.lang._
  val corpus = scala.io.Source.fromFile("results/" + "fp_" + file).mkString
  val co = corpus.split("> ").toArray.filter(_.contains("gnd>"))
  val cor = co.mkString.split("\n")
  val corp = cor.toArray.filterNot(_.contains("<u>")).filterNot(_.contains("no result"))
  val hist = corp.groupBy(e => e).map(e => e._1 -> e._2.length)
  import scala.collection.immutable.ListMap
  val histo = ListMap(hist.toSeq.sortWith(_._2 > _._2):_*).take(thresh).toString
  val histog = histo.replaceAll("[()]","").trim.replaceAll("ListMap","").trim.replaceAll(" -> ","#")
  val histogr = histog.split(", ").toArray
  val hd = new BufferedWriter(new FileWriter(new File("results/" + "gnds_" + file)))
  for (diff <- histogr) hd.write(diff + "\n")
  hd.close()
  println("created list of gerunds in RESULTS directory")
}

def gdvHist(thresh : Int = 999999999, file: String): Unit = {
  import java.io._
  import java.lang._
  val corpus = scala.io.Source.fromFile("results/" + "fp_" + file).mkString
  val co = corpus.split("> ").toArray.filter(_.contains("gdv>"))
  val cor = co.mkString.split("\n")
  val corp = cor.toArray.filterNot(_.contains("<u>")).filterNot(_.contains("no result"))
  val hist = corp.groupBy(e => e).map(e => e._1 -> e._2.length)
  import scala.collection.immutable.ListMap
  val histo = ListMap(hist.toSeq.sortWith(_._2 > _._2):_*).take(thresh).toString
  val histog = histo.replaceAll("[()]","").trim.replaceAll("ListMap","").trim.replaceAll(" -> ","#")
  val histogr = histog.split(", ").toArray
  val hd = new BufferedWriter(new FileWriter(new File("results/" + "gdvs_" + file)))
  for (diff <- histogr) hd.write(diff + "\n")
  hd.close()
  println("created list of gerundives in RESULTS directory")
}

def infinHist(thresh : Int = 999999999, file: String): Unit = {
  import java.io._
  import java.lang._
  val corpus = scala.io.Source.fromFile("results/" + "fp_" + file).mkString
  val co = corpus.split("> ").toArray.filter(_.contains("infin>"))
  val cor = co.mkString.split("\n")
  val corp = cor.toArray.filterNot(_.contains("<u>")).filterNot(_.contains("no result"))
  val hist = corp.groupBy(e => e).map(e => e._1 -> e._2.length)
  import scala.collection.immutable.ListMap
  val histo = ListMap(hist.toSeq.sortWith(_._2 > _._2):_*).take(thresh).toString
  val histog = histo.replaceAll("[()]","").trim.replaceAll("ListMap","").trim.replaceAll(" -> ","#")
  val histogr = histog.split(", ").toArray
  val hd = new BufferedWriter(new FileWriter(new File("results/" + "infins_" + file)))
  for (diff <- histogr) hd.write(diff + "\n")
  hd.close()
  println("created list of infinitives in RESULTS directory")
}

def supHist(thresh : Int = 999999999, file: String): Unit = {
  import java.io._
  import java.lang._
  val corpus = scala.io.Source.fromFile("results/" + "fp_" + file).mkString
  val co = corpus.split("> ").toArray.filter(_.contains("sup>"))
  val cor = co.mkString.split("\n")
  val corp = cor.toArray.filterNot(_.contains("<u>")).filterNot(_.contains("no result"))
  val hist = corp.groupBy(e => e).map(e => e._1 -> e._2.length)
  import scala.collection.immutable.ListMap
  val histo = ListMap(hist.toSeq.sortWith(_._2 > _._2):_*).take(thresh).toString
  val histog = histo.replaceAll("[()]","").trim.replaceAll("ListMap","").trim.replaceAll(" -> ","#")
  val histogr = histog.split(", ").toArray
  val hd = new BufferedWriter(new FileWriter(new File("results/" + "sups_" + file)))
  for (diff <- histogr) hd.write(diff + "\n")
  hd.close()
  println("created list of supines in RESULTS directory")
}

def guide : Unit = {
println("\nMake a histogram of all words:\n")
println("\thistogram(THRESHOLD, EDITION)")
println("\nMake a histogram of nouns:\n")
println("\tnounHist(THRESHOLD, EDITION)")
println("\nMake a histogram of verbs:\n")
println("\tverbHist(THRESHOLD, EDITION)")
println("\nMake a histogram of participles:\n")
println("\tptcplHist(THRESHOLD, EDITION)")
println("\nMake a histogram of adjectives:\n")
println("\tadjHist(THRESHOLD, EDITION)")
println("\nMake a histogram of indeclinables:\n")
println("\tindeclHist(THRESHOLD, EDITION)")
println("\nMake a histogram of pronouns:\n")
println("\tpronHist(THRESHOLD, EDITION)")
println("\nMake a histogram of gerunds:\n")
println("\tgndHist(THRESHOLD, EDITION)")
println("\nMake a histogram of gerundives:\n")
println("\tgdvHist(THRESHOLD, EDITION)")
println("\nMake a histogram of infinitives:\n")
println("\tinfinHist(THRESHOLD, EDITION)")
println("\nMake a histogram of supines:\n")
println("\tsupHist(THRESHOLD, EDITION)")
println("\nDo NOT use any of these functions before parsing an edition.\n")
println("These functions turn full parse files into histograms. Specify")
println("the number of word forms you want in the histogram with THRESHOLD.")
println("The default is a histogram including every word in the edition.")
}
println("\n\nFor a quick guide:")
println("\n\tguide")
