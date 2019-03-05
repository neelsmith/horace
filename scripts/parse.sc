import edu.holycross.shot.tabulae.builder._

import sys.process._
import scala.language.postfixOps

import better.files._
import java.io.{File => JFile}
import better.files.Dsl._

val compiler = "/usr/bin/fst-compiler-utf8"
val fstinfl = "/usr/bin/fst-infl"
val make = "/usr/bin/make"

def wordList(file: String): Unit = {
  import java.io._
  import java.lang._
  val corpus = scala.io.Source.fromFile("editions/" + file).mkString.filter(!_.isDigit)
  val corpusnospace = corpus.replaceAll("\n"," ").trim
  val corpussplit = corpusnospace.replaceAll("\\p{Punct}|\\d","").toLowerCase.split("\\s+").toSet
  val finalcorpus = corpussplit.toArray.sorted
  val md = new BufferedWriter(new FileWriter(new File("results/" + "wl_" + file)))
  for (diff <- finalcorpus) md.write(diff + "\n")
  md.close()
  println(finalcorpus.size + " unique lexical tokens")
}

def histogram(file: String): Unit = {
  import java.io._
  import java.lang._
  val corpus = scala.io.Source.fromFile("editions/" + file).mkString
  val corpusnospace = corpus.replaceAll("\n"," ").trim
  val corpussplit = corpusnospace.replaceAll("\\p{Punct}|\\d","").trim.replaceAll("\n"," ").toLowerCase.split("\\s+")
  val hist = corpussplit.groupBy(e => e).map(e => e._1 -> e._2.length)
  import scala.collection.immutable.ListMap
  val histo = ListMap(hist.toSeq.sortWith(_._2 > _._2):_*).toString
  val histog = histo.replaceAll("[()]","").trim.replaceAll("ListMap","").trim.replaceAll(" -> ","#")
  val histogr = histog.split(", ").toArray
  val hd = new BufferedWriter(new FileWriter(new File("results/" + "hist_" + file)))
  for (diff <- histogr) hd.write(diff + "\n")
  hd.close()
  println("created histogram in RESULTS directory")
}

def makeWordList(file: String): String = {
  import java.io._
  import java.lang._
  val corpus = scala.io.Source.fromFile("editions/" + file).mkString
  val corpusnospace = corpus.replaceAll("\\p{Punct}|\\d","").toLowerCase
  val corpussplit = corpusnospace.split("\\s+")
  val md = new BufferedWriter(new FileWriter(new File("results/" + "pl_" + file)))
  for (diff <- corpussplit) md.write(diff + "\n")
  md.close()
  val fl = System.getProperty("user.dir")
  val fll = fl + "/results/" + "pl_" + file
  return fll
}

def compile(corpus: String, datasets: String = ".") = {
  val repo = File("./tabulae")
  val conf =  Configuration(compiler, fstinfl, make, datasets)
  try {
    FstCompiler.compile(File(datasets), repo, corpus, conf, true)
    val tabulaeParser = repo/"parsers"/corpus/"latin.a"

    println("\nCompilation completed.\nParser is available in " +  tabulaeParser)
  } catch {
    case t: Throwable => println("Error trying to compile:\n" + t.toString)
  }
}


/**  Parse words listed in a file, and return their analyses
* as a String.
*
* @param wordsFile File with words to parse listed one per line.
* @param parser Name of corpus-specific parser, a subdirectory of
* tabulae/parsers.
*/

def parse(wordFile: String, parser: String = "horace-morphology") : String = {
  val fcp = makeWordList(wordFile)
  def compiled = s"tabulae/parsers/${parser}/latin.a"
  val cmd = fstinfl + " " + compiled + "  " + fcp
  println("Beginning to parse word list in " + wordFile)
  println("Please be patient: there will be a pause after")
  println("the messages 'reading transducer...' and 'finished' while the parsing takes place.")
  cmd !!
}

def toFile(result: String, name: String): Unit = {
  import java.io._
  import java.lang._
  val pw = new BufferedWriter(new FileWriter(new File("results/" + "fp_" + name)))
  pw.write(result)
  pw.close()
}

println("\n\nCompile a parser:\n")
println("\tcompile(CORPUS)\n")
println("\nMake a word list:\n")
println("\twordList(EDITION)")
println("\nParse a word list:\n")
println("\tparse(WORDLIST, CORPUS)")
println("\nParse a text:\n")
println("\tparse(EDITION, CORPUS)")
println("\nPut the full parse into a file:\n")
println("\ttoFile(STRING, such as res7; EDITION)")
println("\nAll results can be found in the RESULTS directory.\n")
