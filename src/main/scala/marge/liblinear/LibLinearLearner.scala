package marge.liblinear

import marge.{Predictor, Learner}
import marge.data.SparseVector
import java.io._
import collection.mutable.ArrayBuffer

/**
 * Twimpact
 *
 * User: mikio
 * Date: 4/7/11
 * Time: 3:08 PM
 */

class ProcessPrinterThread(in: InputStream) extends Thread {
  private var pleaseStop = false

  def stopPrinting() {
    pleaseStop = true
  }

  override def run() {
    while (!pleaseStop) {
      var c = in.read()
      while (c != -1) {
        print(c.toChar);
        System.out.flush()
        c = in.read()
      }
      Thread.sleep(100)
    }
  }
}

class LibLinearLearner(c: Double, trainType: Int = 1) extends Learner[SparseVector, Double] {
  val LIBLINEAR_DIR = "/home/mikio/build/liblinear-1.8"

  def writeSVMLightFormat(out: PrintStream, ds: Seq[(SparseVector, Double)]) {
    ds.foreach {
      ex =>
        out.println("%+d ".format(ex._2.toInt) + ex._1.keys.map(i => "%d:%f".format(i + 1, ex._1(i))).mkString(" "))
    }
    out.flush()
  }

  def train(ds: Seq[(SparseVector, Double)]): LibLinearPredictor = {
    val temp = File.createTempFile("liblinear", "train")
    //println("Writing training data into " + temp)
    val out = new PrintStream(temp)
    writeSVMLightFormat(out, ds)
    out.close()
    //println("Training")
    val model = runLibLinear(temp)
    //println("Result data: " + model)
    //print(loadFile(model))
    temp.delete()
    val p = LibLinearPredictor.parseModelFile(model)
    model.delete()
    p
  }


  def runLibLinear(file: File): File = {
    val modelTemp = File.createTempFile("liblinear", "model")
    val cmdline = "%s/train -s %d -q -c %f %s %s"
        .format(LIBLINEAR_DIR, trainType, c, file.getAbsolutePath, modelTemp.getAbsolutePath)
    val p = Runtime.getRuntime.exec(cmdline)
    val printerThread = new ProcessPrinterThread(p.getInputStream)
    printerThread.start()
    p.waitFor()
    printerThread.stopPrinting()
    modelTemp
  }

  def loadFile(file: File): String = {
    val in = new BufferedReader(new FileReader(file))
    val sb = new StringBuilder
    while (in.ready) {
      sb.append(in.readLine())
      sb.append("\n")
    }
    sb.result()
  }
}

class LibLinearPredictor(val weights: Array[Double], val bias: Double) extends Predictor[SparseVector, Double] {
  def apply(x: SparseVector): Double = x * weights

  // bias
}

object LibLinearPredictor {
  def parseModelFile(file: File): LibLinearPredictor = {
    val in = new BufferedReader(new FileReader(file))
    val wb = new ArrayBuffer[Double]
    var bias = 0.0
    var inHeader = true
    while (in.ready) {
      val line = in.readLine()
      if (inHeader) {
        if (line startsWith "bias ") {
          bias = line.substring(5).toInt
        } else if (line == "w") {
          inHeader = false
        }
      } else {
        wb.append(line.toDouble)
      }
    }

    new LibLinearPredictor(wb.toArray, bias)
  }
}
