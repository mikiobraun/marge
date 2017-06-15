package marge

class DataSetAdditions[X, Y](examples: Seq[(X, Y)]) {
  def xs: Seq[X] = examples.map(xy => xy._1)

  def ys: Seq[Y] = examples.map(xy => xy._2)

  def subset(indices: Seq[Int]): Seq[(X, Y)] = indices.map(i => examples(i))

  def permute(): Seq[(X, Y)] = subset(randomPermutation(examples.length))

  def subsample(m: Int): Seq[(X, Y)] = if (m >= examples.length) examples else subset(randomSubset(m, examples.length))

  def mapX[A](fct: (X) => A): Seq[(A, Y)] = examples.map(xy => (fct(xy._1), xy._2))

  def mapY[A](fct: (Y) => A): Seq[(X, A)] = examples.map(xy => (xy._1, fct(xy._2)))

  def filterX(p: (X) => Boolean): Seq[(X, Y)] = examples.filter(xy => p(xy._1))

  def filterY(p: (Y) => Boolean): Seq[(X, Y)] = examples.filter(xy => p(xy._2))

  def oneAgainstAll(pos: Y): Seq[(X, Double)] = mapY(y => if (y == pos) 1.0 else -1.0)

  def subsampleSameNumberOfNegativeExamples(pos: Y): Seq[(X, Y)] = {
    val positiveCount = filterY(_ == pos).length
    filterY(_ == pos) ++ filterX(_ != pos).subsample(positiveCount)
  }

  def subsampleBalancedClasses(itemsPerClass: Int): Seq[(X, Y)] = {
    classes.toSeq.flatMap(c => filterY(_ == c).subsample(itemsPerClass))
  }

  def classes: Set[Y] = ys.toSet
}
