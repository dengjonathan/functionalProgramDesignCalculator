package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal {
    Math.pow(b(), 2.0) - 4 * a() * c()
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    if (delta() < 0) { Set() } else {
      val negativeB = - 1.0 * b()
      val twoA = 2 * a()
      val squareRootOfDelta = Math.pow(delta(), 0.5)
      Set(
        (negativeB + squareRootOfDelta) / twoA,
        (negativeB - squareRootOfDelta) / twoA
      )
    }
  }
}
