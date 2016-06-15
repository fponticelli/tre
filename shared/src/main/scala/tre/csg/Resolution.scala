package tre.csg

trait Resolution {
  def resolutionByRadius(radius: Double): Int
}

object Resolution {
  def byFeature(minFeature: Double): Resolution = {
    new Resolution {
      def resolutionByRadius(radius: Double): Int = {
        val p = radius * 2 * Math.PI
        println(p, minFeature, Math.ceil(p / minFeature))
        Math.ceil(p / minFeature).toInt max 3
      }
    }
  }

  object StandardResolution extends Resolution {
    def resolutionByRadius(radius: Double): Int = 36
  }
}
