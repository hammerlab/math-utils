package hammerlab

trait stats {
  type Stats[K, V] = org.hammerlab.stats.Stats[K, V]
  val Stats = org.hammerlab.stats.Stats
}
object stats extends stats
