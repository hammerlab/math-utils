package hammerlab

trait str {
  type Name = org.hammerlab.str.Name
  type  Str = org.hammerlab.str.Name
  implicit def symbolToString(s: Symbol): String = s.name
}

object str extends str
