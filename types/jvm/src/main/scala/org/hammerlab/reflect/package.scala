package org.hammerlab

import hammerlab.str.Name

import scala.reflect.ClassTag

package object reflect {
  implicit class Ops[T](val t: T) extends AnyVal {
    def get_![U](name: Name, params: Arg[_]*): U = {
      val cls = t.getClass
      hammerlab.reflect.get_![T, U](t, cls, name, params: _*)
    }

    def set_![V](name: Name, v: V): Unit = {
      val cls = t.getClass
      val field = cls.getDeclaredField(name)
      field.setAccessible(true)
      field.set(t, v)
    }
  }

  trait syntax {

    @inline implicit def ReflectOps[T](t: T): org.hammerlab.reflect.Ops[T] = org.hammerlab.reflect.Ops(t)

    // call Java static method; no companion object exists
    def get_![T, V](name: Name, params: Arg[_]*)(implicit ct: ClassTag[T]): V =
      get_!(
        null,
        ct.runtimeClass,
        name,
        params: _*
      )

    def set_![T](name: Name, v: Arg[_])(implicit ct: ClassTag[T]): Unit =
      set_!(ct.runtimeClass, name, v)

    def set_![T](cls: Class[T], name: Name, v: Arg[_]): Unit = {
      val field = cls.getDeclaredField(name)
      field.setAccessible(true)
      field.set(null, v.obj)
    }

    def get_![T, V](t: T, cls: Class[_], name: Name, params: Arg[_]*): V =
      (
        cls
          .getDeclaredFields
          .find(_.getName == name.toString) match {
            case Some(field) ⇒
              field.setAccessible(true)
              field.get(t)
            case None ⇒
              val classes = params.map(_.ct.runtimeClass)
              val method = cls.getDeclaredMethod(name, classes: _*)
              if (method == null) {
                throw new NoSuchMethodError(s"${cls.getCanonicalName} $name")
              }
              method.setAccessible(true)
              method.invoke(t, params.map(_.obj): _*)
          }
      )
      .asInstanceOf[V]
  }


  sealed case class Arg[T](obj: Object)(implicit val ct: ClassTag[T])
  object Arg {
    implicit def fromObj[T: ClassTag](t: T)(implicit fn: T ⇒ Object) = Arg(fn(t))
  }
}
