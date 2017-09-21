package com.github.mdr.ajd

import org.json4s._

import scala.reflect.runtime.universe._

abstract class CustomDeserializer[T](implicit val typeTag: TypeTag[T]) {

  def deserialize(klass: Class[_], value: JValue): T

}