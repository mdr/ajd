package com.github.mdr.ajd

import java.math.RoundingMode

import com.fasterxml.jackson.annotation.JsonProperty
import com.google.common.math.DoubleMath
import org.json4s._

import scala.reflect.runtime.universe._

class JsonDeserializer(customDeserializers: Seq[CustomDeserializer[_]] = Seq()) {

  def deserialize[T: TypeTag](value: JValue): T = {
    val tpe = implicitly[TypeTag[T]].tpe
    doDeserialize(tpe, value).asInstanceOf[T]
  }

  private val mirror: Mirror = runtimeMirror(getClass.getClassLoader)

  private class TypeInfo(tpe: Type) {

    lazy val isString: Boolean = tpe <:< typeTag[String].tpe

    lazy val isBigDecimal: Boolean = tpe <:< typeTag[BigDecimal].tpe

    lazy val isCaseClass: Boolean = tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isCaseClass

    lazy val isOption: Boolean = tpe <:< typeTag[Option[_]].tpe

    lazy val isSeq: Boolean = tpe <:< typeTag[Seq[_]].tpe

    lazy val isMap: Boolean = tpe <:< typeTag[Map[_, _]].tpe

    lazy val classSymbol: ClassSymbol = tpe.typeSymbol.asClass

    lazy val constructorSymbol: Symbol = classSymbol.primaryConstructor

    lazy val constructor: MethodMirror = mirror.reflectClass(classSymbol).reflectConstructor(constructorSymbol.asMethod)

    lazy val paramLists: Seq[List[Symbol]] = constructorSymbol.asMethod.paramLists

    lazy val customDeserializer: Option[CustomDeserializer[_]] = customDeserializers.find(tpe <:< _.typeTag.tpe)

    lazy val runtimeClass: Class[_] = mirror.runtimeClass(tpe)
  }

  private var typeInfoCache: Map[Type, TypeInfo] = Map()

  private def describeBriefly(value: JValue) = value match {
    case JNothing    ⇒ "nothing"
    case JNull       ⇒ "null"
    case _: JString  ⇒ "String"
    case _: JDouble  ⇒ "Double"
    case _: JDecimal ⇒ "Decimal"
    case _: JInt     ⇒ "Int"
    case _: JLong    ⇒ "Long"
    case _: JBool    ⇒ "Boolean"
    case _: JObject  ⇒ "Object"
    case _: JArray   ⇒ "Array"
    case _: JSet     ⇒ "Set"
  }

  private def doDeserialize(tpe: Type, value: JValue, pathOpt: Option[String] = None): Any = {
    val typeInfo = getTypeInfo(tpe)
    typeInfo.customDeserializer match {
      case Some(deserializer) ⇒
        deserializer.deserialize(typeInfo.runtimeClass, value)
      case None               ⇒
        if (tpe == definitions.IntTpe)
          deserializeToInt(value, pathOpt)
        else if (typeInfo.isString)
          deserializeToString(value, pathOpt)
        else if (typeInfo.isBigDecimal)
          deserializeToBigDecimal(value, pathOpt)
        else if (typeInfo.isCaseClass)
          deserializeToCaseClass(value, typeInfo, pathOpt)
        else if (typeInfo.isOption)
          deserializeToOption(tpe, value, pathOpt)
        else if (typeInfo.isSeq)
          deserializeToSeq(tpe, value, pathOpt)
        else if (typeInfo.isMap)
          deserializeToMap(tpe, value, pathOpt)
        else
          throw new DeserialisationException(s"${errorPrefix(pathOpt)}No strategy to deserialize value of ${describeBriefly(value)} to a ${tpe.typeSymbol.name.toString}")
    }
  }

  private def getTypeInfo(tpe: Type) =
    typeInfoCache.getOrElse(tpe, {
      val typeInfo = new TypeInfo(tpe)
      typeInfoCache += tpe → typeInfo
      typeInfo
    })

  private def deserializeToMap(tpe: Type, value: JValue, pathOpt: Option[String]) =
    value match {
      case obj: JObject ⇒
        val Seq(keyType, valueType) = tpe.typeArgs
        obj.obj.map { case (fieldName, fieldValue) ⇒
          val deserializedKey = doDeserialize(keyType, JString(fieldName), pathOpt)
          val deserializedValue = doDeserialize(valueType, fieldValue, Some(pathOpt.getOrElse("") + s".$fieldName"))
          deserializedKey → deserializedValue
        }.toMap
      case _            ⇒
        error(pathOpt, value, tpe.typeSymbol.name.toString)
    }

  private def deserializeToSeq(tpe: Type, value: JValue, pathOpt: Option[String]) =
    value match {
      case array: JArray ⇒
        val innerTpe = tpe.typeArgs.headOption.getOrElse {
          val path = pathOpt.getOrElse("root")
          throw new DeserialisationException(s"${errorPrefix(pathOpt)}Seq had no type argument")
        }
        val children = array.arr.zipWithIndex.map { case (childValue, i) ⇒ doDeserialize(innerTpe, childValue, Some(pathOpt.getOrElse("") + s"[$i]")) }
        Seq(children: _*)
      case _             ⇒
        error(pathOpt, value, tpe.typeSymbol.name.toString)
    }

  private def deserializeToOption(tpe: Type, value: JValue, pathOpt: Option[String]) =
    value match {
      case JNull ⇒ None
      case _     ⇒
        val innerTpe = tpe.typeArgs.headOption.getOrElse {
          throw new DeserialisationException(s"${errorPrefix(pathOpt)}Option had no type argument")
        }
        Some(doDeserialize(innerTpe, value))
    }

  private def errorPrefix(pathOpt: Option[String]) = {
    val path = pathOpt.getOrElse("root")
    s"Error at $path: "
  }

  private def deserializeToCaseClass(value: JValue, typeInfo: TypeInfo, pathOpt: Option[String]) =
    value match {
      case obj: JObject ⇒
        typeInfo.paramLists match {
          case Seq(params) ⇒
            val values = params.map { param ⇒
              val paramTpe = param.typeSignature
              val paramTypeInfo = getTypeInfo(paramTpe)
              val name = getFieldNameFromAnnotation(param) getOrElse param.name.toString
              val value = obj.obj.find(_._1 == name) match {
                case None                  ⇒
                  if (paramTypeInfo.isOption)
                    None
                  else {
                    throw new DeserialisationException(s"${errorPrefix(pathOpt)}could not deserialize into a '${typeInfo.classSymbol.name}' -- missing field '$name'")
                  }
                case Some((_, fieldValue)) ⇒
                  doDeserialize(paramTpe, fieldValue, Some(pathOpt.getOrElse("") + "." + name))
              }
              value
            }
            typeInfo.constructor.apply(values: _*)

          case lists ⇒
            throw new DeserialisationException(s"${errorPrefix(pathOpt)}could not deserialize into a '${typeInfo.classSymbol.name}' -- constructor must have one parameter list, but there were ${lists.size}")
        }
      case _            ⇒ error(pathOpt, value, typeInfo.classSymbol.name.toString)
    }

  private def getFieldNameFromAnnotation(param: Symbol) =
    param.annotations
      .find(_.tree.tpe <:< typeTag[JsonProperty].tpe)
      .flatMap(_.javaArgs.values.headOption)
      .collect { case LiteralArgument(Constant(value: String)) ⇒ value }

  private def deserializeToBigDecimal(value: JValue, pathOpt: Option[String]) =
    value match {
      case n: JInt     ⇒ BigDecimal(n.num)
      case n: JDouble  ⇒ BigDecimal(n.num)
      case n: JLong    ⇒ BigDecimal(n.num)
      case n: JDecimal ⇒ n.num
      case _           ⇒ error(pathOpt, value, "BigDecimal")
    }

  private def deserializeToString(value: JValue, pathOpt: Option[String]) =
    value match {
      case s: JString ⇒ s.s
      case _          ⇒ error(pathOpt, value, "String")
    }

  private def deserializeToInt(value: JValue, pathOpt: Option[String]): Int =
    value match {
      case n: JInt     ⇒ deserializeToInt(n.num, pathOpt, value)
      case n: JLong    ⇒ deserializeToInt(BigInt(n.num), pathOpt, value)
      case n: JDouble  ⇒
        val bigInteger =
          try
            DoubleMath.roundToBigInteger(n.num, RoundingMode.UNNECESSARY)
          catch {
            case _: ArithmeticException ⇒ error(pathOpt, value, "Int")
          }
        deserializeToInt(bigInteger, pathOpt, value)
      case n: JDecimal ⇒ deserializeToInt(n.num, pathOpt, value)
      case _           ⇒ error(pathOpt, value, "Int")
    }

  private def deserializeToInt(bigDecimal: BigDecimal, pathOpt: Option[String], value: JValue): Int =
    try
      bigDecimal.underlying.intValueExact
    catch {
      case e: ArithmeticException ⇒ error(pathOpt, value, "Int")
    }

  private def deserializeToInt(bigInt: BigInt, pathOpt: Option[String], value: JValue): Int =
    try
      bigInt.underlying.intValueExact
    catch {
      case e: ArithmeticException ⇒ error(pathOpt, value, "Int")
    }

  private def error(pathOpt: Option[String], value: JValue, target: String) = {
    throw new DeserialisationException(s"${errorPrefix(pathOpt)}could not deserialize a value of type ${describeBriefly(value)} as a $target")
  }

}
