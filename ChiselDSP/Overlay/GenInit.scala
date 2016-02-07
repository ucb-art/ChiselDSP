// TODO: Make multi JSON param file usable

package ChiselDSP
import Chisel._
import org.json4s._
import org.json4s.native.JsonMethods._
import native.Serialization._

/** Custom serializer to extract Complex TrimType from JSON */
case object TrimTypeSer extends CustomSerializer[TrimType](format => (
  {
    case JString(trimType) =>  trimType match {
      case "NoTrim" => NoTrim
      case "Truncate" => Truncate
      case "Round" => Round
      case _ => Error("Invalid JSON Complex Trim Type"); NoTrim
    }
    case JNull => null
  },
  {
    case trimType: TrimType => JString(trimType.getClass.getSimpleName.replace("$",""))
  }
))

/** Custom serializer to extract OverflowType from JSON */
case object OverflowTypeSer extends CustomSerializer[OverflowType](format => (
  {
    case JString(overflowType) =>  overflowType match {
      case "Grow" => Grow
      case "Saturate" => Saturate
      case "Wrap" => Wrap
      case _ => Error("Invalid JSON Complex Overflow Type"); Grow
    }
    case JNull => null
  },
  {
    case overflowType: OverflowType => JString(overflowType.getClass.getSimpleName.replace("$",""))
  }
))

/** All user-defined case classes for generator parameterization should extend JSONParams */
class JSONParams (val complexInit: ComplexParams)

/** Initial setup (get user parameters from JSON file, determine whether to run in fixed or double mode)
  * gen = case class you want to extract the JSON to
  * jsonName = the name of the JSON file i.e. the * in *.json
  * args = main args
  * ser = if needed, a list of additional custom serializers for user parameters (see above)/
  * Returns isFixed (true if fixed mode, else double) & user parameters to be used
  */
object Init {

  def apply [T <: JSONParams](gen : => T, jsonName: String, args: Array[String])
                             (implicit m: Manifest[T]): Tuple2[Boolean,T] = {
    apply(gen,jsonName,args,List())
  }

  def apply [T <: JSONParams, S <: CustomSerializer[_]](gen : => T, jsonName: String, args: Array[String],
                                                        ser: List[S])
                                                       (implicit m: Manifest[T]) : Tuple2[Boolean,T] = {
    try {
      val jsonContents = scala.io.Source.fromFile("src/main/resources/" + jsonName + ".json").getLines.mkString
      val json = parse(jsonContents)
      Status("User parameters: " + pretty(render(json)))

      // How to serialize JSON
      implicit val formats = DefaultFormats ++ List(TrimTypeSer, OverflowTypeSer) ++ ser
      val p = json.extract[T]
      apply(p,args,ser)
    }
    catch {
      case ex: Exception => apply(gen,args,ser)
    }

  }

  def apply [T <: JSONParams] (p: T, args: Array[String]) : Tuple2[Boolean,T] = apply(p,args,List())

  def apply [T <: JSONParams, S <: CustomSerializer[_]]
            (p: T, args: Array[String], ser: List[S]) : Tuple2[Boolean,T] = {
    // Set Complex params
    Complex.opts = p.complexInit

    implicit val formats = DefaultFormats ++ List(TrimTypeSer,OverflowTypeSer) ++ ser

    // Print JSON to file
    val complexParamStr = "{\"complex\":" + write(p.complexInit) + ","
    val newJSON = complexParamStr + write(p).substring(1) + "\n"
    scala.tools.nsc.io.File("generator_out.json").appendAll(newJSON)

    // SBT parameters (used to set Fixed/Dbl)
    val paramsSBT = """-params_(.*)""".r.findFirstMatchIn(args(0))
    val isFixed = paramsSBT.get.group(1).toBoolean
    val mode = if (isFixed) "fixed point" else "double precision floating point"
    Status("Compiling in " + mode + " mode")
    (isFixed,p)
  }
}