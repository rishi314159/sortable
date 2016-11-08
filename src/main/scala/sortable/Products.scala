package sortable
import play.api.libs.json._

 trait ProductsTrait {
  def find(listing:JsObject):Option[JsObject]
}

class Products(val filename:String) extends ProductsTrait{
  
  
  
  /** Takes a listing . Returns
   *  None if product is not found products list, and returns Some(product) 
   *  if the product was found
   */
  def find(listing:JsObject):Option[JsObject] = {
    
    val manufacturer = listing.value("manufacturer").toString().toLowerCase().replaceAll("[^A-Za-z0-9]","")
    val title = listing.value("title").toString().toLowerCase().replaceAll("[^A-Za-z0-9]","")
    
    val foundManufacturer = manufacturers.find { x => manufacturer.contains(x)}
    
    
    foundManufacturer match {
      case None => None
      case Some(manuf) => {
        val models = dataMap(manuf).keySet
        val foundModel = models.find ( x => title.contains(x))
        foundModel match {
          case None => None
          case Some(m) => Some(dataMap(manuf)(m))
        }
      }
    }

  }
  
  
  
  // dataMap is dictionary with manufacturers as the keys. This is
  //constructed to make search more efficient in find. One can view it like a tree
  //              datamap
  //            /      /   ...
  //       sony    canon ......
  //      /  | ..   /   | ...
  //   DSC1 T99 .. Z400  z2000 ....
  private val dataMap: Map[String, Map[String, JsObject]] = constructDataMap(filename)
  private val manufacturers: Set[String] = dataMap.keySet
  
  
  private def constructDataMap(filename: String):Map[String, Map[String, JsObject]] ={
    val f = scala.io.Source.fromFile(filename)
    f.getLines().map(Json.parse).foldLeft(Map().withDefault { x:String => Map[String, JsObject]() })(extendMap)
    
  }
  
  private def extendMap(initial:Map[String, Map[String, JsObject]], itemValue: JsValue) = {
    val item = itemValue.as[JsObject]
      val manufacturer =  item.value("manufacturer").toString().toLowerCase().replaceAll("[^A-Za-z0-9]","")
      val model =  item.value("model").toString().toLowerCase().replaceAll("[^A-Za-z0-9]","")
      initial.updated(manufacturer, initial(manufacturer).updated(model, item))
  }
}
