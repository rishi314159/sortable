package sortable
import play.api.libs.json._

 trait ProductsTrait {
  type Product = JsObject
  type Listing = JsObject
  def find(listing:Listing):Option[Product]
}

class Products(val filename:String) extends ProductsTrait{
  
  
  
  /** Takes a listing . Returns None if the product is not found in the 
   *  products list, and returns Some(product) if the product is found
   */
  def find(listing:Listing):Option[Product] = {
    
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
  private val dataMap: Map[String, Map[String, Product]] = constructDataMap(filename)
  private val manufacturers: Set[String] = dataMap.keySet
  
  
  private def constructDataMap(filename: String):Map[String, Map[String, Product]] ={
    val f = scala.io.Source.fromFile(filename)
    f.getLines().map(Json.parse).foldLeft(Map().withDefault { x:String => Map[String, Product]() })(extendMap)
    
  }
  
  private def extendMap(initial:Map[String, Map[String, Product]], itemValue: JsValue) = {
    val item = itemValue.as[JsObject]
      val manufacturer =  item.value("manufacturer").toString().toLowerCase().replaceAll("[^A-Za-z0-9]","")
      val model =  item.value("model").toString().toLowerCase().replaceAll("[^A-Za-z0-9]","")
      initial.updated(manufacturer, initial(manufacturer).updated(model, item))
  }
}
