package sortable
import play.api.libs.json._

 trait ProductsTrait {
  def find(prodDescription:JsObject):Option[JsObject]
}

class Products(val filename:String) extends ProductsTrait{
  
  
  
  /** Takes a listing (prodDescription). Returns
   *  None if product is not found products list, and returns Some(product) 
   *  if the product was found
   */
  def find(prodDescription:JsObject):Option[JsObject] = {
    
    val manufacturer = prodDescription.value("manufacturer").toString().toLowerCase().replaceAll("[^A-Za-z0-9]","")
    val title = prodDescription.value("title").toString().toLowerCase().replaceAll("[^A-Za-z0-9]","")
    
    val manufacturerfind = manufacturers.find { x => manufacturer.contains(x)}
    

    if (manufacturerfind != None) {
      val Some(manuf) = manufacturerfind
      val models = dataMap(manuf).keySet
      val modelOption = models.find ( x => title.contains(x))
      if (modelOption == None) { //model not found
        None
      }
      else { //Model found
        val Some(m) = modelOption
        Some(dataMap(manuf)(m))
      }
    }
    else { //Manufacturer not found
      None
    }
  }
  
  
  
  // dataMap is dictionary with manufacturers as the keys. This is
  //constructed to make search more efficient in find. One can view it like a tree
  //              datamap
  //            /      /   ...
  //       sony    canon ......
  //      /  | ..   /   | ...
  //   DSC1 T99 .. Z400  z2000 ....
  private val dataMap: Map[String, Map[String, JsObject]] = constructProductTable(filename)
  private val manufacturers: Set[String] = dataMap.keySet
  
  
  private def constructProductTable(filename: String) ={
    val f = scala.io.Source.fromFile(filename)
    constructMap(Map(), f.getLines().map(Json.parse))
    
  }
  
  //TODO refactor to use foldLeft
  private def constructMap(initial:Map[String, Map[String, JsObject]], items: Iterator[JsValue]): Map[String, Map[String,JsObject]] = {
    if (items.isEmpty) {
      initial
    }
    else {
      val item = items.next().as[JsObject]
      val manufacturer =  item.value("manufacturer").toString().toLowerCase().replaceAll("[^A-Za-z0-9]","")
      val model =  item.value("model").toString().toLowerCase().replaceAll("[^A-Za-z0-9]","")
      
      //TODO  refactor by using initial withDefaultValue
      if (initial.keySet.contains(manufacturer))  {
        constructMap(initial.updated(manufacturer, initial(manufacturer).updated(model, item)),items)
      }
      else {
        constructMap(initial.updated(manufacturer, Map(model->item)), items)
      }
    }
  }
}
