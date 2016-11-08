package sortable
import play.api.libs.json._

 trait ProductsTrait {
  def find(prodDescription:JsObject):Option[JsObject]
}

class Products(val filename:String) extends ProductsTrait{
  
  
  
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
  
  
  
  private val dataMap: Map[String, Map[String, JsObject]] = constructProductTable(filename)
  private val manufacturers: Set[String] = dataMap.keySet
  
  
  private def constructProductTable(filename: String) ={
    val f = scala.io.Source.fromFile(filename)
    constructMap(Map(), f.getLines().map(Json.parse))
    
  }
  
  private def constructMap(initial:Map[String, Map[String, JsObject]], items: Iterator[JsValue]): Map[String, Map[String,JsObject]] = {
    if (items.isEmpty) {
      initial
    }
    else {
      val item = items.next().as[JsObject]
      val manufacturer =  item.value("manufacturer").toString().toLowerCase().replaceAll("[^A-Za-z0-9]","")
      val model =  item.value("model").toString().toLowerCase().replaceAll("[^A-Za-z0-9]","")
      
      //to be refactored by using initial withDefaultValue
      if (initial.keySet.contains(manufacturer))  {
        constructMap(initial.updated(manufacturer, initial(manufacturer).updated(model, item)),items)
      }
      else {
        constructMap(initial.updated(manufacturer, Map(model->item)), items)
      }
    }
  }
}