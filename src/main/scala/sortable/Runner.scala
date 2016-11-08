package sortable
import play.api.libs.json._

object Runner extends App {
  val listing: String = args(0)

  val products: String = args(1)
  val prod= new Products(products)
  val listingJson: Iterator[JsValue] = scala.io.Source.fromFile(listing).getLines().map(Json.parse)
  
  
  
  
  def append(initial: Map[JsString, JsArray],listing: JsValue): Map[JsString, JsArray]= {
    val optionproduct=prod.find(listing.as[JsObject])
    if (optionproduct == None) {
      initial
    }
    else {
      val Some(product) = optionproduct
      val product_name = product.value("product_name").as[JsString]
      initial.updated(product_name, initial(product_name) :+ listing)
    }
  }
  
  val result = listingJson.foldLeft(Map[JsString, JsArray]().withDefaultValue(JsArray()))(append)
  
  def constructJsObject(s:JsString, a: JsArray): JsValue = {
    JsObject(Seq(
        "product_name" -> s,
        "listings" -> a))
  }
  
  result.keys.map{x => println(constructJsObject(x, result(x)))}
  
}