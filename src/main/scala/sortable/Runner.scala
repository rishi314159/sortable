package sortable
import play.api.libs.json._
import java.io.PrintWriter
import java.io.File


object Runner extends App {
  val listing: String = args(0)

  val products: String = args(1)
  val prod= new Products(products)
  val listingJson: Iterator[JsValue] = scala.io.Source.fromFile(listing).getLines().map(Json.parse)
  
  
  
  /** Takes a dictionary and listing and appends the listing to the appropriate
   *  product_name 
   */
  def append(initial: Map[JsString, JsArray],listing: JsValue): Map[JsString, JsArray]= {
    val optionproduct=prod.find(listing.as[JsObject])
    optionproduct match {
      case None => initial
      case Some(product) =>{
        val product_name = product.value("product_name").as[JsString]
        initial.updated(product_name, initial(product_name) :+ listing)
      }
    }
  }
  
  val result = listingJson.foldLeft(Map[JsString, JsArray]().withDefaultValue(JsArray()))(append)
  
  def constructJsObject(s:JsString, a: JsArray): JsValue = {
    JsObject(Seq(
        "product_name" -> s,
        "listings" -> a))
  }
  
  if (args.length == 2) {
    result.keys.map{x => println(constructJsObject(x, result(x)))}
  }
  else {
    val writer = new PrintWriter(new File(args(2)))
    result.keys.map{x => writer.println(constructJsObject(x, result(x)).toString)}
    writer.close()
  }
  
}
