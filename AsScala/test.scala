import scala.io.Source
import scala.collection.mutable
import math._
import scala.collection.LinearSeq
import scala.annotation.tailrec
object tools  {
	def sum(iter:LinearSeq[Double]):Double={
		var summary:Double = 0.0
		for( item:Double <- iter){
		 	summary+=item
	}
		summary
		}
	def sumWith(iter:LinearSeq[Double],kernel: Double=>Double):Double={
		var summary:Double = 0.0
		for( item:Double <- iter){
		 	summary+= kernel(item)
		}
		summary
		}
}

def toCSv(src:String, rowNum:Int,spliter:String=","): List[List[Any]]={
	val source= Source.fromFile(src)
	return source.getLines().map{ _.split(spliter).toList} .toList
}


class BaseStats{

	var Vars:Map[String,Any] = mutable.Map("title"->"","samples"->"","average"->"")
	
	def ocm(order:Int):List[Double]=
		( Vars("samples").asInstanceOf[List[List[Double]]] ).
			map( 
			( x:List[Double]) => tools.sumWith ( 
								x.map( (y:Double)=> y - Vars("average").asInstanceOf[Double] )
												, (value:Double)=> ( pow( value,order) ) 
												) / x.length
			 )  

}