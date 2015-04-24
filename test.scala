import scala.collection.mutable.Map
import scala.util.matching.Regex
import scala.collection.mutable.StringBuilder

case class Member(sign: String, coef: String, x: String, degree: String)

val whole = args(0).split("=")

val myMap = Map[String, Float]()

val Pattern = ("""([+-]|\A)\s*(\d+.\d+|\d+)?\s*\Q*\E?\s*(X)?\s*(?:\Q^\E\s*([012]))?\s*""").r

for ( side <- whole ) {
	val (dup1, dup2) = (Pattern findAllMatchIn side.trim).duplicate
	errorCheck(dup1, side)
	for ( Pattern(sign, coef, x, degree) <- dup2 ) {
		val member = Member(sign, coef, x, degree)
		if (side == whole.head)
			transform(member, (z, y) => z + y)
		else
			transform(member, (z, y) => z - y)
	}
}

myMap foreach println

def transform(member: Member, f: (Float, Float) => Float) = member match {
	case Member(sign, coef, null, null) => fillMap("0", (sign + coef).toFloat)(f)
	case Member(sign, coef, x, null) => fillMap("1", (sign + coef).toFloat)(f)
	case Member(sign, coef, x, degree) => fillMap(degree, (sign + coef).toFloat)(f)
	case _ => println("error")
}

def fillMap(degree: String, coef: Float)(f: (Float, Float) => Float) {
	if ( !myMap.contains(degree) )
		myMap += ( degree -> coef )
	else
		myMap += ( degree -> f(myMap(degree), coef) )
}

def errorCheck(matchIter: Iterator[Regex.Match], side: String) {
	val check: String = matchIter.mkString
	println(check)
	println(side)
	if (side !== check)
	{
		println("error")
		System.exit(1)
	}
}
