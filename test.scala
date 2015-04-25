import scala.collection.mutable.Map
import scala.util.matching.Regex

case class Member(sign: String, coef: String, x: String, degree: String)

val whole = args(0).split("=")
if (whole.size != 2)
	System.exit(0)

val myMap = Map[String, Float]("0" -> 0, "1" -> 0, "2" -> 0)

val Pattern = ("""([+-]|\A)\s*(\d+.\d+|\d+)?\s*\Q*\E?\s*(X)?\s*(?:\Q^\E\s*([012]))?\s*""").r

for ( side <- whole ) {
	val trimmed = side.trim
	val (dup1, dup2) = (Pattern findAllMatchIn trimmed).duplicate
	errorCheck(dup1, trimmed)
	for ( Pattern(sign, coef, x, degree) <- dup2 ) {
		val member = Member(sign, coef, x, degree)
		if (side == whole.head)
			transform(member, _ + _)
		else
			transform(member, _ - _)
	}
}

// myMap foreach println
val disc = calcDiscriminant(myMap)
// println(disc)
val mySome = getSome(myMap)
// println(mySome.mkString)
solutions(mySome, disc)
// res foreach println

def solve(a: Float, b: Float, c: Float, disc: Float) {
	println((-b - disc)/(2 * a))
	println((-b + disc)/(2 * a))
}

def solutions(mySome: Option[(Float, Float, Float)], disc: Float) = mySome match{
	case Some(_) if disc < 0 => println("noSolution")
	case Some((0, 0, 0)) => println("everything")
	case Some((0, 0, _)) => println("noSolution")
	case Some((0, b, c)) => solve(0, b, c, disc)
	case Some((a, b, c)) if disc == 0 => solve(a, b, c, disc)
	case Some((a, b, c)) if disc > 0 => solve(a, b, c, disc)
}

def getSome(myMap: Map[String, Float]): Option[(Float, Float, Float)] = {
	Some(myMap("2"), myMap("1"), myMap("0"))
}

def transform(member: Member, f: (Float, Float) => Float) = member match {
	case Member(sign, coef, null, null) => fillMap("0", (sign + coef).toFloat)(f)
	case Member(sign, null, x, null) => fillMap("1", (sign + "1").toFloat)(f)
	case Member(sign, null, x, degree) => fillMap(degree, (sign + "1").toFloat)(f)
	case Member(sign, coef, x, null) => fillMap("1", (sign + coef).toFloat)(f)
	case Member(sign, coef, x, degree) => fillMap(degree, (sign + coef).toFloat)(f)
	case _ => println("error")
}

def fillMap(degree: String, coef: Float)(f: (Float, Float) => Float) {
		myMap += ( degree -> f(myMap(degree), coef) )
}

def calcDiscriminant(myMap: Map[String, Float]): Float = {
	myMap("1") * myMap("1") - 4 * myMap("2") * myMap("0")
}

def errorCheck(matchIter: Iterator[Regex.Match], side: String) {
	val check = matchIter.mkString
	// println(check)
	// matchIter foreach println
	// println(side)
	if (side != check)
	{
		println("error")
		System.exit(1)
	}
}
