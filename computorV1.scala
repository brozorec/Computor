import scala.collection.mutable.Map
import scala.util.matching.Regex

case class Member(sign: String, coef: String, x: String, degree: String)

object computor {

	val myMap = Map[String, Float]("0" -> 0, "1" -> 0, "2" -> 0)
	val df = new java.text.DecimalFormat("#####.######")

	def main(args: Array[String]) = {
		if ( args.size != 1 ) {
			println("Computor takes only one argument.")
			System.exit(0)
		}

		val whole = args(0).split("=")
		if ( whole.size != 2 ) {
			println("Invalid argument. Ex: X^2 + 2 * X + 1 = 0")
			System.exit(0)
		}
		val Pattern = ("""([+-]|\A)\s*(\d+.\d+|\d+)?\s*\Q*\E?\s*(X)?\s*(?:\Q^\E\s*(\d+))?\s*""").r

		for ( side <- whole ) {
			val trimmed = side.trim
			val (dup1, dup2) = (Pattern findAllMatchIn trimmed).duplicate
			errorParsing(dup1, trimmed)
			for ( Pattern(sign, coef, x, degree) <- dup2 ) {
				if (side == whole.head)
					transform( Member(sign, coef, x, degree), _ + _ )
				else
					transform( Member(sign, coef, x, degree), _ - _ )
			}
		}

		reducedForm(myMap)
		polynomialDegree(myMap)
		val disc = calcDiscriminant(myMap)
		val mySome = Some(myMap("2"), myMap("1"), myMap("0"))
		solutions(mySome, disc)
	}

	def transform(member: Member, f: (Float, Float) => Float) = member match {
		case Member(sign, coef, null, null) => fillMap("0", (sign + coef).toFloat)(f)
		case Member(sign, null, x, null) => fillMap("1", (sign + "1").toFloat)(f)
		case Member(sign, null, x, degree) => fillMap(degree, (sign + "1").toFloat)(f)
		case Member(sign, coef, x, null) => fillMap("1", (sign + coef).toFloat)(f)
		case Member(sign, coef, x, degree) => fillMap(degree, (sign + coef).toFloat)(f)
		case _ => println("error"); System.exit(0)
	}

	def fillMap(degree: String, coef: Float)(f: (Float, Float) => Float) {
		if ( !myMap.contains(degree) )
			myMap += (degree -> coef)
		else
			myMap += ( degree -> f(myMap(degree), coef) )
	}

	def solutions(mySome: Option[(Float, Float, Float)], disc: Float) = mySome match{
		case Some((0, 0, 0)) => println("Every real number is a solution."); System.exit(0)
		case Some((0, 0, _)) => println("There's no solution."); System.exit(0)
		case Some((a, b, c)) if disc >= 0 => solve(a, b, c, disc)
	}

	def solve(a: Float, b: Float, c: Float, disc: Float) {
		if ( disc == 0 && a != 0) {
			val res = -(b / (2 * a))
			if (res != 0)
				println( df.format(res) )
			else
				println("0")
		}
		else if ( a != 0 && disc > 0) {
			val discSqrt = sqrt(disc)
			if (-b - discSqrt == 0)
				println("0")
			else
				println( df.format((-b - discSqrt) / (2 * a)) )

			if (-b + discSqrt == 0)
				println("0")
			else
				println( df.format((-b + discSqrt) / (2 * a)) )
		}
		else
			println( df.format(-c / b) )
	}

	def reducedForm(myMap: Map[String, Float]) {
		print("Reduced form: ")
		var check = 0
		for ( (key, value) <- myMap ) {
			Some((key, value, check)) match {
				case Some((key, value, _)) if value == 0 => print("")
				case Some((key, value, _)) if value < 0 => print("- " + df.format(abs(value)))
				case Some((key, value, check)) if value > 0 && check > 0 => print("+ " + df.format(abs(value)))
				case _ => print(df.format(abs(value)))
			}
			Some((key, value, check)) match {
				case Some((key, value, _)) if value == 0 => print("")
				case Some((key, value, _)) if key == "0" =>  print( " "); check = 1
				case Some((key, value, _)) if key == "1" =>  print( " * X "); check = 1
				case Some((key, value, _)) =>  print( " * X^" + key + " "); check = 1
				case _ => print(" "); check = 1
			}
		}
		if ( check == 0 )
			println("0 = 0")
		else
			print("= 0\n")
	}

	def polynomialDegree(myMap: Map[String, Float]) {
		var degree = 0
		for ( (key, value) <- myMap ) {
			val keyInt = key.toInt
			if (value != 0 && keyInt > degree)
				degree = keyInt
		}
		println("Polynomial degree: " + degree)
		if (degree > 2) {
			println("The polynomial degree is stricly greater than 2, I can't solve.")
			System.exit(0)
		}
	}

	def calcDiscriminant(myMap: Map[String, Float]): Float = {
		val disc = myMap("1") * myMap("1") - 4 * myMap("2") * myMap("0")
		if (disc < 0) {
			println("Discriminant is strictly negative.\nI can't solve.")
			System.exit(0)
		}
		else if (disc > 0 && myMap("2") != 0)
			println("Discriminant is strictly positive.\nThe two solutions are:")
		else
			println("The solution is:")
		disc
	}

	def abs(num: Float) = if (num < 0) -num else num


	def sqrt(num: Float): Float = {

		def sqrtX(num: Float, guess: Float): Float = {
			if ( guess * guess == num ) guess
			else if (guess * guess > num) 0
			else sqrtX(num, guess + 1)
		}

		def sqrtY(num: Float, guess: Float): Float = {
			if ( abs(guess * guess - num) / num < 0.0001 ) guess
			else sqrtY( num, (guess + num / guess) / 2 )
		}
		val a = sqrtX(num, 1)
		if (a != 0) a
		else sqrtY(num, num / 2)
	}

	def errorParsing(matchIter: Iterator[Regex.Match], side: String) {
		val check = matchIter.mkString
		if (side != check)
		{
			println("Parsing Error. Ex: X^2 + 2 * X + 1 = 0")
			System.exit(1)
		}
	}
}

// 9 * X^2 - 9 * X = 0
// 9.3 * X^2 = 0 ok
// - 9.3 * X^2 + 8 * X = 0
