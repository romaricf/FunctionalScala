object w1 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(55); 
  println("Welcome to the Scala worksheet");$skip(48); 
  
  val a = Empty.incl(5).union(Empty.incl(1));System.out.println("""a  : IntSet = """ + $show(a ));$skip(48); val res$0 = 
  
  (new NonEmpty(7, Empty, Empty)) contains 7;System.out.println("""res0: Boolean = """ + $show(res$0))}
}


abstract class IntSet {
def incl(x: Int): IntSet
def contains(x: Int): Boolean
def union(other: IntSet): IntSet
}


object Empty extends IntSet {
	def contains(x: Int): Boolean = false
	def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
	def union(other: IntSet): IntSet = other
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

	def contains(x: Int): Boolean =
		if (x < elem) left contains x
		else if (x > elem) right contains x
		else true
		
	def incl(x: Int): IntSet =
		if (x < elem) new NonEmpty(elem, left incl x, right)
		else if (x > elem) new NonEmpty(elem, left, right incl x)
		else this
	
	def union(other: IntSet): IntSet = {
		other match {
			case Empty => this
			case o: NonEmpty => {
				if(o.contains(elem))
					right.union(left.union(other))
				else
					right.union(left.union(other.incl(elem)))
			}
			case _ => throw new ClassCastException
		}
	}
}
