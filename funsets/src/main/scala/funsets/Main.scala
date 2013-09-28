package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  
  printSet(singletonSet(30))
  
  val firstUnion = union(singletonSet(3), singletonSet(10))
  val secondUnion = union(firstUnion, singletonSet(5))
  val finalUnionSet = union(secondUnion, singletonSet(8))

  printSet(finalUnionSet)
  
  printSet(filter(finalUnionSet, x => x % 2 == 0))
  
  printSet(diff(Set(1, 3, 5, 7, 1000), Set(1,2,3,4)))
  
  printSet(diff(Set(1,2,3,4), Set(-1000,0)))
}
