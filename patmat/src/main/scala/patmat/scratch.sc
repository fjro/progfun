import patmat.Huffman._

val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)

//t1.weight
//t1.chars

//t2.weight
//t2.chars

//Huffman.times(List('a', 'b', 'a'))

//println("Encode t1: ")
//encode(t1)("ab".toList)
//
//println("Decode t1 ")
//decode(t2, List(0,1))

//Huffman.decodedSecret

//BCD
val cd = Fork(Leaf('C', 1), Leaf('D', 1), List('C', 'D'), 2)
val ef = Fork(Leaf('E', 1), Leaf('F', 1), List('E', 'F'), 2)
val gh =Fork(Leaf('G', 1), Leaf('H', 1), List('G', 'H'), 2)

val bcd = Fork(Leaf('B', 1), Fork(Leaf('C', 1), Leaf('D', 1), List('C', 'D'), 2), List('B', 'C', 'D'), 3)
val efgh = Fork(ef, gh, List('E', 'F', 'G', 'H'), 4)
val bcdefgh = Fork(bcd, efgh, List('B', 'C', 'D', 'E', 'F', 'G', 'H'), 9)
val all = Fork(Leaf('A', 8), bcdefgh, List('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'), 17)

//println("A = " )
//encode(all)("D".toList)
//
//encode(cd)("C".toList)
//encode(cd)("D".toList)
//
//encode(bcd)("B".toList)
//encode(bcd)("C".toList)
//encode(bcd)("D".toList)
//
//encode(efgh)("E".toList)
//encode(efgh)("F".toList)
//encode(efgh)("G".toList)
//encode(efgh)("H".toList)

//decode(all, List(1,0,0,0,1,0,1,0))
//decode(all, List(1,0,1,1,0))

createCodeTree(List('A', 'B', 'C'))
//singleton(List(all, bcd))
//
//combine(List(Leaf('B', 1), Leaf('C', 1)))
//
//combine(List(cd, ef, gh))

