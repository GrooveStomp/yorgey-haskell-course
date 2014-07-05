type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 p1 p2 _ = [(p1, p2)]
hanoi n p1 p2 p3 = hanoi (n - 1) p1 p3 p2 ++ [(p1, p2)] ++ hanoi (n - 1) p3 p2 p1
