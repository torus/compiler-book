structure TM =
struct
  datatype D = R | L
  datatype S = B | I | O
  datatype Q = M | H | W
  datatype A = Move of D | Write of S

  type delta = ((Q * S) * (Q * A)) list
  type program = Q * delta
  type tape = S list * S * S list
  val P = (M, [(* ((M, I), (M, O, L)), *)
               ((M, I), (W, Write O)),
               ((W, O), (M, Move L)),

               (* ((M, O), (H, I, L)), *)
               ((M, O), (H, Write I)),

               (* ((M, B), (H, I, L)) *)
               ((M, B), (H, Write I))])
end
