structure TM =
struct
  datatype D = R | L
  datatype S = B | I | O
  datatype Q = M | H | A | AA
  type delta = ((Q * S) * (Q * S * D)) list
  type program = Q * delta
  type tape = S list * S * S list
  val P = (AA,
              [((M, I), (M, O, L)),
               ((M, O), (H, I, L)),
               ((M, B), (H, I, L)),

               ((AA, B), (A, B, R)),

               ((A, B), (M, B, L)),
               ((A, I), (A, I, R)),
               ((A, O), (A, O, R)),

               ((H, I), (H, I, L)),
               ((H, O), (H, O, L))
          ])
end
