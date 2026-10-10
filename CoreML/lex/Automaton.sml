structure Automaton =
struct
  type S = string
  type Q = int
  type delta = (Q * (S * Q list) list) list
  type NFA = {Q:Q list, S:S list, delta:delta, q0:Q,
              F:Q list}
  type state = Q list
  type Delta = (state * (S * state) list) list
  type DFA = {Q:state list, S:S list, Delta:Delta,
              Q0:state, F:state list}

  (* the label of an epsilon transition *)
  val epsilon = ""

  fun member x l = List.exists (fn y => y = x) l

  (* a DFA state is kept as a sorted list without duplicates,
     so that two equal sets are equal as lists *)
  fun insert (x, nil) = [x]
    | insert (x, l as y::ys) =
      if x < y then x :: l
      else if x = y then l
      else y :: insert (x, ys)

  (* delta(q, s) *)
  fun move (delta:delta) (q, s) =
      case List.find (fn (q', _) => q' = q) delta of
        NONE => nil
      | SOME (_, trans) =>
        List.concat (map #2 (List.filter (fn (s', _) => s' = s) trans))

  (* Cl(qs): the epsilon closure of qs *)
  fun closure delta qs =
      let
        fun loop (visited, nil) = visited
          | loop (visited, q::rest) =
            if member q visited then loop (visited, rest)
            else loop (insert (q, visited), move delta (q, epsilon) @ rest)
      in
        loop (nil, qs)
      end

  (* delta_D(A, s) = Cl(the union of delta(q, s) for q in A) *)
  fun deltaD delta (A, s) =
      closure delta (List.concat (map (fn q => move delta (q, s)) A))

  fun addS delta (A, s) (Q1, Q2, Omega) =
      let
        val A' = deltaD delta (A, s)
        val Q1' = if member A' (A :: Q1 @ Q2) then Q1 else A' :: Q1
      in
        (Q1', (s, A') :: Omega)
      end

  fun addQ delta sigma A (Q1, Q2, Delta) =
      let
        val (Q1n, Omegan) =
            foldl (fn (s, (Q1i, Omegai)) => addS delta (A, s) (Q1i, Q2, Omegai))
                  (Q1, nil)
                  sigma
      in
        (Q1n, A :: Q2, (A, Omegan) :: Delta)
      end

  fun subsets delta sigma (nil, Q2, Delta) = (Q2, Delta)
    | subsets delta sigma (A :: Q1, Q2, Delta) =
      subsets delta sigma (addQ delta sigma A (Q1, Q2, Delta))

  fun toDFA ({Q, S, delta, q0, F}:NFA) : DFA =
      let
        val sigma = List.filter (fn s => s <> epsilon) S
        val A = closure delta [q0]
        val (QD, DeltaD) = subsets delta sigma ([A], nil, nil)
        val FD = List.filter (fn A => List.exists (fn q => member q F) A) QD
      in
        {Q = QD, S = sigma, Delta = DeltaD, Q0 = A, F = FD}
      end
end
