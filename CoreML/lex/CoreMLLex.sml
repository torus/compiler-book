structure CoreMLLex =
struct
  exception LexError

  (* regular expressions over characters *)
  datatype regex
    = EPS
    | CHARS of char list
    | SEQ of regex * regex
    | ALT of regex * regex
    | STAR of regex

  val allChars = List.tabulate (Char.maxOrd + 1, chr)
  fun lit s = foldr (fn (c, r) => SEQ (CHARS [c], r)) EPS (explode s)
  fun range (a, b) = List.tabulate (ord b - ord a + 1, fn i => chr (ord a + i))
  fun notChars cs =
      CHARS (List.filter (fn c => not (List.exists (fn d => d = c) cs)) allChars)
  fun seqs rs = foldr SEQ EPS rs
  fun alts [r] = r
    | alts (r :: rs) = ALT (r, alts rs)
    | alts nil = CHARS nil
  fun plus r = SEQ (r, STAR r)
  fun opt r = ALT (r, EPS)

  (* token definitions *)
  val alpha = CHARS (range (#"A", #"Z") @ range (#"a", #"z"))
  val digit = CHARS (range (#"0", #"9"))
  val id = SEQ (alpha, STAR (ALT (alpha, digit)))
  val num = plus digit
  val frac = SEQ (lit ".", num)
  val exponent = seqs [CHARS [#"e", #"E"], opt (lit "~"), num]
  val real = SEQ (opt (lit "~"),
                  ALT (seqs [num, opt frac, exponent],
                       seqs [num, frac, opt exponent]))
  val ws = alts [lit " ", lit "\t", lit "\r\n", lit "\n", lit "\r"]

  (* the lexical rules, in priority order.
     an action returning NONE skips the matched text *)
  val rules : (regex * (string -> Token.token option)) list =
      [(seqs [lit "\"", STAR (notChars [#"\""]), lit "\""],
        fn s => SOME (Token.STRING (String.substring (s, 1, size s - 2)))),
       (lit "_", fn _ => SOME Token.UNDERBAR),
       (id, fn s => SOME (Token.ID s)),
       (real, fn s => SOME (Token.REAL s)),
       (ws, fn _ => NONE),
       (notChars [#"\n"], fn s => SOME (Token.SPECIAL s))]

  (* Thompson's construction.  n is the next unused NFA state.
     returns (start, final, next unused state, edges) *)
  fun build (r, n, edges) =
      case r of
        EPS => (n, n + 1, n + 2, (n, Automaton.epsilon, n + 1) :: edges)
      | CHARS cs =>
        (n, n + 1, n + 2, map (fn c => (n, str c, n + 1)) cs @ edges)
      | SEQ (r1, r2) =>
        let
          val (s1, f1, n1, e1) = build (r1, n, edges)
          val (s2, f2, n2, e2) = build (r2, n1, e1)
        in
          (s1, f2, n2, (f1, Automaton.epsilon, s2) :: e2)
        end
      | ALT (r1, r2) =>
        let
          val (s, f) = (n, n + 1)
          val (s1, f1, n1, e1) = build (r1, n + 2, edges)
          val (s2, f2, n2, e2) = build (r2, n1, e1)
        in
          (s, f, n2,
           (s, Automaton.epsilon, s1) :: (s, Automaton.epsilon, s2)
           :: (f1, Automaton.epsilon, f) :: (f2, Automaton.epsilon, f) :: e2)
        end
      | STAR r1 =>
        let
          val (s, f) = (n, n + 1)
          val (s1, f1, n1, e1) = build (r1, n + 2, edges)
        in
          (s, f, n1,
           (s, Automaton.epsilon, s1) :: (s, Automaton.epsilon, f)
           :: (f1, Automaton.epsilon, s1) :: (f1, Automaton.epsilon, f) :: e1)
        end

  (* turn a list of edges (q, s, q') into Automaton.delta *)
  fun addTrans (s, q', nil) = [(s, [q'])]
    | addTrans (s, q', (s', qs) :: rest) =
      if s = s' then (s', q' :: qs) :: rest
      else (s', qs) :: addTrans (s, q', rest)
  fun addEdge ((q, s, q'), nil) = [(q, [(s, [q'])])]
    | addEdge ((q, s, q'), (p, trans) :: rest) =
      if p = q then (p, addTrans (s, q', trans)) :: rest
      else (p, trans) :: addEdge ((q, s, q'), rest)

  (* the NFA of all rules: state 0 has an epsilon edge to the start of
     each rule, and finals records which rule each final state belongs to *)
  val (numStates, edges, finals, _) =
      foldl (fn ((r, _), (n, edges, finals, i)) =>
                let val (s, f, n', edges') = build (r, n, edges)
                in (n', (0, Automaton.epsilon, s) :: edges', (f, i) :: finals, i + 1)
                end)
            (1, nil, nil, 0)
            rules

  val nfa : Automaton.NFA =
      {Q = List.tabulate (numStates, fn i => i),
       S = map str allChars,
       delta = foldl addEdge nil edges,
       q0 = 0,
       F = map #1 finals}

  val dfa = Automaton.toDFA nfa

  (* number the DFA states 0, 1, ...; the empty state (dead state) is ~1 *)
  val states = List.filter (fn A => not (null A)) (#Q dfa)
  val numDStates = length states
  fun indexOf nil = ~1
    | indexOf A =
      let
        fun find (_, nil) = raise Fail "CoreMLLex: unknown DFA state"
          | find (i, B :: rest) = if A = B then i else find (i + 1, rest)
      in
        find (0, states)
      end

  val numChars = Char.maxOrd + 1

  (* table[i * numChars + ord c] is the state after reading c in state i *)
  val table = Array.array (numDStates * numChars, ~1)
  val _ =
      app (fn (A, omega) =>
              if null A then ()
              else
                let val i = indexOf A
                in app (fn (s, A') =>
                           Array.update (table,
                                         i * numChars + ord (String.sub (s, 0)),
                                         indexOf A'))
                       omega
                end)
          (#Delta dfa)

  (* accept[i] is the rule accepted in state i (the earliest one if
     there are several), or ~1 *)
  val accept =
      Vector.fromList
        (map (fn A =>
                 foldl (fn ((f, i), r) =>
                           if List.exists (fn q => q = f) A
                              andalso (r < 0 orelse i < r)
                           then i else r)
                       ~1
                       finals)
             states)

  val actions = Vector.fromList (map #2 rules)
  val start = indexOf (#Q0 dfa)

  fun makeLexer input =
      let
        (* the input read so far; the current token starts at index 0 *)
        val buf = ref ""
        val atEOF = ref false
        fun available p =
            if p < size (!buf) then true
            else if !atEOF then false
            else case input 1024 of
                   "" => (atEOF := true; false)
                 | s => (buf := !buf ^ s; available p)

        (* run the DFA from position p and return the longest match
           as SOME (rule, length) *)
        fun scan (state, p, last) =
            if not (available p) then last
            else
              let
                val next = Array.sub (table,
                                      state * numChars + ord (String.sub (!buf, p)))
              in
                if next < 0 then last
                else
                  let val r = Vector.sub (accept, next)
                  in scan (next, p + 1, if r >= 0 then SOME (r, p + 1) else last)
                  end
              end

        fun lex () =
            case scan (start, 0, NONE) of
              NONE => if available 0 then raise LexError else Token.EOF
            | SOME (rule, len) =>
              let
                val text = String.substring (!buf, 0, len)
                val _ = buf := String.extract (!buf, len, NONE)
              in
                case Vector.sub (actions, rule) text of
                  SOME token => token
                | NONE => lex ()
              end
      in
        lex
      end
end
