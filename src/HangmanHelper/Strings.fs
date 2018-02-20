module public HangmanHelper.Strings

let uppercase (s: string) = s.ToUpperInvariant()
let trim (s: string) = s.Trim()
let sameLength s = String.length >> (=) (String.length s)
let sameStart s1 s2 = Seq.forall2 (=) s1 s2
let sameEnd s1 s2 = sameStart (Seq.rev s1) (Seq.rev s2)