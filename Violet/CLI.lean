def List.has (args : List String) (name : String) : Bool × List String :=
  match args with
    | [] => ⟨false, []⟩
    | x :: xs =>
      if x == name then ⟨true, xs⟩
      else
        let (b, rest) := xs.has name
        ⟨b, x :: rest⟩
def List.hasOr (args : List String) (names : List String) : Bool × List String := Id.run do
  for n in names do
    let (b, rest) := args.has n
    if b then
      return (b, rest)
  return (false, args)

def getOptionArg [Monad m] [MonadStateOf (List String) m] (abbrevs : List String) : m Bool := do
  let args ← get
  let (b, args') := args.hasOr abbrevs
  set args'
  return b
