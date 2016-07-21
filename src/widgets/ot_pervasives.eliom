[%%shared.start]

let (@:) x xs = x :: xs

let (@?) x xs = match x with
  | None -> xs
  | Some x -> x :: xs
