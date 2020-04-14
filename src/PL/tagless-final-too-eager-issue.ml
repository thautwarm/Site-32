type 'repr sym = {
  lit : int -> 'repr;
  add : 'repr -> 'repr -> 'repr
}

type poly = {e : 'a . 'a sym -> 'a}

let lit : int -> poly = fun i ->
  { e = fun m -> m.lit i }

let add : poly -> poly -> poly = fun {e = l} {e = r} ->
  { e = fun m -> m.add (l m) (r m)}

let show = {
  lit = string_of_int;
  add = fun a b -> "(" ^ a ^ " + " ^ b ^ ")"
}

let eval: int sym = {
  lit = (fun a -> a);
  add = fun a b -> a + b
}

let not_lit: (unit -> unit) sym = {
  lit = (fun _ _ -> failwith "eager");
  add = fun _ _ _ -> ()
}

let run {e} = e

let _ =
    let i = add (lit 1) (lit 2) in
    print_endline @@ run i show;
    print_int @@ run i eval;
    run i not_lit
