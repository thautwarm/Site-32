module Spec = struct
  
type 'a iter = {run : unit -> 'a option}


let take : 'a. int -> 'a iter -> 'a list = fun n {run} ->
    let res, i, finished = ref [], ref 0, ref false in
    while not !finished && !i < n do
        match run() with
        | Some x -> res := x :: !res; incr i
        | None   -> finished := true
    done;    
    List.rev !res

type _ spec =
    | Gen    : (unit -> 't) -> 't spec
    | Rep    : 't spec -> 't iter spec
    | App    : ('a -> 't) spec * 'a spec -> 't spec
    | Lst    : 'a spec list -> 'a list spec
    | Map    : ('a -> 'b) * 'a spec -> 'b spec
    | Or     : 'a spec * 'a spec -> 'a spec

let generate = 
    let rec (!) : type a. a spec -> a =
        function
        | Gen run       -> run()
        | Rep spec      -> {run = fun () -> Some(!spec)}
        | App(fm, argm) -> !fm !argm
        | Lst ys        -> List.map (!) ys
        | Map(f, m)     -> f !m
        | Or(m1, m2)    -> !(if Random.(bool()) then m1 else m2)
    in (!)
  
  let create_string : char list -> string = fun chs ->
        let buf = Buffer.create @@ List.length chs in
        let _ = List.iter (Buffer.add_char buf) chs
        in Buffer.contents buf

  let lift : ('a -> 'b) -> 'a spec -> 'b spec =
             fun f      -> fun b   ->
             let run () =
                f @@ generate b
             in Gen run

  let pow2 k =
      if k < 0 then failwith "invalid arg"
      else let rec pow2 res = function
            | 0 -> res
            | n -> pow2 (res * 2) (n - 1)
           in pow2 1 k

  let int nbyte: int spec = Gen (fun () -> Random.(int @@ 1 lsl nbyte))

  let char : char spec =  Map(Char.chr, int 8)

  let string : string spec =
      let fm = lift take @@ int 3 in
      let argm = Rep char in
      let charlistm = App(fm, argm)
      in Map(create_string,  charlistm)

  let tuple2 : ('a spec * 'b spec) -> ('a * 'b) spec = fun (l, r) ->
      let (!) = generate in 
      let run () = (!l, !r)
      in Gen run

  let tuple3 : ('a spec * 'b spec * 'c spec) -> ('a * 'b * 'c) spec = fun (l, m, r) ->
      let (!) = generate in 
      let run () = (!l, !m, !r)
      in Gen run
  
  let spec0 = Rep (tuple3(Lst [int 8; int 8], string, string))
end

open Spec;;
take 20 @@ generate @@ Rep(int 4);;
(* take 3 @@ generate @@ spec0;; *)

(* - : (int list * string * string) list =
[([205; 118], "E", "(�\017�"); ([194; 129], "\015k �4", "\\A�T�yy")] *)
