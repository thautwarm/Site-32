module Spec = struct
  
type 'a iter = {run : unit -> 'a option}

let take' : 'a. int -> 'a iter -> 'a list = fun x ->
    let rec take' : 'a. 'a list -> int -> 'a iter -> 'a list =
        fun init n {run} ->
        if n <= 0 then List.rev init
        else
        let n = n - 1 in
        match run() with
        | Some elt -> take' (elt::init) n {run}
        | _        -> List.rev init
    in take' [] x

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
        | Gen run -> run()
        | Rep spec -> {run = fun () -> Some(!spec)}
        | App(fm, argm) -> !fm !argm
        | Lst ys -> List.map (!) ys
        | Map(f, m) -> f !m
        | Or(m1, m2) -> !begin
            match Random.(bool()) with
            | true -> m1
            | false -> m2
        end
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
      else
      let rec pow2 res = function
        | 0 -> res
        | n -> pow2 (res * 2) (n - 1)
      in pow2 1 k
  let int bit: int spec = Gen (fun () -> Random.(int @@ pow2 bit))
  let char : char spec =  Map(Char.chr, int 8)
  let string : string spec =
      let fm = lift take' @@ int 3 in
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

(**
# open Spec;;
# take' 2 @@ generate spec0;;
- : (int list * string * string) list =
[([205; 118], "E", "(�\017�"); ([194; 129], "\015k �4", "\\A�T�yy")]
*)