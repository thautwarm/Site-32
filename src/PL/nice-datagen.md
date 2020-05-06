# Design for A "Nice" Test Data Generator

```ocaml
module Spec = struct
type 'a iter = {run : unit -> 'a option}
let take n {run} =
    let rec take n {run} init =
        if n <= 0 then init
        else
        let n = n - 1 in
        match run() with
        | Some elt -> take n @@ elt:init
        | _        -> List.rev init

type _ spec =
    | Gen    : (() -> 't) -> 't spec
    | Modify : ('t -> 't) * 't spec -> 't spec
    | Rep    : 't spec -> 't iter spec
    | App    : ('a -> 't) spec * 'a spec -> 't spec
    | Lst    : 'a spec list -> 'a list spec
    | Map    : ('a -> 'b) * 'a spec -> 'b spec
    | Or     : 'a spec * 'a spec -> 'a spec

let generate = 
    let rec (!) : 'a spec -> 'a =
        function
        | Gen run -> run()
        | Modify(by, spec) -> by !spec
        | Rep spec -> {run = fun () -> !spec}
        | App(fm, argm) -> !fm !argm
        | Lst xs -> List.map (!) xs
        | Map(f, m) -> f !m
        | Or(m1, m2) ->
            !match Random.(bool()) with
            | true -> m1
            | false -> m2
    in (!)

let int : int spec =
    Gen

end
```