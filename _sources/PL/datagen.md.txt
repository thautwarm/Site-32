Suppose we want to generate runtime data for purposes such as testing.

```ocaml
type 'a iter = {run : unit -> 'a option}

let take : 'a. int -> 'a iter -> 'a list = fun n {run} ->
    let res, i, finished = ref [], ref 0, ref false in
    while not !finished && !i < n do
        match run() with
        | Some x -> res := x :: !res; incr i
        | None   -> finished := true
    done;    
    List.rev !res

take 5 @@ {run = fun () -> Some (Random.int 16)};;
- : int list = [3; 0; 4; 9; 1]
```

However above code does not suffice generating structural data:

```ocaml
type s1 = S1_a of int | S2_b of int * float
type s2  = S2_a of int | S2_b of string * s1

(* generating a sequence of s2 ? *)
```

The idea of generating structural data is **recursively generating their components**.

We can make a **constructive way** to do this conveniently, with the need of Generalised Algebraic Data Types(GADTs).

```ocaml
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
        | Or(m1, m2)    ->
            !(if Random.(bool()) then m1 else m2)
    in (!)
```

The cool of part of the above design is its **composability**:

```ocaml
let int n: int spec =
    Gen (fun () -> Random.(int @@ n))

type c = C1 of int | C2 of int
let c1 x = C1 x
let c2 x = C2 x
let c = Gen (fun () -> if Random.bool() then c1 else c2)
generate @@ App(c, int 2)
(* - : c = C2 1 *)
generate @@ App(c, int 2);;
(* - : c = C1 1 *)
generate @@ App(c, int 2);;
(* - : c = C1 0 *)
# generate @@ Lst [ App(c, int 2); App(c, int 50)];;
(* - : c list = [C2 1; C1 1] *)
```

The key point is the type relationships built by GADTs. Without GADTs or other advanced types, you just cannot express this.

```ocaml
type _ spec =
| Lst    : 'a spec list -> 'a list spec
```

Above code shows the specification of building a list data generator from its element generators.