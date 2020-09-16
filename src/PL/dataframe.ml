type _ df =
| EmptyDF : unit df
| JoinDF : 'a array * 'b df -> ('a * 'b) df

type (_, _) index =
| TOS : ('e * 'o, 'e) index
| NEXT : ('o, 'e) index -> ('tos * 'o, 'e) index

let rec get : type a e. (a, e) index -> a df -> e array =
    fun ind df ->
    match ind, df with
    | TOS, JoinDF(x, _) ->  x
    | NEXT ind, JoinDF(_, tl) -> get ind tl

type apply = {
    f : 'a. 'a array -> 'a array
}

let rec apply : type a. apply -> a df -> a df = fun {f} ->
    function
    | EmptyDF -> EmptyDF
    | JoinDF(hd, tl) -> JoinDF(f hd, apply {f} tl)
    
    
let x = TOS

let arr1 = JoinDF([|1|], EmptyDF)
let arr2 = JoinDF([|1.0|], EmptyDF)
let arr3 = JoinDF([|"a"|], arr2)

let take n xs =
    if Array.length xs < n then
        xs
    else let ys = Array.make n (Obj.magic 0) in
         for i = 0 to n - 1 do
            ys.(i) <- xs.(i)
         done;
         ys

let _ = print_endline @@ string_of_int @@ (get x arr1).(0)
let _ = print_endline @@ string_of_float @@ (get x arr2).(0)
let _ = print_endline @@ (get x arr3).(0)

let _ = print_endline @@ string_of_int @@ Array.length @@ get x @@ apply {f = fun x -> take 0 x} arr3
