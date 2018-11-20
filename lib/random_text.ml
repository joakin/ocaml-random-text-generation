(* type ltable = (string * string list) list *)

type distribution = {total: int; amounts: (string * int) list}

(* type htable = (string, distribution) Hashtbl.t *)

type ptable = {prefix_length: int; table: (string list, distribution) Hashtbl.t}

(* -- Part A -------------------------------------------------------------- *)

let add_word words buf =
  if Buffer.length buf > 0 then (
    words := Buffer.contents buf :: !words ;
    Buffer.reset buf )

(* 
let words str =
  let len = String.length str in
  let words = ref [] in
  let tmp = Buffer.create 16 in
  for i = 0 to len - 1 do
    let chr = str.[i] in
    let code = Char.code chr in
    if (code >= 65 && code <= 90) || (code >= 97 && code <= 122) then
      Buffer.add_char tmp chr
    else (* Ignore char *)
      add_word words tmp
  done ;
  add_word words tmp ;
  (* Add last captured word *)
  List.rev !words
*)

let start_c = "START"

let stop_c = "STOP"

(*
let add_next_word_to_table word next_word table =
  if List.exists (fun (w, _) -> w = word) table then
    List.map
      (fun (w, next_words) ->
        if w = word then (w, next_word :: next_words) else (w, next_words) )
      table
  else (word, [next_word]) :: table

let build_ltable (words : string list) : ltable =
  let f ((maybe_last, tbl) : string option * ltable) (word : string) :
      string option * ltable =
    let last = match maybe_last with Some w -> w | None -> start_c in
    (Some word, add_next_word_to_table last word tbl)
  in
  let (last, tbl) : string option * ltable =
    List.fold_left f (None, []) words
  in
  match last with Some l -> add_next_word_to_table l stop_c tbl | None -> tbl

let rec next_in_ltable table word =
  match table with
  | (w, next_words) :: tail ->
      if w = word then
        List.nth next_words (List.length next_words |> Random.int)
      else next_in_ltable tail word
  | [] -> raise Not_found

exception Stop_generating_words

let walk_ltable table =
  let rec iter words last =
    let word = next_in_ltable table last in
    if word = stop_c then List.rev words else iter (word :: words) word
  in
  iter [] start_c
*)

(* -- Part B -------------------------------------------------------------- *)

(* compute_distribution ["a";"b";"c";"b";"c";"a";"b";"c";"c";"c"] *)
let compute_distribution l =
  let total = List.length l in
  let sorted = List.sort compare l in
  let last, dist =
    List.fold_left
      (fun ((w, frq), res) word ->
        if w = word then ((w, frq + 1), res) else ((word, 1), (w, frq) :: res)
        )
      ((List.hd sorted, 0), [])
      sorted
  in
  {total; amounts= last :: dist}

let add_next_word_to_htable last next tbl =
  Hashtbl.replace tbl last
    (try next :: Hashtbl.find tbl last with Not_found -> [next])

(*
let build_htable words =
  let tbl = Hashtbl.create 100 in
  let f maybe_last word =
    let last = match maybe_last with Some w -> w | None -> start_c in
    add_next_word_to_htable last word tbl ;
    Some word
  in
  ( match List.fold_left f None words with
  | Some l -> add_next_word_to_htable l stop_c tbl
  | None -> () ) ;
  let newtbl = Hashtbl.create (Hashtbl.length tbl) in
  Hashtbl.iter (fun k v -> Hashtbl.add newtbl k (compute_distribution v)) tbl ;
  newtbl
*)

let next_in_htable table word =
  let {total; amounts} = Hashtbl.find table word in
  let r = Random.int total + 1 in
  let rec iter r amounts =
    match amounts with
    | [] -> raise Not_found
    | (w, n) :: xs ->
        let r' = r - n in
        if r' <= 0 then w else iter r' xs
  in
  iter r amounts

(*
let walk_htable table =
  let rec iter words last =
    let word = next_in_htable table last in
    if word = stop_c then List.rev words else iter (word :: words) word
  in
  iter [] start_c
*)

(* -- Part C -------------------------------------------------------------- *)

let add_sentence sentences sentence =
  if List.length !sentence > 0 then (
    sentences := List.rev !sentence :: !sentences ;
    sentence := [] )

let sentences str =
  let len = String.length str in
  let ss = ref [] in
  let sentence = ref [] in
  let tmp = Buffer.create 16 in
  for i = 0 to len - 1 do
    let chr = str.[i] in
    let code = Char.code chr in
    if
      (code >= 65 && code <= 90)
      (* Uppercase *)
      || (code >= 97 && code <= 122)
      (* Lowercase *)
      || (code >= 128 && code <= 255)
      || (* Other non roman letters *)
         (code >= 48 && code <= 57)
      (* Numbers *)
    then Buffer.add_char tmp chr
    else if
      chr = ';' || chr = ',' || chr = ':' || chr = '-' || chr = '"'
      || chr = '\'' || chr = '?' || chr = '!' || chr = '.'
    then (
      add_word sentence tmp ;
      (* Clear previous words, then add this standalone one *)
      Buffer.add_char tmp chr ;
      add_word sentence tmp ;
      if chr = '?' || chr = '!' || chr = '.' then
        (* Finish sentence *)
        add_sentence ss sentence )
    else
      (* Ignore char, maybe add prev word to sentence *)
      add_word sentence tmp
  done ;
  (* Add last captured word  and sentence*)
  add_word sentence tmp ;
  add_sentence ss sentence ;
  List.rev !ss

let rec start pl = if pl = 0 then [] else start_c :: start (pl - 1)

let shift l x = match l with _ :: xs -> xs @ [x] | [] -> [x]

let build_ptable (words : string list) (pl : int) =
  let table = Hashtbl.create 100 in
  let f prefix (word : string) =
    add_next_word_to_htable prefix word table ;
    shift prefix word
  in
  let last = List.fold_left f (start pl) words in
  let newtbl = Hashtbl.create (Hashtbl.length table) in
  add_next_word_to_htable last stop_c table ;
  Hashtbl.iter (fun k v -> Hashtbl.add newtbl k (compute_distribution v)) table ;
  {prefix_length= pl; table= newtbl}

let walk_ptable {table; prefix_length= pl} =
  let rec iter words last =
    let word = next_in_htable table last in
    if word = stop_c then List.rev words
    else iter (word :: words) (shift last word)
  in
  iter [] (start pl)

exception Unable_to_merge_ptable

let merge_lists l1 l2 =
  let l2_common, l2_other =
    List.partition (fun (w, _) -> List.exists (fun (w', _) -> w' = w) l1) l2
  in
  let l1_with_l2_common =
    List.map
      (fun (w, frq) ->
        try
          let _, frq' = List.find (fun (w', _) -> w' = w) l2_common in
          (w, frq + frq')
        with Not_found -> (w, frq) )
      l1
  in
  l1_with_l2_common @ l2_other

let merge_distributions d1 d2 =
  let amounts = merge_lists d1.amounts d2.amounts in
  {total= d1.total + d2.total; amounts}

let merge_ptable a b =
  if a.prefix_length <> b.prefix_length then raise Unable_to_merge_ptable
  else
    let table = Hashtbl.copy a.table in
    let merge k v =
      Hashtbl.replace table k
        (try merge_distributions v (Hashtbl.find table k) with Not_found -> v)
    in
    Hashtbl.iter merge b.table ;
    {prefix_length= a.prefix_length; table}

let merge_ptables = function
  | [t] -> t
  | t :: ts -> List.fold_left merge_ptable t ts
  | [] -> raise Unable_to_merge_ptable

(* Example usage, assuming a bunch of text in variables
 * -------

    let sauce_ptable =
      merge_ptables
        (List.map
          (fun s -> build_ptable s 2)
          ( sentences some_cookbook_sauce_chapter
          @ sentences grimms_travelling_musicians
          @ sentences grimms_cat_and_mouse_in_partnership
          @ sentences the_war_of_the_worlds_chapter_one
          @ sentences history_of_ocaml ))

    let _ = display_quote (walk_ptable sauce_ptable)
*)
