type ptable

val sentences : string -> string list list

val build_ptable : string list -> int -> ptable

val walk_ptable : ptable -> string list

exception Unable_to_merge_ptable

val merge_ptables : ptable list -> ptable
