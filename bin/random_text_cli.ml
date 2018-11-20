open Lib

let () =
  let ptable =
    Random_text.merge_ptables
      (List.map
         (fun s -> Random_text.build_ptable s 2)
         ( Random_text.sentences Miss_nume_of_japan.text
         @ Random_text.sentences Alices_adventures_in_wonderland.text
         @ Random_text.sentences Dracula.text
         @ Random_text.sentences Frankenstein.text
         @ Random_text.sentences Pride_and_prejudice.text ))
  in
  Random.self_init () ;
  for _ = 0 to 10 do
    print_endline @@ String.concat " " @@ Random_text.walk_ptable ptable ;
    print_endline ""
  done
