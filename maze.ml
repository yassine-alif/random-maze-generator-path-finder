open Grid


let dump_file f=
  let rec aux in_c=
    try
      let tmp=input_line in_c in 
      Printf.printf "%s \n" tmp ;
      aux in_c
    with
      End_of_file -> Printf.printf "\n"
  in
  aux f
;;

let string_to_char_array s =
  s
  |> String.to_seq
  |> Seq.filter (fun c -> c <> '\n')
  |> Array.of_seq
;;

let read_file in_c =
  let rec loop acc =
    match input_line in_c with
    s -> loop ((string_to_char_array s) :: acc)
    | exception End_of_file -> List.rev acc 
  in
  Array.of_list (loop [])
;;


  let gestionLigneCommande  =
  match (Array.to_list Sys.argv) with
      [ _; "--help" ] ->  Printf.printf "./maze.exe print fichier.laby : affiche un message Affichage du labyrinthe à faire!\n./maze.exe solve fichier.laby x y : Résout un labyrinthe du dossier test de x*y!\n./maze.exe random x y : Résoult un labyrinthe aléatoire!\n";
      | [ _; "print"; fichier; n; m ] ->  let t = read_file (open_in fichier) in
                                          let g = convertisseur t (int_of_string(n))  (int_of_string(m)) in
                                          affiche_grille g;
      |[_; "init";n;m]                -> let g = grille_vide (int_of_string(n))  (int_of_string(m)) in
                                         affiche_grille g;
      | [ _; "solve" ; fichier; n; m ]->  let t = read_file (open_in fichier) in
                                          let g = convertisseur t (int_of_string(n))  (int_of_string(m)) in
                                          resolution g;
                                          grille_resolue g;
                                          affiche_grille g;
      | [ _; "random" ; n; m]         ->  let g = generate (int_of_string(n))  (int_of_string(m)) in
                                          affiche_grille g;
                                          resolution g;
                                          grille_resolue g;
                                          affiche_grille g;
      | [ _; "print";"--pretty"; fichier; n; m ] ->   let t = read_file (open_in fichier) in
                                                      let g = convertisseur t (int_of_string(n))  (int_of_string(m)) in
                                                      affiche_grille_complex g;
      | [ _; "solve" ;"--pretty"; fichier; n; m ]->   let t = read_file (open_in fichier) in
                                                      let g = convertisseur t (int_of_string(n))  (int_of_string(m)) in
                                                      resolution g;
                                                      grille_resolue g;
                                                      affiche_grille_complex g;
      | [ _; "random" ;"--pretty"; n; m]         ->   let g = generate (int_of_string(n))  (int_of_string(m)) in
                                                      affiche_grille_complex g;
                                                      resolution g;
                                                      grille_resolue g;
                                                      affiche_grille_complex g;
      | _ -> failwith("ligne de commande invalide")
;;