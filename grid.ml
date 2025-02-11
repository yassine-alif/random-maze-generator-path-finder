open Random;;
Random.self_init ();;

type situation = V | MH | MV | C | E | S | Chemin ;;
type case = {l: int ; c: int ; mutable s: situation ; mutable dis_S: int };;
type grille ={t: case array array; nb_l : int; nb_c: int};;

let grille_vide l c =
  let nbLignes= l*2+1 in
  let nbColonnes= c*2+1 in
  let t1 = Array.init nbLignes (fun i ->
    Array.init nbColonnes (fun k ->
      if (i mod 2=0 && k mod 2=0) then
        {l=i; c=k; s=C; dis_S= -1}
      else if (k mod 2 = 0) then
        {l=i; c=k; s=MV; dis_S= -1}
      else if (i mod 2 = 0) then
        {l=i; c=k; s=MH; dis_S= -1}
      else 
        {l=i; c=k; s=V; dis_S= -1}
    )
  ) 
  in
  {t= t1 ;nb_l=nbLignes;nb_c=nbColonnes}
;;


let affiche_situation s=
  match s with
     V  -> Printf.printf " "
    |MH -> Printf.printf "-"
    |MV -> Printf.printf "|"
    |C  -> Printf.printf "+"
    |E  -> Printf.printf "E"
    |S  -> Printf.printf "S"
    |Chemin -> Printf.printf "."
;;


let affiche_case c=
  affiche_situation c.s 
;;

let affiche_ligne l=
Array.iter (fun c -> affiche_case c ) l;
Printf.printf "\n"
;;

let affiche_grille g =
  Array.iter (fun l-> affiche_ligne l ) g.t
;;

let affiche_case_complex c nb_l nb_c=
  match c.s with
     V  -> Printf.printf " "
    |MH -> Printf.printf "━"
    |MV -> Printf.printf "┃"
    |C  -> if c.l=0 && c.c=0 then
            Printf.printf "┏"
          else if c.l=(nb_l-1) && c.c=0 then
            Printf.printf "┗"
          else if c.l=0 && c.c=(nb_c-1) then
            Printf.printf "┓"
          else if c.l=(nb_l-1) && c.c=(nb_c-1) then
            Printf.printf "┛"
          else if c.c=0 then
            Printf.printf "┣"
          else if c.c=(nb_c-1) then
            Printf.printf "┫"
          else if c.l=0 then
            Printf.printf "┳"
          else if c.l=(nb_l-1) then
            Printf.printf "┻"
          else
            Printf.printf "╋"
    |E  -> Printf.printf "E"
    |S  -> Printf.printf "S"
    |Chemin -> Printf.printf "."
;;

let affiche_ligne_complex l nb_l nb_c=
Array.iter (fun c -> affiche_case_complex c nb_l nb_c) l;
Printf.printf "\n"
;;

let affiche_grille_complex g =
  Array.iter (fun l-> affiche_ligne_complex l g.nb_l g.nb_c ) g.t
;;


let situation_of_char s =
  match s with
    ' ' -> V 
   |'|' -> MV
   |'-' -> MH 
   |'+' -> C 
   |'E' -> E 
   |'S' -> S 
   |'X' -> Chemin
   | _  -> failwith("le fichier n'est convertissable");
;;

let case_of_char s i j=
  {l=i;c=j;s= situation_of_char s; dis_S = -1}
;;

     
let contientEntreeEtSortie g=
  let bE,bS= Array.fold_left (fun acc x -> Array.fold_left (fun (acc_E,acc_S) c -> 
                                                                                  match c.s with
                                                                                    E -> (acc_E+1,acc_S)
                                                                                    |S -> (acc_E,acc_S+1) 
                                                                                    | _-> (acc_E,acc_S)
                                                                                                        )  acc x ) (0,0) g.t in
  if bE<>1 && bS<>1 then 
    failwith "vhugg"
;;


let tableau_correct g =
  Array.iter(fun x-> Array.iter (fun y-> 
                                          match y.s with
                                             V  -> if y.l mod 2 = 0 && y.c mod 2 = 0 then failwith "Vide invalide"
                                            |C  -> if y.l mod 2 = 1 || y.c mod 2 = 1 then failwith "Coin invalide"
                                            |MV -> if y.l mod 2 = 0 || y.c mod 2 = 1 then failwith "Mur Vertical invalide"
                                            |MH -> if y.l mod 2 = 1 || y.c mod 2 = 0 then failwith "Mur horizontal invalide"
                                            |_ ->  if y.l mod 2 = 0 || y.c mod 2 = 0 then failwith "Case invalide"
                                                                                                                      ) x                                          
                                                                                                                          ) g.t

 

let verifieGrille g =
  if Array.length(g.t) <> g.nb_l || Array.length(g.t.(0)) <> g.nb_c then
    failwith "Fichier invalide"
  else
    contientEntreeEtSortie g;
    tableau_correct g;
;;


let convertisseur t l c=
  let nbLignes= l*2+1 in
  let nbColonnes= c*2+1 in
  let t1 = Array.init nbLignes (fun i -> Array.init nbColonnes (fun k -> (case_of_char t.(i).(k) i k))) in   
  let g = {t=t1;nb_l=nbLignes;nb_c=nbColonnes} in
  verifieGrille g ;
  g
;; 

let trouve_E g =
  Array.fold_left(fun acc x -> Array.fold_left (fun acc2 y -> if y.s=E then (y.l,y.c) else acc2) acc x) (-1,-1) g.t
;;

let trouve_S g =
  Array.fold_left(fun acc x -> Array.fold_left (fun acc2 y -> if y.s=S then (y.l,y.c) else acc2) acc x) (-1,-1) g.t
;;

let resolution g =
  let start_x, start_y = trouve_S g in 

  let rec aux g x y dist =
    match g.t.(x).(y).s with
    | E ->
      g.t.(x).(y).dis_S <- dist;
    | V | S  ->
      if g.t.(x).(y).dis_S < 0 then begin
        g.t.(x).(y).dis_S <- dist;
        aux g (x-1) y (dist+1);
        aux g (x+1) y (dist+1);
        aux g x (y-1) (dist+1);
        aux g x (y+1) (dist+1);
      end
    | _ -> ()
  in

  aux g start_x start_y 0
;;

let coord_prochain_chemin g x y =
  let voisins = [(x-1, y); (x+1, y); (x, y-1); (x, y+1)] in
  let voisins_non_mur = List.filter (fun (x', y') -> x' >= 0 && x' < g.nb_l && y' >= 0 && y' < g.nb_c && g.t.(x').(y').dis_S > -1) voisins in
  match voisins_non_mur with
  | [] -> failwith "Le labyrinthe n'est pas resolvable"
  | _ ->
      List.fold_left (fun acc (x1, y1) ->
        if g.t.(x1).(y1).dis_S < g.t.(fst acc).(snd acc).dis_S then (x1, y1) else acc
      ) (x, y) voisins_non_mur
;;

let grille_resolue g =
  let rec aux g x y acc=
    match acc with
    | 1 -> Printf.printf "Labyrinthe resolu :\n";
    | _ ->
        let min_x, min_y = coord_prochain_chemin g x y in 
        g.t.(min_x).(min_y).s <- Chemin;
        aux g min_x min_y (acc-1)
  in
  let start_x, start_y = trouve_E g in
  aux g start_x start_y g.t.(start_x).(start_y).dis_S
;;


let caseAleatoire nb_lignes nb_colonnes =
  Random.self_init ();
  let x= Random.int (2*nb_lignes -1) +1  in
  let y= Random.int (2*nb_colonnes -1) +1 in
  let x= if x mod 2=0 then x+1 else x in
  let y= if y mod 2=0 then y+1 else y in
  (x,y)
;;


let directionsAleatoires () =
  Random.self_init ();
  List.sort (fun d1 d2 -> Random.int 4 - 2) ["haut"; "bas"; "gauche"; "droite"]
;;

 
let choisiCaseNonVisitee g x y =
  let tab=Array.of_list (directionsAleatoires()) in
  let rec aux td cpt =
    if cpt<4 then
      match td.(cpt) with
      | "haut" when x-2>= 0 && g.t.(x-2).(y).dis_S = -1 ->
          g.t.(x-1).(y).s <- V;
          Some (x-2, y)
      | "bas" when x+2<g.nb_l && g.t.(x+2).(y).dis_S = -1 ->
          g.t.(x+1).(y).s <- V;
          Some (x+2, y)
      | "gauche" when y-2>=0 && g.t.(x).(y-2).dis_S = -1 ->
          g.t.(x).(y-1).s <- V;
          Some (x, y-2)
      | "droite" when y + 2 < g.nb_c && g.t.(x).(y+2).dis_S = -1 ->
          g.t.(x).(y+1).s <- V;
          Some (x,y+2)
      | _ ->
          aux td (cpt+1)
    else
      None
  in
  aux tab 0
;;

let generate nbLignes nbColonnes =
  let g=grille_vide nbLignes nbColonnes in
  let x,y=caseAleatoire nbLignes nbColonnes in
  g.t.(x).(y).dis_S <- -2;
  let nbcase=nbLignes*nbColonnes in
  let pile=[(x,y)] in
  let rec aux g pile cpt =
    match pile with
    | [] -> g 
    | (x,y) :: casesRestantes ->
        if cpt=0 then g
        else
          match choisiCaseNonVisitee g x y with
          | Some (next_x, next_y) -> 
                                      g.t.(next_x).(next_y).dis_S <- -2;
                                      aux g ((next_x, next_y)::pile) (cpt-1)
          | None ->
                      aux g casesRestantes cpt 
  in
  let gFinal=aux g pile nbcase in
  let x1,y1=caseAleatoire nbLignes nbColonnes in
  let rec coord x1 y1=
    let x2,y2=caseAleatoire nbLignes nbColonnes in
    if x1=x2 && y1=y2 then
      coord x1 y1
    else
      x2,y2
  in
  let x2,y2=coord x1 y1 in
  gFinal.t.(x1).(y1).s <- E;
  gFinal.t.(x2).(y2).s <- S;
  gFinal
;;