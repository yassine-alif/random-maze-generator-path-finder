type situation = V | MH | MV | C | E | S |  Chemin
type case = { l : int; c : int; mutable s : situation; mutable dis_S: int }
type grille = { t : case array array; nb_l : int; nb_c : int; }
val grille_vide : int -> int -> grille
val affiche_situation : situation -> unit
val affiche_case : case -> unit
val affiche_ligne : case array -> unit
val affiche_grille : grille -> unit
val affiche_case_complex : case -> int -> int -> unit
val affiche_ligne_complex : case array -> int -> int -> unit
val affiche_grille_complex : grille -> unit
val situation_of_char : char -> situation
val case_of_char : char -> int -> int -> case
val contientEntreeEtSortie : grille -> unit
val tableau_correct : grille -> unit
val verifieGrille : grille -> unit
val convertisseur : char array array -> int -> int -> grille
val trouve_E : grille -> int * int
val trouve_S : grille -> int * int
val resolution : grille -> unit
val coord_prochain_chemin : grille -> int -> int -> int*int
val grille_resolue : grille -> unit
val caseAleatoire : int -> int -> int * int
val directionsAleatoires : unit->string list
val choisiCaseNonVisitee : grille -> int -> int -> (int * int) option
val generate : int -> int -> grille