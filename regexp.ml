type regExp =
| Epsilon
| All of int
| Lettre of char*int
| Concat of regExp*regExp
| Altern of regExp*regExp
| Etoile of regExp

exception Invalid_RegExp



let creer_liste_lettre () =
  let rec aux i l =
    if i<256 then 
      aux (i+1) ( (Char.chr i, 0)::l )
    else l 
  in
  aux 0 []

let rec lettre_dans_liste c l = match l with
| [] -> -1
| (a,i)::q -> if a = c then i else lettre_dans_liste c q

let rec incr_lettre_liste c l = match l with
| [] -> (c,0)::[]
| (a,i)::q -> if a = c then (a,i+1)::q else (a,i)::(incr_lettre_liste c q)



let string_to_regexp s = (*création de la regexp à partir du str rentré par l'utilisateur*)
  let n = String.length s in
  if n = 0 then raise Invalid_RegExp else (*une regexp vide est invalide*)
  let pile = Stack.create () in (*La pile est indispensable, car on utilise dans le str l'odre postfixe*)
  let all_cmp = ref 0 in
  let lettre_cmp = ref (creer_liste_lettre ()) in
  for i=0 to (n-1) do
    match s.[i] with
    | '.' -> (Stack.push (All (!all_cmp)) pile ; all_cmp := 1 + !all_cmp )
    | '@' -> Stack.push (Concat (Stack.pop pile, Stack.pop pile)) pile
    | '|' -> Stack.push (Altern (Stack.pop pile, Stack.pop pile)) pile
    | '*' -> Stack.push (Etoile (Stack.pop pile)) pile
    | '?' -> Stack.push (Altern (Stack.pop pile, Epsilon)) pile
    | a   -> begin
      let i = lettre_dans_liste a !lettre_cmp in
      if i = -1 then (
        raise Invalid_RegExp )
      else (
        Stack.push (Lettre (a,i)) pile;
        lettre_cmp := incr_lettre_liste a !lettre_cmp )
    end
  done;
  if (Stack.length pile) <> 1 then raise Invalid_RegExp
  else Stack.pop pile
