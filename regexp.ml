type regExp =
| Epsilon
| All of int
| Lettre of char*int
| Concat of regExp*regExp
| Altern of regExp*regExp
| Etoile of regExp

exception Invalid_RegExp

let rec eps_appartient e = match e with
| Epsilon | Etoile _ -> true
| All _ | Lettre _ -> false
| Concat (e1,e2) -> (eps_appartient e1) && (eps_appartient e2)
| Altern (e1,e2) -> (eps_appartient e1) || (eps_appartient e2)

let string_to_regexp s = 
  let n = String.length s in
  if n = 0 then raise Invalid_RegExp else
  let pile = Stack.create () in
  let i_all = ref 0 in
  let i_lettre = ref 0 in
  for i=0 to (n-1) do
    if s.[i] = '.' then (
      Stack.push (All (!i_all)) pile;
      i_all := 1 + !i_all )
    else
      if s.[i] = '@' then 
        Stack.push (Concat (Stack.pop pile, Stack.pop pile)) pile
      else
        if s.[i] = '|' then 
          Stack.push (Altern (Stack.pop pile, Stack.pop pile)) pile
        else
          if s.[i] = '*' then 
            Stack.push (Etoile (Stack.pop pile)) pile
          else
            if s.[i] = '?' then 
              Stack.push (Altern (Stack.pop pile, Epsilon)) pile
            else (
              Stack.push (Lettre (s.[i], !i_lettre)) pile;
              i_lettre := 1 + !i_lettre )
            done;
  if (Stack.length pile) > 1 then raise Invalid_RegExp
  else Stack.pop pile

let rec pref r = 