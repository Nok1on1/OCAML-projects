let rec member c t l =
  match l with
  | [] -> false
  | x::xs -> if (c x t) = 0 then true else member c t xs;;

let equal_second_components x y =
  match x, y with
  | (_,x), (_,y) -> compare x y;;

let evens_eq_evens_odds_eq_odds x y =
  compare (x mod 2) (y mod 2);;

let count_occurrences list =
  let rec aux list list1 list2 count y =
    match list with
    | [] ->
      if list1 != [] then
        aux list1 [] ((y, count)::list2) 0 (List.hd list1)
      else
        ((y, count)::list2)
    | x::xs ->
      if y = x then
        aux xs list1 list2 (count+1) y
      else
        aux xs (x::list1) list2 count y
  in aux list [] [] 0 (List.hd list);;

let rec drop_last = function
  | [] -> []
  | [_] -> []
  | x::xs -> x::drop_last xs;;

let zip_with f lst lst1 =
  let rec aux f lst lst1 list2 =
    match lst, lst1 with
    | [], [] | _ , [] | [], _ -> list2
    | x::xs, y::ys -> aux f xs ys (list2@[f x y])
  in aux f lst lst1 [];;

let unzip lst =
  let rec aux list1 list2 = function
    | [] -> (list1,list2)
    | (a,b)::tl -> aux (list1@[a]) (list2 @[b]) tl
  in aux [] [] lst;;

type team = Arg | Sau | Mex | Pol | Por | Kor | Uru | Gha
type tuple = {
  team : team;
  games : int; 
  wins : int;
  draws : int;
  losses : int;
  goals : int;
  lgoals : int
};;

let listofgames (lst : (team * 'a list * team * 'a list) list) team =
  let rec aux lst lst1 team =
    match lst with
    | [] -> lst1
    | (team1, goal1, team2, goal2)::xs ->
      if team1 = team || team2 = team then
        aux xs (((team1, goal1, team2, goal2))::lst1) team
      else
        aux xs (lst1) team
  in aux lst [] team;;

let rec listofteams (lst : (team * 'a list * team * 'a list) list) teams =
  match lst with
  | [] -> List.sort_uniq compare teams
  | (team1, _, team2, _)::xs -> listofteams xs (team1 :: team2 :: teams);;

let wl goal1 goal2 = (List.length goal1) > (List.length goal2);;

let draw goal1 goal2 = (List.length goal1) = (List.length goal2);;

let rec tuples lstofgames team (tuple : tuple) =
  match lstofgames with
  | [] -> tuple
  | (team1, goal1, _team2, goal2)::xs ->
    if team = team1 then
      tuples xs team {
        team = team;
        games = tuple.games+1;
        wins = if wl goal1 goal2 then tuple.wins + 1 else tuple.wins;
        losses = if wl goal2 goal1 then tuple.wins + 1 else tuple.wins;
        draws = if draw goal1 goal2 then tuple.draws + 1 else tuple.draws;
        goals = List.length goal1;
        lgoals = List.length goal2
      }
    else
      tuples xs team {
        team = team;
        games = tuple.games+1;
        wins = if wl goal2 goal1 then tuple.wins + 1 else tuple.wins;
        losses = if wl goal1 goal2 then tuple.wins + 1 else tuple.wins;
        draws = if draw goal1 goal2 then tuple.draws + 1 else tuple.draws;
        goals = List.length goal2;
        lgoals = List.length goal1
      };;

let table_and_scorers (lst : (team * 'a list * team * 'a list) list) =
  let rec aux (lst : (team * 'a list * team * 'a list) list) table teams =
    match teams with
    | [] -> table
    | x::xs ->
      aux lst ((tuples (listofgames lst x) x {
          team = x;
          games = 0;
          wins = 0;
          draws = 0;
          losses = 0;
          goals = 0;
          lgoals = 0
        })::table) xs
  in aux lst [] (listofteams lst (listofteams lst []));;