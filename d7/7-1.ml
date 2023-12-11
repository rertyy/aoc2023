(* sort by hand strength weakest first
   within a hand, sort lexicographically
   then in that order ascending, multply by its bid
   and sum everything *)

type record = { hand : string; bid : int }

type hand_type =
  (* strongest to weakest*)
  | FiveOfAKind
  | FourOfAKind
  | FullHouse
  | ThreeOfAKind
  | TwoPair
  | OnePair
  | HighCard

let compare_hand_type h1 h2 =
  let strength_order = function
    | FiveOfAKind -> 7
    | FourOfAKind -> 6
    | FullHouse -> 5
    | ThreeOfAKind -> 4
    | TwoPair -> 3
    | OnePair -> 2
    | HighCard -> 1
  in
  compare (strength_order h1) (strength_order h2)

let count_characters s =
  let char_counts = Hashtbl.create 5 in
  String.iter
    (fun c ->
      let current_count =
        match Hashtbl.find_opt char_counts c with
        | Some count -> count
        | None -> 0
      in
      Hashtbl.replace char_counts c (current_count + 1))
    s;
  char_counts

let most_common_character_counts s =
  let char_counts = count_characters s in
  Hashtbl.to_seq char_counts |> List.of_seq
  (* count2 count1 to sort in reverse*)
  |> List.sort (fun (_, count1) (_, count2) -> compare count2 count1)

let match_type chars_w_counts =
  let counts = List.map (fun (_, count) -> count) chars_w_counts in
  match counts with
  | [ 5 ] -> FiveOfAKind
  | [ 4; 1 ] -> FourOfAKind
  | [ 3; 2 ] -> FullHouse
  | [ 3; 1; 1 ] -> ThreeOfAKind
  | [ 2; 2; 1 ] -> TwoPair
  | [ 2; 1; 1; 1 ] -> OnePair
  | [ 1; 1; 1; 1; 1 ] -> HighCard
  | _ -> failwith "Invalid hand"

let card_strength c =
  match c with
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> 11
  | 'T' -> 10
  | _ -> Char.code c - Char.code '0'

let rec cmp l ll =
  match (l, ll) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | h :: t, hh :: tt ->
      if card_strength h > card_strength hh then 1
      else if card_strength h < card_strength hh then -1
      else cmp t tt

let compare_lexicographic s1 s2 =
  let l1 = s1 |> String.to_seq |> List.of_seq in
  let l2 = s2 |> String.to_seq |> List.of_seq in
  cmp l1 l2

let compare_records r1 r2 =
  let s1 = r1.hand in
  let s2 = r2.hand in
  let s1_type = most_common_character_counts s1 |> match_type in
  let s2_type = most_common_character_counts s2 |> match_type in

  let stronger = compare_hand_type s1_type s2_type in
  match stronger with 0 -> compare_lexicographic s1 s2 | cmp -> cmp

let parse_line s =
  match String.split_on_char ' ' s with
  | [ hand; bid ] -> { hand; bid = int_of_string bid }
  | _ -> { hand = ""; bid = 0 }

let read_lines file =
  In_channel.with_open_text file In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter (fun line -> line <> "")

let process lines =
  List.map parse_line lines |> List.sort compare_records
  |> List.mapi (fun idx x -> x.bid * (idx + 1))
  |> List.fold_left ( + ) 0

let () =
  let records = read_lines "input.txt" in
  let total = process records in
  Printf.printf "Total: %d\n" total
(* List.iter
   (fun record -> Printf.printf "Hand: %s, Bid: %d\n" record.hand record.bid)
   total *)




