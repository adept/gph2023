open! Core

let dim_rows = 25
let dim_cols = 24

module Coord = struct
  module T = struct
    type t = int*int [@@deriving sexp, compare, equal]
  end
  include T
  include Comparable.Make(T)
end

let words =
  [ "CIRCUIT"
  ; "EJECTION"
  ; "EVALUATION"
  ; "INTEGRATED"
  ; "LOTTERY"
  ; "MESSAGE"
  ; "PAYMENTMETHOD"
  ; "PERMANENT"
  ; "PLASTIC"
  ; "PLAYOFFS"
  ; "QUALIFIER"
  ; "REPETITION"
  ; "RESIDENCY"
  ; "SCHOLASTIC"
  ; "SOCCER"
  ; "SPACED"
  ; "TICKET"
  ; "VACATION"
  ]
  |> String.Set.of_list 

let magic_squares =
  [ 3,6 (* reputation boost. plastic + payment method => credit card *)
  ; 5,9 (* station master. vacation + message? => post card *)
  ; 7, 14 (* cardinal director. soccer + ejection => red card *)
  ; 7,17 (* magenta square, not a mastery *)
  ; 10, 17 (* spark of inspiration. spaced + repetition => flash card *)
  ; 12, 22 (* resourceful. integrated circuit => smart card *)
  ; 14,8 (* erase pain. lottery + ticket => scratch card *)
  ; 16, 15 (* new field. permanent + residency => green card *)
  ; 18, 3 (* crazed frenzy. playoffs + qualifier => wild card *)
  ; 23, 11 (* rumor has it. scholastic evaluation => report card *)
  ]
  |> List.map ~f:(fun (x,y) -> x-1, y-1)
  |> Coord.Set.of_list

let word_squares = 
  [ "PLASTIC", "PAYMENTMETHOD", (3,6) (* reputation boost. plastic + payment method => credit card *)
  ; "VACATION", "MESSAGE", (5,9) (* station master. vacation + message? => post card *)
  ; "SOCCER", "EJECTION", (7, 14) (* cardinal director. soccer + ejection => red card *)
  ; "SPACED", "REPETITION", (10,17) (* spark of inspiration. spaced + repetition => flash card *)
  ; "INTEGRATED", "CIRCUIT", (12, 22) (* resourceful. integrated circuit => smart card *)
  ; "LOTTERY", "TICKET", (14,8) (* erase pain. lottery + ticket => scratch card *)
  ; "PERMANENT", "RESIDENCY", (16, 15) (* new field. permanent + residency => green card *)
  ; "PLAYOFFS", "QUALIFIER", (18, 3) (* crazed frenzy. playoffs + qualifier => wild card *)
  ; "SCHOLASTIC", "EVALUATION", (23, 11) (* rumor has it. scholastic evaluation => report card *)
  ]
  |> List.concat_map ~f:(fun (w1,w2,(x,y)) -> [w1, (x-1, y-1); w2, (x-1,y-1)])
  |> String.Map.of_alist_exn

let magic_rows = Int.Set.map magic_squares ~f:fst

let magic_columns = Int.Set.map magic_squares ~f:snd

module Board = struct
  type t  = { board : char Array.t Array.t
            ; magic_squares : Coord.Set.t
            ; empty : bool
            ; hit_squares : Coord.Set.t
            }

  module Hashable = struct
    type t = char list [@@deriving hash]
  end

  let hash t =
    let board = Array.to_list t.board |> List.concat_map ~f:Array.to_list in
    Hashable.hash board

  let create () =
    { board = Array.make_matrix ~dimx:dim_rows ~dimy:dim_cols '.'
    ; magic_squares 
    ; empty = true
    ; hit_squares = Coord.Set.empty
    }

  let copy t =
    {t with board = Array.copy_matrix t.board}

  let print t =
    printf "\n=====\n";
    printf "empty : %b\n" t.empty;
    printf !"hit squares : %{sexp: Coord.Set.t}\n%!" t.hit_squares;
    printf !"remaining squares : %d\n%!" (Set.length (Set.diff t.magic_squares t.hit_squares));
    let word = ref [] in
    List.iter (List.init dim_rows ~f:Fn.id) ~f:(fun row ->
        List.iter (List.init dim_cols ~f:Fn.id) ~f:(fun col ->
            if Set.mem t.magic_squares (row,col) then (
              let c = t.board.(row).(col) in
              word := c::!word;
              printf "%c" c)
            else
              printf "%c" (Char.lowercase t.board.(row).(col))
          );
        printf "\n%!"
      );
    printf "SPELLS: <%s>\n%!" (String.lowercase (String.of_char_list (List.rev !word)))

  let next row col dir =
    match dir with
    | `Horiz -> row, (col+1)
    | `Vert ->(row+1), col

  let in_bounds row col =
    row >= 0 && col >= 0 && row < dim_rows && col < dim_cols 

  let is_empty t (row,col) = 
    match in_bounds row col with
    | false -> true
    | true -> Char.equal t.board.(row).(col) '.'

  let sibling1 row col dir =
    match dir with
    | `Horiz -> row-1, col
    | `Vert -> row, col-1

  let sibling2 row col dir =
    match dir with
    | `Horiz -> row+1, col
    | `Vert -> row, col+1

  let before row col dir = 
    match dir with
    | `Horiz -> row, col-1
    | `Vert -> row-1, col

  let place t word (row, col) dir =
    match is_empty t (before row col dir) with
    | false -> `Nope
    | true ->
      let t = copy t in
      let hit_other = ref false in
      let result =
        List.fold (String.to_list word) ~init:(`Ok (Coord.Set.empty, (row, col))) ~f:(fun state c ->
            match state with
            | `Oob | `Clash -> state
            | `Ok (squares, (row, col)) ->
              match in_bounds row col with
              | false -> `Oob
              | true ->
                let squares = Set.add squares (row,col) in
                (match t.board.(row).(col) with
                 | '.' ->
                   if is_empty t (sibling1 row col dir) && is_empty t (sibling2 row col dir)
                   then (t.board.(row).(col) <- c; `Ok (squares, (next row col dir)))
                   else `Clash
                 | something ->
                   if Char.equal something c 
                   then ( hit_other := true; `Ok (squares, (next row col dir)))
                   else `Clash))
      in
      match result with
      | `Oob | `Clash -> `Nope
      | `Ok (squares, after) -> (
          match is_empty t after with
          | false -> `Nope
          | true -> (
              match t.empty, !hit_other with
              | false, false -> `Nope
              | true, _ | false, true ->
                (* hit some magic squares *)
                let hit_magic = Set.inter t.magic_squares squares in
                let _hit_new_magic = Set.diff hit_magic t.hit_squares in
                match Set.is_empty hit_magic (* || Set.is_empty hit_new_magic *) with
                | true -> `Nope
                | false -> `Ok { t with hit_squares = Set.union t.hit_squares hit_magic; empty = false })
        )

end



let main () =
  let levels = "0123456789abcdefghijklmnopqrstuvwxyz" in
  let best_remaining = ref 100 in
  let seen = Int.Hash_set.create () in
  let ticks = ref 0 in
  let rec loop depth words board =
    incr ticks;
    if !ticks mod 100 = 0 then printf "%c%!" (String.get levels depth);
    match Set.is_empty words with
    | true ->
      ((* hit purple? *)
        match Set.mem board.Board.hit_squares (6,16) with
        | false -> ()
        | true ->
          let remaining_squares = Set.length (Set.diff board.Board.magic_squares board.Board.hit_squares) in
          if remaining_squares < !best_remaining || remaining_squares = 0 then (
            best_remaining := remaining_squares;
            Board.print board
          ) else ())
    | false ->
      Set.iter words ~f:(fun w ->
          List.iter [`Horiz; `Vert] ~f:(fun dir ->
              let rows, cols =
                match Map.find word_squares w with
                | None -> (Set.to_list magic_rows, Set.to_list magic_columns)
                | Some (w_row, w_col) -> [w_row], [w_col]
              in
              let rows, cols =
                match dir with
                | `Horiz -> rows, (List.init (dim_cols - String.length w +1) ~f:Fn.id)
                | `Vert -> (List.init (dim_rows - String.length w + 1) ~f:Fn.id), cols
              in
              List.iter rows ~f:(fun row ->
                  List.iter cols ~f:(fun col ->
                      match Board.place board w (row,col) dir with
                      | `Ok board ->
                        let h = Board.hash board in
                        (match Hash_set.mem seen h with
                         | true -> ()
                         | false ->
                           Hash_set.add seen h;
                           (*Board.print board;*)
                           loop (depth+1) (Set.remove words w) board)
                      | `Nope -> ()
                    ))))
  in
  loop 0 words (Board.create())
;;

let () = main ()
