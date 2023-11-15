open! Core

let dim_rows = 5
let dim_cols = 5

module Coord = struct
  module T = struct
    type t = int*int [@@deriving sexp, compare, equal]
  end
  include T

  let in_bounds (row,col) =
    row >= 0 && col >= 0 && row < dim_rows && col < dim_cols 

  let restrict t = if in_bounds t then Some t else None

  let next dir (row,col) =
    match dir with
    | `Right ->  restrict (row, col+1)
    | `Down  ->  restrict (row+1, col)
    | `Up    ->  restrict (row-1, col)
    | `Left   -> restrict (row, col-1)

  let neighbors ?(how=`Ortho) (r,c) =
    match how with
    | `Ortho ->
      List.filter_map [ `Up; `Down; `Left; `Right ] ~f:(fun d -> next d (r,c))
    | `Omni ->
      List.filter_map ~f:restrict
        [r+1,c; r-1,c; r,c+1; r,c-1; r+1,c+1; r-1,c-1; r+1,c-1; r-1,c+1]

  let%expect_test "neighbors" =
    printf !"%{sexp: t list}\n" (neighbors (0,0));
    [%expect {| ((1 0) (0 1)) |}];
    printf !"%{sexp: t list}\n" (neighbors (3,3));
    [%expect {| ((2 3) (4 3) (3 2) (3 4)) |}];
    printf !"%{sexp: t list}\n" (neighbors ~how:`Omni (3,3));
    [%expect {| ((4 3) (2 3) (3 4) (3 2) (4 4) (2 2) (4 2) (2 4)) |}]
  ;;

  include Comparable.Make(T)
end

module Color = struct
  type t = B(*black*) | W(*white*) [@@deriving compare, equal, hash]

  let other = function
    | B -> W
    | W -> B

  let of_string = function
    | "B" -> B
    | "W" -> W
    | other -> failwithf "invalid color %s" other ()

end

module Piece = struct
  type t =
    | Plain of Color.t
    | W of Color.t * int (* Watchtower *)
    | IW of Color.t * int (* Inverted watchtower *)
    | N of Color.t * int (* Neighbor *)
    | IVS of Color.t*int (* Inverted vertical scout *)
    | IHS of Color.t*int (* Inverted horizontal scout *)
    | NS of Color.t*int (* Neighborhood surveyor *)
    | ISu of Color.t*int (* Inverted Surveyor *)
    | ISc of Color.t*int (* Inverted scanner, like a minesweeper number *)
  [@@deriving hash]

  let color = function
    | Plain c -> c
    |W (c,_) | IW (c,_) | N (c,_) | IVS (c,_) | IHS (c,_) | NS (c,_) | ISu (c,_) | ISc (c,_) -> c

  let to_string ~just_plain t =
    let s =
      match just_plain, t with 
      | _, None            ->         " "
      | _, Some (Plain B ) ->         "B"
      | _, Some (Plain W ) ->         "W"
      | true,  _ -> " "
      | false, Some (W (W,n)) -> sprintf "W W %d" n
      | false, Some (W (B,n)) -> sprintf "W B %d" n
      | false, Some (IW (W,n)) -> sprintf "IW W %d" n
      | false, Some (IW (B,n)) -> sprintf "IW B %d" n
      | false, Some (N (W,n)) -> sprintf "N W %d" n
      | false, Some (N (B,n)) -> sprintf "N B %d" n
      | false, Some (IVS (W,n)) -> sprintf "IVS W %d" n
      | false, Some (IVS (B,n)) -> sprintf "IVS B %d" n
      | false, Some (IHS (W,n)) -> sprintf "IHS W %d" n
      | false, Some (IHS (B,n)) -> sprintf "IHS B %d" n
      | false, Some (NS (W,n)) -> sprintf "NS W %d" n
      | false, Some (NS (B,n)) -> sprintf "NS B %d" n
      | false, Some (ISu (W,n)) -> sprintf "ISu W %d" n
      | false, Some (ISu (B,n)) -> sprintf "ISu B %d" n
      | false, Some (ISc (W,n)) -> sprintf "ISc W %d" n
      | false, Some (ISc (B,n)) -> sprintf "ISc B %d" n
    in
    (* pad *)
    sprintf "%7s" s
end

module Board = struct
  type t=
    { board : Piece.t option Array.t Array.t
    ; free_cells : int
    }

  module Hashable = struct
    type t = Piece.t option list [@@deriving hash]
  end

  let hash t =
    let board = Array.to_list t.board |> List.concat_map ~f:Array.to_list in
    Hashable.hash board

  let parse strings =
    let color = Color.of_string in
    let num = Int.of_string in
    let board =
      List.map strings ~f:(fun s ->
          List.map (String.split s ~on:',') ~f:(fun cell ->
              match String.split ~on:' ' (String.strip cell) with
              | [""] -> None
              | ["B"] -> Some (Piece.Plain B)
              | ["W"] -> Some (Plain W)
              | ["W";c;count] -> Some (W (color c, num count))
              | ["IW";c;count] -> Some (IW (color c, num count))
              | ["IVS";c;count] -> Some (IVS (color c, num count))
              | ["IHS";c;count] -> Some (IHS (color c, num count))
              | ["N";c;count] -> Some (N (color c, num count))
              | ["NS";c;count] -> Some (NS (color c, num count))
              | ["ISu";c;count] -> Some (ISu (color c, num count))
              | ["ISc";c;count] -> Some (ISc (color c, num count))
              | parts -> failwithf !"Failed to parse: cell:<%s>, parts:%{sexp: string list}" cell parts ()
            )
          |> Array.of_list
        ) |> Array.of_list
    in
    let free_cells =
      Array.to_list board
      |> List.concat_map ~f:Array.to_list
      |> List.filter ~f:Option.is_none
      |> List.length
    in
    {board; free_cells}

  let create () =
    { board = Array.make_matrix ~dimx:dim_rows ~dimy:dim_cols None
    ; free_cells = dim_rows*dim_cols
    }

  let copy t =
    {t with board = Array.copy_matrix t.board}

  let place t (row,col) color =
    let t = copy t in
    t.board.(row).(col) <- Some (Piece.Plain color);
    {t with  free_cells = t.free_cells -1}

  let print t =
    printf "\n===== free:%d =====\n" t.free_cells;
    Array.iter t.board ~f:(fun row ->
        Array.to_list row
        |> List.map ~f:(fun cell ->
            sprintf "%6s" (Piece.to_string ~just_plain:true cell)
          )
        |> String.concat ~sep:","
        |>print_endline
      );
    printf "\n%!"

  let in_bounds (row,col) =
    row >= 0 && col >= 0 && row < dim_rows && col < dim_cols 

  let is_empty t (row,col) =
    Option.is_none t.board.(row).(col)

  let color_at t (row,col) =
    match in_bounds (row,col) with
    | false -> None
    | true ->
      match t.board.(row).(col) with
      | None -> None
      | Some piece -> Some (Piece.color piece)

  let all_color_coords color t =
    let cells = Queue.create () in
    Array.iteri t.board ~f:(fun row row_a ->
        Array.iteri row_a ~f:(fun col cell ->
            match cell with
            | None -> ()
            | Some piece -> if Color.equal (Piece.color piece) color
              then Queue.enqueue cells (row,col)
          );
      );
    Queue.to_list cells |> Coord.Set.of_list

  let remove_connected_component ?(how=`Ortho) start cells =
    let rec loop queue component cells =
      match queue with
      | [] -> component, cells
      | (row,col)::rest ->
        let component =
          if Set.mem cells (row,col)
          then Set.add component (row,col)
          else component
        in
        let cells = Set.remove cells (row,col) in
        let ns =
          Coord.neighbors ~how (row,col)
          |> List.filter ~f:(Set.mem cells)
        in
        loop (List.append rest ns) component cells
    in
    loop [start] Coord.Set.empty cells

  let connected_components ?(how=`Ortho) color t = 
    let cells = all_color_coords color t in
    let rec loop components cells = 
      match Set.min_elt cells with
      | None -> components
      | Some cell ->
        (* remove the connected component that cell is a part of *)
        let component, remainder = remove_connected_component ~how cell cells in
        loop (component::components) remainder
    in
    loop [] cells

  let count_connected_components ?(how=`Ortho) color t =
    List.length (connected_components ~how color t)

  let connected_component_size t color start =
    let cells = all_color_coords color t in
    let component, _remainder = remove_connected_component start cells in
    Set.length component

  let component_width cells =
    let columns = Int.Set.map ~f:snd cells in
    Set.max_elt_exn columns - Set.min_elt_exn columns + 1

  let component_height cells =
    let rows = Int.Set.map ~f:fst cells in
    Set.max_elt_exn rows - Set.min_elt_exn rows + 1

  let all_connected ?(how=`Ortho) ~complete color t =
    if not complete then true
    else (
      count_connected_components ~how color t = 1
    )

  let exactly_n_connected_components ~complete ~n color t =
    if not complete then true
    else (
      count_connected_components color t = n
    )

  let width_one ~complete color t =
    if not complete then true
    else (
      let valid = ref true in
      Array.iteri t.board ~f:(fun row row_a ->
          Array.iteri row_a ~f:(fun col cell ->
              match cell with
              | None -> ()
              | Some piece ->
                if Color.equal (Piece.color piece) color
                then
                  if List.for_all 
                      [ row+1,col; row, col+1; row+1, col+1]
                      ~f:(fun (row,col) ->
                          match color_at t (row,col) with
                          | None -> false
                          | Some c -> Color.equal c color)
                  then valid:=false
            );
        );
      !valid
    )

  let rec count_ray t dir color coord =
    match Coord.next dir coord with
    | None -> 0
    | Some (r,c) ->
      match t.board.(r).(c) with
      | None -> 0
      | Some piece ->
        if Color.equal (Piece.color piece) color then 1+count_ray t dir color (r,c)
        else 0


  let courtyards ?(square=false) ~complete color t =
    if not complete then true
    else (
      let components = connected_components color t in
      let valid = ref true in
      List.iter components ~f:(fun c ->
          let w = component_width c in
          let h = component_height c in
          if not ( (if square then w=h else true) && w*h = Set.length c) then valid:=false
        );
      !valid
    )
  ;;

  let plazas ~complete color t =
    courtyards ~square:true ~complete color t
  ;;

  let count_rays t color (row,col) =
    List.sum (module Int) [`Up; `Down; `Left; `Right] ~f:(fun dir ->
        count_ray t dir color (row,col))

  let count_row t color (row,_col) =
    List.filter_map (List.init dim_cols ~f:Fn.id) ~f:(fun col ->
        Option.map (color_at t (row,col)) ~f:(fun c -> Color.equal color c))
    |> List.filter ~f:Fn.id
    |> List.length

  let count_column t color (_row,col) =
    List.filter_map (List.init dim_rows ~f:Fn.id) ~f:(fun row ->
        Option.map (color_at t (row,col)) ~f:(fun c -> Color.equal color c))
    |> List.filter ~f:Fn.id
    |> List.length

  let count_neighbors ?(how=`Ortho) t color (row,col) =
    List.filter_map (Coord.neighbors ~how (row,col)) ~f:(fun coord ->
        Option.map (color_at t coord) ~f:(fun c -> Color.equal color c))
    |> List.filter ~f:Fn.id
    |> List.length

  (* Each clue belongs to a single region, each region has a single clue *)
  let signposted ~complete color t =
    if not complete then true
    else 
      let components = connected_components color t in
      let valid = ref true in
      List.iter components ~f:(fun c ->
          let coords = Set.to_list c in
          let clues = ref 0 in
          List.iter coords ~f:(fun (r,c) ->
              match t.board.(r).(c) with
              | None -> ()
              | Some (Plain _) -> ()
              | Some _ -> incr clues
            );
          if !clues <> 1 then valid:=false
        );
      !valid

  let valid ~complete ~extra_checks t =
    let valid = ref true in
    let equal_accept_less a b =
      let res = if complete then a = b else a<=b in
      if not res then valid:=false
    in
    Array.iteri t.board ~f:(fun row row_a ->
        Array.iteri row_a ~f:(fun col cell ->
            match cell with
            | None -> ()
            | Some (Plain _) -> ()
            | Some (W (c,n)) ->
              equal_accept_less (1 + count_rays t c (row,col)) n
            | Some (IW (c,n)) ->
              equal_accept_less (count_rays t (Color.other c) (row,col)) n
            | Some (N (c,n)) ->
              equal_accept_less (count_neighbors t c (row,col)) n
            | Some (ISc (c,n)) ->
              equal_accept_less (count_neighbors ~how:`Omni t (Color.other c) (row,col)) n
            | Some (IVS (c,n)) ->
              equal_accept_less (count_column t (Color.other c) (row,col)) n
            | Some (IHS (c,n)) ->
              equal_accept_less (count_row t (Color.other c) (row,col)) n
            | Some (NS (c,n)) ->
              equal_accept_less (connected_component_size t c (row,col)) n
            | Some (ISu (c,n)) ->
              equal_accept_less (connected_component_size t (Color.other c) (row,col)) n
          );
      );
    List.iter extra_checks ~f:(fun f ->
        if not (f ~complete t) then valid:=false
      );
    !valid

  let fill ~debug ~extra_checks t = 
    let seen = Int.Hash_set.create () in
    let ticks = ref 0 in
    let rec loop depth t =
      incr ticks;
      if debug && !ticks mod 100 = 0 then printf ".%!";
      match t.free_cells = 0 with
      | true ->
        print t
      | false ->
        (* printf "level %d\n%!" depth; *)
        let coord =
          Array.foldi ~init:None t.board ~f:(fun row coord row_a ->
              match coord with
              | Some c -> Some c
              | None -> 
                Array.foldi ~init:None row_a ~f:(fun col coord cell ->
                    match coord with
                    | Some c -> Some c
                    | None -> (
                        match cell with
                        | Some _ -> coord
                        | None -> Some (row,col)
                      ))
            )
        in
        match coord with
        | None -> failwith "board is full and not full?!"
        | Some (row,col) ->
          let colors=
            if (row+col%2=0) then [Color.B;W] else [Color.W;B]
          in
          List.iter colors ~f:(fun color  ->
              let board = place t (row,col) color in
              let complete = board.free_cells = 0 in
              match valid ~complete ~extra_checks board with
              | false -> ()
              | true ->
                (* printf "at depth %d (%d,%d)\n%!" depth row col;
                 * print board; *)
                let h = hash board in
                (match Hash_set.mem seen h with
                 | true -> ()
                 | false ->
                   Hash_set.add seen h;
                   (*Board.print board;*)
                   loop (depth+1) board)
            )

    in
    loop 0 t
  ;;


  let%expect_test "parse/print" =
    let test ss = parse ss |> print in
    test
      [ " , "
      ; " , "
      ];
    [%expect {|
      ===== free:4 =====
             ,
             , |}];
    test
      [ " , , , , "
      ; " , , , , "
      ; " , , , , "
      ; " , , , , "
      ; " , , , , "
      ];
    [%expect {|
      ===== free:25 =====
             ,       ,       ,       ,
             ,       ,       ,       ,
             ,       ,       ,       ,
             ,       ,       ,       ,
             ,       ,       ,       , |}];

    test
      [ "B, , , , "
      ; "W, , , , "
      ; " , , , ,IW W 3 "
      ; " , , , , "
      ; "IW B 5, , , , "
      ];
    [%expect {|
      ===== free:21 =====
            B,       ,       ,       ,
            W,       ,       ,       ,
             ,       ,       ,       ,
             ,       ,       ,       ,
             ,       ,       ,       , |}];
  ;;


  let test_valid ~complete ss = parse ss |> valid ~complete ~extra_checks:[] |> printf "%b"

  let%expect_test "fully valid" =
    test_valid ~complete:true
      [ "IW W 2,B, , , "
      ; "B, , , , "
      ; "W, , , , "
      ; " , , , , "
      ; " , , , , "
      ];
    [%expect {| true |}]

  let%expect_test "not invalid" =
    test_valid ~complete:false
      [ "IW W 2, , , , "
      ; "B, , , , "
      ; "W, , , , "
      ; " , , , , "
      ; " , , , , "
      ];
    [%expect {| true |}]

  let%expect_test "valid, from the game" =
    test_valid ~complete:true
      [ "W,W,W,IW W 4,W"
      ; "W,IW W 3, IW W 1,B,W"
      ; "IW W 2,B,IW W 3,B,W"
      ; "B,B,B,B,W"
      ; "W,B,W,B,IW W 1"
      ];
    [%expect {| true |}]

  let%expect_test "valid, from the game" =
    parse
      [ "W,W,W,IW W 4,W"
      ; "W,IW W 3, IW W 1,B,W"
      ; "IW W 2,B,IW W 3,B,W"
      ; "B,B,B,B,W"
      ; "W,B,W,B,IW W 1"
      ]
    |> valid ~complete:true
      ~extra_checks:[all_connected Color.B
                    ;width_one Color.B
                    ]
    |> printf "%b";
    [%expect {| true |}]

  let%expect_test "not width one" =
    parse
      [ "W,W,W,IW W 4,W"
      ; "W,IW W 3, IW W 1,B,W"
      ; "IW W 2,B,IW W 3,B,W"
      ; "B,B,B,B,W"
      ; "B,B,W,B,IW W 1"
      ]
    |> valid ~complete:true
      ~extra_checks:[all_connected Color.B
                    ;width_one Color.B
                    ]
    |> printf "%b";
    [%expect {| false |}]

  let%expect_test "count_column/row" =
    let b = 
      parse
        [ "IVS W 3,IVS W 1,W,B,W"
        ; "N B 1,W,W,N B 2,B"
        ; "B,W,IVS W 1,IHS W 2,B"
        ; "B,B,B,B,B"
        ; "W,W,W,W,W"
        ]
    in
    printf "%d\n" (count_column b Color.B (0,0));
    [%expect {| 3 |}];
    printf "%d\n" (count_row b Color.B (2,4));
    [%expect {| 2 |}];
    printf "%d\n" (count_neighbors b Color.B (1,0));
    [%expect {| 1 |}];
    printf "%d\n" (count_neighbors b Color.B (1,4));
    [%expect {| 2 |}]

  let%expect_test "connected_components" =
    let b = 
      parse
        [ "B,B,B,B,B"
        ; "W,W,W,W,W"
        ; "B,B,B,B,B"
        ; "B,B,B,B,B"
        ; "B,B,B,B,B"
        ]
    in
    printf "%d\n" (count_connected_components Color.W b);
    [%expect {| 1 |}];
    printf "%d\n" (count_connected_components Color.B b);
    [%expect {| 2 |}];
    printf "%d\n" (connected_component_size b Color.B (1,1));
    [%expect {| 20 |}];
    List.iter (connected_components Color.B b) ~f:(fun c->
        printf "HxW = %dx%d\n" (component_height c) (component_width c)
      );
    [%expect {|
      HxW = 3x5
      HxW = 1x5 |}]



  let%expect_test "level 10" =
    parse
      [ "IVS W 3,IVS W 1,W,B,W"
      ; "N B 1,W,W,N B 2,B"
      ; "B,W,IVS W 1,IHS W 2,B"
      ; "B,B,B,B,B"
      ; "W,W,W,W,W"
      ]
    |> valid ~complete:true
      ~extra_checks:[]
    |> printf "%b";
    [%expect {| true |}]

  let%expect_test "level 10.2" =
    let b= 
      parse
        [ "B,B,B,B,W"
        ; "IVS W 1, W,W,B,W"
        ; "IHS W 1, W,W,B,W"
        ; "W,W,W,B,B"
        ; "W,W,W,IVS W 4,N B 1"
        ]
    in
    printf "%d\n" (count_connected_components Color.W b);
    [%expect {| 2 |}];
    printf "%d\n" (count_connected_components Color.B b);
    [%expect {| 1 |}];
    printf "%b\n" (all_connected ~complete:true Color.B b);
    [%expect {| true |}];
    printf "%b\n" (width_one ~complete:true Color.B b);
    [%expect {| true |}];
    valid b ~complete:true
      ~extra_checks:[all_connected Color.B
                    ;width_one Color.B
                    ;exactly_n_connected_components ~n:2 Color.W
                    ]
    |> printf "%b";
    [%expect {| true |}]

  let%expect_test "level 10" =
    parse
      [ "B,B,NS W 1,B,NS W 1"
      ; "NS W 3,B,B,W,B"
      ; "W,B,NS W 2,B,W"
      ; "W,B,W,B,W"
      ; "B,W,B,B,NS W 3"
      ]
    |> valid ~complete:true
      ~extra_checks:[all_connected ~how:`Omni Color.B
                    ; width_one Color.B]
    |> printf "%b";
    [%expect {| true |}]

  let%expect_test "plazas" =
    parse
      [ "B,W, , , "
      ; "B,B, , , "
      ; " , , , , "
      ; " , , , , "
      ; " , , , , "
      ]
    |> plazas ~complete:true Color.B
    |> printf "%b";
    [%expect {| false |}]

  let%expect_test "plazas 2" =
    let b =
      parse
        [ "B,B, , ,W"
        ; "B,B, ,W,B"
        ; " , ,W,W,W"
        ; "B, ,W,W,W"
        ; " ,B,W,W,W"
        ]
    in
    plazas ~complete:true Color.B b
    |> printf "%b\n";
    plazas ~complete:true Color.W b
    |> printf "%b\n";
    [%expect {|
      true
      false |}]

end



