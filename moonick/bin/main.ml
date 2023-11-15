open! Core

open Moonick.Board

let level9 () =
  Board.parse 
    [ " , , , ,IW W 1"
    ; " , , , ,IW W 7"
    ; " , , , , "
    ; "IW W 1,IW W 5, , , "
    ; " , , , , "
    ]
  |> Board.fill ~debug:true
    ~extra_checks:[Board.all_connected Color.B
                  ;Board.width_one Color.B
                  ]
;;
ignore(level9)

let level10 () =
  Board.parse 
    [ " , , ,N B 2, "
    ; "IVS W 3,N B 1, , , "
    ; " , , , ,IHS W 3"
    ; " , , , , "
    ; " , , , , "
    ]
  |> Board.fill ~debug:true
    ~extra_checks:[Board.all_connected Color.B
                  ;Board.width_one Color.B
                  ; Board.exactly_n_connected_components ~n:2 Color.W
                  ]
;;
ignore(level10)

let level11 () =
  Board.parse 
    [ " , ,NS W 1, , "
    ; "NS W 2, , , ,NS W 1"
    ; " , , , , "
    ; "NS W 2, , , ,NS W 2"
    ; " , , , , "
    ]
  |> Board.fill ~debug:true
    ~extra_checks:[Board.all_connected ~how:`Omni Color.W
                  ;Board.all_connected ~how:`Omni Color.B
                  ;Board.width_one Color.B
                  ]
;;
ignore(level11)

let level12 () =
  Board.parse 
    [ " , , , , "
    ; " , , ,ISu W 5, "
    ; " , , , ,ISu W 2"
    ; " ,ISu W 2, , , "
    ; " , ,ISu W 1, , "
    ]
  |> Board.fill ~debug:true
    ~extra_checks:[Board.all_connected Color.W; Board.plazas Color.B]
;;
ignore(level12)

let level13 () =
  Board.parse 
    [ " , , ,N B 1, "
    ; " ,ISc B 2, , , "
    ; "N B 1, , ,ISc W 8, "
    ; " ,ISc W 7, , , "
    ; " , , , ,ISc W 2"
    ]
  |> Board.fill ~debug:true
    ~extra_checks:[]
;;
ignore(level13)

let level14 () =
  Board.parse 
    [ " , , ,IHS W 3, "
    ; " ,IHS W 3, , ,IVS B 3"
    ; "IVS B 1,IHS B 1, ,IVS B 4, "
    ; "IHS W 1, , , , "
    ; " ,IVS B 2, , ,IHS B 2"
    ]
  |> Board.fill ~debug:true
    ~extra_checks:[]
;;
ignore(level14)

let level15 () =
  Board.parse 
    [ " , ,IW W 2, , "
    ; " , ,IW W 3, ,W W 2"
    ; "W B 1, , ,IW W 2, "
    ; " , ,W B 2, , "
    ; " , , , ,IW B 3"
    ]
  |> Board.fill ~debug:true
    ~extra_checks:[]
;;
ignore(level15)

let level16 () =
  Board.parse 
    [ " , , , , "
    ; " ,ISu W 13, , , "
    ; " , , , ,ISu B 1"
    ; "ISu W 5, ,NS W 7, , "
    ; " , ,NS B 8, , "
    ]
  |> Board.fill ~debug:true
    ~extra_checks:[]
;;
ignore(level16)

let level17 () =
  Board.parse 
    [ " , , , , "
    ; " , , , , "
    ; " , , , , "
    ; " , , , , "
    ; " ,B, , , "
    ]
  |> Board.fill ~debug:true
    ~extra_checks:[ Board.signposted Color.B
                  ; Board.signposted Color.W]
;;
ignore(level17)

let level19 () =
  Board.parse 
    [ " , ,W, , "
    ; " , , ,B, "
    ; " ,B,B,B, "
    ; "B, , ,W, "
    ; " , ,B, ,W"
    ]
  |> Board.fill ~debug:true
    ~extra_checks:[Board.courtyards Color.W
                  ;Board.courtyards Color.B
                  ]
;;
ignore(level19)

let level20 () =
  Board.parse 
    [ " , , , , "
    ; " , , , , "
    ; " , , , , "
    ; " , , ,W, "
    ; " , , ,B, "
    ]
  |> Board.fill ~debug:true
    ~extra_checks:[Board.plazas Color.W
                  ;Board.plazas Color.B
                  ]
;;
ignore(level20)

let level21 () =
  Board.parse 
    [ " , , , , "
    ; " ,W, , ,W"
    ; " , , , , "
    ; " , , , , "
    ; " , ,W, , "
    ]
  |> Board.fill ~debug:true
    ~extra_checks:[ Board.all_connected Color.B
                  ; Board.width_one Color.B
                  ; Board.all_connected Color.W
                  ; Board.width_one Color.W
                  ]
;;
ignore(level21)

let level22 () =
  Board.parse 
    [ " , , ,ISc W 1, "
    ; "ISc W 5, , , , "
    ; " , , ,ISc W 2,ISc W 1"
    ; " ,ISc W 6, ,ISc W 3, "
    ; " , , ,ISc W 3, "
    ]
  |> Board.fill ~debug:true
    ~extra_checks:[Board.all_connected Color.B
                  ;Board.width_one Color.B
                  ;]
;;
ignore(level22)

let level23 () =
  Board.parse 
    [ " , ,W W 3, , "
    ; " ,W W 5, , ,W W 8"
    ; "W W 6, ,W W 7,W W 8, "
    ; " , , , , "
    ; " , ,W W 4, , "
    ]
  |> Board.fill ~debug:true
    ~extra_checks:[ Board.all_connected Color.W
                  ]
;;
ignore(level23)

let level24 () =
  Board.parse 
    [ " , , , , "
    ; " , , , ,NS B 4"
    ; " , , , , "
    ; "NS B 4, ,NS B 1, , "
    ; " ,NS B 1, ,NS B 2, "
    ]
  |> Board.fill ~debug:true
    ~extra_checks:[Board.courtyards Color.B]
;;
ignore(level24)

let level25 () =
  Board.parse 
    [ " , , , , "
    ; " ,NS B 5, ,IVS B 2,W W 4"
    ; " , , , , "
    ; " , , ,N W 2, "
    ; " , , ,NS B 2, "
    ]
  |> Board.fill ~debug:true
    ~extra_checks:[Board.courtyards Color.B
                  ;Board.width_one Color.B]
;;

let _template () =
  Board.parse 
    [ " , , , , "
    ; " , , , , "
    ; " , , , , "
    ; " , , , , "
    ; " , , , , "
    ]
  |> Board.fill ~debug:true
    ~extra_checks:[]
;;

let main () = level25 ();
;;

let () = main ()
