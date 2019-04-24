open Pervasives
open Objects

type command =
  | Quit
  | Continue
  | Disease

exception Empty
exception Malformed
exception Disease_Malformed

let disease_parse (str : string) (disease : Disease.t) =
  let command_list = (String.split_on_char ' ' str) in
  let filtered_list = List.filter (fun ele -> ele <> "") command_list in
  let lst_to_int lst = if List.length lst <> 1 then raise Disease_Malformed else
      try int_of_string (List.hd lst)
      with Failure x -> raise Disease_Malformed in
  match filtered_list with
  | [] -> disease
  | h::t ->
    if h = "exit" then raise Disease_Malformed else
    if h = "1" then {disease with inner_tile_spread = lst_to_int t} else
    if h = "2" then {disease with tile_to_tile_spread = lst_to_int t} else
    if h = "3" then {disease with spread_probability = lst_to_int t} else
    if h = "4" then {disease with lethality = lst_to_int t} else
      raise Disease_Malformed

let parse (str : string) =
  let command_list = (String.split_on_char ' ' str) in
  let filtered_list = List.filter (fun ele -> ele <> "") command_list in
  match filtered_list with
  | [] -> raise Empty
  | h::t ->
    if h = "quit" then Quit else
    if h = "continue" then Continue else
    if h = "disease" then Disease else raise Malformed

let print_disease_menu (disease : Disease.t) =
  print_string "Current disease stats:\n1. Inner tile spread: ";
  print_endline (string_of_int disease.inner_tile_spread);
  print_string "2. Tile to tile spread: ";
  print_endline (string_of_int disease.tile_to_tile_spread);
  print_string "3. Spread probability: ";
  print_endline (string_of_int disease.spread_probability);
  print_string "4. Lethality: ";
  print_endline (string_of_int disease.lethality);
  print_endline "If you would like to change anything, 
  type the stat number followed by the new value 
  ([1 40] changes the inner tile spread to 40.";
  try disease_parse (read_line ()) disease
  with Disease_Malformed -> disease

let print_disease_info disease =
  print_string "\n                                                                                                                                                   
                                                                                                                                                         t
                                                                                                                                                       :t%
                                                                                                                                                      ;S@@
                                                                                                            :  %S; S@88X%S8X8X8@S 8@@t8@@@::::  ::   ;88X8
                                                                                                    :S88:;@St@@8@88888888888X8@XX88@@888S8X8 8X8888 @%@%X@
                                                                                                :SS88X8SX88S@888888888X@8888XXtX8888888@8S8888888S8tX888%8
                                                                                           :;X88%88888888S88;X@88888%88888X8888S888X@8;888888%8888@88888S8
                                                                                       ;%%888@88@@S8XS8888XS88  88@SX SS  S 8SS%t8XS;8XX S :8XS%S SX888@88
                                                                                      %%@8888S888888  8888tX;:8S:8;88;S88S;;;: ;%S8 8XXS;S%S%@t;%8 XS   % 
                                                                                 ttt;XtS88S@8S8t8@888 8;  88;;@888S:S  ;:        @8;8 @        88 88 88888
                                                                               tXS@S8@8t888X88tX8888 88St;      8 X@8S           tX8S t       ;88S8X8X888S
                                                                            tS88S@88@@8St8t8 XS8%8888t:          8X%8St          88X:S       S%8888888S%SS
                                                                          :%SSttXX88t 888 8888888@%X t            X@S 8          XS8S8X     ;S8SX888@:    
                                                                      :8%@S8S88888XS@ 8t8%;;tt ;8 8X88;          X8@;88;:        8X t@8:   t8Xt8888X      
                                                                     %XS8S8 8 :888S8 tS:         8888@S          S 8@X8:         @8;%XX   @@8X 8X88:      
                                                                   :%8@8X888X;88S88 :            S88888          ;8@8SXX:        8XS88   @X88 888 ;       
                                                                  XtX@88888%88SX; 88t@           :%8@8SS          %88X88;        8;8tX  S8%88 8:@         
                                                                ;XXXXX888;8888t tt8tX :            8S8@8:         :@8S88X        SX8 8:8@8X88888          
                                                              :@S@X888X88 X@t:   ;8X@8 ;           8;@88:          8;t8X8       ;@8 88@@888@ 88t          
                                                             %@88@@S SX8;t        @%8S@%%           @ X88@        : X8S:8:       S 8 88X888;8X:           
                                                           S888X8;S@XS8S;          8@8888           ;8%88 :         8888%       : :X8X8S@888;             
                                                         :tS%@8X8@%8888t           X @88@8t         ;@S88X8         t;;SX@       %88S888 @ S              
                                                       :@S888XX8 S88888S@           ;88;8X%;         :8S 8St         88888:      8S:@ ;@:88               
                                                       t888X888St88%88X@ :           ;tX8@@           % S88S%        X  8        8@S8S888X;               
                                                     S8888 88@8 S: %888%S@             8X @tt          ;8888 ;        88S;8:   :%@tt@8@X 8                
                                                    t8@888;@S ;;    :S 888@X           %88%8X8         :8XX%88        88@@XS  t8888  ::8t                 
                                                  :X888@8@X8X8;       ;%@8;@:%          S8t 8@;         X 8X8         ; 88SS  %88888X88t                  
                                                 ;8@;8888X8 :t         X8@;88;           X%%88@          tX8:X%        8@S88 8S:8% 8  8                   
                                                S@S@88%8t 8X            ;;t@8XX8          X@88X%S        ::@88X%      ;888%888 8S8%%S                     
                                              :SX@88%@S@S@ S              %:; 8;          :888SXSt        X@8S88;     :8S88%@@888%8;                      
                                             ;XSS888@88  8S:S8@            888X8SX         :St8XS8        ;X;X88X      @ 88S8@X888%                       
    ::;;;:%%S%;:  :tt:                       8@8888S888@X888 8 ;:          :S8888;8X:        8X888Xt       : 8888@    SStXS8%t@ 88                        
S;X@XS88XX8888X%88%8@8XX@t;SSt;:             St@888 8S;  :8X 8888t           :SX8@88@        ;88X8t8:       X 888X;  t88 8S8X  8X                         
S@X@S888S8SS888%S@88@X8@X888@8@;@8888SSt;:;8S88X@S%8;     S88:8X 88;          :8  :8;        :@%XS8@8;       % 8@ 888S8 @8S8X 8%                          
8@X888XXSX8888XX8@%88%888888@@%8@%8X88888@@8 :SXS X X        @888:88 8@        ;%888S 8         @ X88SS       8  8Xt88%@8 8 88;                           
@S@8XX@8888888888888888@888%88888tS8888888S888888;8tSt8S@8X8% ;S8SX8X8S8         @: @S 8:        8888%8:      @S@t; 88tX:@%@;                             
X888888@888;SX@8888888@t;XX88@8888888@888888X8S888@888888 X8 8XX888tt8X@S;:       :SS%8@ ;        88:@8S @@@XX8X88t@X%S X 8t                              
8888 88888S8%8888%@8888@8@888:88888%  SS::XSSSX8@@@888@8SS%@@;t88@ 8S   88S;@;@%8S8SXX@@@Xt::   : 8%88 XXXX;888;8@ 88:888;:                               
88@8Xtt;;       ;S88888X :;;::%t;tX88888StX8%S@@X8SX8888@88888X8S888888 88S8888S@@88%8t88%8888@S8@XS %S 88 8t%tX888t888;:                                 
88888            8XS8@ :          88888S8S8@ 8888888 t:%t8XX88@XX88888X8888X@8@8X XX8S%8 StX8888X8X@8StX88% 8SSS@X@8S@t                                   
 SXS88           t 88@ :         SX%888X8X8 X::;;;;t    %88@88888 8tX@XX@8@888S88S @8@S88X 8XX::8 SX888 @X @8t88%8;                                       
88888:;          ;X@X@X8        :888888@88@:                   :;:tS8%88%%S8X % @8%t@8 @8tS8 S8;88:888 X8 X @%@:t:                                        
8888888          tt@88@8:       t88X8 XSXtS                         ::t;   ;St;X:t88S%8;8S: 8XXS 8t@X @8X88t:                                             
;;X8@%:            8@8 8       t@8888;:@8:                                            ;    ;SX::::%St                                                     
 %X@88@t          S8S8@ :     %X8%8888 S                                                      :                                                           
 ;S888X%          St88888   :tSt8% :88%X                                                                                                                  
  tt888@X         X @8XXX   S888tS8@ @t                                                                                                                   
   t88888         %;X %88% tS 88X888S :                                                                                                                   
   ;t8@88X         ;t@88 S8S8@888XSt;                                                                                                                     
     @8@8 t        :XS@8%%X8t@88888:                                                                                                                      
     @S88SS:       :%8X8:S88888t88;                                                                                                                       
     t;@@888        SS8@:@8888SS ::                                                                                                                       
      888X8@        S88@8X88X8 8:                                                                                                                         
      %88@88S       @888t8X8:8:;                                                                                                                          
      :XX8S88S    :SX888@@8@@8;                                                                                                                           
       88@88X8:  %;X888@88S8%;                                                                                                                            
       ::X88XX%8X88888X88X8;                                                                                                                              "