open Pervasives
open Objects
open State
open Upgrade

type command =
  | Quit
  | Continue
  | Upgrade
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
    if h = "upgrade" then Upgrade else
    if h = "disease" then Disease else raise Malformed

(** [print_disease_menu_number disease] prints out information about the given
    disease [disease]. *)
let print_disease_menu_numbers (disease : Disease.t) =
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

(** [print_disease_menu disease] prints out information about the disease 
    [disease] and prompts the user to make any desired modifications 
    to the disease stats. *)
let print_disease_menu (disease : Disease.t) =
  let rec print_bar_helper percent color =
    let threshold = 5 in
    if percent >= threshold 
    then (print_string (color ^ "  ");
          print_bar_helper (percent - 5) color); in
  print_endline "1. Infectivity:";
  print_bar_helper disease.inner_tile_spread "\x1B[48;2;0;58;0m";
  print_bar_helper (100 - disease.inner_tile_spread) "\x1B[48;2;255;255;255m";
  print_endline "\027[0m\n";
  print_endline "2. Transmission Rate:";
  print_bar_helper disease.spread_probability "\x1B[48;2;0;58;0m";
  print_bar_helper (100 - disease.spread_probability) "\x1B[48;2;255;255;255m";
  print_endline "\027[0m\n";
  print_endline "3. Incidence:";
  print_bar_helper disease.tile_to_tile_spread "\x1B[48;2;0;58;0m";
  print_bar_helper (100 - disease.tile_to_tile_spread) "\x1B[48;2;255;255;255m";
  print_endline "\027[0m\n";
  print_endline "4. Virulence:";
  print_bar_helper disease.lethality "\x1B[48;2;200;0;0m";
  print_bar_helper (100 - disease.lethality) "\x1B[48;2;255;255;255m";
  print_endline "\027[0m\n";
  print_endline "If you would like to change anything, 
  type the stat number followed by the new value 
  ([1 40] changes the Infectivity to 40.)";
  try disease_parse (read_line ()) disease
  with Disease_Malformed -> disease

let check upgrade_name (state: State.t) =
  List.mem upgrade_name state.upgrades

(** [print_upgrade_menu state] prints out the disease information and allows
    the user to select any upgrades that they want to make to the disease. *)
let rec print_upgrade_menu (state: State.t) : State.t = 
  print_string ("\x1Bc
\027[0m                                              \x1B[38;2;51;0;0m " ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "                                                                         :%SSttXX31t 311 3110311@%X t            X@S 8          XS8S8X     ;S8SX311@:    
\027[0m                                              \x1B[38;2;70;0;0m " ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "                                                                     :8%@S8S31108XS@ 8t8%;;tt ;8 8X31;          X8@;31;:        8X t@8:   t8Xt3110X      
\027[0m                                              \x1B[38;2;91;0;0m " ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "                                                                    %XS8S8 8 :311S8 tS:         3110@S          S 8@X8:         " ^ (if (check "Vomiting" state) then "\x1B[38;2;0;0;153m" else "") ^ "@8;%XX   @@8X 8X31:\x1B[38;2;91;0;0m       
\027[0m                                              \x1B[38;2;111;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "                                                                   :%8@8X311X;31S31 :            S31108          ;8@8SXX:       " ^ (if (check "Vomiting" state) then "\x1B[38;2;0;0;153m" else "") ^ " 8XS31   @X31 311 ;\x1B[38;2;111;0;0m       
\027[0m                                              \x1B[38;2;133;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "                                                                  XtX@31108%31SX; 31t@           :%8@8SS          %31X31;       " ^ (if (check "Vomiting" state) then "\x1B[38;2;0;0;153m" else "") ^ " 8;8tX  S8%31 8:@  \x1B[38;2;133;0;0m       
\027[0m                                              \x1B[38;2;144;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "                                                                ;XXXXX311;3110t tt8tX :            8S8@8:         :@8S31X       " ^ (if (check "Vomiting" state) then "\x1B[38;2;0;0;153m" else "") ^ " SX8 8:8@8X31108   \x1B[38;2;144;0;0m       
\027[0m                                              \x1B[38;2;155;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "                                                              :@S@X311X31 X@t:   ;8X@8 ;           8;@31:          8;t8X8       " ^ (if (check "Vomiting" state) then "\x1B[38;2;0;0;153m" else "") ^ ";@8 31@@311@ 31t   \x1B[38;2;155;0;0m       
\027[0m                                              \x1B[38;2;166;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "                                                             %@31@@S SX8;t        @%8S@%%           @ X31@        : X8S:8:      " ^ (if (check "Vomiting" state) then "\x1B[38;2;0;0;153m" else "") ^ " S 8 31X311;8X:    \x1B[38;2;166;0;0m       
\027[0m                                              \x1B[38;2;173;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "                                                           S311X8;S@XS8S;          8@3110           ;8%31 :         3110%       " ^ (if (check "Vomiting" state) then "\x1B[38;2;0;0;153m" else "") ^ ": :X8X8S@311;      \x1B[38;2;173;0;0m       
\027[0m                                              \x1B[38;2;180;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "                                             " ^ (if (check "Avian Transmission" state) then "\x1B[38;2;204;0;204m" else "") ^ "            :tS%@8X8@%3110t          \x1B[38;2;180;0;0m" ^ (if (check "Fever" state) then "\x1B[38;2;51;153;255m" else "") ^ "X @31@8t    \x1B[38;2;180;0;0m     ;@S31X8         t;;SX@       %31S311 @ S              
\027[0m                                              \x1B[38;2;200;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "                                             " ^ (if (check "Avian Transmission" state) then "\x1B[38;2;204;0;204m" else "") ^ "          :@S311XX8 S31108S@         \x1B[38;2;200;0;0m" ^ (if (check "Fever" state) then "\x1B[38;2;51;153;255m" else "") ^ " ;31;8X%;   \x1B[38;2;200;0;0m      :8S 8St         31108:      8S:@ ;@:31               
\027[0m                                              \x1B[38;2;211;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "                                             " ^ (if (check "Avian Transmission" state) then "\x1B[38;2;204;0;204m" else "") ^ "          t311X311St31%31X@ :        \x1B[38;2;211;0;0m" ^ (if (check "Fever" state) then "\x1B[38;2;51;153;255m" else "") ^ "  ;tX8@@    \x1B[38;2;211;0;0m       % S31S%        X  8        8@S8S311X;               
\027[0m                                              \x1B[38;2;223;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "                                             " ^ (if (check "Avian Transmission" state) then "\x1B[38;2;204;0;204m" else "") ^ "        S3110 31@8 S: %311%S@        \x1B[38;2;223;0;0m" ^ (if (check "Fever" state) then "\x1B[38;2;51;153;255m" else "") ^ "    8X @tt  \x1B[38;2;223;0;0m        ;3110 ;        31S;8:   :%@tt@8@X 8                
\027[0m                                              \x1B[38;2;211;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "                                             " ^ (if (check "Avian Transmission" state) then "\x1B[38;2;204;0;204m" else "") ^ "       t8@311;@S ;;    :S 311@X      \x1B[38;2;211;0;0m" ^ (if (check "Fever" state) then "\x1B[38;2;51;153;255m" else "") ^ "    %31%8X8 \x1B[38;2;211;0;0m        :8XX%31        31@@XS  t3110  ::8t                 
\027[0m                                              \x1B[38;2;200;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "                                             " ^ (if (check "Avian Transmission" state) then "\x1B[38;2;204;0;204m" else "") ^ "     :X311@8@X8X8;       ;%@8;@:%    \x1B[38;2;200;0;0m" ^ (if (check "Fever" state) then "\x1B[38;2;51;153;255m" else "") ^ "     S8t 8@;\x1B[38;2;200;0;0m         X 8X8         ; 31SS  %31108X31t                  
\027[0m                                              \x1B[38;2;180;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "                                                 ;8@;3110X8 :t         " ^ (if (check "Aggression" state) then "\x1B[38;2;255;255;51m" else "") ^ "X8@;31;           X%%31@          tX8:X%     \x1B[38;2;180;0;0m   8@S31 8S:8% 8  8                   
\027[0m                                              \x1B[38;2;170;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "                                                S@S@31%8t 8X           " ^ (if (check "Aggression" state) then "\x1B[38;2;255;255;51m" else "") ^ " ;;t@8XX8          X@31X%S        ::@31X%    \x1B[38;2;170;0;0m  " ^ (if (check "Cannibalism" state) then "\x1B[38;2;255;102;102m" else "") ^ ";311%311 \x1B[38;2;170;0;0m8S8%%S                     
\027[0m                                              \x1B[38;2;160;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "                                              :SX@31%@S@S@ S           " ^ (if (check "Aggression" state) then "\x1B[38;2;255;255;51m" else "") ^ "   %:; 8;          :311SXSt        X@8S31;   \x1B[38;2;160;0;0m  " ^ (if (check "Cannibalism" state) then "\x1B[38;2;255;102;102m" else "") ^ ":8S31%@@3\x1B[38;2;160;0;0m11%8;                      
\027[0m                                              \x1B[38;2;155;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "                                             ;XSS311@31  8S:S8@        " ^ (if (check "Aggression" state) then "\x1B[38;2;255;255;51m" else "") ^ "    311X8SX         :St8XS8        ;X;X31X   \x1B[38;2;155;0;0m  " ^ (if (check "Cannibalism" state) then "\x1B[38;2;255;102;102m" else "") ^ " @ 31S8@X\x1B[38;2;155;0;0m311%                       
\027[0m                                              \x1B[38;2;144;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "    ::;;;:%%S%;:  :tt:                       8@3110S311@X311 8 ;:      " ^ (if (check "Aggression" state) then "\x1B[38;2;255;255;51m" else "") ^ "    :S3110;8X:        8X311Xt       : 3110@  \x1B[38;2;144;0;0m  SStXS8%t@ 31                        
\027[0m                                              \x1B[38;2;120;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "" ^ (if (check "Mosquito Transmission" state) then "\x1B[38;2;0;204;204m" else "") ^ "S;X@XS31XX3110X%31%8@8XX@t;SSt;:\x1B[38;2;120;0;0m             St@311 8S;  :8X 3110t           :SX8@31@        ;31X8t8:       X 311X;  t31 8S8X  8X                         
\027[0m                                              \x1B[38;2;111;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "" ^ (if (check "Mosquito Transmission" state) then "\x1B[38;2;0;204;204m" else "") ^ "S@X@S311S8SS311%S@31@X8@X311@8@;@3110SSt;:;8S31X@S%8;\x1B[38;2;111;0;0m     S31:8X 31;          :8  :8;        :@%XS8@8;       % 8@ 311S8 @8S8X 8%                          
\027[0m                                              \x1B[38;2;104;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "8@X311XXSX3110XX8@%31%311031@@%8@%8X31108@@8 :SXS X X        @311:31 8@        ;%311S 8         @ X31SS       8  8Xt31%@8 8 31;                           
\027[0m                                              \x1B[38;2;154;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "" ^ (if (check "Uncontained Rabies" state) then "\x1B[38;2;102;255;178m" else "") ^ "@S@8XX@3110311031103110@311%31108tS3110311S311031;8tSt8S@8X8% ;S8SX8X8S8  \x1B[38;2;154;0;0m       @: @S 8:        3110%8:      @S@t; 31tX:@%@;                             
\027[0m                                              \x1B[38;2;180;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "" ^ (if (check "Uncontained Rabies" state) then "\x1B[38;2;102;255;178m" else "") ^ "X311031@311;SX@3110311@t;XX31@3110311@311031X8S311@311031 X8 8XX311tt8X@S;\x1B[38;2;180;0;0m:       :SS%8@ ;        31:@8S @@@XX8X31t@X%S X 8t                              
\027[0m                                              \x1B[38;2;220;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "" ^ (if (check "Uncontained Rabies" state) then "\x1B[38;2;102;255;178m" else "") ^ "3110 31108S8%3110%@3110@8@311:31108%  SS::XSSSX8@@@311@8SS%@@;t31@ 8S   31\x1B[38;2;220;0;0mS;@;@%8S8SXX@@@Xt::   : 8%31 XXXX;311;8@ 31:311;:                               
\027[0m                                              \x1B[38;2;255;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "31@8Xtt;;       ;S31108X :;;::%t;tX31108StX8%S@@X8SX3" ^ (if (check "Rat Transmission" state) then "\x1B[38;2;0;204;102m" else "") ^ "110@31108X8S311031 31S3110S@@31%8t31%31\x1B[38;2;220;0;0m10@S8@XS %S 31 8t%tX311t311;:                                 
\027[0m                                              \x1B[38;2;215;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "31108            8XS8@ :          31108S8S8@ 3110311" ^ (if (check "Rat Transmission" state) then "\x1B[38;2;0;204;102m" else "") ^ "t:%t8XX31@XX31108X3110X@8@8X XX8S%8 StX\x1B[38;2;220;0;0m3110X8X@8StX31% 8SSS@X@8S@t                                   
\027[0m                                              \x1B[38;2;180;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ " SXS31           t 31@ :         SX%311X8X8 X::;;;;t    %31@31108 8tX@XX@8@311S31S @8@S31X 8XX::8 SX311 @X @8t31%8;                                       
\027[0m                                              \x1B[38;2;150;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "31108:;          ;X@X@X8    " ^ (if (check "Water Supply Infection" state) then "\x1B[38;2;255;153;51m" else "") ^ "    :311031@31@:\x1B[38;2;150;0;0m                   :;:tS8%31%%S8X % @8%t@8 @8tS8 S8;31:311 X8 X @%@:t:                                        
\027[0m                                              \x1B[38;2;125;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "3110311          tt@31@8:   " ^ (if (check "Water Supply Infection" state) then "\x1B[38;2;255;153;51m" else "") ^ "    t31X8 XSXtS \x1B[38;2;125;0;0m                        ::t;   ;St;X:t31S%8;8S: 8XXS 8t@X @8X31t:                                             
\027[0m                                              \x1B[38;2;100;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ ";;X8@%:            8@8 8    " ^ (if (check "Water Supply Infection" state) then "\x1B[38;2;255;153;51m" else "") ^ "   t@3110;:@8:  \x1B[38;2;100;0;0m                                          ;    ;SX::::%St                                                     
\027[0m                                              \x1B[38;2;105;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ " %X@31@t          S8S8@ :   " ^ (if (check "Water Supply Infection" state) then "\x1B[38;2;255;153;51m" else "") ^ "  %X8%3110 S    \x1B[38;2;105;0;0m                                                  :                                                           
\027[0m                                              \x1B[38;2;110;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ " ;S311X%          St31108   " ^ (if (check "Water Supply Infection" state) then "\x1B[38;2;255;153;51m" else "") ^ ":tSt8% :31%X    \x1B[38;2;110;0;0m                                                                                                              
\027[0m                                              \x1B[38;2;125;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "  tt311@X         X @8XXX   S311tS8@ @t                                                                                                                   
\027[0m                                              \x1B[38;2;135;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "   t31108         " ^ (if (check "Internal Hemorrhaging" state) then "\x1B[38;2;255;255;51m" else "") ^ "%;X %31% tS 31X311S :\x1B[38;2;135;0;0m                                                                                                                   
\027[0m                                              \x1B[38;2;143;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "   ;t8@31X        " ^ (if (check "Internal Hemorrhaging" state) then "\x1B[38;2;255;255;51m" else "") ^ " ;t@31 S8S8@311XSt;  \x1B[38;2;143;0;0m                                                                                                                   
\027[0m                                              \x1B[38;2;158;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "     @8@8 t       " ^ (if (check "Internal Hemorrhaging" state) then "\x1B[38;2;255;255;51m" else "") ^ " :XS@8%%X8t@31108:   \x1B[38;2;158;0;0m                                                                                                                   
\027[0m                                              \x1B[38;2;165;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "     @S31SS:      " ^ (if (check "Internal Hemorrhaging" state) then "\x1B[38;2;255;255;51m" else "") ^ " :%8X8:S31108t31;    \x1B[38;2;165;0;0m                                                                                                                   
\027[0m                                              \x1B[38;2;178;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "     t;@@311      " ^ (if (check "Internal Hemorrhaging" state) then "\x1B[38;2;255;255;51m" else "") ^ "  SS8@:@3110SS ::    \x1B[38;2;178;0;0m                                                                                                                   
\027[0m                                              \x1B[38;2;190;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "      311X8@        S31@8X31X8 8:                                                                                                                         
\027[0m                                              \x1B[38;2;210;0;0m" ^ (if (check "Total Collapse" state) then "\x1B[38;2;0;0;0m\027[1m" else "") ^ "      %31@31S       @311t8X8:8:;                                                                                                                          
\027[0m\n");

  let disease : Disease.t = state.disease in
  let rec print_bar_helper percent color =
    let threshold = 5 in
    if percent >= threshold 
    then (print_string (color ^ " ");
          print_bar_helper (percent - threshold) color); in
  print_string "\027[1m\x1B[22;10HDisease Information:\027[0m";
  print_string "\x1B[24;10H1. Infectivity:";
  print_string "\x1B[25;10H";
  print_bar_helper disease.inner_tile_spread "\x1B[48;2;0;58;0m";
  print_bar_helper (100 - disease.inner_tile_spread) "\x1B[48;2;160;160;160m";
  print_string "\027[0m\n";
  print_string "\x1B[27;10H2. Transmission Rate:";
  print_string "\x1B[28;10H";
  print_bar_helper disease.spread_probability "\x1B[48;2;0;58;0m";
  print_bar_helper (100 - disease.spread_probability) "\x1B[48;2;160;160;160m";
  print_string "\027[0m\n";
  print_string "\x1B[30;10H3. Incidence:";
  print_string "\x1B[31;10H";
  print_bar_helper disease.tile_to_tile_spread "\x1B[48;2;0;58;0m";
  print_bar_helper (100 - disease.tile_to_tile_spread) "\x1B[48;2;160;160;160m";
  print_string "\027[0m\n";
  print_string "\x1B[33;10H4. Virulence:";
  print_string "\x1B[34;10H";
  print_bar_helper disease.lethality "\x1B[48;2;200;0;0m";
  print_bar_helper (100 - disease.lethality) "\x1B[48;2;160;160;160m";
  print_string "\027[0m\n";

  print_string "\x1B[40;100H\x1B[38;2;255;255;255m\027[1mAvailable Upgrades:\n";
  print_string "\027[0m\x1B[99C";
  let count = ref 0 in
  List.iter (fun ele -> 
      if not(List.mem (String.lowercase_ascii ele) (List.map String.lowercase_ascii state.upgrades))
      then print_string (ele ^ (if !count >= (List.length upgrade_list - List.length state.upgrades) then "" else ", ") ^ (count := !(count) + 1; if (!count) mod 4 = 0 then "\n\x1B[99C" else ""))) upgrade_list; 

  print_endline ("\x1B[4;10H\x1B[38;2;255;255;255m\027[1m" ^ state.name);
  print_string "\n\027[0m\x1B[5;10H\x1B[38;2;255;255;255mType \"[upgrade_name]\" to upgrade your disease. Type [continue] to exit.";
  print_string "\x1B[6;10H\x1B[38;2;255;255;255m> ";
  match String.lowercase_ascii (read_line ()) with
  | "mosquito transmission" -> print_upgrade_menu {state with disease = upgrade_disease mosquito_transmission disease;
                                                              upgrades = "Mosquito Transmission"::state.upgrades}
  | "rat transmission" -> print_upgrade_menu {state with disease = upgrade_disease rat_transmission disease;
                                                         upgrades = "Rat Transmission"::state.upgrades}
  | "water supply infection" -> print_upgrade_menu {state with disease = upgrade_disease water_supply_infection disease;
                                                               upgrades = "Water Supply Infection"::state.upgrades}
  | "avian transmission" -> print_upgrade_menu {state with disease = upgrade_disease avian_transmission disease;
                                                           upgrades = "Avian Transmission"::state.upgrades}
  | "fever" -> print_upgrade_menu {state with disease = upgrade_disease fever disease;
                                              upgrades = "Fever"::state.upgrades}
  | "vomiting" -> print_upgrade_menu {state with disease = upgrade_disease vomiting disease;
                                                 upgrades = "Vomiting"::state.upgrades}
  | "aggression" -> print_upgrade_menu {state with disease = upgrade_disease aggression disease;
                                                   upgrades = "Aggression"::state.upgrades}
  | "internal hemorrhaging" -> print_upgrade_menu {state with disease = upgrade_disease internal_hemorrhaging disease;
                                                              upgrades = "Internal Hemorrhaging"::state.upgrades}
  | "cannibalism" -> print_upgrade_menu {state with disease = upgrade_disease cannibalism disease;
                                                    upgrades = "Cannibalism"::state.upgrades}
  | "uncontained rabies" -> print_upgrade_menu {state with disease = upgrade_disease uncontained_rabies disease;
                                                           upgrades = "Uncontained Rabies"::state.upgrades}
  | "total collapse" -> print_upgrade_menu {state with disease = upgrade_disease total_collapse disease;
                                                       upgrades = "Total Collapse"::state.upgrades}
  | "continue" -> state
  | _ -> print_string "\x1B[9C\027[31mThat is not an available upgrade! Press [Enter] to continue."; 
    flush Pervasives.stdout;
    (match input_char Pervasives.stdin with
     | _ -> print_upgrade_menu state);