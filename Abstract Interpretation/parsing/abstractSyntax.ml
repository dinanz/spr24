(* file abstractSyntax.ml   © P. Cousot 2021 *)

type variable = string
type aexpr = Num of int | Var of string | Minus of aexpr * aexpr
type bexpr = Lt of aexpr * aexpr | Nand of bexpr * bexpr
type 'a tree =
  | Prog of 'a tree list * 'a
  | Assign of variable * aexpr * 'a
  | Emptystmt of 'a
  | If of bexpr * 'a tree * 'a
  | Ifelse of bexpr * 'a tree * 'a tree * 'a
  | While of bexpr * 'a tree * 'a
  | Break of 'a
  | Stmtlist of 'a tree list * 'a (* trees in inverse order *)
type program_label = int
type labelling = program_label * program_label * bool * program_label
     (* at, after, escape, break *)
let void_label = 0
let void_labelling = (void_label, void_label, false, void_label)
type labelled_tree = labelling tree (* with labeling attributes *)

let rec add_attributes s = match s with 
    | Prog (sl, _) -> Prog (add_attributes_treelist sl, void_labelling)
    | Assign (v, a, _) ->Assign (v, a, void_labelling)
    | Emptystmt _ -> Emptystmt void_labelling
    | If (b, st, _) -> If (b, add_attributes st, void_labelling)
    | Ifelse (b, st, se, _) -> Ifelse (b, add_attributes st, add_attributes se, void_labelling)
    | While (b, sb, _) -> While (b, add_attributes sb, void_labelling)
    | Break _ -> Break void_labelling
    | Stmtlist (sl, _)  -> Stmtlist (add_attributes_treelist sl, void_labelling)
and add_attributes_treelist sl = match sl with 
    | [] -> []
    | [s] -> [add_attributes s]
    | s :: sl' -> add_attributes s :: add_attributes_treelist sl';; (* trees in inverse order *)

let at_stmt s = match s with 
    | Prog (sl, (at,af,es,br)) -> at
    | Assign (v, a, (at,af,es,br)) -> at
    | Emptystmt (at,af,es,br) -> at
    | If (b, st, (at,af,es,br)) -> at
    | Ifelse (b, st, se, (at,af,es,br)) -> at
    | While (b, sb, (at,af,es,br)) -> at
    | Break (at,af,es,br) -> at
    | Stmtlist (sl, (at,af,es,br)) -> at

let after_stmt s = match s with 
    | Prog (sl, (at,af,es,br)) -> af
    | Assign (v, a, (at,af,es,br)) -> af
    | Emptystmt (at,af,es,br) -> af
    | If (b, st, (at,af,es,br)) -> af
    | Ifelse (b, st, se, (at,af,es,br)) -> af
    | While (b, sb, (at,af,es,br)) -> af
    | Break (at,af,es,br) -> af
    | Stmtlist (sl, (at,af,es,br)) -> af

let escape_stmt s = match s with 
    | Prog (sl, (at,af,es,br)) -> es
    | Assign (v, a, (at,af,es,br)) -> es
    | Emptystmt (at,af,es,br) -> es
    | If (b, st, (at,af,es,br)) -> es
    | Ifelse (b, st, se, (at,af,es,br)) -> es
    | While (b, sb, (at,af,es,br)) -> es
    | Break (at,af,es,br) -> es
    | Stmtlist (sl, (at,af,es,br)) -> es

let break_stmt s = match s with 
    | Prog (sl, (at,af,es,br)) -> br
    | Assign (v, a, (at,af,es,br)) -> br
    | Emptystmt (at,af,es,br) -> br
    | If (b, st, (at,af,es,br)) -> br
    | Ifelse (b, st, se, (at,af,es,br)) -> br
    | While (b, sb, (at,af,es,br)) -> br
    | Break (at,af,es,br) -> br
    | Stmtlist (sl, (at,af,es,br)) -> br
    
let rec is_in_stmt l s = match s with 
    | Prog (sl, (at,af,es,br)) -> (l=at || l=af || is_in_stmt_list l sl)
    | Assign (v, a, (at,af,es,br)) -> (l=at)
    | Emptystmt (at,af,es,br) -> (l=at)
    | If (b, st, (at,af,es,br)) -> (l=at || is_in_stmt l st)
    | Ifelse (b, st, se, (at,af,es,br)) -> (l=at || is_in_stmt l st || is_in_stmt l se)
    | While (b, sb, (at,af,es,br)) -> (l=at || is_in_stmt l sb)
    | Break (at,af,es,br) -> (l=at)
    | Stmtlist (sl, (at,af,es,br)) -> (l=at || is_in_stmt_list l sl)
and is_in_stmt_list l sl = match sl with
    | [] -> false
    | s' :: sl' -> (is_in_stmt l s') || (is_in_stmt_list l sl');;

let rec set_at_tree s l =  match s with (* l is the last label set *)
    | Prog (sl, (at,af,es,br)) -> let (sl', next) = set_at_tree_list sl l in
                                      (Prog (sl', (l,af,es,br)), next)
    | Assign (v, a, (at,af,es,br)) -> (Assign (v, a, (l,af,es,br)), l+1)
    | Emptystmt (at,af,es,br) -> (Emptystmt (l,af,es,br), l+1)
    | If (b, st, (at,af,es,br)) -> let (st', next) = set_at_tree st (l+1) in
                                       (If (b, st', (l,af,es,br)), next)
    | Ifelse (b, st, se, (at,af,es,br)) -> let (st', next) = set_at_tree st (l+1) in
                                               let (se', next') = set_at_tree se next in
                                                  (Ifelse (b, st', se', (l,af,es,br)), next')
    | While (b, sb, (at,af,es,br)) -> let (sb', next) = set_at_tree sb (l+1) in
                                           ( While (b, sb', (l,af,es,br)), next)
    | Break (at,af,es,br) -> (Break (l,af,es,br), l+1)
    | Stmtlist (sl, (at,af,es,br)) -> let (sl', next) = set_at_tree_list sl l in 
                                 (Stmtlist (sl', (l,af,es,br)), next)
and set_at_tree_list sl l = match sl with 
    | [] -> ([], l)
    | [s'] -> let (s'', next) = set_at_tree s' l in
                 ([s''], next)
    | s' :: sl' -> (* trees in inverse order *)
                   let (sl'', next) = set_at_tree_list sl' l in 
                     let (s'', next') = set_at_tree s' next in
                        (s'' :: sl'', next');;
                        

let rec set_labelling_tree s after break = (* set the af, es, and br attributes *)
    match s with
    | Prog (sl, (at,af,es,br)) -> let (sl', es') = set_labelling_tree_list sl after break in
                                        if es' then print_string "Error: no break allowed out of a program\n";
                                        (Prog (sl', (at,after,false,void_label)), false)
    | Assign (v, a, (at,af,es,br)) ->  (Assign (v, a, (at,after,false,void_label)), false)
    | Emptystmt (at,af,es,br) -> (Emptystmt (at,after,false,void_label), false)
    | If (b, st, (at,af,es,br)) -> let (st',es') = set_labelling_tree st after break in
                                        if es' then
                                           (If (b, st', (at,after,es',break)),es')
                                        else
                                           (If (b, st', (at,after,es',void_label)),es')
     | Ifelse (b, st, se, (at,af,es,br)) -> 
                                    let (st',es') = set_labelling_tree st after break in
                                       let (se',es'') = set_labelling_tree se after break in
                                         let es''' = es' || es'' in
                                            if es''' then
                                               (Ifelse (b, st', se', (at,after,es''',break)), es''')
                                            else
                                               (Ifelse (b, st', se', (at,after,es''',void_label)), es''')
    | While (b, sb, (at,af,es,br)) -> let (sb',es') = set_labelling_tree sb at after in
                                           (While (b, sb', (at,after,false,void_label)),false)
    | Break (at,af,es,br) -> (Break (at,after,true,break)), true
    | Stmtlist (sl, (at,af,es,br)) -> let (sl', es') = set_labelling_tree_list sl after break in
                               if es' then
                                  (Stmtlist (sl', (at,after,es',break)), es')
                               else
                                  (Stmtlist (sl', (at,after,es',void_label)), es')
and set_labelling_tree_list sl after break = match sl with
    | [] -> ([], false)
    | [s'] -> let (s'', es') = set_labelling_tree s' after break in
                ([s''], es')
    | s' :: sl' -> let (sl'', es') = set_labelling_tree_list sl' (at_stmt s') break in
                      let (s'', es'') = set_labelling_tree s' after break in
                         let es''' = es' || es'' in
                            (s'' :: sl'', es''');;
    
let built_abstract_syntax p =
   let first_label = 1 in
      let (p, next) = set_at_tree (add_attributes (p)) first_label in
         let (p', b) = (set_labelling_tree p next next) in
            p';;
