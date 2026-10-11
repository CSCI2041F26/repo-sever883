type 'a lb_tree = Leaf of 'a
                | Branch of 'a lb_tree * 'a * 'a lb_tree

let lb_t1 = Leaf 4;;
let lb_t2 = Branch (Leaf 2, 3, Leaf 4);;
let lb_t3 = Branch (Branch (Leaf 1, 4, Leaf 2),
                    5,
                    Branch (Branch (Leaf 2, 1, Leaf 3),
                            3,
                            Leaf 1)
              );;
let lb_t4 = Branch (Branch (Branch (Leaf 2, 2, Leaf 3),
                            3,
                            Leaf 6),
                    5,
                    Branch (Leaf 9, 4, Leaf 5)
              );;

let rec lb_product (tree: int lb_tree): int = 
	match tree with
	| Leaf value -> value
	| Branch (l_tree, value, r_tree) -> value * lb_product l_tree * lb_product r_tree

let rec lb_square_all (i_tree: int lb_tree): int lb_tree =
	match i_tree with
	| Leaf v -> Leaf (v * v)
	| Branch (l_tree, v, r_tree) -> 
		Branch (lb_square_all l_tree, v * v, lb_square_all r_tree)

let rec lb_max_val (tree: int lb_tree): int = 
	match tree with
	| Leaf v -> v
	| Branch (lt, v, rt) -> max (max (lb_max_val lt) (lb_max_val rt)) v

let rec lb_depth (tree: 'a lb_tree): int =
	match tree with
	| Leaf _ -> 1
	| Branch (lt, _, rt) -> 1 + max (lb_depth lt) (lb_depth rt)

let lb_path_range (tree: 'a lb_tree): int * int = 
	let rec long_helper (tree: 'a lb_tree): int =
		match tree with
		| Leaf _ -> 1
		| Branch (lt, _, rt) -> 1 + max (long_helper lt) (long_helper rt)
	in
	let rec short_helper (tree: 'a lb_tree): int =
		match tree with
		| Leaf _ -> 1
		| Branch (lt, _, rt) -> 1 + max (short_helper lt) (short_helper rt)
	in
	match tree with
	| Leaf _ -> (1,1)
	| Branch (lt, _, rt) -> (
		1 + min (short_helper lt) (short_helper rt),
		1 + max (long_helper lt) (long_helper rt)
		)