datatype 'a binary_tree =
    Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;

fun add_to_search_tree Empty x = Node(x, Empty, Empty)
    | add_to_search_tree (Node(node, left, right)) x =
    if x > node then Node(node, left, (add_to_search_tree right x))
    else Node(node, (add_to_search_tree left x), right);


(*
The problem with floating point numbers is that the operator > works on integers only by default. In order for that to
work on real numbers we just need to change this line 
    if x > node then Node(node, left, (add_to_search_tree right x))


to this line 

    if (x: real) > node then Node(node, left, (add_to_search_tree right x))
    This tells the compiler that the whole tree is from real type
*)