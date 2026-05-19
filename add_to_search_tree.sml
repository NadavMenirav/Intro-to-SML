datatype 'a binary_tree =
    Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;

fun add_to_search_tree Empty x = Node(x, Empty, Empty)
    | add_to_search_tree (Node(node, left, right)) x =
    if x > node then Node(node, left, (add_to_search_tree right x))
    else Node(node, (add_to_search_tree left x), right);