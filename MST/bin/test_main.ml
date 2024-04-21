(* Test case 1: Graph with 3 vertices *)
let graph1 = [(1, 2.0, 3); (2, 3.0, 4); (3, 4.0, 5)]
let expected_mst1 = [(1, 2.0, 3); (2, 3.0, 4); (3, 4.0, 5)]
assert (mst graph1 = expected_mst1)

(* Test case 2: Graph with 4 vertices *)
let graph2 = [(1, 2.0, 3); (2, 3.0, 4); (3, 4.0, 5); (4, 5.0, 6)]
let expected_mst2 = [(1, 2.0, 3); (2, 3.0, 4); (3, 4.0, 5); (4, 5.0, 6)]
assert (mst graph2 = expected_mst2)

(* Test case 3: Graph with 5 vertices *)
let graph3 = [(1, 2.0, 2); (2, 3.0, 3); (3, 4.0, 4); (4, 5.0, 5); (5, 6.0, 6)]
let expected_mst3 = [(1, 2.0, 2); (2, 3.0, 3); (3, 4.0, 4); (4, 5.0, 5)]
assert (mst graph3 = expected_mst3)