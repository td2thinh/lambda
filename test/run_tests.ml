open CoreLib.ListUtils

(* Testing list funcitions *)
let listA = [ 1; 2; 3; 4; 5 ]
let listB = [ 3; 4; 5; 6; 7 ]

let test_union_list () =
  let result = union_list listA listB in
  Alcotest.(check (list int)) "union_list" [ 1; 2; 3; 4; 5; 6; 7 ] result

let test_minus_list () =
  let result = minus_list listA listB in
  Alcotest.(check (list int)) "minus_list" [ 1; 2 ] result

let test_remove_elem () =
  let result = remove_elem listA 3 in
  Alcotest.(check (list int)) "remove_elem" [ 1; 2; 4; 5 ] result

let () =
  let open Alcotest in
  run "Lambda"
    [
      ( "ListUtils",
        [
          test_case "union_list" `Quick test_union_list;
          test_case "minus_list" `Quick test_minus_list;
          test_case "remove_elem" `Quick test_remove_elem;
        ] );
    ]
