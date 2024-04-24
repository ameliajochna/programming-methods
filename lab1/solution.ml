let ( let* ) xs ys = List.concat_map ys xs

let rec choose m n =
  if m > n then [] else m :: choose (m+1) n

let build_row ps n =

  let rec add_to_each lst lst_of_lsts =
    match lst_of_lsts with
    | [] -> []
    | hd::tl -> (lst @ hd) :: add_to_each lst tl in

  let rec generate_true_list count =
    if count = 0 then []
    else true :: generate_true_list (count-1) in

    let rec building cur_des cur_char cur_len =
    if (List.length cur_des) = 0 && cur_len = n then [[]]
    else if cur_len >= n then []

    else if cur_char = true then
      if (List.length cur_des) = 0 then []
      else
        let true_list = generate_true_list (List.hd cur_des) in
        let solutions = building (List.tl cur_des) false (cur_len + (List.hd cur_des)) in
        if solutions != [[]] then add_to_each true_list solutions
        else [[]]

    else
      let true_solutions = building cur_des true (cur_len+1) in
      let false_solutions = building cur_des false (cur_len+1) in
      if true_solutions = [] && false_solutions = [] then []
      else if true_solutions = [] then add_to_each [false] false_solutions
      else if false_solutions = [] then add_to_each [false] true_solutions
      else if true_solutions = [[]] && false_solutions = [[]] then add_to_each [false] true_solutions
      else (add_to_each [false] false_solutions) @ (add_to_each [false] true_solutions)
    in (building ps true 0) @ (building ps false 0)

let rec build_candidate pss n =
  let rec add_to_each lst lst_of_lsts =
    match lst_of_lsts with
    | [] -> []
    | hd::tl -> (lst @ hd) :: add_to_each lst tl in
  let rec combine_solutions row1 row2 =
    match row1 with
    | [] -> []
    | hd :: tl -> add_to_each hd row2 @ combine_solutions tl row2 in

  let rec additional_brackets lst_lsts =
    match lst_lsts with
    | [] -> []
    | hd :: tl -> [hd] :: additional_brackets tl in
  match pss with
  | [] -> [[]]
  | hd::tl ->
    let cur_build = build_row hd n in
    combine_solutions (additional_brackets cur_build) (build_candidate tl n)

let verify_row ps xs =
  let rec verifying des sol true_count =
    match sol with
    | [] -> (((List.length des) = 1 && (List.hd des) = true_count) || ((List.length des) = 0 && true_count = 0))
    | hd :: tl ->
      if hd = true then verifying des tl (true_count+1)
      else
        if true_count>0 then
          if (List.length des)>0 && (List.hd des) = true_count then verifying (List.tl des) tl 0
          else false
        else
          verifying des tl 0
        in
    verifying ps xs 0

let rec verify_rows pss xss =
  match pss with
  | [] -> true
  | hd_row_des::tl_row_des -> (List.length xss)>0 && verify_row hd_row_des (List.hd xss) && verify_rows tl_row_des (List.tl xss)

let rec transpose xss =
  let rec split_elements lst =
    match lst with
    | [] -> []
    | hd::tl -> [hd] :: split_elements tl in
  let rec add_split_lists lst1 lst2 =
      match lst1 with
      | [] -> []
      | hd::tl ->
        if List.length lst2 = 0 then lst1
        else (hd @ (List.hd lst2)) :: add_split_lists tl (List.tl lst2) in
  match xss with
  | [] -> []
  | hd::tl -> add_split_lists (split_elements hd) (transpose tl)

type nonogram_spec = {rows: int list list; cols: int list list}

let solve_nonogram nono =
  build_candidate (nono.rows) (List.length (nono.cols))
  |> List.filter (fun xss -> transpose xss |> verify_rows nono.cols)

let example_1 = {
  rows = [[2];[1];[1]];
  cols = [[1;1];[2]]
}

let example_2 = {
  rows = [[2];[2;1];[1;1];[2]];
  cols = [[2];[2;1];[1;1];[2]]
}

let big_example = {
  rows = [[1;2];[2];[1];[1];[2];[2;4];[2;6];[8];[1;1];[2;2]];
  cols = [[2];[3];[1];[2;1];[5];[4];[1;4;1];[1;5];[2;2];[2;1]]
}
