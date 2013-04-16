(*
 * Bug report to: Zhiqiang Ma (http://www.zhiqiangma.com)
 *)

open Printf;;

let rec _is_palindrome s i j =
  i >= j || 
  (s.[i] = s.[j] && 
      _is_palindrome s (i + 1) (j - 1))
;;

let is_palindrome s =
  let sl = String.length s in
    sl > 0 && (_is_palindrome s 0 (sl - 1))
;;

let rec del_zeros s =
  let sl = String.length s in
    if (sl < 1) then
      s
    else
      (if s.[0] = '0' then
         del_zeros (String.sub s 1 (sl - 1))
       else
         s
      )
;;

let c2i c =
  Char.code c - Char.code '0'
;;

let i2c i =
  Char.chr (i + Char.code '0')
;;

(* only for finding fair and square numbers *)
let square s =
  let slen = String.length s in
    if slen < 1 then
      ""
    else
      let reslen = 2 * slen in
      let t = ref 0 in
        t := 0;
        (* fast check *)
        (let i = reslen/2 in
           for j = (slen - 1) downto 0 do
             if (i - 1 - j) >= 0 && (i - 1 - j) < slen then
               t := !t + (c2i s.[j]) * (c2i s.[i - 1 - j]);
           done;
           if !t > 9 then
             (* jump out *)
             raise (Invalid_argument "carry");
        ); 
        (let res = String.make reslen '0' in
           (* do the square cal now *)
           for i = (reslen - 1) downto 1 do
             t := 0;
             for j = (slen - 1) downto 0 do
               if (i - 1 - j) >= 0 && (i - 1 - j) < slen then
                 t := !t + (c2i s.[j]) * (c2i s.[i - 1 - j]);
             done;
             if !t > 9 then
               (* jump out *)
               raise (Invalid_argument "carry");
             res.[i] <- i2c !t;
           done;
           del_zeros res
        );
;;

let rec check_fs fsns p =
  try let sq = square p in
    if (is_palindrome sq) then
      sq :: fsns
    else
      fsns
  with Invalid_argument "carry" ->
    fsns
;;

(* build the fair and square number list *)
(* dfs *)
let rec create_fair_square_nums fsns p sum max_num_digs =
  let l = String.length p in
    if l > max_num_digs || sum > 9 then
          fsns
    else
      let fsns = create_fair_square_nums fsns ("0" ^ p ^ "0") sum max_num_digs in
      let fsns = create_fair_square_nums fsns ("1" ^ p ^ "1") (sum + 1) max_num_digs in
      let fsns = create_fair_square_nums fsns ("2" ^ p ^ "2") (sum + 4)  max_num_digs in
      let fsns = create_fair_square_nums fsns ("3" ^ p ^ "3") (sum + 9) max_num_digs in
      let fsns = check_fs fsns p in
        fsns
;;

let rec print_fsns fsns =
  List.iter (fun s -> printf "%s\n" s) fsns;
;;

let num_str_cmp s1 s2 =
  let len1 = String.length s1 in
  let len2 = String.length s2 in
    match (len1 - len2) with
      | 0 ->
          String.compare s1 s2
      | cmp -> cmp
;;

(* works *)

let max_dig = 50;;

let fsns = 
  let fsns = create_fair_square_nums [] "" 0 max_dig in
  let fsns = create_fair_square_nums fsns "0" 0 max_dig in
  let fsns = create_fair_square_nums fsns "1" 1 max_dig in
  let fsns = create_fair_square_nums fsns "2" 4 max_dig in
    create_fair_square_nums fsns "3" 9 max_dig
;;

let fsns = List.sort num_str_cmp fsns;;

print_fsns fsns;;

