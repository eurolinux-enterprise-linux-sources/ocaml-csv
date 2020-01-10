(* csv.ml - comma separated values parser
 *
 * $Id: csv.ml,v 1.15 2008-10-27 21:57:48 rich Exp $
 *)

(* The format of CSV files:
 * 
 * Each field starts with either a double quote char or some other
 * char. For the some other char case things are simple: just read up
 * to the next comma (,) which marks the end of the field.
 * 
 * In the case where a field begins with a double quote char the
 * parsing rules are different. Any double quotes are doubled ("") and
 * we finish reading when we reach an undoubled quote. eg: "The
 * following is a quote: "", and that's all" is the CSV equivalent of
 * the following literal field: The following is a quote: ", and that's
 * all
 *
 * "0 is the quoted form of ASCII NUL.
 * 
 * CSV fields can also contain literal carriage return characters, if
 * they are quoted, eg: "This field
 * is split over lines" represents a
 * single field containing a \n.
 * 
 * Excel will only use the quoting format if a field contains a double
 * quote or comma, although there's no reason why Excel couldn't always
 * use the quoted format.
 * 
 * The practical upshot of this is that you can't split a line in a CSV
 * file just by looking at the commas. You need to parse each field
 * separately.
 * 
 * How we represent CSV files:
 * 
 * We load in the whole CSV file at once, and store it internally as a
 * 'string list list' type (note that each line in the CSV file can,
 * and often will, have different lengths). We then provide simple
 * functions to read the CSV file line-by-line, copy it out, or copy a
 * subset of it into a matrix.
 *)

open Printf

(* Uncomment the next line to enable Extlib's List function.  These
 * avoid stack overflows on really huge CSV files.
 *)
(*open ExtList*)

type t = string list list

exception Bad_CSV_file of string

let rec dropwhile f = function
  | [] -> []
  | x :: xs when f x -> dropwhile f xs
  | xs -> xs

(* from extlib: *)
let rec drop n = function
  | _ :: l when n > 0 -> drop (n-1) l
  | l -> l

let rec take n = function
  | x :: xs when n > 0 -> x :: take (pred n) xs
  | _ -> []

let lines = List.length

let columns csv =
  List.fold_left max 0 (List.map List.length csv)

type state_t = StartField
	       | InUnquotedField
	       | InQuotedField
	       | InQuotedFieldAfterQuote

let load_rows ?(separator = ',') f chan =
  let row = ref [] in			(* Current row. *)
  let field = ref [] in			(* Current field. *)
  let state = ref StartField in		(* Current state. *)
  let end_of_field () =
    let field_list = List.rev !field in
    let field_len = List.length field_list in
    let field_str = String.create field_len in
    let rec loop i = function
	[] -> ()
      | x :: xs ->
	  field_str.[i] <- x;
	  loop (i+1) xs
    in
    loop 0 field_list;
    row := field_str :: !row;
    field := [];
    state := StartField
  in
  let empty_field () =
    row := "" :: !row;
    field := [];
    state := StartField
  in
  let end_of_row () =
    let row_list = List.rev !row in
    f row_list;
    row := [];
    state := StartField
  in
  let rec loop () =
    let c = input_char chan in
    if c != '\r' then (			(* Always ignore \r characters. *)
      match !state with
	  StartField ->			(* Expecting quote or other char. *)
	    if c = '"' then (
	      state := InQuotedField;
	      field := []
	    ) else if c = separator then (* Empty field. *)
	      empty_field ()
	    else if c = '\n' then (	(* Empty field, end of row. *)
	      empty_field ();
	      end_of_row ()
	    ) else (
	      state := InUnquotedField;
	      field := [c]
	    )
	| InUnquotedField ->		(* Reading chars to end of field. *)
	    if c = separator then	(* End of field. *)
	      end_of_field ()
	    else if c = '\n' then (	(* End of field and end of row. *)
	      end_of_field ();
	      end_of_row ()
	    ) else
	      field := c :: !field
	| InQuotedField ->		(* Reading chars to end of field. *)
	    if c = '"' then
	      state := InQuotedFieldAfterQuote
	    else
	      field := c :: !field
	| InQuotedFieldAfterQuote ->
	    if c = '"' then (		(* Doubled quote. *)
	      field := c :: !field;
	      state := InQuotedField
	    ) else if c = '0' then (	(* Quote-0 is ASCII NUL. *)
	      field := '\000' :: !field;
	      state := InQuotedField
	    ) else if c = separator then (* End of field. *)
	      end_of_field ()
	    else if c = '\n' then (	(* End of field and end of row. *)
	      end_of_field ();
	      end_of_row ()
	    ) else (			(* Bad single quote in field. *)
	      field := c :: '"' :: !field;
	      state := InQuotedField
	    )
    ); (* end of match *)
    loop ()
  in
  try
    loop ()
  with
      End_of_file ->
	(* Any part left to write out? *)
	(match !state with
	     StartField ->
	       if !row <> [] then
		 ( empty_field (); end_of_row () )
	   | InUnquotedField | InQuotedFieldAfterQuote ->
	       end_of_field (); end_of_row ()
	   | InQuotedField ->
	       raise (Bad_CSV_file "Missing end quote after quoted field.")
	)

let load_in ?separator chan =
  let csv = ref [] in
  let f row =
    csv := row :: !csv
  in
  load_rows ?separator f chan;
  List.rev !csv

let load ?separator filename =
  let chan, close =
    match filename with
    | "-" -> stdin, false
    | filename -> open_in filename, true in
  let csv = load_in ?separator chan in
  if close then close_in chan;
  csv

let trim ?(top=true) ?(left=true) ?(right=true) ?(bottom=true) csv =
  let rec empty_row = function
    | [] -> true
    | x :: xs when x <> "" -> false
    | x :: xs -> empty_row xs
  in
  let csv = if top then dropwhile empty_row csv else csv in
  let csv =
    if right then
      List.map (fun row ->
		  let row = List.rev row in
		  let row = dropwhile ((=) "") row in
		  let row = List.rev row in
		  row) csv
    else csv in
  let csv =
    if bottom then (
      let csv = List.rev csv in
      let csv = dropwhile empty_row csv in
      let csv = List.rev csv in
      csv
    ) else csv in

  let empty_left_cell =
    function [] -> true | x :: xs when x = "" -> true | _ -> false in
  let empty_left_col =
    List.fold_left (fun a row -> a && empty_left_cell row) true in
  let remove_left_col =
    List.map (function [] -> [] | x :: xs -> xs) in
  let rec loop csv =
    if empty_left_col csv then
      remove_left_col csv
    else
      csv
  in

  let csv = if left then loop csv else csv in

  csv

let square csv =
  let columns = columns csv in
  List.map (
    fun row ->
      let n = List.length row in
      let row = List.rev row in
      let rec loop acc = function
	| 0 -> acc
	| i -> "" :: loop acc (i-1)
      in
      let row = loop row (columns - n) in
      List.rev row
  ) csv

let is_square csv =
  let columns = columns csv in
  List.for_all (fun row -> List.length row = columns) csv

let rec set_columns cols = function
  | [] -> []
  | r :: rs ->
      let rec loop i cells =
	if i < cols then (
	  match cells with
	  | [] -> "" :: loop (succ i) []
	  | c :: cs -> c :: loop (succ i) cs
	)
	else []
      in
      loop 0 r :: set_columns cols rs

let rec set_rows rows csv =
  if rows > 0 then (
    match csv with
    | [] -> [] :: set_rows (pred rows) []
    | r :: rs -> r :: set_rows (pred rows) rs
  )
  else []

let set_size rows cols csv =
  set_columns cols (set_rows rows csv)

let sub r c rows cols csv =
  let csv = drop r csv in
  let csv = List.map (drop c) csv in
  let csv = set_rows rows csv in
  let csv = set_columns cols csv in
  csv

(* Compare two rows for semantic equality - ignoring any blank cells
 * at the end of each row.
 *)
let rec compare_row (row1 : string list) row2 =
  match row1, row2 with
  | [], [] -> 0
  | x :: xs, y :: ys ->
      let c = compare x y in
      if c <> 0 then c else compare_row xs ys
  | "" :: xs , [] ->
      compare_row xs []
  | x :: xs, [] ->
      1
  | [], "" :: ys ->
      compare_row [] ys
  | [], y :: ys ->
      -1

(* Semantic equality for CSV files. *)
let rec compare (csv1 : t) csv2 =
  match csv1, csv2 with
  | [], [] -> 0
  | x :: xs, y :: ys ->
      let c = compare_row x y in
      if c <> 0 then c else compare xs ys
  | x :: xs, [] ->
      let c = compare_row x [] in
      if c <> 0 then c else compare xs []
  | [], y :: ys ->
      let c = compare_row [] y in
      if c <> 0 then c else compare [] ys

(* Concatenate - arrange left to right. *)
let rec concat = function
  | [] -> []
  | [csv] -> csv
  | left_csv :: csvs ->
      (* Concatenate the remaining CSV files. *)
      let right_csv = concat csvs in

      (* Set the height of the left and right CSVs to the same. *)
      let nr_rows = max (lines left_csv) (lines right_csv) in
      let left_csv = set_rows nr_rows left_csv in
      let right_csv = set_rows nr_rows right_csv in

      (* Square off the left CSV. *)
      let left_csv = square left_csv in

      (* Prepend the right CSV rows with the left CSV rows. *)
      List.map (
	fun (left_row, right_row) -> List.append left_row right_row
      ) (List.combine left_csv right_csv)

let to_array csv =
  Array.of_list (List.map Array.of_list csv)

let of_array csv =
  List.map Array.to_list (Array.to_list csv)

let associate header data =
  let nr_cols = List.length header in
  let rec trunc = function
    | 0, _ -> []
    | n, [] -> "" :: trunc (n-1, [])
    | n, (x :: xs) -> x :: trunc (n-1, xs)
  in
  List.map (
    fun row ->
      let row = trunc (nr_cols, row) in
      List.combine header row
  ) data

let save_out ?(separator = ',') chan csv =
  (* Quote a single CSV field. *)
  let quote_field field =
    if String.contains field separator ||
      String.contains field '\"' ||
      String.contains field '\n'
    then (
      let buffer = Buffer.create 100 in
      Buffer.add_char buffer '\"';
      for i = 0 to (String.length field) - 1 do
	match field.[i] with
            '\"' -> Buffer.add_string buffer "\"\""
	  | c    -> Buffer.add_char buffer c
      done;
      Buffer.add_char buffer '\"';
      Buffer.contents buffer
    )
    else
      field
  in

  let separator = String.make 1 separator in
  List.iter (fun line ->
	       output_string chan (String.concat separator
				     (List.map quote_field line));
	       output_char chan '\n') csv

let print ?separator csv =
  save_out ?separator stdout csv; flush stdout

let save ?separator file csv =
  let chan = open_out file in
  save_out ?separator chan csv;
  close_out chan

let save_out_readable chan csv =
  (* Escape all the strings in the CSV file first. *)
  (* XXX Why are we doing this?  I commented it out anyway.
  let csv = List.map (List.map String.escaped) csv in
  *)

  (* Find the width of each column. *)
  let widths =
    (* Don't consider rows with only a single element - typically
     * long titles.
     *)
    let csv = List.filter (function [_] -> false | _ -> true) csv in

    (* Square the CSV file - makes the next step simpler to implement. *)
    let csv = square csv in

    match csv with
      | [] -> []
      | row1 :: rest ->
	  let lengths_row1 = List.map String.length row1 in
	  let lengths_rest = List.map (List.map String.length) rest in
	  let max2rows r1 r2 =
	    let rp =
	      try List.combine r1 r2
	      with
		Invalid_argument "List.combine" ->
		  failwith (sprintf "Csv.save_out_readable: internal error: length r1 = %d, length r2 = %d" (List.length r1) (List.length r2)) in
	    List.map (fun ((a : int), (b : int)) -> max a b) rp
	  in
	  List.fold_left max2rows lengths_row1 lengths_rest in

  (* Print out each cell at the correct width. *)
  let rec repeat f = function
    | 0 -> ()
    | i -> f (); repeat f (i-1)
  in
  List.iter (
    function
    | [cell] ->				(* Single column. *)
	output_string chan cell;
	output_char chan '\n'
    | row ->				(* Other. *)
	(* Pair up each cell with its max width. *)
	let row =
	  let rec loop = function
	    | ([], _) -> []
	    | (_, []) -> failwith "Csv.save_out_readable: internal error"
	    | (cell :: cells, width :: widths) ->
		(cell, width) :: loop (cells, widths)
	  in
	  loop (row, widths) in
	List.iter (
	  fun (cell, width) ->
	    output_string chan cell;
	    let n = String.length cell in
	    repeat (fun () -> output_char chan ' ') (width - n + 1)
	) row;
	output_char chan '\n'
  ) csv

let print_readable = save_out_readable stdout
