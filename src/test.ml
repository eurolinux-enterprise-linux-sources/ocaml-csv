(* $Id: test.ml,v 1.2 2006/02/23 15:24:25 rich Exp $ *)

open Printf
open Csv

let do_testcsv filename expected =
  let csv = load filename in
  if csv <> expected then (
    printf "input file: %s\n" filename;
    printf "Csv library produced:\n";
    print csv;
    printf "Expected:\n";
    print expected;
    failwith "failed"
  )
  else ()

let () =
  do_testcsv
    "testcsv1.csv"
    [ [ "This is a test\nwith commas,,,,,\n\nand carriage returns." ] ]
let () =
  do_testcsv
    "testcsv2.csv"
    [ [ "Normal field"; "Quoted field"; "Quoted field with \"\" quotes" ] ]
let () =
  do_testcsv
    "testcsv3.csv"
    [ [ "" ];
      [ ""; "" ];
      [ ""; ""; "" ];
      [ ""; ""; ""; "" ];
      [ ""; ""; ""; ""; "" ] ]
let () =
  do_testcsv
    "testcsv4.csv"
    []
let () =
  do_testcsv
    "testcsv5.csv"
    [ [ "This is a test\nwith commas,,,,,\n\nand carriage returns.";
	"a second field"; "a third field" ];
      [ "a fourth field on a new line" ] ]
let () =
  do_testcsv
    "testcsv6.csv"
    [ [ "This is a test\nwith commas,,,,,\n\nand carriage returns\nand \000";
	"a second field"; "a third field" ];
      [ "a fourth field on a new line" ] ]

let () =
  let csv1 = [ [ "a"; "b"; "c"; ""; "" ];
	       [ "f"; "g"; "h"; "i"; "" ];
	       [ "" ];
	       [ ] ] in
  let csv2 = trim ~top:false ~left:false ~right:true ~bottom:true csv1 in
  assert (compare csv1 csv2 = 0)
let () =
  let csv1 = [ [ "a"; "b"; "c"; ""; "" ];
	       [ "f"; "g"; "h"; "i"; "" ];
	       [ "" ];
	       [ ] ] in
  let csv2 = [ [ "a"; "b"; "c"; "d"; "" ];
	       [ "f"; "g"; "h"; "i"; "" ];
	       [ "" ];
	       [ ] ] in
  assert (compare csv1 csv2 < 0)


;;

print_endline "All tests succeeded."
