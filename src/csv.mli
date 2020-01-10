(** csv.mli - comma separated values parser
  *
  * $Id: csv.mli,v 1.11 2007-04-23 16:42:33 rich Exp $
  *)

type t = string list list
(** Representation of CSV files. *)

exception Bad_CSV_file of string
(** Badly formed CSV files throw this exception. *)

val lines : t -> int
(** Work out the number of lines in a CSV file. *)

val columns : t -> int
(** Work out the (maximum) number of columns in a CSV file. Note that each
  * line may be a different length, so this finds the one with the most
  * columns.
  *)

val load_in : ?separator:char -> in_channel -> t
(** Load a CSV file.
  * @param chan Input file stream
  *)

val load : ?separator:char -> string -> t
(** Load a CSV file.
  * @param filename CSV filename.
  * If [filename] is ["-"] then load from [stdin].
  *)

val load_rows : ?separator:char -> (string list -> unit) -> in_channel -> unit
(** For very large CSV files which cannot be processed in memory at once,
  * this function is appropriate. It parses the input one row at a time and
  * calls your function once for each row.
  *
  * @param f Callout function.
  * @param chan Input file stream.
  *)

val trim : ?top:bool -> ?left:bool -> ?right:bool -> ?bottom:bool -> t -> t
(** This takes a CSV file and trims empty cells.
  *
  * All four of the option arguments ([~top], [~left], [~right], [~bottom])
  * default to [true].
  *
  * The exact behaviour is:
  *
  * [~right]: If true, remove any empty cells at the right hand end of
  * any row.  The number of columns in the resulting CSV structure will
  * not necessarily be the same for each row.
  *
  * [~top]: If true, remove any empty rows (no cells, or containing just empty
  * cells) from the top of the CSV structure.
  *
  * [~bottom]: If true, remove any empty rows from the bottom of the
  * CSV structure.
  *
  * [~left]: If true, remove any empty columns from the left of the
  * CSV structure.  Note that [~left] and [~right] are quite different:
  * [~left] considers the whole CSV structure, whereas [~right] considers
  * each row in isolation.
  *)

val square : t -> t
(** Make the CSV data "square" (actually rectangular).  This pads out
  * each row with empty cells so that all rows are the same length as
  * the longest row.  After this operation, every row will have length
  * {!Csv.columns}.
  *)

val is_square : t -> bool
(** Return true iff the CSV is "square" (actually rectangular).  This
  * means that each row has the same number of cells.
  *)

val set_columns : int -> t -> t
(** [set_columns cols csv] makes the CSV data square by forcing the width
  * to the given number of [cols].  Any short rows are padded with blank
  * cells.  Any long rows are truncated.
  *)

val set_rows : int -> t -> t
(** [set_rows rows csv] makes the CSV data have exactly [rows] rows
  * by adding empty rows or truncating rows as necessary.
  *
  * Note that [set_rows] does not make the CSV square.  If you want it
  * to be square, call either {!Csv.square} or {!Csv.set_columns} after.
  *)

val set_size : int -> int -> t -> t
(** [set_size rows cols csv] makes the CSV data square by forcing the
  * size to [rows * cols], adding blank cells or truncating as necessary.
  * It is the same as calling [set_columns cols (set_rows rows csv)]
  *)

val sub : int -> int -> int -> int -> t -> t
(** [sub r c rows cols csv] returns a subset of [csv].  The subset is
  * defined as having top left corner at row [r], column [c] (counting
  * from [0]) and being [rows] deep and [cols] wide.
  *
  * The returned CSV will be square.
  *)

val compare : t -> t -> int
(** Compare two CSV files for equality, ignoring blank cells at the end
  * of a row, and empty rows appended to one or the other.  This is
  * "semantic" equality - roughly speaking, the two CSV files would
  * look the same if opened in a spreadsheet program.
  *)

val concat : t list -> t
(** Concatenate CSV files so that they appear side by side, arranged
  * left to right across the page.  Each CSV file (except the final
  * one) is first squared.
  *
  * (To concatenate CSV files so that they appear from top to bottom,
  * just use {!List.concat}).
  *)

val to_array : t -> string array array
val of_array : string array array -> t
(** Convenience functions to convert to and from a matrix representation.
  * [to_array] will produce a ragged matrix (not all rows will have the
  * same length) unless you call {!Csv.square} first.
  *)

val associate : string list -> t -> (string * string) list list
(** [associate header data] takes a block of data and converts each
  * row in turn into an assoc list which maps column header to data cell.
  *
  * Typically a spreadsheet will have the format:
  * {v
  *   header1   header2   header3
  *   data11    data12    data13
  *   data21    data22    data23
  *     ...
  * v}
  *
  * This function arranges the data into a more usable form which is
  * robust against changes in column ordering.  The output of the
  * function is:
  * {v
  *   [ ["header1", "data11"; "header2", "data12"; "header3", "data13"];
  *     ["header1", "data21"; "header2", "data22"; "header3", "data23"];
  *     etc. ]
  * v}
  *
  * Each row is turned into an assoc list (see [List.assoc]).
  *
  * If a row is too short, it is padded with empty cells ([""]).  If
  * a row is too long, it is truncated.
  *
  * You would typically call this function as:
  *
  * {v
  * let header, data = match csv with h :: d -> h, d | [] -> assert false;;
  * let data = Csv.associate header data;;
  * v}
  *
  * The header strings are shared, so the actual space in memory consumed
  * by the spreadsheet is not much larger.
  *)

val print : ?separator:char -> t -> unit
(** Print string list list - same as [save_out stdout] *)

val save_out : ?separator:char -> out_channel -> t -> unit
(** Save string list list to a channel. *)

val save : ?separator:char -> string -> t -> unit
(** Save string list list to a file. *)

val print_readable : t -> unit
(** Print the CSV data to [stdout] in a human-readable format.  Not much
  * is guaranteed about how the CSV is printed, except that it will be
  * easier to follow than a "raw" output done with {!Csv.print}.  This is
  * a one-way operation.  There is no easy way to parse the output of
  * this command back into CSV data.
  *)
val save_out_readable : out_channel -> t -> unit
(** As for {!Csv.print_readable}, allowing the output to be sent to a channel.
  *)
