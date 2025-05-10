(*
  OCaml-Prolog, a Prolog interpreter, by Karol Stosiek and Szymon Fogiel

  Copyright (C) 2008  Karol Stosiek and Szymon Fogiel

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Lib

let load_file f = In_channel.with_open_bin f In_channel.input_all

let concat_files filenames =
  List.map load_file filenames |>
      String.concat ""

let prompt () =
  print_string ":- "; 
  flush stdout

let rec repeat thunk = function
  | 0 -> ()
  | n -> thunk (); repeat thunk (n - 1)

let rec loop_forever thunk =
  thunk ();
  loop_forever thunk

let parse_err_msg = "Parse error. Did you forget a dot?"

let read_eval_print database behaviour =
  prompt ();

  try 
    let query = read_line () in
    let execute () =
      Interpreter.interpret_string database behaviour query
    in
      Limit.get_limit behaviour.limit |> repeat execute
  with
  | Parsing.Parse_error ->
    print_endline parse_err_msg
  | Failure s ->
    let msg = if s = "lexing: empty token" then parse_err_msg else "Failed: " ^ s in
    print_endline msg

let repl database behaviour = 
  try 
    loop_forever (fun () -> read_eval_print database behaviour)
  with
  | End_of_file -> (print_string "\n"; exit 0)
  | _           -> (print_endline "Error occurred."; exit 0)

let main () =
  let filenames = ref [] in
  let randomise = ref false in
  let interactive = ref true in
  let quiet = ref false in
  let limit = ref 0 in
  let specs = [
    ("-r", Arg.Set randomise, "randomise");
    ("-n", Arg.Clear interactive, "non-interactive");
    ("-q", Arg.Set quiet, "quiet");
    ("-l", Arg.Set_int limit, "limit");
  ]
  and add_to_filenames = fun s -> filenames := s :: !filenames 
  in
    Arg.parse specs add_to_filenames "Usage: opl filename1 filename2 ...";

    if !randomise
    then Random.self_init ()
    else ();

    let behaviour = {
      Interpreter.randomise = !randomise;
      interactive = !interactive;
      quiet = !quiet;
      limit = Limit.set_limit !limit;
    } in

    let database = 
      concat_files !filenames |> Interpreter.clauses_from_string
    in

      repl database behaviour

let () = main ()
