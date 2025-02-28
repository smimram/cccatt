(** Interaction with a webpage. *)

open Js_of_ocaml

module Html = Dom_html

let doc = Html.document

let button txt action =
  let button_type = Js.string "button" in
  let b = Html.createInput ~_type:button_type doc in
  b##.value := Js.string txt;
  b##.onclick := Dom_html.handler (fun _ -> action (); Js._true);
  b

[%%if ocaml_version = (4,14,1)]
let debug s = Firebug.console##debug (Js.string s)
[%%else]
let debug s = Console.console##debug (Js.string s)
[%%endif]

let env = ref ([],[])

let loop s =
  env := Lang.exec !env (Lang.parse s)

[%%if ocaml_version = (4,14,1)]
let number_of_int n = n
[%%else]
let number_of_int n = Js.number_of_float (float n)
[%%endif]

let run _ =
  let jsget x = Js.Opt.get x (fun () -> assert false) in
  let get_element_by_id id = doc##getElementById (Js.string id) |> jsget in
  let input = get_element_by_id "input" |> Html.CoerceTo.textarea |> jsget in
  let output = get_element_by_id "output" |> Html.CoerceTo.textarea |> jsget in
  let send = get_element_by_id "send" |> Html.CoerceTo.button |> jsget in
  let clear = get_element_by_id "clear" |> Html.CoerceTo.button |> jsget in
  let example = get_element_by_id "example" |> Html.CoerceTo.select |> jsget in
  let mode = get_element_by_id "mode" |> Html.CoerceTo.select |> jsget in
  let dim = get_element_by_id "dim" |> Html.CoerceTo.input |> jsget in

  let print s =
    let s = Js.to_string output##.value ^ s in
    output##.value := Js.string s;
    output##.scrollTop := number_of_int output##.scrollHeight
  in
  let error s =
    print ("=¡.¡= Error: " ^ s ^ "\n")
  in
  let set_mode () =
    let mode = Js.to_string mode##.value in
    Setting.parse ("mode : " ^ mode)
  in
  let read () =
    Js.to_string input##.value
  in
  let do_send () =
    output##.value := Js.string "";
    try read () |> String.trim |> loop
    with
    | Failure e -> error e
    | e -> error (Printexc.to_string e)
  in
  let load_example () =
    mode##.value := Js.string "ccc";
    set_mode ();
    input##.value := example##.value |> Js.to_string |> Examples.get |> Js.string;
    do_send ();
  in
  Common.print_fun := print;
  Setting.on_mode
    (fun s ->
       let m =
         match s with
         | `Category -> "category"
         | `Cartesian_closed -> "ccc"
         | `Monoidal -> "monoidal"
         | `Symmetric_monoidal -> "smc"
         | `Symmetric_monoidal_closed -> "smcc"
         | `Cartesian -> "cartesian category"
         | _ -> Js.to_string mode##.value
       in
       mode##.value := Js.string m
    );
  Setting.on_dim (fun d -> dim##.value := Js.string (if d = max_int then "∞" else string_of_int d));

  send##.onclick :=
    Html.handler
      (fun _ ->
         do_send ();
         Js.bool true
      );
  clear##.onclick :=
    Html.handler
      (fun _ ->
         input##.value := Js.string "";
         output##.value := Js.string "";
         Js.bool true
      );
  example##.onchange :=
    Html.handler
      (fun _ ->
         load_example ();
         Js.bool true
      );
  mode##.onchange :=
    Html.handler
      (fun _ ->
         set_mode ();
         Js.bool true
      );
  dim##.onchange :=
    Html.handler
      (fun _ ->
         Setting.set_dim @@ int_of_string @@ Js.to_string dim##.value;
         Js.bool true
      );

  input##focus;
  input##select;

  begin
    let ex = Url.Current.get_fragment () in
    if ex <> "" then
      (
        example##.value := Js.string ex;
        load_example ()
      )
  end;

  ignore (Js.Unsafe.eval_string "init();");

  Js._false

let () =
  Html.window##.onload := Html.handler run
