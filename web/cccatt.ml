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

let debug s =
  Firebug.console##debug (Js.string s)

let env = ref ([],[])

let loop s =
  env := Lang.exec !env (Prover.parse s)

let run _ =
  let top =
    Js.Opt.get
      (doc##getElementById (Js.string "toplevel"))
      (fun () -> assert false)
  in

  let areas = Html.createDiv doc in
  Dom.appendChild top areas;

  let input = Html.createTextarea doc in
  input##.id := Js.string "input";
  input##.cols := 80;
  input##.rows := 25;
  input##.placeholder := Js.string "Write CCCaTT code here...";
  Dom.appendChild areas input;

  let output = Html.createTextarea doc in
  output##.id := Js.string "output";
  output##.cols := 80;
  output##.rows := 25;
  output##.placeholder := Js.string "Output";
  output##.readOnly := Js.bool true;
  Dom.appendChild areas output;

  let print s =
    let s = Js.to_string output##.value ^ s in
    output##.value := Js.string s
  in
  let read () =
    Js.to_string input##.value
  in

  Common.print_fun := print;

  Dom.appendChild top (Html.createBr doc);

  let send =
    button
      "Send"
      (fun () ->
         let s = read () |> String.trim in
         output##.value := Js.string "";
         loop s;
         input##focus;
         doc##.documentElement##.scrollTop := doc##.body##.scrollHeight
      )
  in
  send##.id := Js.string "send";
  Dom.appendChild top send;
  input##focus;
  input##select;

  ignore (Js.Unsafe.eval_string "init();");

  Js._false

let () =
  Html.window##.onload := Html.handler run
