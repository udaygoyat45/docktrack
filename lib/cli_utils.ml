open Core

module CliUtils = struct
  type input_bool = Yes | No
  type flag_input = { label : string; doc : string }
  type docktrack_subcmd = { cmd : string; flags : flag_input list }

  exception TooManyFlags of int

  let print_dashed_line =
   fun () -> Printf.printf "-----------------------------------\n"

  let print_header header =
    print_dashed_line ();
    Printf.printf "%s\n" header;
    print_dashed_line ()

  let run_unix cmd =
    let inp = Core_unix.open_process_in cmd in
    let r = In_channel.input_all inp in
    In_channel.close inp;
    r

  let git_subcmd_names =
    [
      "init";
      "clone";
      "status";
      "add";
      "commit";
      "push";
      "pull";
      "fetch";
      "branch";
      "checkout";
      "merge";
      "rebase";
      "log";
      "diff";
      "reset";
      "revert";
      "tag";
      "stash";
      "remote";
      "show";
    ]

  let git_subcmds =
    List.map git_subcmd_names ~f:(fun name -> { cmd = name; flags = [] })

  let docktrack_subcmds : docktrack_subcmd list =
    [
      {
        cmd = "view-feature";
        flags =
          [ { label = "feature-name"; doc = "Name of the feature to view" } ];
      };
      {
        cmd = "remove-feature";
        flags =
          [ { label = "feature-name"; doc = "Name of the feature to remove" } ];
      };
      {
        cmd = "view-updates";
        flags =
          [
            {
              label = "feature-name";
              doc = "Name of the feature whose updates to list";
            };
          ];
      };
      {
        cmd = "view-update";
        flags =
          [
            { label = "feature-name"; doc = "Name of the feature" };
            {
              label = "update-name";
              doc = "Name of the specific update to view";
            };
          ];
      };
      {
        cmd = "document-next-update";
        flags =
          [
            {
              label = "feature-name";
              doc = "Name of the feature for which to document the next update";
            };
          ];
      };
      {
        cmd = "document-update";
        flags =
          [
            { label = "feature-name"; doc = "Name of the feature" };
            { label = "update-name"; doc = "Name of the update to document" };
          ];
      };
      {
        cmd = "remove-update";
        flags =
          [
            { label = "feature-name"; doc = "Name of the feature" };
            { label = "update-name"; doc = "Name of the update to remove" };
          ];
      };
      {
        cmd = "remove-file";
        flags =
          [ { label = "file-path"; doc = "Path of the file to be removed" } ];
      };
      { cmd = "add-feature"; flags = [] };
      { cmd = "add-file"; flags = [] };
      { cmd = "view-code-tree"; flags = [] };
      { cmd = "view-features"; flags = [] };
      { cmd = "add-update"; flags = [] };
    ]

  let create_basic_command summary handler =
    Command.basic ~summary
      (let%map_open.Command () = return () in
       fun () -> handler [])

  let create_basic_command_one_flag summary flag_input handler =
    let flag_label = "--" ^ flag_input.label in
    Command.basic ~summary
      (let%map_open.Command flag1 =
         flag flag_label (required string) ~doc:flag_input.doc
       in
       fun () -> handler [ flag1 ])

  let create_basic_command_two_flag summary flag_input_one flag_input_two
      handler =
    let flag_label_one = "--" ^ flag_input_one.label in
    let flag_label_two = "--" ^ flag_input_two.label in
    Command.basic ~summary
      (let%map_open.Command flag_one =
         flag flag_label_one (required string) ~doc:flag_input_one.doc
       and flag_two =
         flag flag_label_two (required string) ~doc:flag_input_two.doc
       in
       fun () -> handler [ flag_one; flag_two ])

  let create_group cmd summary handler valid_cmds =
    let rec create_group_commands' cmds =
      match cmds with
      | { cmd = subcmd; flags } :: rest ->
          let rest_cmds = create_group_commands' rest in
          let comb_cmd = cmd ^ " " ^ subcmd in
          let c_cmd =
            match flags with
            | [] -> (subcmd, create_basic_command comb_cmd (handler subcmd))
            | [ flag ] ->
                ( subcmd,
                  create_basic_command_one_flag comb_cmd flag (handler subcmd)
                )
            | [ flag_one; flag_two ] ->
                ( subcmd,
                  create_basic_command_two_flag comb_cmd flag_one flag_two
                    (handler subcmd) )
            | _ -> raise (TooManyFlags (List.length flags))
          in
          c_cmd :: rest_cmds
      | [] -> []
    in
    let cmd_grp = create_group_commands' valid_cmds in
    Command.group ~summary cmd_grp

  let git_docktrack_combined summary git_handler docktrack_handler =
    let git_group =
      create_group "git"
        "Docktrack has custom commands accesed through git subcmd" git_handler
        git_subcmds
    in
    let docktrack_group =
      create_group "docktrack"
        "Docktrack has custom commands accesed through dock subcmd"
        docktrack_handler docktrack_subcmds
    in
    Command.group ~summary [ ("git", git_group); ("dock", docktrack_group) ]

  let trim_string str =
    let rec trim_string' str in_quote =
      if String.is_empty str then str
      else
        let fl = String.get str 0 in
        let ll = String.get str (String.length str - 1) in
        if Char.equal fl ' ' then
          trim_string'
            (String.sub str ~pos:1 ~len:(String.length str - 1))
            in_quote
        else if Char.equal ll ' ' then
          trim_string'
            (String.sub str ~pos:0 ~len:(String.length str - 1))
            in_quote
        else if
          (not in_quote)
          && String.length str > 2
          && Char.equal fl '"' && Char.equal ll '"'
        then
          trim_string'
            (String.sub str ~pos:1 ~len:(String.length str - 2))
            in_quote
        else str
    in
    trim_string' str false

  type input_validate = { validator : string -> bool; error_msg : string }

  let inline_input ?optional query validator error_msg =
    let requery () =
      Printf.printf "%s\n" error_msg;
      Printf.printf "%s: " query;
      Out_channel.flush stdout
    in
    printf "%s: " query;
    Out_channel.flush stdout;
    let rec inline_input' () =
      let output = String.strip (In_channel.input_line_exn In_channel.stdin) in
      if String.length output = 0 then (
        match optional with
        | Some opt_val -> opt_val
        | None ->
            requery ();
            inline_input' ())
      else if validator output then output
      else (
        requery ();
        inline_input' ())
    in
    let raw_input = inline_input' () in
    trim_string raw_input

  let inline_input_multi ?optional query input_validators general_error_msg =
    let requery error_msg () =
      Printf.printf "%s\n" error_msg;
      Printf.printf "%s: " query;
      Out_channel.flush stdout
    in
    let rec loop_validate' input input_validators' =
      match input_validators' with
      | { validator; error_msg } :: rest ->
          if validator input then loop_validate' input rest
          else `Error error_msg
      | [] -> `Ok
    in
    printf "%s: " query;
    Out_channel.flush stdout;
    let rec inline_input' () =
      let output = String.strip (In_channel.input_line_exn In_channel.stdin) in
      if String.length output = 0 then (
        match optional with
        | Some opt_val -> opt_val
        | None ->
            requery general_error_msg ();
            inline_input' ())
      else
        match loop_validate' output input_validators with
        | `Ok -> output
        | `Error error_msg ->
            requery error_msg ();
            inline_input' ()
    in
    let raw_input = inline_input' () in
    trim_string raw_input

  let inline_input_bool query =
    let valid_yes =
      Set.of_list (module String) [ "Y"; "y"; "yes"; "YES"; "" ]
    in
    let valid_no = Set.of_list (module String) [ "N"; "n"; "no"; "NO" ] in
    let rec loop () =
      Printf.printf "%s [Y/n]: " query;
      Out_channel.flush stdout;
      let output = In_channel.input_line_exn In_channel.stdin |> String.strip in
      if Set.mem valid_yes output then Yes
      else if Set.mem valid_no output then No
      else loop ()
    in
    loop ()
end
