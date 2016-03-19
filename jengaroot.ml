open Core.Std
open Jenga_lib.Api

let ( *>>= ) = Dep.bind
let ( *>>| ) = Dep.map

let (^/) dir name = Path.relative ~dir name
let (/^) = (^/)

let ocamlfind = "ocamlfind"
let ocamlc_prog     = "ocamlc"
let ocamlopt_prog     = "ocamlopt"

(*
 let message fmt = ksprintf (fun s -> Core.Std.Printf.printf "!!JengaRoot.ml : %s\n%!" s) fmt
 *)

let basename = Path.basename

let nothing_to_build_rules ~dir =
  Dep.return [Rule.default ~dir []]

let everything_under_targets ~dir ~subdirs =
  let subdirs =
    List.map subdirs ~f:(fun subdir -> dir ^/ subdir)
  in
  Dep.List.concat_map subdirs ~f:(fun subdir ->
    Dep.subdirs ~dir:subdir)

module Alias = struct
  include Alias
  let default ~dir = Alias.create ~dir "DEFAULT"
  let runtest ~dir = Alias.create ~dir "runtest"
  let runbench ~dir = Alias.create ~dir "runbench"
  let merlin ~dir = Alias.create ~dir "merlin"
  let utop ~dir = Alias.create ~dir "utop"
end

module Ocaml = struct
  module Lib : sig
    type t

    include Comparable.S with type t := t
    include Hashable.S with type t := t

    val name : t -> string
    val of_name : string -> t

    val suffixed_name : t -> string
    val of_suffixed_name : string -> t

    val dir : t -> Path.t
  end = struct
    include String_id.Make(struct let module_name = "Jengaroot.Lib" end)()

    let name = to_string

    let of_name str =
      if String.is_suffix str ~suffix:"_lib" then
        failwith ("Lib name already suffixed: " ^ str);
        of_string str
  

    let suffixed_name t =
      to_string t ^ "_lib"
    

    let of_suffixed_name str =
      match String.chop_suffix str ~suffix:"_lib" with
      | None      -> failwith ("Suffixed lib name missing suffix: " ^ str)
      | Some name -> of_name name
    

    let dir t =
      Path.the_root /^ "lib" /^ to_string t
    
    
  end

  module Liblinks = struct
    let lib_dir_path ~lib =
      Path.the_root /^ "liblinks" /^ Lib.suffixed_name lib
  

    let deps ~suffixes libs =
      Dep.all_unit
        (List.map libs ~f:(fun lib ->
          let dir = lib_dir_path ~lib in
          Dep.all_unit
             (List.map suffixes ~f:(fun suffix ->
               Dep.path (dir ^/ Lib.suffixed_name lib ^ suffix)))))


    let rules ~lib =
      let liblinks_dir = lib_dir_path ~lib in
      List.map [".cmx"; ".cmi"; ".cmxa"; ".a"; ".o"] ~f:(fun suffix ->
        let file = (Lib.suffixed_name lib) ^ suffix in
        let link_file =
          let source = Lib.dir lib ^/ file in
          Dep.path source
          *>>| fun () ->
            let relative_source =
              Path.reach_from ~dir:liblinks_dir source
          in
          Action.process ~dir:liblinks_dir
            ~prog:"ln" ~args:["-sf"; relative_source; "."]()
        in
        Rule.create ~targets:[liblinks_dir ^/ file] (
            link_file
        ))


    let include_dirs ~dir libs =
      List.map libs ~f:(fun lib ->
        Path.reach_from ~dir (lib_dir_path ~lib))
  end

  let inline_testing_packages =
      [ "oUnit"; "ppx_jane"; "ppx_inline_test"; "core" ]
    
  let ppx_args lib =
      ["-predicates"; "ppx_driver"; "-package"; "ppx_inline_test"; "-ppx"; "ppx -inline-test-lib "^lib]

  let ocamlcompile ~dir ~external_libraries ~foreign_libraries ~for_pack
        ~include_dirs ~args ~allow_unused_opens ~ppx_args name =
          let packages_args  =
            match external_libraries with
      | [] -> []
      | _  -> ["-package"; String.concat ~sep:"," (external_libraries @ inline_testing_packages)]
        in
    let pack_args =
      Option.value_map for_pack ~default:[]
      ~f:(fun pack -> ["-for-pack"; String.capitalize (Lib.suffixed_name pack)])
          in
    let include_args =
      List.concat_map include_dirs ~f:(fun include_dir ->
        ["-I"; include_dir])
    in
    let foreign_args =
      match foreign_libraries with
      | [] -> []
      | _ ->
          ["-ccopt"; "-L/usr/lib64"]
          @ List.concat_map foreign_libraries ~f:(fun foreign_library ->
            ["-cclib"; "-l" ^ foreign_library])
    in
    let warning_args =
      let ignored_warnings = ["4"; "23"; "40"; "41"; "42"; "44"; "45"] in
      let ignored_warnings =
        if allow_unused_opens then "33" :: ignored_warnings else ignored_warnings
      in
      ["-w"; String.concat ~sep:"-" ("@A" :: ignored_warnings)]
      in
    let cmt_args =
      ["-bin-annot"; "-g"]
    in
    let args = (List.concat [ [name; "-thread"]; args; packages_args; pack_args; include_args
                         ; foreign_args; warning_args; ppx_args; cmt_args]) in
    Action.process ~dir ~prog:ocamlfind ~args ()
  
  let ocamlopt ~dir ~external_libraries ~foreign_libraries ~for_pack ~include_dirs ~args ~allow_unused_opens ~ppx_args =
    ocamlcompile ~dir ~external_libraries ~foreign_libraries ~for_pack ~include_dirs ~args ~allow_unused_opens ~ppx_args ocamlopt_prog
  
  let ocamlc ~dir ~external_libraries ~foreign_libraries ~for_pack ~include_dirs ~args ~allow_unused_opens ~ppx_args =
    ocamlcompile ~dir ~external_libraries ~foreign_libraries ~for_pack ~include_dirs ~args ~allow_unused_opens ~ppx_args ocamlc_prog 

  let ocamldep ~dir ~args =
    Action.process ~dir ~prog:ocamlfind
      ~args:(List.concat [ ["ocamldep"]; args; 
      ])()
  

  let glob_ml ~dir =
    Glob.create ~dir "*.ml"
  

  let glob_mli ~dir =
    Glob.create ~dir "*.mli"
  

  let non_blank str =
    match String.strip str with
    | "" -> false
    | _  -> true
  

  let split_into_lines str =
    List.filter ~f:non_blank (String.split ~on:'\n' str)
  

  let split_into_words str =
    List.filter ~f:non_blank (String.split ~on:'\ ' str)
  
  module Mlbuild : sig
    type t

    val load : dir : Path.t -> t Dep.t

    val with_mlbuild :
      dir : Path.t
      -> if_missing : 'a Dep.t
      -> (t -> 'a Dep.t)
      -> 'a Dep.t

    val libraries : t -> Lib.t list

    val external_libraries : t -> string list

    val foreign_libraries : t -> string list

    val classic_libs : t -> string list
  end = struct
    type t = {
      libraries          : string list sexp_option;
      external_libraries : string list sexp_option;
      foreign_libraries  : string list sexp_option;
      classic_libs      : string list sexp_option;
    } [@@deriving sexp]

    let load ~dir =
      Dep.contents (dir ^/ "mlbuild")
      *>>| fun mlbuild ->
        t_of_sexp (Sexp.of_string (String.strip mlbuild))
    

    let with_mlbuild ~dir ~if_missing f =
      Dep.file_exists (dir ^/ "mlbuild")
      *>>= fun mlbuild_present ->
        if mlbuild_present then begin
          load ~dir
          *>>= fun mlbuild -> f mlbuild
          end else begin
            if_missing
        end
    

    let libraries t =
      List.map (Option.value ~default:[] t.libraries) ~f:Lib.of_name
    

    let external_libraries t =
      Option.value ~default:[] t.external_libraries
    

    let foreign_libraries t =
      Option.value ~default:[] t.foreign_libraries
    

    let classic_libs t =
      Option.value ~default:[] t.classic_libs

  end

  let ocamldep_deps ~dir ~source =
    let ppx = ppx_args (basename dir) in
    Dep.action_stdout
    (Dep.all_unit
       [ Dep.glob_change (glob_ml ~dir)
       ; Dep.glob_change (glob_mli ~dir)
       ; Dep.path source
    ]
    *>>| fun () -> ocamldep ~dir ~args:(["-native"; basename source]@ppx))
    *>>| fun deps ->
      let deps = String.Search_pattern.(replace_all (create "\\\n") ~in_:deps ~with_:" ")
  in
  List.map (split_into_lines deps) ~f:(fun dep ->
    match String.split dep ~on:':' with
    | [before; after] ->
        let before =
          Option.value_exn (List.hd (split_into_words before))
          ~message:("invalid ocamldep before: " ^ before)
      in
      (before, split_into_words after)
    | _ ->
        failwith ("invalid ocamldep output line: " ^ dep))
  

  let deps_paths ~dir ~source ~target =
    ocamldep_deps ~dir ~source
    *>>= fun deps ->
      let dep_paths =
        List.map deps ~f:(fun (before, after) ->
          (dir ^/ before, List.map after ~f:(fun after -> dir ^/ after)))
          in
    match List.Assoc.find dep_paths target with
    | None ->
        failwith ("missing in ocamldep output: " ^ Path.to_string target)
    | Some paths ->
        Dep.all_unit
        (List.map paths ~f:(fun path -> Dep.path path))
  

  let compiled_files_for ~dir names =
    let libname = (basename dir) ^ "_lib" in
    List.concat_map names ~f:(fun name ->
      [ Dep.path (dir ^/ name ^ ".cmx")
      ; Dep.path (dir ^/ name ^ ".cmi")
      ; Dep.path (dir ^/ name ^ ".o")
      ]
      @ 
      match ((String.equal name "inline_tests_runner") || (String.equal name libname)) with
      | true -> []
      | false -> [Dep.path (dir ^/ name ^ ".cmo")]
    )
  

  let toposort ~targets deps =
    let sorted_targets = Queue.create () in
    let rec loop deps =
      if Hashtbl.is_empty deps
      then begin
        ()
      end 
      else begin
        let (ready, still_have_deps) =
          Hashtbl.partition_map deps ~f:(fun target_deps ->
            if List.is_empty target_deps
                  then `Fst ()
                  else `Snd target_deps)
        in
        if Hashtbl.is_empty ready then
          failwith ("Toposort failed on: "
                    ^ (String.concat ~sep:" " (Set.to_list targets)));
        let ready = String.Set.of_list (Hashtbl.keys ready) in
        Set.iter ready ~f:(Queue.enqueue sorted_targets);
        let still_have_deps =
          String.Table.map still_have_deps ~f:(fun target_deps ->
            List.filter target_deps ~f:(fun target_dep ->
              not (Set.mem ready target_dep)))
        in
        loop still_have_deps
      end
        in
    loop deps;
    Queue.to_list sorted_targets
  

  let toposort_deps ~dir targets =
    Dep.glob_listing (glob_ml ~dir)
    *>>= fun mls ->
      Dep.all (List.map mls ~f:(fun ml -> ocamldep_deps ~dir ~source:ml))
      *>>| fun deps ->
        let targets = String.Set.of_list targets in
        let deps =
          String.Table.of_alist_exn
        (List.filter_map (List.concat deps) ~f:(fun (target, target_deps) ->
          if Set.mem targets target
           then Some (target, List.filter target_deps ~f:(Set.mem targets))
           else None))
        in
    toposort ~targets deps
  
  let transitive_libraries ~libraries =
    let rec loop ~found lib =
      Mlbuild.load ~dir:(Lib.dir lib)
      *>>= fun mlbuild ->
        let libraries = Mlbuild.libraries mlbuild in
        if List.exists libraries ~f:(List.mem found) then
          failwith ("Dependency cycle in library deps for " ^ Lib.name lib);
        Dep.all (List.map libraries ~f:(loop ~found:(lib :: found)))
      *>>| fun lib_deps ->
        (lib :: List.concat lib_deps) in 
        Dep.List.concat_map libraries ~f:(loop ~found:[])
      *>>| fun libs ->
        Lib.Set.(to_list (of_list libs))

  let library_deps ~libraries =
    Dep.all (List.map libraries ~f:(fun lib ->
      Mlbuild.load ~dir:(Lib.dir lib)
      *>>| fun mlbuild -> (lib, mlbuild)))
      *>>| fun mlbuilds ->
        let (deps, external_libraries) =
          List.fold_left ~init:([], []) mlbuilds
          ~f:(fun (deps, ext) (lib, mlbuild) ->
            ((Lib.name lib, List.map ~f:Lib.name (Mlbuild.libraries mlbuild)) :: deps,
             ext @ Mlbuild.external_libraries mlbuild))
          in
          (String.Table.of_alist_exn deps, `External external_libraries)
  
(*----------------------------------------------------------------------
 bash
 ----------------------------------------------------------------------*)
  let shell_escape s =
    "'" ^ String.concat_map s ~f:(function
      | '\'' -> "'\\''"
    | c -> String.make 1 c
  ) ^ "'"


  let bash ~dir command_string =
    Action.process ~dir ~prog:"bash" ~args:[
      "-e"; "-u"; "-o"; "pipefail";
    "-c"; command_string
    ]()

  module Bash : sig
    type t
    val create : prog:string -> args:string list -> target:string option -> t
    val action: dir:Path.t -> t list -> Action.t
  end = struct
    type t = string

    let create ~prog ~args ~target =
      let com = String.concat ~sep:" "
        (prog :: List.map args ~f:shell_escape)
      in
      match target with
      | None -> com
      | Some target -> sprintf "%s > %s" com target

    let action ~dir ts =
      let command_string = String.concat ~sep:"; " ts in
      bash ~dir command_string
  end

  let bash1 ?target prog args = Bash.create ~prog ~args ~target

  let echo_string ~target string =
    bash1 ~target "echo" ["-e"; string]

    (* basic action for writing strings to files *)
  let write_string_action ?chmod_x string ~target =
    let dir = Path.dirname target in
    Bash.action ~dir ([
      echo_string ~target:(basename target) string
    ] @ (
      match chmod_x with | None -> [] | Some () ->
        [bash1 "chmod" ["+x"; basename target]]
      ))

  let create1 ~targets ~deps ~action =
    Rule.create ~targets (
      Dep.all_unit deps *>>| fun () -> action
      )
  let write_string_rule ?chmod_x string ~target =
    create1 ~deps:[] ~targets:[target]
    ~action:(write_string_action ?chmod_x string ~target)

  let bashf ~dir fmt =
    ksprintf (fun str -> bash ~dir str) fmt


  let link_exe_rule ~dir ~libraries ~external_libraries ~foreign_libraries ~classic_libs ~exe names =
    let link_exe =
      Dep.all_unit (compiled_files_for ~dir names)
      *>>= fun () ->
        transitive_libraries ~libraries
      *>>= fun libraries ->
        library_deps ~libraries
      *>>= fun (lib_deps, `External lib_external_libraries) ->
        let external_libraries =
          String.Set.(to_list (of_list (external_libraries @ lib_external_libraries)))
      in
      let libraries =
        let targets = String.Set.of_list (Hashtbl.keys lib_deps) in
        List.map ~f:Lib.of_name (toposort ~targets lib_deps)
      in
      Liblinks.deps libraries ~suffixes:[".cmxa"; ".a"]
      *>>= fun () ->
      toposort_deps ~dir (List.map names ~f:(fun name -> name ^ ".cmx"))
      *>>| fun cmxs ->
        ocamlopt ~dir ~external_libraries ~foreign_libraries
        ~for_pack:None ~allow_unused_opens:false
        ~include_dirs:(Liblinks.include_dirs ~dir libraries)
        ~args:(List.concat
                 [ List.map classic_libs ~f:(fun m -> m ^ ".cmxa")
                 ; [ "-linkpkg"; "-o"; basename exe]
                 ; List.map libraries ~f:(fun lib -> Lib.suffixed_name lib ^ ".cmxa")
                 ; cmxs
                 ])
        ~ppx_args:[]
        in
    Rule.create ~targets:[exe] link_exe
  
  let link_inline_tests_runner_exe_rule ~dir ~libraries ~external_libraries ~foreign_libraries ~classic_libs ~exe =
    let link_exe =
      let lib = Lib.of_name (basename dir) in
      let libname = Lib.suffixed_name lib in
      let names = [libname; "inline_tests_runner"] in
      let other_libraries = libraries in
      let libraries = lib::libraries in
      Dep.all_unit (compiled_files_for ~dir names)
      *>>= fun () ->
        transitive_libraries ~libraries
      *>>= fun libraries ->
        library_deps ~libraries
      *>>= fun (lib_deps, `External lib_external_libraries) ->
        let external_libraries =
          String.Set.(to_list (of_list (external_libraries @ lib_external_libraries)))
      in
      let libraries =
        let targets = String.Set.of_list (Hashtbl.keys lib_deps) in
        List.map ~f:Lib.of_name (toposort ~targets lib_deps)
        in

      Liblinks.deps libraries ~suffixes:[".cmxa"; ".a"]
      *>>= fun () ->
      toposort_deps ~dir (List.map names ~f:(fun name -> name ^ ".cmx"))
      *>>| fun cmxs ->
        let include_dirs = Liblinks.include_dirs ~dir other_libraries in
        ocamlopt ~dir ~external_libraries ~foreign_libraries
        ~for_pack:None ~allow_unused_opens:false
        ~include_dirs
        ~args:(List.concat
                 [ List.map classic_libs ~f:(fun m -> m ^ ".cmxa")
                 ; [ "-linkpkg"; "-o"; basename exe]
                 ; List.map libraries ~f:(fun lib -> Lib.suffixed_name lib ^ ".cmxa")
                 ; cmxs
                 ])
        ~ppx_args:[]
        in
    Rule.create ~targets:[exe] link_exe
  
  

  let link_lib_rules ~dir ~external_libraries ~foreign_libraries ~lib_cmxa ~lib names =
    let lib_a = dir ^/ Lib.suffixed_name lib ^ ".a" in
    let lib_o = dir ^/ Lib.suffixed_name lib ^ ".o" in
    let lib_cmx = dir ^/ Lib.suffixed_name lib ^ ".cmx" in
    let lib_cmi = dir ^/ Lib.suffixed_name lib ^ ".cmi" in
    let lib_cma = dir ^/ Lib.suffixed_name lib ^ ".cma" in
    let libpath = Liblinks.lib_dir_path ~lib in
    let link_cmxa =
      Dep.all_unit (List.map ~f:Dep.path [lib_cmx; lib_cmi; lib_o])
      *>>| fun () ->
        ocamlopt ~dir ~external_libraries ~foreign_libraries
        ~include_dirs:[Path.to_string libpath] ~for_pack:None ~allow_unused_opens:false
        ~args:["-a"; "-o"; basename lib_cmxa; basename lib_cmx]
        ~ppx_args:[]
    in
    let pack_lib_cmx =
      Dep.all_unit (compiled_files_for ~dir names)
      *>>= fun () -> toposort_deps ~dir (List.map names ~f:(fun name -> name ^ ".cmx"))
      *>>| fun cmxs ->
          ocamlopt ~dir ~external_libraries ~allow_unused_opens:false
        ~for_pack:None ~include_dirs:[] ~foreign_libraries:[]
        ~args:(List.concat
                 [ ["-pack"; "-o"; basename lib_cmx]
                 ; cmxs
                 ])
        ~ppx_args:[]
    in
    let pack_lib_cma =
      Dep.all_unit (compiled_files_for ~dir names)
      *>>= fun () -> 
        toposort_deps ~dir (List.map names ~f:(fun name -> name ^ ".cmo"))
      *>>| fun cmos ->
          ocamlc ~dir ~external_libraries ~allow_unused_opens:false
        ~for_pack:None ~include_dirs:[] ~foreign_libraries:[]
        ~args:(List.concat
                 [ ["-pack"; "-o"; basename lib_cma]
                 ; cmos
                 ])
        ~ppx_args:[]
    in
    [ Rule.create ~targets:[lib_cmxa; lib_a] link_cmxa
    ; Rule.create ~targets:[lib_cmx; lib_cmi; lib_o] pack_lib_cmx
    ; Rule.create ~targets:[lib_cma;] pack_lib_cma
    ]

  let compile_ml ~dir ~name ~external_libraries ~libraries ~for_pack ~include_dirs ~cmx =
    let ml = dir ^/ name ^ ".ml" in
    Dep.path ml
    *>>= fun () ->
      Liblinks.deps libraries ~suffixes:[".cmi"; ".cmx"; ".o"]
    *>>= fun () ->
      deps_paths ~dir ~source:ml ~target:cmx
    *>>| fun () ->
      let lib = Lib.of_name (basename dir) in
      let libname = Lib.suffixed_name lib in
      ocamlopt ~dir ~external_libraries ~include_dirs ~for_pack
      ~foreign_libraries:[] ~allow_unused_opens:false
      ~args:["-c"; basename ml]
      ~ppx_args:(ppx_args libname)
  
  let byte_compile_ml ~dir ~name ~external_libraries ~for_pack ~include_dirs ~cmo =
    let ml = dir ^/ name ^ ".ml" in
    Dep.path ml
    *>>= fun () ->
      deps_paths ~dir ~source:ml ~target:cmo
    *>>| fun () ->
    (* If there is no .mli file, then the default behaviour of the byte-compiler (ocamlc) is
       to generate a .cmi file. We DONT want this, because our rules are setup so the .cmi
       is generated from the native-compiler.

       If the native & byte compilers run at the same time, we may get a corrupted .cmi file

       To prevent the byte-compiler from writing the .cmi we use a -o directive to get
       generated outputs with a different basename & then rename the output we want (the
       .cmo) back to the original basename.

       Even if there is an .mli, the same problem happens because of cmt files, so the
       bytecode compilation always happens in a subdirectory. *)
      let lib = Lib.of_name (basename dir) in
      let libname = Lib.suffixed_name lib in
      ocamlc ~dir ~external_libraries ~include_dirs ~for_pack
      ~foreign_libraries:[] ~allow_unused_opens:false
      ~args:[
          "-o"; basename cmo;
          "-c"; basename ml; 
      ]
      ~ppx_args:(ppx_args libname)
      

  let byte_compile_ml_rule ~dir ~libraries ~external_libraries ~for_pack name =
    let cmo = dir ^/ name ^ ".cmo" in
    let include_dirs = Liblinks.include_dirs ~dir libraries in

    Rule.create ~targets:[cmo]
    (byte_compile_ml ~dir ~name ~external_libraries ~for_pack ~include_dirs ~cmo)

  let compile_ml_rule ~dir ~libraries ~external_libraries ~for_pack name =
    let cmi = dir ^/ name ^ ".cmi" in
    let cmx = dir ^/ name ^ ".cmx" in
    let o = dir ^/ name ^ ".o" in
    let include_dirs = Liblinks.include_dirs ~dir libraries in

    Rule.create ~targets:[cmi; cmx; o]
    (compile_ml ~dir ~name ~external_libraries ~libraries ~for_pack ~include_dirs ~cmx)
  
  let compile_inline_test_ml_rule ~dir ~libraries ~external_libraries ~for_pack name lib =
    let cmi = dir ^/ name ^ ".cmi" in
    let cmx = dir ^/ name ^ ".cmx" in
    let o = dir ^/ name ^ ".o" in
    let include_dirs = Liblinks.include_dirs ~dir libraries in
    let libraries = lib::libraries in

    Rule.create ~targets:[cmi; cmx; o]
    (compile_ml ~dir ~name ~external_libraries ~libraries ~for_pack ~include_dirs ~cmx)

  let compile_ml_mli_rules ~dir ~libraries ~external_libraries ~for_pack name =
    let cmi = dir ^/ name ^ ".cmi" in
    let cmx = dir ^/ name ^ ".cmx" in
    let o = dir ^/ name ^ ".o" in
    let include_dirs = Liblinks.include_dirs ~dir libraries in
    let compile_mli =
      let mli = dir ^/ name ^ ".mli" in
      Dep.path mli
      *>>= fun () -> Liblinks.deps libraries ~suffixes:[".cmi"]
      *>>= fun () -> deps_paths ~dir ~source:mli ~target:cmi
      *>>| fun () ->
        ocamlopt ~dir ~external_libraries ~for_pack ~include_dirs
        ~foreign_libraries:[] ~allow_unused_opens:true
        ~args:["-c"; basename mli]
        ~ppx_args:[]
      in
      let compile_ml = compile_ml ~dir ~name ~external_libraries ~libraries ~for_pack ~include_dirs ~cmx in
      [ Rule.create ~targets:[cmx; o] compile_ml 
      ; Rule.create ~targets:[cmi] compile_mli
      ]
    

  let compile_mls_in_dir_rules ~dir ~libraries ~external_libraries ~for_pack =
    Dep.glob_listing (glob_ml ~dir)
    *>>= fun mls -> Dep.glob_listing (glob_mli ~dir)
    *>>| fun mlis ->
        let mls = List.filter mls ~f:(fun ml -> not(String.equal (basename ml) "inline_tests_runner.ml")) in

        let names =
          List.map mls ~f:(fun ml ->
            String.chop_suffix_exn (basename ml) ~suffix:".ml")
    in
    let rules =
      List.concat_map names ~f:(fun name ->
        if List.mem mlis (dir ^/ name ^ ".mli")
        then compile_ml_mli_rules ~dir ~libraries ~external_libraries ~for_pack name @
              [byte_compile_ml_rule ~dir ~libraries ~external_libraries ~for_pack name]
        else [compile_ml_rule ~dir ~libraries ~external_libraries ~for_pack name;
              byte_compile_ml_rule ~dir ~libraries ~external_libraries ~for_pack name;
            ])
    in
    (names, rules)
  

  let compile_tests_runner_in_dir_rules ~dir ~libraries ~external_libraries =
    let name = "inline_tests_runner" in
    let lib = Lib.of_name (basename dir) in
    compile_inline_test_ml_rule ~dir ~libraries ~external_libraries ~for_pack:None name lib

  let compile_link_tests_runner_rules ~dir ~libraries ~external_libraries ~foreign_libraries ~classic_libs =
    let exe = dir ^/ "inline_tests_runner.exe" in
    let rules = compile_tests_runner_in_dir_rules ~dir ~libraries ~external_libraries in
    List.concat[ 
      [ Rule.default ~dir [Dep.path exe]
      ; link_inline_tests_runner_exe_rule ~dir ~libraries ~external_libraries ~foreign_libraries ~classic_libs ~exe
      ; rules
      ]
    ]
  

  (*----------------------------------------------------------------------
  Rules that check if libraries define tests or benchs
  ----------------------------------------------------------------------*)

  let fgrep_rule ~dir ~filename ~macros ~mls =
    let target = Path.relative ~dir filename in
    let sources = List.map mls ~f:(fun impl -> Path.relative ~dir (impl ^ ".ml")) in
    let targets = [target] in
    let deps = List.map sources ~f:Dep.path in
    let action = (
      bashf ~dir "\
 (cat %s | fgrep -w -f <(%s) || true) > %s"
        (String.concat ~sep:" " (List.map ~f:basename sources))
        (String.concat ~sep:"; " (List.map macros ~f:(sprintf "echo %s")))
        (basename target)
        ) in
    Rule.create ~targets (
      Dep.all_unit deps *>>| fun () -> action
      )

  let fgrep_test_filename = "fgrep_tests.out"
  let test_macros = ["let%test"; "let%test_unit"; "let%test_module"]

 (*----------------------------------------------------------------------
 inline_tests & benchmarks
 ----------------------------------------------------------------------*)
  let inline_tests_gen_file_rules ~dir ~libname = [
    (* The [let M = ... ] is to avoid the linker disregarding top-level module
     initialization code (which runs the tests) in an otherwise unused module *)

    write_string_rule ~target:(Path.relative ~dir "inline_tests_runner.ml")
    ("let run_inline_tests () =\n" ^
     "Ppx_inline_test_lib.Runtime.(summarize () |>\n"^
     "                             Test_result.record;\n"^
     "                             Test_result.exit ())"^
    (sprintf "let () = let module M = %s in run_inline_tests (); "
       (String.capitalize libname)));

    write_string_rule ~chmod_x:() ~target:(Path.relative ~dir "inline_tests_runner") 
    ("#!/bin/sh\\n# This file was generated, dont edit by hand\\ncd $(dirname $(readlink -f $0))\\nexec ./inline_tests_runner.exe inline-test-runner "^libname^" $@");
  ]

  let time_limit = Path.root_relative "bin/time_limit"

  let run_inline_action ~dir filename =
    let f = Path.relative ~dir in
    let sources = List.map ~f ([filename; filename ^ ".exe"]) in
    Dep.action
    (Dep.all_unit (List.map sources ~f:Dep.path)
    *>>| fun () ->
      (Action.process ~dir ~prog:(Path.reach_from ~dir time_limit)
      ~args:["300"; "./" ^ filename])())

  let run_inline_tests_action ~dir =
    run_inline_action ~dir "inline_tests_runner"

  let non_empty_file ~dir ~filename =
    let path = Path.relative ~dir filename in
    Dep.contents path *>>= fun s ->
      let has_file = match (String.strip s) with "" -> false | _ -> true in
      Dep.return has_file

  let has_tests ~dir =
    non_empty_file ~dir ~filename:fgrep_test_filename

  (*let dash_ml_wrap = ".ml-wrap"*)
  let inline_tests_rules ~dir ~libraries ~external_libraries ~foreign_libraries ~classic_libs ~names =
    let lib = Lib.of_name (basename dir) in
    let libname = Lib.suffixed_name lib in
    let name = "inline_tests_runner" in

    List.concat [
      (* generate test .ml and runner script *)
      inline_tests_gen_file_rules ~dir ~libname;

      (* compile and link rules *)
      compile_link_tests_runner_rules ~dir ~libraries ~external_libraries ~foreign_libraries ~classic_libs;

      (* aliases *)
      [
        Rule.alias (Alias.default ~dir) [
          has_tests ~dir *>>= function
            | false -> Dep.return ()
            | true ->
                let deps = [name ^ ".cmx"; name; name ^ ".exe"; libname ^ ".cmx"] in
                Dep.all_unit (List.map deps ~f:(fun name -> Dep.path (Path.relative ~dir name)))
                ];
        fgrep_rule ~dir ~filename:fgrep_test_filename ~macros:test_macros ~mls:names;
        Rule.alias (Alias.runtest ~dir) [
          has_tests ~dir *>>= function
            | false -> Dep.return ()
            | true -> run_inline_tests_action ~dir
        ];
        ]
      ]

  let app_rules ~dir =
    Mlbuild.with_mlbuild ~dir ~if_missing:(nothing_to_build_rules ~dir) (fun mlbuild ->
      let external_libraries = Mlbuild.external_libraries mlbuild in
      let foreign_libraries = Mlbuild.foreign_libraries mlbuild in
      let libraries = Mlbuild.libraries mlbuild in
      let classic_libs = Mlbuild.classic_libs mlbuild in
      let exe = dir ^/ (basename dir) ^ ".exe" in
      compile_mls_in_dir_rules ~dir ~libraries ~external_libraries ~for_pack:None
      *>>| fun (names, compile_mls_rules) ->
      List.concat
        [ [ Rule.default ~dir [Dep.path exe]
        ; link_exe_rule ~dir ~libraries ~external_libraries ~foreign_libraries ~classic_libs ~exe names
        ]
        ; compile_mls_rules
        ])
  

  let lib_rules ~dir =
    Mlbuild.with_mlbuild ~dir ~if_missing:(nothing_to_build_rules ~dir) (fun mlbuild ->
      let external_libraries = Mlbuild.external_libraries mlbuild in
      let foreign_libraries = Mlbuild.foreign_libraries mlbuild in
      let libraries = Mlbuild.libraries mlbuild in
      let classic_libs = Mlbuild.classic_libs mlbuild in
      let lib = Lib.of_name (basename dir) in
      let lib_cmxa = dir ^/ Lib.suffixed_name lib ^ ".cmxa" in
      let lib_cma = dir ^/ Lib.suffixed_name lib ^ ".cma" in
      compile_mls_in_dir_rules ~dir ~libraries ~external_libraries ~for_pack:(Some lib)
      *>>| fun (names, compile_mls_rules) ->
        List.concat
        [ [ Rule.default ~dir [Dep.path lib_cmxa] ]
        ; [ Rule.default ~dir [Dep.path lib_cma] ]
        ; link_lib_rules ~dir ~external_libraries ~foreign_libraries ~lib_cmxa ~lib names
        ; compile_mls_rules
        ; inline_tests_rules ~dir ~libraries ~external_libraries ~foreign_libraries ~classic_libs ~names
        ])
  

  let liblinks_rules ~dir =
    Dep.return (Liblinks.rules ~lib:(Lib.of_suffixed_name (basename dir)))
  
end

(*----------------------------------------------------------------------
recursive aliases
----------------------------------------------------------------------*)
let recursive_aliases = [
  Alias.default;
  Alias.merlin;
  Alias.utop;
]

let recursive_test_aliases = [
  Alias.runtest;
  Alias.runbench;
]

let generate_recursive_aliases_rules  ~dir ~subdirs ~aliases  =
  List.map aliases ~f:(fun make_alias ->
    Rule.alias (make_alias ~dir) (
      List.map subdirs ~f:(fun subdir -> Dep.alias (make_alias ~dir:subdir))
    ))

let gen_recursive_aliases ~dir =
  everything_under_targets ~dir ~subdirs:["app";"lib"]
  *>>| fun subdirs ->
    let lib_subdirs = List.filter subdirs ~f:(fun dir -> String.equal (Path.to_string (Path.dirname dir)) "lib") in
    List.concat
    [
        generate_recursive_aliases_rules ~dir ~subdirs ~aliases:recursive_aliases;
        generate_recursive_aliases_rules ~dir ~subdirs:lib_subdirs ~aliases:recursive_test_aliases
    ]

let scheme ~dir =
  let is_mlbuild path = match basename path with
    | "mlbuild"  -> true
    | "resbuild" -> true
    | _          -> false
  in
  let rules =
    if Path.(the_root = dir)
    then
      gen_recursive_aliases ~dir
    else
        match Path.(to_string (dirname dir)) with
      | "app"      -> Ocaml.app_rules ~dir
      | "lib"      -> Ocaml.lib_rules ~dir
      | "liblinks" -> Ocaml.liblinks_rules ~dir
      | _          -> nothing_to_build_rules ~dir
    in
    Scheme.exclude is_mlbuild (Scheme.rules_dep rules)

let setup () = Async.Std.Deferred.return (Env.create (fun ~dir -> scheme ~dir))
