let get_ok = function | Result.Ok x -> x | Result.Error _ -> failwith "oops"

let (>>|) a b = match a with | Result.Ok r -> Result.Ok (b r) | Result.Error _ as x -> x

let _ =
  if Array.length Sys.argv != 2 then
    Printf.printf "%s IFACE" Sys.argv.(0)
  else
    let socket = get_ok (Socketcan.Socket.create Sys.argv.(1)) in
    let rec loop () =
      let () =
        match Socketcan.Socket.receive socket >>| Socketcan.Frame.print with
        | Result.Ok () -> ()
        | Result.Error (`EUnix errno) -> print_endline (Unix.error_message errno)
      in
      loop ()
    in
    loop ()

