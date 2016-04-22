let get_ok = function | Result.Ok x -> x | Result.Error _ -> failwith "oops"

let (>>|) a b = match a with | Result.Ok r -> Result.Ok (b r) | Result.Error _ as x -> x

let _ =
  match Sys.argv.(1) with
  | "in" ->
    begin
      let () = print_endline "in" in
      let () = Printf.printf "line %d\n" __LINE__ in
      let fd_ = Can.Socket.create "vcan0" in
      let () = Printf.printf "line %d\n%!" __LINE__ in
      let fd = get_ok fd_ in
      let () = Printf.printf "line %d\n%!" __LINE__ in
      let fd = get_ok (Can.Socket.set_receive_filter fd
        [
          Can.Filter.create ~error_frames:`Also ~remote_frames:`Also ~mask:Can.Mask.sff (Can.Id.create_sff 815);
          Can.Filter.create ~mask:Can.Mask.sff (Can.Id.create_sff 42);
        ])
      in
      let () = Printf.printf "line %d\n%!" __LINE__ in
      let () =
        match ((Can.Socket.receive fd) >>| Can.Frame.print) with
          | Result.Ok () -> ()
          | Result.Error (`EUnix errno) -> print_endline (Unix.error_message errno)
          | Result.Error _ -> print_endline "wtf?"
      in
      let _ = (Can.Socket.receive fd) >>| Can.Frame.print in
      let _ = (Can.Socket.receive fd) >>| Can.Frame.print in
      ()
    end
  | "out" ->
    begin
      let () = print_endline "out" in
      let fd = get_ok (Can.Socket.create "vcan0") in
      let msg = get_ok (Can.Frame.create (Can.Id.create_sff 42) "hello!") in
      let () = Can.Frame.print msg in
      let _ = Can.Socket.send fd msg in
      let msg = get_ok (Can.Frame.create (Can.Id.create_sff 815) "hello") in
      let () = Can.Frame.print msg in
      let _ = Can.Socket.send fd msg in
      let msg = get_ok (Can.Frame.create (Can.Id.create_sff 23) "hello") in
      let () = Can.Frame.print msg in
      let _ = Can.Socket.send fd msg in
      ()
    end
  | _ ->
    Printf.printf "Usage: %s [in|out]\n" Sys.argv.(0)

