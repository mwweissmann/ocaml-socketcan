let get_ok = function | Result.Ok x -> x | Result.Error _ -> failwith "oops"

let (>>|) a b = match a with | Result.Ok r -> Result.Ok (b r) | Result.Error _ as x -> x

let _ =
  match Sys.argv.(1) with
  | "in" ->
    begin
      let () = print_endline "in" in
      let () = Printf.printf "line %d\n" __LINE__ in
      let fd_ = Socketcan.Socket.create "vcan0" in
      let () = Printf.printf "line %d\n%!" __LINE__ in
      let fd = get_ok fd_ in
      let () = Printf.printf "line %d\n%!" __LINE__ in
      let fd = get_ok (Socketcan.Socket.set_receive_filter fd
        [
          Socketcan.Filter.create ~error_frames:`Also ~remote_frames:`Also ~mask:Socketcan.Mask.sff (Socketcan.Id.create_sff 815);
          Socketcan.Filter.create ~mask:Socketcan.Mask.sff (Socketcan.Id.create_sff 42);
        ])
      in
      let () = Printf.printf "line %d\n%!" __LINE__ in
      let () =
        match ((Socketcan.Socket.receive fd) >>| Socketcan.Frame.print) with
          | Result.Ok () -> ()
          | Result.Error (`EUnix errno) -> print_endline (Unix.error_message errno)
          | Result.Error _ -> print_endline "wtf?"
      in
      let _ = (Socketcan.Socket.receive fd) >>| Socketcan.Frame.print in
      let _ = (Socketcan.Socket.receive fd) >>| Socketcan.Frame.print in
      ()
    end
  | "out" ->
    begin
      let () = print_endline "out" in
      let fd = get_ok (Socketcan.Socket.create "vcan0") in
      let msg = get_ok (Socketcan.Frame.create (Socketcan.Id.create_sff 42) "hello!") in
      let () = Socketcan.Frame.print msg in
      let _ = Socketcan.Socket.send fd msg in
      let msg = get_ok (Socketcan.Frame.create (Socketcan.Id.create_sff 815) "hello") in
      let () = Socketcan.Frame.print msg in
      let _ = Socketcan.Socket.send fd msg in
      let msg = get_ok (Socketcan.Frame.create (Socketcan.Id.create_sff 23) "hello") in
      let () = Socketcan.Frame.print msg in
      let _ = Socketcan.Socket.send fd msg in
      ()
    end
  | _ ->
    Printf.printf "Usage: %s [in|out]\n" Sys.argv.(0)

