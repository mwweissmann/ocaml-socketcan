let get_ok = function | Result.Ok x -> x | Result.Error _ -> failwith "oops"

let parse_hex_string str =
  let () = if String.length str > 16 then failwith "payload too long" else () in
  let front str : Bytes.t =
    let head = String.sub str 0 2 in
    Bytes.make 1 (Char.chr (int_of_string ("0x" ^ head)))
  in
  let tail = ref str in
  let stream = ref Bytes.empty in
  try
    for i = 0 to 7 do
      let () = stream := Bytes.concat Bytes.empty [!stream; front !tail] in
      tail := String.sub !tail 2 (String.length !tail - 2)
    done;
    !stream
  with
    Invalid_argument _ -> !stream

let _ =
  if Array.length Sys.argv != 4 then
    Printf.printf "%s IFACE CANID PAYLOAD" Sys.argv.(0)
  else
    let canid = Socketcan.Id.create_eff (int_of_string Sys.argv.(2)) in
    let payload = parse_hex_string Sys.argv.(3) in
    let msg = get_ok (Socketcan.Frame.create canid payload) in
    let socket = get_ok (Socketcan.BCM.create Sys.argv.(1)) in

    let t1 = Posix_time.Timeval.create 0L 0L in
    let t2 = Posix_time.Timeval.create 0L 100000L in

    let open Socketcan.BCM in
    let _ = Socketcan.BCM.write socket [TX_SETUP] [SETTIMER; STARTTIMER] (0, t1, t2, canid) [msg] in
    Unix.sleep(600)

