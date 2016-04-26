(*
Copyright (c) 2016 Markus W. Weissmann <markus.weissmann@in.tum.de>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*)

let is_set flag v = Int32.logand flag v = flag

let set flag v = Int32.logor flag v

let unset flag v = Int32.logand (Int32.lognot flag) v

let eff_flag = 0x80000000l
let rtr_flag = 0x40000000l
let err_flag = 0x20000000l

let sff_mask = 0x000007FFl
let eff_mask = 0x1FFFFFFFl

module Id = struct
  type t = Int32.t

  let noflags = Int32.logor eff_mask eff_flag

  let make_sff = Int32.logand sff_mask

  let create_sff x = make_sff (Int32.of_int x)

  let make_eff x = set eff_flag (Int32.logand eff_mask x)

  let create_eff x = make_eff (Int32.of_int x)

  let to_int x = Int32.to_int (unset eff_flag (Int32.logand eff_mask x))

  let is_sff x = not (is_set eff_flag x)

  let is_eff x = is_set eff_flag x
end

module Mask = struct
  type t = Int32.t

  let sff = sff_mask

  let eff = eff_mask

  let create_sff x = Int32.logand sff_mask (Int32.of_int x)

  let create_eff x = set eff_flag (Int32.logand eff_mask (Int32.of_int x))

  let to_int x = Int32.to_int (unset eff_flag (Int32.logand eff_mask x))
end

module Frame = struct
  type t = { id : Id.t; data : Bytes.t; timestamp : Posix_time.Timespec.t }

  let t_zero = Posix_time.Timespec.create 0L 0L

  let max_dlen = 8

  let create id ?rtr:(r=false) ?error:(e=false) ?timestamp:(t=t_zero) data =
    if Bytes.length data > max_dlen then Result.Error `Exceeded_payload_size
    else
      let id = if e then set err_flag id else id in
      let id = if r then set rtr_flag id else id in
      Result.Ok { id; data; timestamp = t }

  let create_exn id ?rtr:(r=false) ?error:(e=false) ?timestamp:(t=t_zero) data =
    if Bytes.length data > max_dlen then raise (Invalid_argument "payload size exceeded")
    else
      let id = if e then set err_flag id else id in
      let id = if r then set rtr_flag id else id in
      { id; data; timestamp = t }

  let create_data id data =
    if Bytes.length data > max_dlen then Result.Error `Exceeded_payload_size
    else Result.Ok { id; data; timestamp = t_zero }

  let create_data_exn id data =
    if Bytes.length data > max_dlen then raise (Invalid_argument "payload size exceeded")
    else { id; data; timestamp = t_zero }

  let create_rtr id = { id = set rtr_flag id; data = Bytes.empty; timestamp = t_zero }

  let id frame = Int32.logand frame.id Id.noflags

  let data frame = frame.data

  let set_data frame data =
    if Bytes.length data > max_dlen then Result.Error `Exceeded_payload_size
    else Result.Ok { frame with data = data }

  let timestamp frame = frame.timestamp

  let set_timestamp frame t = { frame with timestamp = t }

  let is_rtr frame = is_set rtr_flag (id frame)

  let set_rtr frame = { frame with id = set rtr_flag (id frame) }

  let unset_rtr frame = { frame with id = unset rtr_flag (id frame) }

  let is_error frame = is_set err_flag (id frame)

  let set_error frame = { frame with id = set err_flag (id frame) }

  let unset_error frame = { frame with id = unset err_flag (id frame) }

  let is_sff frame = Id.is_sff (id frame)

  let set_sff frame = { frame with id = Id.make_sff (id frame) }

  let is_eff frame = Id.is_eff (id frame)

  let set_eff frame = { frame with id = Id.make_eff (id frame) }

  let to_string frame =
    let sec, nsec =
      let t = timestamp frame in
      Posix_time.Timespec.(t.tv_sec, t.tv_nsec)
    in
    let res = ref (Printf.sprintf "%Ld:%09Ld <%08x> [%d]" sec nsec (Id.to_int (id frame)) (Bytes.length frame.data)) in
    let () = Bytes.iter (fun x -> res := (Printf.sprintf "%s %02x" !res (Char.code x))) (data frame) in
    let () =
      if (is_error frame) || (is_rtr frame) then
        res := Printf.sprintf "%s [%s%s]" !res
          (if is_rtr frame then "r" else "")
          (if is_error frame then "e" else "")
      else ()
    in
    !res

  let print frame = print_endline (to_string frame)
end

module Filter = struct
  type t = { can_id : Id.t; can_mask : Id.t }

  let create ?error_frames:(err=`No) ?remote_frames:(rtr=`No)
      ?extended_frames:(eff=`Also) ?mask:(m=eff_mask) id =
    let bits s = function
      | `No -> Int32.zero, s
      | `Exclusive -> s, s
      | `Also -> Int32.zero, Int32.zero
    in
    let eff_id, eff_mask = bits eff_flag eff in
    let rtr_id, rtr_mask = bits rtr_flag rtr in
    let err_id, err_mask = bits err_flag err in

    let effbit_mask = Int32.lognot eff_flag in
    let can_id =
      Int32.logor err_id (Int32.logor rtr_id (Int32.logor eff_id (Int32.logand effbit_mask (Int32.logand id eff_mask))))
    in
    let can_mask =
      Int32.logor err_mask (Int32.logor rtr_mask (Int32.logor eff_mask (Int32.logand effbit_mask (Int32.logand m eff_mask))))
    in
    Printf.printf "filter: id=0x%lX, mask=0x%lX\n%!" can_id can_mask;
    { can_id; can_mask }
end

module Socket = struct
  type t = Unix.file_descr

  type error_flag =
    | CAN_ERR_TX_TIMEOUT
    | CAN_ERR_LOSTARB
    | CAN_ERR_CRTL
    | CAN_ERR_PROT
    | CAN_ERR_TRX
    | CAN_ERR_ACK
    | CAN_ERR_BUSOFF
    | CAN_ERR_BUSERROR
    | CAN_ERR_RESTARTED

  external create : string -> (t, [>`EUnix of Unix.error]) Result.result = "can_open"

  let close = Unix.close

  external set_receive_filter : t -> Filter.t list -> (t, [> `EUnix of Unix.error ]) Result.result = "can_receive_filter"

  external set_error_flags : t -> error_flag list -> (t, [>`EUnix of Unix.error]) Result.result = "can_error_flags"

  external receive : t -> (Frame.t, [>`EUnix of Unix.error]) Result.result = "can_receive"

  external send : t -> Frame.t -> (int, [>`EUnix of Unix.error]) Result.result = "can_send"

  external fd : t -> t = "%identity"
end

module BCM = struct
  type t = Unix.file_descr

  type opcode =
    | TX_SETUP
    | TX_DELETE
    | TX_READ
    | TX_SEND
    | TX_STATUS
    | TX_EXPIRED
    | RX_SETUP
    | RX_DELETE
    | RX_READ
    | RX_STATUS
    | RX_TIMEOUT
    | RX_CHANGED

  type flag =
    | SETTIMER  
    | STARTTIMER 
    | TX_COUNTEVT
    | TX_ANNOUNCE 
    | TX_CP_CAN_ID
    | RX_FILTER_ID
    | RX_CHECK_DLC   
    | RX_NO_AUTOTIMER   
    | RX_ANNOUNCE_RESUME
    | TX_RESET_MULTI_IDX
    | RX_RTR_FRAME

  type timer = Posix_time.Timeval.t

  external create : string -> (t, [>`EUnix of Unix.error]) Result.result = "bcm_open"

  let close = Unix.close

  external write : t -> opcode list -> flag list -> (int * timer * timer * Id.t) -> Frame.t list -> (unit, [>`EUnix of Unix.error]) Result.result = "bcm_write"

  external fd : t -> t = "%identity"
end

external can_initialize : unit -> unit = "can_initialize"
let () = can_initialize ()

external bcm_initialize : unit -> unit = "bcm_initialize"
let () = bcm_initialize ()

