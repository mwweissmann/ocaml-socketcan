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

let wrap_unix result function_name =
  match result with
  | Result.Ok s -> s
  | Result.Error (`EUnix err) -> raise (Unix.Unix_error (err, function_name, ""))

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

  let is_rtr frame = is_set rtr_flag frame.id

  let set_rtr frame = { frame with id = set rtr_flag frame.id }

  let unset_rtr frame = { frame with id = unset rtr_flag frame.id }

  let is_error frame = is_set err_flag frame.id

  let set_error frame = { frame with id = set err_flag frame.id }

  let unset_error frame = { frame with id = unset err_flag frame.id }

  let is_sff frame = Id.is_sff frame.id

  let set_sff frame = { frame with id = Id.make_sff frame.id }

  let is_eff frame = Id.is_eff frame.id

  let set_eff frame = { frame with id = Id.make_eff frame.id }

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

  let create ?remote_frames:(rtr=`No) ?extended_frames:(eff=`Also)
      ?mask:(m=eff_mask) id =
    let bits s = function
      | `No -> Int32.zero, s
      | `Exclusive -> s, s
      | `Also -> Int32.zero, Int32.zero
    in
    let eff_id, eff_mask = bits eff_flag eff in
    let rtr_id, rtr_mask = bits rtr_flag rtr in

    let effbit_mask = Int32.lognot eff_flag in
    let can_id =
      Int32.logor rtr_id (Int32.logor eff_id (Int32.logand effbit_mask (Int32.logand id Mask.eff)))
    in
    let can_mask =
      Int32.logor rtr_mask (Int32.logor eff_mask (Int32.logand effbit_mask (Int32.logand m Mask.eff)))
    in
    (* Printf.printf "filter: id=0x%lX, mask=0x%lX\n%!" can_id can_mask; *)
    { can_id; can_mask }
end

module Error = struct
  type t =
    | CAN_ERR_TX_TIMEOUT
    | CAN_ERR_LOSTARB
    | CAN_ERR_CRTL
    | CAN_ERR_PROT
    | CAN_ERR_TRX
    | CAN_ERR_ACK
    | CAN_ERR_BUSOFF
    | CAN_ERR_BUSERROR
    | CAN_ERR_RESTARTED

  type status =
    | CAN_ERR_CRTL_RX_OVERFLOW
    | CAN_ERR_CRTL_TX_OVERFLOW
    | CAN_ERR_CRTL_RX_WARNING
    | CAN_ERR_CRTL_TX_WARNING
    | CAN_ERR_CRTL_RX_PASSIVE
    | CAN_ERR_CRTL_TX_PASSIVE
    | CAN_ERR_PROT_BIT
    | CAN_ERR_PROT_FORM
    | CAN_ERR_PROT_STUFF
    | CAN_ERR_PROT_BIT0
    | CAN_ERR_PROT_BIT1
    | CAN_ERR_PROT_OVERLOAD
    | CAN_ERR_PROT_ACTIVE
    | CAN_ERR_PROT_TX
    | CAN_ERR_PROT_LOC_SOF
    | CAN_ERR_PROT_LOC_ID28_21
    | CAN_ERR_PROT_LOC_ID20_18
    | CAN_ERR_PROT_LOC_SRTR
    | CAN_ERR_PROT_LOC_IDE
    | CAN_ERR_PROT_LOC_ID17_13
    | CAN_ERR_PROT_LOC_ID12_05
    | CAN_ERR_PROT_LOC_ID04_00
    | CAN_ERR_PROT_LOC_RTR
    | CAN_ERR_PROT_LOC_RES1
    | CAN_ERR_PROT_LOC_RES0
    | CAN_ERR_PROT_LOC_DLC
    | CAN_ERR_PROT_LOC_DATA
    | CAN_ERR_PROT_LOC_CRC_SEQ
    | CAN_ERR_PROT_LOC_CRC_DEL
    | CAN_ERR_PROT_LOC_ACK
    | CAN_ERR_PROT_LOC_ACK_DEL
    | CAN_ERR_PROT_LOC_EOF
    | CAN_ERR_PROT_LOC_INTERM
    | CAN_ERR_TRX_CANH_NO_WIRE
    | CAN_ERR_TRX_CANH_SHORT_TO_BAT
    | CAN_ERR_TRX_CANH_SHORT_TO_VCC
    | CAN_ERR_TRX_CANH_SHORT_TO_GND
    | CAN_ERR_TRX_CANL_NO_WIRE
    | CAN_ERR_TRX_CANL_SHORT_TO_BAT
    | CAN_ERR_TRX_CANL_SHORT_TO_VCC
    | CAN_ERR_TRX_CANL_SHORT_TO_GND
    | CAN_ERR_TRX_CANL_SHORT_TO_CANH

  let string_of = function
    | CAN_ERR_TX_TIMEOUT -> "TX timeout (by netdevice driver)"
    | CAN_ERR_LOSTARB -> "lost arbitration"
    | CAN_ERR_CRTL -> "controller problems"
    | CAN_ERR_PROT -> "protocol violations"
    | CAN_ERR_TRX -> "transceiver status"
    | CAN_ERR_ACK -> "received no ACK on transmission"
    | CAN_ERR_BUSOFF -> "bus off"
    | CAN_ERR_BUSERROR -> "bus error (may flood!!)"
    | CAN_ERR_RESTARTED -> "controller restarted"

  let string_of_status e =
    let ctrl x = "controller: " ^ x in
    let prot x = "protocol: " ^ x in
    let trx x = "transceiver: " ^ x in
    match e with
    | CAN_ERR_CRTL_RX_OVERFLOW -> ctrl "RX buffer overflow"
    | CAN_ERR_CRTL_TX_OVERFLOW -> ctrl "TX buffer overflow"
    | CAN_ERR_CRTL_RX_WARNING -> ctrl "reached warning level for RX errors"
    | CAN_ERR_CRTL_TX_WARNING -> ctrl "reached warning level for TX errors"
    | CAN_ERR_CRTL_RX_PASSIVE -> ctrl "reached error passive status RX"
    | CAN_ERR_CRTL_TX_PASSIVE -> ctrl "reached error passive status TX"
    | CAN_ERR_PROT_BIT -> prot "single bit error"
    | CAN_ERR_PROT_FORM -> prot "frame format error"
    | CAN_ERR_PROT_STUFF -> prot "bit stuffing error"
    | CAN_ERR_PROT_BIT0 -> prot "unable to send dominant bit"
    | CAN_ERR_PROT_BIT1 -> prot "unable to send recessive bit"
    | CAN_ERR_PROT_OVERLOAD -> prot "bus overload"
    | CAN_ERR_PROT_ACTIVE -> prot "active error announcement"
    | CAN_ERR_PROT_TX -> prot "error occurred on transmission"
    | CAN_ERR_PROT_LOC_SOF -> prot "start of frame"
    | CAN_ERR_PROT_LOC_ID28_21 -> prot "ID bits 28 - 21 (SFF: 10 - 3)"
    | CAN_ERR_PROT_LOC_ID20_18 -> prot "ID bits 20 - 18 (SFF: 2 - 0)"
    | CAN_ERR_PROT_LOC_SRTR -> prot "substitute RTR (SFF: RTR)"
    | CAN_ERR_PROT_LOC_IDE -> prot "identifier extension"
    | CAN_ERR_PROT_LOC_ID17_13 -> prot "ID bits 17-13"
    | CAN_ERR_PROT_LOC_ID12_05 -> prot "ID bits 12-5"
    | CAN_ERR_PROT_LOC_ID04_00 -> prot "ID bits 4-0"
    | CAN_ERR_PROT_LOC_RTR -> prot "RTR"
    | CAN_ERR_PROT_LOC_RES1 -> prot "reserved bit 1"
    | CAN_ERR_PROT_LOC_RES0 -> prot "reserved bit 0"
    | CAN_ERR_PROT_LOC_DLC -> prot "data length code"
    | CAN_ERR_PROT_LOC_DATA -> prot "data section"
    | CAN_ERR_PROT_LOC_CRC_SEQ -> prot "CRC sequence"
    | CAN_ERR_PROT_LOC_CRC_DEL -> prot "CRC delimiter"
    | CAN_ERR_PROT_LOC_ACK -> prot "ACK slot"
    | CAN_ERR_PROT_LOC_ACK_DEL -> prot "ACK delimiter"
    | CAN_ERR_PROT_LOC_EOF -> prot "end of frame"
    | CAN_ERR_PROT_LOC_INTERM -> prot "intermission"
    | CAN_ERR_TRX_CANH_NO_WIRE -> trx "can-high no wire"
    | CAN_ERR_TRX_CANH_SHORT_TO_BAT -> trx "can-high short to bat"
    | CAN_ERR_TRX_CANH_SHORT_TO_VCC -> trx "can-high short to vcc"
    | CAN_ERR_TRX_CANH_SHORT_TO_GND -> trx "can-high short to gnd"
    | CAN_ERR_TRX_CANL_NO_WIRE -> trx "can-low no write"
    | CAN_ERR_TRX_CANL_SHORT_TO_BAT -> trx "can-low short to bat"
    | CAN_ERR_TRX_CANL_SHORT_TO_VCC -> trx "can-low short to vcc"
    | CAN_ERR_TRX_CANL_SHORT_TO_GND -> trx "can-low short to gnd"
    | CAN_ERR_TRX_CANL_SHORT_TO_CANH -> trx "can-low short to can-high"

  let of_frame frame =
    let is_set_i flag c = flag land c = flag in

    let open Frame in
    match Frame.is_error frame with
    | false -> [], []
    | true ->
      let e_class = List.fold_left (fun xs (bit, prop) -> if is_set bit frame.id then prop::xs else xs)
        [] [
          0x00000001l, CAN_ERR_TX_TIMEOUT;
          0x00000002l, CAN_ERR_LOSTARB;
          0x00000004l, CAN_ERR_CRTL;
          0x00000008l, CAN_ERR_PROT;
          0x00000010l, CAN_ERR_TRX;
          0x00000020l, CAN_ERR_ACK;
          0x00000040l, CAN_ERR_BUSOFF;
          0x00000080l, CAN_ERR_BUSERROR;
          0x00000100l, CAN_ERR_RESTARTED
        ]
      in
      let f i p =
        let b = Char.code (Bytes.get frame.data i) in
        List.fold_left (fun xs (bit, prop) -> if p bit b then prop::xs else xs)
      in
      let e_status = f 1 is_set_i [] [
          0x01, CAN_ERR_CRTL_RX_OVERFLOW;
          0x02, CAN_ERR_CRTL_TX_OVERFLOW;
          0x04, CAN_ERR_CRTL_RX_WARNING;
          0x08, CAN_ERR_CRTL_TX_WARNING;
          0x10, CAN_ERR_CRTL_RX_PASSIVE;
          0x20, CAN_ERR_CRTL_TX_PASSIVE;
        ]
      in
      let e_status = f 2 is_set_i e_status [
          0x01, CAN_ERR_PROT_BIT;
          0x02, CAN_ERR_PROT_FORM;
          0x04, CAN_ERR_PROT_STUFF;
          0x08, CAN_ERR_PROT_BIT0;
          0x10, CAN_ERR_PROT_BIT1;
          0x20, CAN_ERR_PROT_OVERLOAD;
          0x40, CAN_ERR_PROT_ACTIVE;
          0x80, CAN_ERR_PROT_TX;
        ]
      in
      let e_status = f 3 (=) e_status [
          0x03, CAN_ERR_PROT_LOC_SOF;
          0x02, CAN_ERR_PROT_LOC_ID28_21;
          0x06, CAN_ERR_PROT_LOC_ID20_18;
          0x04, CAN_ERR_PROT_LOC_SRTR;
          0x05, CAN_ERR_PROT_LOC_IDE;
          0x07, CAN_ERR_PROT_LOC_ID17_13;
          0x0F, CAN_ERR_PROT_LOC_ID12_05;
          0x0E, CAN_ERR_PROT_LOC_ID04_00;
          0x0C, CAN_ERR_PROT_LOC_RTR;
          0x0D, CAN_ERR_PROT_LOC_RES1;
          0x09, CAN_ERR_PROT_LOC_RES0;
          0x0B, CAN_ERR_PROT_LOC_DLC;
          0x0A, CAN_ERR_PROT_LOC_DATA;
          0x08, CAN_ERR_PROT_LOC_CRC_SEQ;
          0x18, CAN_ERR_PROT_LOC_CRC_DEL;
          0x19, CAN_ERR_PROT_LOC_ACK;
          0x1B, CAN_ERR_PROT_LOC_ACK_DEL;
          0x1A, CAN_ERR_PROT_LOC_EOF;
          0x12, CAN_ERR_PROT_LOC_INTERM;
        ]
      in
      let e_status = f 4 (=) e_status [
          0x04, CAN_ERR_TRX_CANH_NO_WIRE;
          0x05, CAN_ERR_TRX_CANH_SHORT_TO_BAT;
          0x06, CAN_ERR_TRX_CANH_SHORT_TO_VCC;
          0x07, CAN_ERR_TRX_CANH_SHORT_TO_GND;
          0x40, CAN_ERR_TRX_CANL_NO_WIRE;
          0x50, CAN_ERR_TRX_CANL_SHORT_TO_BAT;
          0x60, CAN_ERR_TRX_CANL_SHORT_TO_VCC;
          0x70, CAN_ERR_TRX_CANL_SHORT_TO_GND;
          0x80, CAN_ERR_TRX_CANL_SHORT_TO_CANH;
        ]
      in
      e_class, e_status
end

module Socket = struct
  type t = Unix.file_descr

  external create : string -> (t, [>`EUnix of Unix.error]) Result.result = "can_open"

  let create_exn i = wrap_unix (create i) "socket"

  let close = Unix.close

  external set_receive_filter : t -> Filter.t list -> (t, [> `EUnix of Unix.error ]) Result.result = "can_receive_filter"

  external set_error_flags : t -> Error.t list -> (t, [>`EUnix of Unix.error]) Result.result = "can_error_flags"

  external receive : t -> (Frame.t, [>`EUnix of Unix.error]) Result.result = "can_receive"

  let receive_exn s = wrap_unix (receive s) "recvmsg"

  external send : t -> Frame.t -> (int, [>`EUnix of Unix.error]) Result.result = "can_send"

  let send_exn s f = wrap_unix (send s f) "write"

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

