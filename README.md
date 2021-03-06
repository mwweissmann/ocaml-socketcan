# OCaml SocketCAN
This OCaml library provides bindings to the
[Linux SocketCAN](https://www.kernel.org/doc/Documentation/networking/can.txt)
interface.

To start programming, check the tools:
* candump is a simple can-sniffer using a blocking receive in a loop
* heartbeat uses the broadcast-manager (BCM) to regularly send packages via the kernel

The full interface can be seen in the [API documentation](http://mwweissmann.github.io/ocaml-socketcan/).

To compile OCaml-SocketCAN you need to have the Linux header files on your system. OCaml-SocketCAN also requires the [OCaml posix-time](https://github.com/mwweissmann/ocaml-posix-time) library for timestamps on CAN frames.

The source code of time is available under the MIT license.

This library is originally written by [Markus Weissmann](http://www.mweissmann.de/)

```ocaml
(** CAN bindings to the Linux SocketCAN interface *)

(** CAN identifiers of either standard (11 Bit) or extended frame format
    (29 Bit) size *)
module Id : sig
  (** A CAN identifier of up to 29 bit;
      it includes the property of it either being a CAN 2.0A (sff, 11 Bit)
      or CAN 2.0B (eff, 29 Bit) identifier. *)
  type t

  (** Create a valid CAN 2.0A {i standard frame format} identifier with 11 bit
      from an integer value.
      Masks all bits that are not in [sff_mask]. *)
  val create_sff : int -> t

  (** Create a valid CAN 2.0B {i extended frame format} identifier with 29 bit
      from an integer value.
      Masks all bits that are not in [eff_mask]. *)
  val create_eff : int -> t

  (** Get the 11 bit (sff) or 29 bit (eff) CAN identifier as integer. *)
  val to_int : t -> int

  (** [make_sff id] transforms a CAN id making it a {i standard frame format}
      identifier. If [id] already is a CAN2.0A identifier, this is a no-op. *)
  val make_sff : t -> t

  (** [is_sff id] returns whether the id [id] is a {i standard frame format}
      identifier. *)
  val is_sff : t -> bool

  (** [make_eff id] transforms a CAN id making it an {i extended frame format}
      identifier. If [id] already is a CAN2.0B identifier, this is a no-op. *)
  val make_eff : t -> t

  (** [is_eff id] returns whether the id [id] is an {i extended frame format}
      identifier.
      This is the negation of [is_sff]: [is_eff x = not is_sff x]. *)
  val is_eff : t -> bool
end

(** CAN frames representing CAN message frames including CAN identifier,
    payload and time of arrival. *)
module Frame : sig
  (** A CAN frame *)
  type t

  (** The maximum payload length in bytes of a CAN frame *)
  val max_dlen : int

  (** [create i data] creates a new CAN frame with CAN-id [i] and payload
      [data]; the payload size must not exceed [max_dlen] bytes else
      [Result.Error] is returned.
      The {i extended frame format} flag ist set, if the supplied CAN id [i] is
      an CAN 2.0B identifier ({i iff} [Id.is_eff i = true]).
      The {i remote transmission request flag} and {i error frame flag} are set
      if [rtr] and [error] are set to [true] respectively.
      The default timestamp is {i 0 sec, 0 nsec}. *)
  val create : Id.t -> ?rtr:bool -> ?error:bool
    -> ?timestamp:Posix_time.Timespec.t -> Bytes.t
    -> (t, [> `Exceeded_payload_size]) Result.result

  (** [create_exn i data] is identical to [create i data] but will raise
      [Invalid_argument] in case the payload size is exceeded. *)
  val create_exn : Id.t -> ?rtr:bool -> ?error:bool
    -> ?timestamp:Posix_time.Timespec.t -> Bytes.t -> t

  (** [create i p] creates a new CAN data frame with CAN-id [i] and payload [p];
      the payload size must not exceed [max_dlen] bytes else [Result.Error] is
      returned. *)
  val create_data : Id.t -> Bytes.t
    -> (t, [> `Exceeded_payload_size]) Result.result

  (** [create_exn i p] is identical to [create i p] but will raise
      [Invalid_argument] in case the payload size is exceeded. *)
  val create_data_exn : Id.t -> Bytes.t -> t

  (** [create_rtr i] creates a new {i remote transmission request} CAN frame
      with CAN-id [i]. The frame does not contain any payload. *)
  val create_rtr : Id.t -> t

  (** [id f] returns the CAN-id of the frame [f]. *)
  val id : t -> Id.t

  (** [data f] returns the payload of the frame [f] *)
  val data : t -> Bytes.t

  (** [set_data f d] returns a new frame with the payload set to [d]. All other
      properties of the new frame are identical to [f]. *)
  val set_data : t -> Bytes.t -> (t, [> `Exceeded_payload_size]) Result.result

  (** [timestamp frame] returns the timestamp of the frame [frame]. *)
  val timestamp : t -> Posix_time.Timespec.t

  (** [set_timestamp frame timestamp] returns a new frame with the timestamp
      set to [timestamp]. *)
  val set_timestamp : t -> Posix_time.Timespec.t -> t

  (** [is_rtr frame] returns whether the frame [frame] is a
      {i remote transmission request} frame. *)
  val is_rtr : t -> bool

  (** [set_rtr frame] returns a new frame with the
      {i remote transmission request} flag set. *)
  val set_rtr : t -> t

  (** [unset_rtr frame] returns a new frame with the
      {i remote transmission request} flag removed. *)
  val unset_rtr : t -> t

  (** [is_error frame] returns whether the frame [frame] is an error message
      frame *)
  val is_error : t -> bool

  (** [set_error frame] returns a new frame with the {i error message}
      flag set. *)
  val set_error : t -> t

  (** [unset_error frame] returns a new frame with the {i error message}
      flag removed. *)
  val unset_error : t -> t

  (** [is_sff frame] returns whether the frame [frame] is a {i standard frame
      format} frame. *)
  val is_sff : t -> bool

  (** [set_sff frame] returns a new frame with the {i standard frame format}
      flag set. The CAN identifier will be masked down to [sff_mask]. *)
  val set_sff : t -> t

  (** [is_eff frame] returns whether the frame [frame] is an {i extended frame
      format} frame.
      This is the negation of [is_sff]: [is_eff x = not is_sff x]. *)
  val is_eff : t -> bool

  (** [set_eff frame] returns a new frame with the {i extended frame format}
      flag set. *)
  val set_eff : t -> t

  (** [to_string f] returns a human-readable string representation of the frame
      [f]. *)
  val to_string : t -> string

  (** [print f] prints the frame f to [stdout] in a human-readable fashion. *)
  val print : t -> unit
end

(** CAN identifier masks for use with filters. *)
module Mask : sig
  (** A CAN identifier mask of up to 29 bit; *)
  type t

  (** Create a valid CAN 2.0A {i standard frame format} mask with 11 bit from
      an integer value.  Masks all bits that are not in [sff_mask]. *)
  val create_sff : int -> t

  (** Create a valid CAN 2.0B {i extended frame format} mask with 29 bit from
      an integer value.  Masks all bits that are not in [eff_mask]. *)
  val create_eff : int -> t

  (** Get the mask value as integer. *)
  val to_int : t -> int

  (** This is the bitmask for a {i standard frame format} CAN identifier with
      11 bit. *)
  val sff : t

  (** This is the bitmask of an {i extended frame format} CAN identifier with
      29 bit. *)
  val eff : t
end

(** Filters for incoming data that can be applied to a socket. *)
module Filter : sig
  (** A CAN filter *)
  type t

  (** [create ~mask:m id] creates a new receive filter that matches can-ids for
      which [received_id & mask = id & mask]; the default value for [can_mask]
      is [eff_mask].
      By default [extended_frames] is [`Also], [error_frames] is [`No] and
      [remote_frames] is [`No]. *)
  val create :
    ?remote_frames:[ `Also | `Exclusive | `No ] ->
    ?extended_frames:[ `Also | `Exclusive | `No ] ->
    ?mask:Mask.t -> Id.t -> t
end

(** Errors of the CAN interface *)
module Error : sig
  (** error class which gets encoded in the can-id of error-frames *)
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

  (** error status that is encoded in the payload of error-frames *)
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

  (** [of_frame f] parses the given frame [f] for it's error class and error
    status. If [f] is not an error frame, empty lists are returned. *)
  val of_frame : Frame.t -> t list * status list

  val string_of : t -> string

  val string_of_status : status -> string
end

(** A CAN socket: This is a file descriptor on which CAN message frames can be
    sent and received. *)
module Socket : sig
  (** A CAN socket for sending and receiving CAN message frames. *)
  type t

  (** [create s] opens the can-interface named [s] (e.g. "can0") *)
  val create : string -> (t, [> `EUnix of Unix.error]) Result.result

  (** [create_exn s] is identical to [create s] but will raise a
    [Unix_error] exception in case of an error. *)
  val create_exn : string -> t

  (** [close s] closes the socket [s]. *)
  val close : t -> unit

  (** [set_receive_filter s fs] adds the list of input filters [fs] to the
      socket [s]; the socket will then only receive frames the can-id of which
      matches any of the given filters;
      Calling [set_receive_filter s []] -- with an empty filter list -- will
      filter out all incoming messages!
      The original socket is modified and returned for convenience. *)
  val set_receive_filter : t -> Filter.t list
    -> (t, [> `EUnix of Unix.error ]) Result.result

  (** [set_error_flags s es] will alter the socket [s] such that all errors as
      selected in [es] will now be sent as CAN frames to the socket.
      The original socket is modified and returned for convenience. *)
  val set_error_flags : t -> Error.t list
    -> (t, [> `EUnix of Unix.error ]) Result.result

  (** [receive s] will blocking wait for the next frame on the socket [s];
      the returned frame is a copy and will not be altered by subsequent calls
      to [receive].
      The timestamp of the frame is set to the (not necessarily monotonic)
      system time indicating the time of arrival of the frame. *)
  val receive : t -> (Frame.t, [>`EUnix of Unix.error]) Result.result

  (** [receive_exn s] works exactly like [receive s] but will raise a
      [Unix_error] exception in case of an error. *)
  val receive_exn : t -> Frame.t

  (** [send s f] will send the frame [f] on the socket [s]; this call will block
      if the interface is not ready. *)
  val send : t -> Frame.t -> (int, [>`EUnix of Unix.error]) Result.result

  (** [send_exn s f] works exactly like [send s f] but will raise a
      [Unix_error] exception in case of an error. *)
  val send_exn : t -> Frame.t -> int

  (** [fs s] will return a Unix-file-descriptor of the socket [s];
      this file-descriptor can then be used with e.g. [Unix.select]. *)
  val fd : t -> Unix.file_descr
end

(** The SocketCAN BroadCast Manager (BCM) allows to send CAN message frames
    from kernel space; the kernel e.g. can then send periodic messages without
    having to be triggered by user space. *)
module BCM : sig
  (** CAN broadcast manager *)
  type t

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

  (** [create s] opens the can-interface named [s] (e.g. "can0") *)
  val create : string -> (t, [>`EUnix of Unix.error]) Result.result

  (** [close s] closes the socket [s]. *)
  val close : t -> unit

  (** time value with seconds [tv_sec] and microseconds [tv_usec] *)
  type timer = Posix_time.Timeval.t

  (** [write socket ops flags (x, t1, t2, id) frames] writes the
      configuration to the broadcast socket *)
  val write : t -> opcode list -> flag list -> (int * timer * timer * Id.t) ->
    Frame.t list -> (unit, [>`EUnix of Unix.error]) Result.result

  (** [fs s] will return a Unix-file-descriptor of the socket [s];
      this file-descriptor can then be used with e.g. [Unix.select]. *)
  val fd : t -> Unix.file_descr
end
```
