(* Copyright (c) 2011 Pedro Borges and contributors *)

external version : unit -> int * int * int = "caml_zmq_version"


type context

external init : int -> context = "caml_zmq_init"
external term : context -> unit = "caml_zmq_term"

type socket

type socket_type = 
  | Req
  | Rep
  | Dealer
  | Router
  | Pub
  | Sub
  | Push
  | Pull
  | Pair

external socket : context -> socket_type -> socket = "caml_zmq_socket"
external close : socket -> unit = "caml_zmq_close"

type 'a socket_option = (socket -> 'a -> unit) * (socket -> 'a)

let no_get_opt _ : socket -> 'a = invalid_arg "This option cannot be gotten."
let no_set_opt _ _ : socket -> 'a -> unit = invalid_arg "This option cannot be set."

type int_option =
  | Linger
  | Reconnect_ivl
  | Reconnect_ivl_max 
  | Backlog

external set_int_sockopt : int_option -> socket -> int -> unit =
        "caml_zmq_set_int_sockopt"
external get_int_sockopt : int_option -> socket -> int =
        "caml_zmq_get_int_sockopt"

type int64_option =
  | Swap
  | Recovery_ivl
  | Recovery_ivl_max
  | Hwm
  | Snd_buff
  | Rcv_buff

external set_int64_sockopt : int64_option -> socket -> int64 -> unit =
        "caml_zmq_set_int64_sockopt"
external get_int64_sockopt : int64_option -> socket -> int64 =
        "caml_zmq_get_int64_sockopt"

type string_option =
  | Identity
  | Subscribe
  | Unsubscribe

external set_string_sockopt : string_option -> socket -> string -> unit =
        "caml_zmq_set_int64_sockopt"
external get_string_sockopt : string_option -> socket -> string =
        "caml_zmq_get_int64_sockopt"

type bool_option =
 | Mcast_loop
 | Rcv_more

external set_bool_sockopt : string_option -> socket -> bool -> unit =
        "caml_zmq_set_bool_sockopt"
external get_bool_sockopt : string_option -> socket -> bool =
        "caml_zmq_get_bool_sockopt"

external get_type_sockopt : socket -> socket_type =
        "caml_zmq_get_type_sockopt"

external get_event_sockopt : socket -> list event =
        "caml_zmq_get_event_sockopt"

external get_fd_sockopt : socket -> Unix.file_descr =
        "caml_zmq_get_fd_sockopt"

type bool_array_option =
  | Affinity

external set_bool_array_sockopt' : bool_array_option -> socket -> bool array -> unit =
        "caml_zmq_set_bool_array_sockopt"

let set_bool_array_sockopt opt sock value =
  if Array.length n <> 64 then
    invalid_arg "Affinity array must have 64 positions"
  else
    set_bool_array_sockopt' opt sock value

external get_bool_array_sockopt : bool_array_option -> socket -> bool array =
        "caml_zmq_get_bool_array_sockopt"


let socket_type type = (no_set_opt, get_type_sockopt)
let linger = (get_int_sockopt Linger, set_int_sockopt Linger)
let reconnect_ivl = (get_int_sockopt Reconnect_ivl, set_int_sockopt Recovery_ivl)
let reconnect_ivl_max = (get_int_sockopt Reconnect_ivl_max, set_int_sockopt Recovery_ivl_max)
val backlog = (get_int_sockopt Backlog, set_int_sockopt Backlog)
val swap : int64 socket_option
val rate : int64 socket_option
val recovery_ivl : int64 socket_option
val recovery_ivl_msec : int64 socket_option
val mcast_loop : bool socket_option
val rcvmore : bool socket_option
val hwm : int64 socket_option
val affinity : bool array socket_option
val sndbuf : int64 socket_option
val rcvbuf : int64 socket_option
val identity : string option socket_option
val subscribe : string socket_option
val unsubscribe : string socket_option
val events : event list socket_option


let getsockopt socket opt = (snd opt) socket
let setsockopt socket opt value = (fst opt) socket value

external connect : socket -> string -> unit =
        "caml_zmq_connect"

external bind : socket -> string -> unit = 
        "caml_zmq_bind"

(** Send / Receive *)

external recv : socket -> recv_option' -> string =
        "caml_zmq_recv"
external send : socket -> string -> send_option' -> unit=
        "caml_zmq_send"

external message : string -> message = 
        "caml_zmq_message"

external send_msg : socket -> message -> send_option list -> unit =
        "caml_zmq_send_message"

type device_type =
  | Queue
  | Streamer
  | Forwarder

external device : device_type -> socket -> socket -> unit = 
        "caml_zmq_device"

type poller
type poll_item = socket * event list

external poller : poll_item list -> poller =
        "caml_zmq_poller"

external poll : poller -> int -> poll_item list =
        "caml_zmq_poll"

let () =
  (** Register exceptions register poly variant values *)
