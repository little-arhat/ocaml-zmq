(* Copyright (c) 2011 Pedro Borges and contributors *)

val version : unit -> int * int * int


(** Context *)
type context

(** Creation and Destruction *)
val init : int -> unit -> context
val term : context -> unit


(** Sockets *)
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

val socket : context -> socket_type -> socket
val close : socket -> unit

type event = 
 | Poll_in
 | Poll_out

(** options *)
type 'a socket_option

val socket_type : socket_type socket_option
val linger : int socket_option
val reconnect_ivl : int socket_option
val reconnect_ivl_max : int socket_option
val backlog : int socket_option
val maxmsgsize : int64 socket_option
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

(** The documenation states that this option
 *  should return and fd to be used with poll
 *  the bindings return a file_descr compatible with Unix.select *)
val fd : Unix.file_descr socket_option

val getsockopt : socket -> 'a socket_option -> 'a
val setsockopt : socket -> 'a socket_option -> 'a -> unit

val connect : socket -> string -> unit
val bind : socket -> string -> unit

type recv_option = [`No_block]
val recv : socket -> recv_option list -> string

type send_option = [`No_block | `Snd_more]
val send : socket -> string -> send_option list -> unit

(** Create a message once and send it multiple times without copy *)
type message
val message : string -> message
val send_msg : socket -> message -> send_option list -> unit

(** Devices *)

type device_type =
  | Queue
  | Streamer
  | Forwarder

val device : device_type -> socket -> socket -> unit

(** Poll *)

type poller
type poll_item = socket * event list
(** Retuns a list of poll items with the available events received *)

val poller : poll_item list -> poller
val poll : poller -> int -> poll_item list


(** Exceptions *)

(* Invalid_argument - standard ocaml error *)
exception Interrupted of string
exception Terminated of string
exception Protocol_not_supported of string
exception Protocol_not_compatible of string
exception Unavailable_threads of string
exception Address_in_use of string
exception Address_not_available of string
exception Nonexistant_interface of string
exception No_memory of string
exception Try_again of string
exception Operation_not_supported of string
exception Invalid_socket_state of string

(* What happens when an option is set from the worng socket type *)

(* If this exception is raised it means there 
 * is an error in the bindings *)
exception Unknown_error of string
