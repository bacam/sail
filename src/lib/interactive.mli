(****************************************************************************)
(*     Sail                                                                 *)
(*                                                                          *)
(*  Sail and the Sail architecture models here, comprising all files and    *)
(*  directories except the ASL-derived Sail code in the aarch64 directory,  *)
(*  are subject to the BSD two-clause licence below.                        *)
(*                                                                          *)
(*  The ASL derived parts of the ARMv8.3 specification in                   *)
(*  aarch64/no_vector and aarch64/full are copyright ARM Ltd.               *)
(*                                                                          *)
(*  Copyright (c) 2013-2021                                                 *)
(*    Kathyrn Gray                                                          *)
(*    Shaked Flur                                                           *)
(*    Stephen Kell                                                          *)
(*    Gabriel Kerneis                                                       *)
(*    Robert Norton-Wright                                                  *)
(*    Christopher Pulte                                                     *)
(*    Peter Sewell                                                          *)
(*    Alasdair Armstrong                                                    *)
(*    Brian Campbell                                                        *)
(*    Thomas Bauereiss                                                      *)
(*    Anthony Fox                                                           *)
(*    Jon French                                                            *)
(*    Dominic Mulligan                                                      *)
(*    Stephen Kell                                                          *)
(*    Mark Wassell                                                          *)
(*    Alastair Reid (Arm Ltd)                                               *)
(*                                                                          *)
(*  All rights reserved.                                                    *)
(*                                                                          *)
(*  This work was partially supported by EPSRC grant EP/K008528/1 <a        *)
(*  href="http://www.cl.cam.ac.uk/users/pes20/rems">REMS: Rigorous          *)
(*  Engineering for Mainstream Systems</a>, an ARM iCASE award, EPSRC IAA   *)
(*  KTF funding, and donations from Arm.  This project has received         *)
(*  funding from the European Research Council (ERC) under the European     *)
(*  Union’s Horizon 2020 research and innovation programme (grant           *)
(*  agreement No 789108, ELVER).                                            *)
(*                                                                          *)
(*  This software was developed by SRI International and the University of  *)
(*  Cambridge Computer Laboratory (Department of Computer Science and       *)
(*  Technology) under DARPA/AFRL contracts FA8650-18-C-7809 ("CIFV")        *)
(*  and FA8750-10-C-0237 ("CTSRD").                                         *)
(*                                                                          *)
(*  SPDX-License-Identifier: BSD-2-Clause                                   *)
(****************************************************************************)

open Ast
open Ast_defs
open Type_check

val opt_interactive : bool ref

(** Each interactive command is passed this struct, containing the
   abstract syntax tree, effect info and the type-checking
    environment. Also contains the default Sail directory *)
module State : sig
  type istate = {
    ctx : Initial_check.ctx;
    ast : Type_check.typed_ast;
    effect_info : Effects.side_effect_info;
    env : Type_check.Env.t;
    default_sail_dir : string;
    config : Yojson.Safe.t option;
  }

  val initial_istate : Yojson.Safe.t option -> string -> istate
end

val arg : string -> string
val command : string -> string

type action =
  | ArgString of string * (string -> action)
  | ArgInt of string * (int -> action)
  | Action of (State.istate -> State.istate)
  | ActionUnit of (State.istate -> unit)

val reflect_typ : action -> typ

val get_command : string -> (string * action) option

val all_commands : unit -> (string * (string * action)) list

val generate_help : string -> string -> action -> string

val run_action : State.istate -> string -> string -> action -> State.istate

(** This is the main function used to register new interactive commands. *)
val register_command : name:string -> help:string -> action -> unit
