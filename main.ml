[@@@ocaml.warning "-32-34-69"]

module type Basic = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type Extended = sig
  include Basic

  type state

  val run : 'a t -> state -> 'a * state
end

module rec Choice : (Extended with type state = State.t) = struct
  type state = State.t
  type 'a t = state -> 'a * state

  let return (v : 'a) : 'a t = fun (s : state) -> (v, s)

  let bind (v : 'a t) (f : 'a -> 'b t) : 'b t =
   fun s ->
    let v, s = v s in
    (f v) (State.incr s)

  let run (v : 'a t) (s : state) : 'a * state = v s
end

and State : sig
  type t

  val init : Memory.t ref -> t
  val n : t -> int
  val incr : t -> t
end = struct
  type t = { n : int; memory : Memory.t ref }

  let init mem = { n = 0; memory = mem }
  let get_memory s = s.memory
  let n s = s.n
  let incr s = { s with n = succ s.n }
end

and Memory : sig
  type t

  val create : int -> t
  val store : t -> int -> int -> unit
  val dummy : t -> int Choice.t
end = struct
  type t = int array

  let create n = Array.make n 0
  let store m n v = Array.set m n v
  let dummy _m = Choice.return 0
end

let ( let* ) = Choice.bind

let () =
  let memory = ref (Memory.create 10) in
  let state = State.init memory in
  let computation =
    let* f1 = Choice.return 1 in
    let* f2 = Choice.return 1 in
    let* f3 = Choice.return (f1 + f2) in
    let* f4 = Choice.return (f2 + f3) in
    Choice.return (f3 + f4)
  in
  let fib5, state = Choice.run computation state in
  Format.printf "Fib5: %d with bind count: %d @." fib5 (State.n state)
