open Containers

module Option =
struct

	include Option

	let get default t = get_or ~default t

	let map_or default f t = map_or ~default f t

end

module List =
struct

	include List

	let rec assoc_all ~eq key = function
		| (key', v) :: tl when eq key' key ->
			v :: assoc_all ~eq key tl
		| _ :: tl	-> assoc_all ~eq key tl
		| []		-> []

end

module Float =
struct
	include Float
	include Float.Infix
end

module Int =
struct
	include Int
	include Int.Infix
end

module Int32 =
struct
	include Int32
	include Int32.Infix
end

module Int64 =
struct
	include Int64
	include Int64.Infix
end

module Array = Array
module ArrayLabels = ArrayLabels
module Array_slice = Array_slice
module Bool = Bool
module Char = Char
module Equal = Equal
module Format = Format
module Fun = Fun
module Hash = Hash
module Hashtbl = Hashtbl
module Heap = Heap
module IO = IO
module ListLabels = ListLabels
module Map = Map
module Nativeint = Nativeint
module Ord = Ord
module Pair = Pair
module Parse = Parse
module Random = Random
module Ref = Ref
module Result = Result
module Set = Set
module String = String
module Vector = Vector
module Monomorphic = Monomorphic
module Utf8_string = Utf8_string

include Fun
include Monomorphic

let _Some v = Some v
