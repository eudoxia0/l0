(*
    Copyright 2018 Fernando Borretti <fernando@borretti.me>

    This file is part of L0.

    L0 is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    L0 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with L0.  If not, see <http://www.gnu.org/licenses/>.
*)

structure OrderedSet :> ORDERED_SET = struct
  datatype ''a set = Set of ''a list

  val empty = Set []

  fun singleton a = Set [a]

  fun add (Set l) elem =
    if Util.member elem l then
        Set l
    else
        Set (elem :: l)

  fun addList set (x::xs) = add (addList set xs) x
    | addList set nil = set

  fun union (Set a) (Set b) = addList (addList empty a) b

  fun unionList l = foldl (fn (a, b) => union a b)
                          empty
                          l

  fun size (Set l) = List.length l

  fun positionOf (Set l) elem = Util.position elem l

  fun filter (Set l) f = Set (List.filter f l)

  fun fromList (x::xs) = add (fromList xs) x
    | fromList nil = empty
end
