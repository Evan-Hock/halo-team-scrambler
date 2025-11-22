module Util exposing (swap, shuffle, splice)

import Random
import Array exposing (Array)


swap : Int -> Int -> Array a -> Array a
swap i j xs =  
    Maybe.map2 (\ xi xj ->
        xs
        |> Array.set i xj
        |> Array.set j xi)
        (Array.get i xs)
        (Array.get j xs)
    |> Maybe.withDefault xs


shuffle : Array a -> Random.Generator (Array a)
shuffle a =
    shuffleLoop a (Array.length a)


splice : Int -> Int -> Array a -> Array a -> Array a
splice index deleteCount toInsert xs =
    Array.append (Array.append (Array.slice 0 index xs) toInsert) (Array.slice (index + deleteCount) (Array.length xs) xs)


shuffleLoop : Array a -> Int -> Random.Generator (Array a)
shuffleLoop xs unshuffledLength =
    if unshuffledLength <= 1 then
        Random.constant xs
    else
    let
        end = unshuffledLength - 1
    in
        Random.int 0 end
        |> Random.andThen (\ choice ->
            shuffleLoop (swap choice end xs) (unshuffledLength - 1))