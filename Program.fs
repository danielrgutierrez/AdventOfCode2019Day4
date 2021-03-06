﻿open System

/// <summary>Turns an int into a list of its digits.</summary>
/// <param name="n">The int to turn into a list.</param>
/// <returns> A list made of the digits of <c>n</c>.</returns>
let intToIntList (n: int) =
    let intToCharList (n: int) = List.ofArray ((string n).ToCharArray())
    let charListToIntList (list: char list) = list |> List.map (string >> Int32.Parse)

    n
    |> intToCharList
    |> charListToIntList

/// <summary>Checks a list of ints for adjacent duplicates.</summary>
/// <param name="list">A list of ints.</param>
/// <returns><c>true</c> if at least one pair of adjacent duplicates were found.</returns>
let listHasAdjacentDuplicates (list: int list) =
    let rec listHasAdjacentDuplicatesAux =
        function
        | n1 :: n2 :: tail -> (n1 = n2) :: listHasAdjacentDuplicatesAux (n2 :: tail)
        | _ :: tail -> listHasAdjacentDuplicatesAux tail
        | [] -> []

    list
    |> listHasAdjacentDuplicatesAux
    |> List.contains true

/// <summary>Checks if a list has at least one pair of adjacent duplicates.</summary>
/// <param name="list">The list to check</param>
/// <returns><c>true</c> if the list has at least one pair of adjacent duplicates.</returns>
let listHasAtLeastOneAdjacentDuplicatePair (list: int list) =
    let insert element state =
        match state with
        | current :: rest ->
            match current with
            | head :: _ when element = head -> (element :: current) :: rest
            | _ -> [ element ] :: state
        | _ -> [ [ element ] ]

    let listOfAdjacentIntLists = List.foldBack insert list []

    listOfAdjacentIntLists
    |> List.map (fun list -> list.Length)
    |> List.exists (fun length -> length = 2)

/// <summary>Checks a list of ints to see if the list never decreases from left to right.</summary>
/// <param name="list">A list of ints.</param>
/// <returns><c>true</c> if the list never decreases from left to right.</returns>
let listNeverDecreases (list: int list) =
    let rec listNeverDecreasesAux =
        function
        | n1 :: n2 :: tail -> (n2 >= n1) :: listNeverDecreasesAux (n2 :: tail)
        | _ :: tail -> listNeverDecreasesAux tail
        | [] -> []

    list
    |> listNeverDecreasesAux
    |> List.contains false
    |> not

/// <summary>Checks if a password is valid for Part 1. A password is valid if it has at least
/// two adjacent duplicate digits and never decreases from left to right.</summary>
/// <param name="password">The password to check.</param>
/// <returns> <c>true</c> if the password is valid.</returns>
let isValidPasswordPart1 (password: int) =
    let list = intToIntList password
    listHasAdjacentDuplicates list && listNeverDecreases list

/// <summary>Checks if a password is valid for Part 2. A password is valid if at has at least one
/// pair of duplicate digits (and larger groups of duplicate digits do not count) and never
/// decreases from left to right.</summary>
/// <param name="password"></param>
/// <returns><c>true</c> if the password is valid.</returns>
let isValidPasswordPart2 (password: int) =
    let list = intToIntList password
    listHasAtLeastOneAdjacentDuplicatePair list && listNeverDecreases list

[<EntryPoint>]
let main argv =
    let part1Result =
        [ 123257 .. 647015 ]
        |> List.map (isValidPasswordPart1)
        |> List.filter (fun (b: bool) -> b)
        |> List.length

    let part2Result =
        [ 123257 .. 647015 ]
        |> List.map (isValidPasswordPart2)
        |> List.filter (fun (b: bool) -> b)
        |> List.length

    printfn "Part 1: The number of valid passwords is %d." part1Result
    printfn "Part 2: The number of valid passwords is %d." part2Result
    0 // return an integer exit code
