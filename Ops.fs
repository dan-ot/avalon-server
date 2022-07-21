module Avalon.Ops

/// A picker is given the highest number possible and picks a number from 0 to one less than that number.
type picker = (int) -> int

/// Using the provided picker, extract one of the input items and return it separate from the others.
let extractOne (rnd: picker) fs =
    match Seq.length fs with
    | 0 -> Error "Need at least one item in the list to extract one"
    | 1 -> Ok (Seq.head fs, [])
    | _ ->
        let asList = Seq.toList fs
        let choice = rnd (List.length asList)
        asList 
            |> List.indexed
            |> List.partition (fun (index, _) -> index = choice)
            |> fun (picked, rest) -> 
                let (_, h) = picked.Head
                (h, rest |> List.map (fun (_, f) -> f))
            |> Ok

/// Using the provided picker, extract several of the input items and return them separate from the remaining ones.
let extractSeveral (rnd: picker) howMany fs =
    if List.length fs < howMany then
        Error $"Not enough list items left to extract {howMany}"
    else
        fs
            |> List.fold (fun (soFar, extracted, remaining) f -> 
                if soFar = howMany then (soFar, extracted, f :: remaining)
                else
                    if rnd (List.length remaining) = 0 then
                        (soFar + 1, f :: extracted, remaining)
                    else
                        (soFar, extracted, f :: remaining)
                )
                (0, [], [])
            |> fun (_, extracted, remaining) -> (extracted, remaining)
            |> Ok

/// Using the provided picker, extract several of the input items. If there are not enough input items, all the filler
/// to determine how to populate the rest. Return the extracted and filled items, with any unextracted input items.
let extractAndFill fillExtras rnd howMany from =
    match List.length from with
    | enough when enough > howMany ->
        match extractSeveral rnd howMany from with
        | Ok extracted -> extracted
        | Error e -> failwith e
    | exactly when exactly = howMany ->
        (from, List.empty)
    | notEnough when notEnough < howMany ->
        (from @ fillExtras (howMany - notEnough), List.empty)
    | x -> failwith $"{x} blew up getting {howMany} from {from |> List.length}"

/// Using the provided picker, change the order of the input items. Note - if the picker just picks the first item (always returns 0),
/// no reordering will happen
let shuffle (rnd: picker) fs =
    match fs with
    | [] -> []
    | [a] -> [a]
    | fs ->
        List.fold (fun shuffled thisOne ->
        let i = rnd (List.length shuffled)
        List.insertAt i thisOne shuffled
        ) [] fs
