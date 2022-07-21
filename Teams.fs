module Avalon.Teams
open Results
open Ops

type Player = { 
    name: string 
}

type PlayerBase = {
    players: Player list
}

let validatePopulation players =
    let population = players |> List.length
    if population >= 5 && population <= 10 then
        Ok { players = players }
    else
        Error "You need anywhere from 5 to 10 players to play!"

let calculateHowManyAreGood playerBase =
    match List.length playerBase.players with
    | 5 -> Ok 3
    | 6 -> Ok 4
    | 7 -> Ok 4
    | 8 -> Ok 5
    | 9 -> Ok 6
    | 10 -> Ok 6
    | x -> Error $"Incorrect number of players, cannot distribute: {x}"

type Character = 
    | Good
    | Evil
    | Merlin
    | TheAssassin

    override this.ToString() =
        match this with
        | Good -> "Good"
        | Evil -> "Evil"
        | Merlin -> "Merlin"
        | TheAssassin -> "The Assassin"

let determineCharacters rnd playerBase =
    resultOf {
        let! good = calculateHowManyAreGood playerBase
        let rearranged = shuffle rnd playerBase.players
        let (good, evil) = List.splitAt good rearranged
        let merlin = good.Head, Merlin
        let otherGood = good.Tail |> List.map (fun g -> g, Good)
        let assassin = evil.Head, TheAssassin
        let otherEvil = evil.Tail |> List.map (fun e -> e, Evil)
        return (merlin :: assassin :: otherGood @ otherEvil) |> Map.ofList
    }

type Team = {
    leader: Player
    voters: Set<Player>
    nextRotation: Player list
}

let makeATeam allPlayers =
    let leader, voters = allPlayers.players |> List.head, allPlayers.players |> List.tail
    {
        leader = leader
        voters = voters |> Set.ofList
        nextRotation = voters @ [leader]
    }

let rotateTeam team =
    let newLeader, formerVoters = team.nextRotation |> List.head, team.nextRotation |> List.tail
    {
        leader = newLeader
        voters = team.leader :: formerVoters |> Set.ofList
        nextRotation = formerVoters @ [newLeader]
    }


type TeamPhases =
    | GatheredPlayers of Player list
    | FormedAPlayerBase of PlayerBase
    | AssignedRoles of Map<Player, Character>
    | InitialTeam of Team