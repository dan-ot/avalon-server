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
    | 5 -> 3
    | 6 -> 4
    | 7 -> 4
    | 8 -> 5
    | 9 -> 6
    | 10 -> 6
    | x -> failwith $"Incorrect number of players, cannot distribute: {x}"

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

type Participants = {
    map: Map<Player, Character>
}

let participantsFromPairs pairs =
    {
        map = Map.ofList pairs
    }

let roleFor participants isPlayer =
    participants.map
    |> Map.find isPlayer

let whoIs role participants =
    participants.map
    |> Map.findKey (fun _ c -> c = role)

let whoAre role participants =
    participants.map
    |> Map.toSeq
    |> Seq.filter (fun (_, c) -> role |> List.contains c)
    |> Seq.map (fun (p, _) -> p)

let determineCharacters rnd playerBase =
    let good = calculateHowManyAreGood playerBase
    let (good, evil) = 
        playerBase.players
        |> extractSeveral rnd good
        |> orFail
    let merlin = good.Head, Merlin
    let otherGood = good.Tail |> List.map (fun g -> g, Good)
    let assassin = evil.Head, TheAssassin
    let otherEvil = evil.Tail |> List.map (fun e -> e, Evil)
    (merlin :: assassin :: otherGood @ otherEvil) 
        |> participantsFromPairs

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
