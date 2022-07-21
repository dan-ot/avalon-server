module Avalon.Game

open Avalon.Results

/// A picker is given the highest number possible and picks a number from 0 to one less than that number.
type picker = (int) -> int

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

let shuffle (rnd: picker) fs =
    match fs with
    | [] -> []
    | [a] -> [a]
    | fs ->
        List.fold (fun shuffled thisOne ->
        let i = rnd (List.length shuffled)
        List.insertAt i thisOne shuffled
        ) [] fs

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

type VoteOnQuest =
    | QuestYea
    | QuestNay

type VotingRecord = Map<Player, VoteOnQuest>

type TeamVoteResults =
    | Incomplete
    | Complete

let participateInVote who voted votingRecord =
    votingRecord
        |> Map.add who voted

let determineVoteResults team votingRecord =
    let everyoneVoted = 
        (votingRecord
            |> Map.keys
            |> Set.ofSeq 
            |> Set.difference team.voters
            |> Set.count) 
            = 0
    match everyoneVoted with
    | true -> Complete
    | false -> Incomplete

let determineOutcome votingRecord =
    let approvals, disapprovals = 
        votingRecord
            |> Map.values
            |> List.ofSeq 
            |> List.partition (fun v -> v = QuestYea)
    let a, d = List.length approvals, List.length disapprovals
    if a > d then
        QuestYea
    else
        QuestNay

type QuestTemplate = {
    population: int
}

type Quest = {
    proposedBy: Player
    members: Player list
    name: string
}

let nextQuestTemplateFrom playerBase history =
    match List.length playerBase.players with
    | 5 -> 
        match List.length history with
        | 0 -> { population = 2 }
        | 1 -> { population = 3 }
        | 2 -> { population = 2 }
        | 3 -> { population = 3 }
        | _ -> { population = 3 }
    | 6 ->
        match List.length history with
        | 0 -> { population = 2 }
        | 1 -> { population = 3 }
        | 2 -> { population = 4 }
        | 3 -> { population = 3 }
        | _ -> { population = 4 }
    | 7 ->
        match List.length history with
        | 0 -> { population = 2 }
        | 1 -> { population = 3 }
        | 2 -> { population = 3 }
        | 3 -> { population = 4 }
        | _ -> { population = 4 }
    | _ ->
        match List.length history with
        | 0 -> { population = 3 }
        | 1 -> { population = 4 }
        | 2 -> { population = 4 }
        | 3 -> { population = 5 }
        | _ -> { population = 5 }

let proposeAQuest getAQuestName getAMember template team =
    let everyone = team.nextRotation |> Set.ofList
    let rec findMembers currentMembers =
        let chosenMember =
            currentMembers
                |> Set.difference everyone
                |> getAMember
        let newMembership = Set.add chosenMember currentMembers
        match Set.count newMembership with
        | full when full = template.population -> newMembership |> Set.toList
        | _ -> findMembers newMembership
    {
        proposedBy = team.leader
        members = findMembers Set.empty
        name = getAQuestName template
    }

type QuestVotingState = {
    record: VotingRecord
    quest: Quest
    votesTaken: int
}

type QuestVoteResults =
    | NoVotesYet
    | Rejected of VotingRecord * int
    /// Once it has been Accepted it shouldn't be voted on again
    | Accepted of VotingRecord * int
    /// Once Evil is victorious it shouldn't be voted on again
    | EvilVictory


let voteUntilDone getAVote everyone votingRecord =
    let rec takeAVote againstRecord = 
        let who, voted = getAVote againstRecord
        let modifiedRecord = participateInVote who voted againstRecord
        match determineVoteResults everyone modifiedRecord with
        | Complete -> modifiedRecord
        | Incomplete -> takeAVote modifiedRecord
    takeAVote votingRecord

let voteOnAQuest getAVote team state =
    match state with
    | NoVotesYet ->
        let record = voteUntilDone getAVote team Map.empty
        match determineOutcome record with
        | QuestYea -> Accepted (record, 1)
        | QuestNay -> Rejected (record, 1)
    | Rejected (_, v) -> 
        let record = voteUntilDone getAVote team Map.empty
        let thisVoteWas = v + 1
        match determineOutcome record with
        | QuestYea -> Accepted (record, thisVoteWas)
        | QuestNay ->
            match thisVoteWas with
            | 5 -> EvilVictory
            | _ -> Rejected (record, thisVoteWas)
    | Accepted _ | EvilVictory -> state

type QuestResult =
    | QuestSuccess
    | QuestFailure

let presentPossibleResults character =
    match character with
    | Good | Merlin -> [QuestSuccess]
    | Evil | TheAssassin -> [QuestSuccess; QuestFailure]

type QuestHistory = {
    quest: Quest
    result: QuestResult
}

let tabulateQuestResult playerBase history results =
    let failuresRequired = 
        match List.length history, List.length playerBase.players with
        // The 4th quest (3 quests have passed) takes 2 failures when there are more than 6 players
        | (3, 7) | (3, 8) | (3, 9) | (3, 10) -> 2
        | _ -> 1
    let failuresPresent =
        results
        |> List.filter (fun r -> r = QuestFailure)
        |> List.length
    if failuresPresent < failuresRequired then
        QuestSuccess
    else
        QuestFailure

let determineQuestFate playerBase getAResult history quest =
    let rec getResults priorResults =
        let (whoVoted, thisResult) = getAResult (quest.members |> Seq.except (Map.keys priorResults))
        let newResults = priorResults |> Map.add whoVoted thisResult
        if Map.count newResults = List.length quest.members then
            tabulateQuestResult playerBase history (Map.values newResults |> List.ofSeq)
        else
            getResults newResults 
    {
        quest = quest
        result = getResults Map.empty
    }

type GameResult =
    | StillPlaying of QuestHistory list
    | GoodVictory
    | EvilVictory

let determineGameResult questHistory =
    if questHistory |> List.filter (fun h -> h.result = QuestSuccess) |> List.length = 3 then
        GoodVictory
    else if questHistory |> List.filter (fun h -> h.result = QuestFailure) |> List.length = 3 then
        EvilVictory
    else
        StillPlaying questHistory

let prepareAssassination participants =
    let (assassin, _) = 
        participants
        |> Map.toSeq
        |> Seq.filter (fun (_, c) -> c = TheAssassin)
        |> Seq.head
    
    let goodGuys =
        participants
        |> Map.toSeq
        |> Seq.filter (fun (_, c) -> c = Merlin || c = Good)

    (assassin, goodGuys)