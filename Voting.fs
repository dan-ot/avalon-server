module Avalon.Voting
open Teams

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

let voteUntilDone getAVote everyone votingRecord =
    let rec takeAVote againstRecord = 
        let who, voted = getAVote againstRecord
        let modifiedRecord = participateInVote who voted againstRecord
        match determineVoteResults everyone modifiedRecord with
        | Complete -> modifiedRecord
        | Incomplete -> takeAVote modifiedRecord
    takeAVote votingRecord
