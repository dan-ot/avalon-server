open Avalon.Results
open Avalon.Game

let randomized = System.Random.Shared.Next

let createGenericPlayers count =
    seq { for p in 1 .. count do  $"Player {p}" }
    |> Seq.map (fun p -> { name = p })
    |> List.ofSeq

let renderPlayer (p: Player) = p.name
let describeParticipant (p, c) = $"{renderPlayer p} is {c}"
let describeQuestFate fate =
    match fate with
    | QuestSuccess -> "succeed"
    | QuestFailure -> "fail"

let rec keepChoosing from =
    match from |> extractOne randomized with
    | Ok (picked, _) -> picked
    | Error e ->
        stdout.WriteLine e
        keepChoosing from

let joined (ss: string seq) =
    System.String.Join (", ", ss)

type QuestSettleResult =
    | Settle of Quest
    | EvilVictory

type FinalVictory =
    | Good
    | Evil

[<EntryPoint>]
let main args =
    let exit = resultOf {
        let! playerCount = 
            match Array.tryHead args with
            | Some pc -> 
                match System.Int32.TryParse pc with
                | (true, p) -> Ok p
                | _ -> Error $"Didn't recognize first argument [{pc}] as a number"
            | None -> Error "Require the first argument to be the number of players"
        let! players = 
            createGenericPlayers playerCount
            |> shuffle randomized
            |> validatePopulation
        let! participants = players |> determineCharacters randomized
        
        let describeSpecificParticipant p =
            match participants.TryFind p with
            | Some c -> $"{renderPlayer p} ({c})"
            | None -> $"No such player {renderPlayer p}"

        let questFateForThesePlayers = determineQuestFate players

        let rec pickAQuestFate from =
            let r = resultOf {
                let! (player, _) = from |> extractOne randomized
                let! character = 
                    participants 
                    |> Map.tryFind player
                    |> fun found ->
                        match found with
                        | Some ch -> Ok ch
                        | None -> Error $"Player {renderPlayer player} wasn't found!"
                let! (result, _) = 
                    presentPossibleResults character
                    |> extractOne randomized
                printfn $"{describeSpecificParticipant player} decides they will {describeQuestFate result}!"
                return (player, result)
            }
            match r with
            | Ok results -> results
            | Error e ->
                stdout.WriteLine e
                pickAQuestFate from

        stdout.WriteLine ""
        stdout.WriteLine "=========="
        stdout.WriteLine (participants |> Map.toList |> List.map describeParticipant |> fun ss -> System.String.Join (", ", ss))
        printfn $"The turn order is {(makeATeam players).nextRotation |> List.map renderPlayer |> joined}"
        stdout.WriteLine "=========="
        stdout.WriteLine ""

        let rec randomVotes quest team votingRecord =
            let whoHasVoted =
                votingRecord
                |> Map.keys
                |> Set.ofSeq
            let whoHasntVoted =
                whoHasVoted
                |> Set.difference team.voters
            match whoHasntVoted |> Set.toList with
            | [p] ->
                printfn $"{describeSpecificParticipant p} casts their ballot... "
                match randomized 3 with
                | 0 -> (p, QuestYea)
                | _ -> (p, QuestNay)
            | ps ->
                match ps |> extractOne randomized with
                | Ok (voter, _) ->
                    printf $"{describeSpecificParticipant voter} casts their ballot... "
                    match randomized 3 with
                    | 0 -> (voter, QuestYea)
                    | _ -> (voter, QuestNay)
                | Error e ->
                    stdout.WriteLine e
                    randomVotes quest team votingRecord


        let proposeAQuestRandomly = proposeAQuest keepChoosing

        let nextQuestTemplateForThesePlayers = nextQuestTemplateFrom players

        let rec questUntilVictory questHistory =
            let gameResult = resultOf {
                let questTemplate = nextQuestTemplateForThesePlayers questHistory
                
                let reportAcceptedVotes voteKind votes =
                    votes
                    |> Map.toSeq
                    |> Seq.filter (fun (_, v) -> v = voteKind)
                    |> Seq.map (fun (p, _) -> describeSpecificParticipant p)
                    |> fun ss -> System.String.Join (", ", ss)
                
                let rec settleOnThisQuestTemplateOrLose currentTeam priorResults =
                    // This will turn a template into a quest, but that means the 2-persion Mission may change names
                    // for each leader. This seems narratively appropriate.
                    let quest = proposeAQuestRandomly questTemplate currentTeam
                    let randomVotesOnThisQuest = randomVotes quest
                    let randomVotesFromThisTeam = randomVotesOnThisQuest currentTeam
                    let voteOnThisQuestForThisTeam = voteOnAQuest randomVotesFromThisTeam
                    let proposeAQuestForThisTeam template = proposeAQuestRandomly template currentTeam

                    stdout.WriteLine ""
                    stdout.WriteLine "TTTTTTTTTTT"
                    printf $"{quest.proposedBy |> describeSpecificParticipant} proposes a Quest: {quest.name}! "
                    printfn $"The following shall go: {quest.members |> List.map describeSpecificParticipant |> joined}"
                    stdout.WriteLine ""

                    let results =
                        voteOnAQuest
                            randomVotesFromThisTeam
                            currentTeam
                            priorResults

                    stdout.WriteLine ""

                    match results with
                    | NoVotesYet -> Error "Nobody even voted!"
                    | QuestVoteResults.EvilVictory -> Ok EvilVictory
                    | Accepted (votes, times) ->
                        printfn $"""{quest.name} was accepted after {times} votes due to the Yea votes of {reportAcceptedVotes QuestYea votes}"""
                        Ok (Settle quest)
                    | Rejected (votes, times) ->
                        printf $"""{quest.name} was rejected due to the Nay votes of {reportAcceptedVotes QuestNay votes}. """
                        printfn $"{5 - times} more rejected votes will cause Evil to triumph!"
                        let newTeam = rotateTeam currentTeam
                        settleOnThisQuestTemplateOrLose newTeam results

                let initialTeam = makeATeam players
                let! settleQuest = settleOnThisQuestTemplateOrLose initialTeam NoVotesYet

                stdout.WriteLine ""
                stdout.WriteLine "QQQQQQQQQQQ"
                let! newQuestHistory = 
                    match settleQuest with
                    | EvilVictory ->
                        Error "Evil has triumphed through the inaction of good!"
                    | Settle acceptedQuest ->
                        let questFate = 
                            questFateForThesePlayers
                                pickAQuestFate
                                questHistory
                                acceptedQuest
                        printfn $"{questFate.quest.name} {describeQuestFate questFate.result}ed!"
                        Ok (questFate :: questHistory)
                stdout.WriteLine ""

                return determineGameResult newQuestHistory
            }
            match gameResult with
            | Ok GoodVictory -> Good
            | Ok GameResult.EvilVictory -> Evil
            | Ok (StillPlaying newHistory) ->
                questUntilVictory newHistory
            | Error e ->
                stdout.WriteLine e
                questUntilVictory questHistory
        
        stdout.WriteLine ""
        match questUntilVictory List.empty with
        | Good ->
            printfn "All quests were successful. BUT!"
            let (assassin, goodGuys) = prepareAssassination participants
            printfn $"{renderPlayer assassin} the Assassin has one last chance! If they can pick Merlin out from the Good lineup ({goodGuys |> Seq.map (fun (g, _) -> renderPlayer g) |> joined}), Evil may yet win..."
            
            let rec pickATarget () =
                match extractOne randomized goodGuys with
                | Ok (pick, _) -> pick
                | Error e ->
                    stdout.WriteLine e
                    pickATarget ()

            let (targetPlayer, targetCharacter) = pickATarget ()
            printfn $"{renderPlayer assassin} has chosen {renderPlayer targetPlayer}!"
            match targetCharacter with
            | Merlin ->
                printfn "Who was Merlin! With Merlin dead, Evil has triumphed! Arther's reign has ended, and Britain will remain a monarchy forever!"
            | _ ->
                printfn "Who was NOT Merlin! Good has triumphed! The blessed reign of Camelot will continue for a whole two years!"
        | Evil ->
            printfn "Evil has triumphed! Arthur's reign has ended, and Britain will remain a monarchy forever!"
    }
    match exit with
        | Ok _ -> 0
        | Error e ->
            printf $"Error: %s{e}"
            -1