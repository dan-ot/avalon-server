open Avalon.Results
open Avalon.Ops
open Avalon.Teams
open Avalon.Voting
open Avalon.Quests
open Avalon.Game

let randomized = System.Random.Shared.Next

let extraPlayerNames = [
    "Ser Mix-A-Lot"
    "Mack the Knife"
    "Lady Blinia"
    "Robin of Locksley"
    "John Locke"
    "The King of Thieves"
    "A Hermit"
    "Lord John"
    "Dame Olivia"
    "Deuce"
    "Frank the Frank"
    "Edgar of the Scots"
    "An Empty Suit of Armor"
    "The Green Knight"
    ]

let createGenericPlayers count =
    let (chosen, _) = 
        extractAndFill
            (fun needed -> 
                seq { for p in 1 .. needed do  $"Player {p}" }
                |> List.ofSeq
            )
            randomized
            count
            extraPlayerNames
    chosen |> List.map (fun c -> { name = c })

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


let oneOf several =
    match extractOne randomized several with
    | Error e -> failwith e
    | Ok (v, _) -> v

let nameAQuest template =
    match template.population with
    | 2 -> oneOf [
        "Two Intelligent Folks to Survey an Estate"
        "Two Noble Fellows to Oppress Some Peasants"
        "Two Clever Scouts to Keep an Eye on the French"
        ]
    | 3 -> oneOf [
        "Three Best of Friends to 'Inspect' Innkeepers' Stores"
        "Three Who are Strong of Arm to Repel a Saxon Invasion"
        "Three Social Souls to Woo some Visiting Wizards"
        ]
    | 4 -> oneOf [
        "Four Deep-Lunged Worthies to Sing a Quartet"
        "Four Gentle Souls to Collect Tax from those Greedy Merchants"
        "Four Stalwart Valiants to hold Hadrian's Wall"
        ]
    | 5 -> oneOf [
        "Five Brave Souls to each Single-handedly Slay a Dragon"
        "Five Sharp Minds to Find Where Missing Socks Go"
        "Five Sturdy Fellows to Vangquish a Giant (he's over 6 feet!)"
        ]
    | _ -> "Inappropriate number of people to quest!"

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

        let reportAcceptedVotes voteKind votes =
            votes
            |> Map.toSeq
            |> Seq.filter (fun (_, v) -> v = voteKind)
            |> Seq.map (fun (p, _) -> describeSpecificParticipant p)
            |> fun ss -> System.String.Join (", ", ss)
        
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

        let proposeAQuestRandomly = proposeAQuest nameAQuest keepChoosing

        let nextQuestTemplateForThesePlayers = nextQuestTemplateFrom players

        let rec questUntilVictory questHistory questingTeam =
            let gameResult = resultOf {
                let questTemplate = nextQuestTemplateForThesePlayers questHistory
                
                let rec settleOnThisQuestTemplateOrLose votingTeam priorResults =
                    // This will turn a template into a quest, but that means the 2-persion Mission may change names
                    // for each leader. This seems narratively appropriate.
                    let quest = proposeAQuestRandomly questTemplate votingTeam
                    let randomVotesFromThisTeam = randomVotes quest votingTeam
                    let voteOnThisQuestForThisTeam = voteOnAQuest randomVotesFromThisTeam
                    let proposeAQuestForThisTeam template = proposeAQuestRandomly template votingTeam

                    stdout.WriteLine ""
                    stdout.WriteLine "TTTTTTTTTTT"
                    printf $"{quest.proposedBy |> describeSpecificParticipant} proposes a Quest: {quest.name}! "
                    printfn $"The following shall go: {quest.members |> List.map describeSpecificParticipant |> joined}"
                    stdout.WriteLine ""

                    let results =
                        voteOnAQuest
                            randomVotesFromThisTeam
                            votingTeam
                            priorResults

                    stdout.WriteLine ""

                    match results with
                    | NoVotesYet -> Error "Nobody even voted!"
                    | QuestVoteResults.EvilVictory votes-> 
                        printfn $"{quest.name} was rejected due to the Nay votes of {reportAcceptedVotes QuestNay votes}. "
                        Ok (EvilVictory, votingTeam)
                    | Accepted (votes, times) ->
                        printfn $"{quest.name} was accepted after {times} votes due to the Yea votes of {reportAcceptedVotes QuestYea votes}"
                        Ok (Settle quest, rotateTeam votingTeam)
                    | Rejected (votes, times) ->
                        printf $"{quest.name} was rejected due to the Nay votes of {reportAcceptedVotes QuestNay votes}. "
                        printfn $"{5 - times} more rejected votes will cause Evil to triumph!"
                        let newTeam = rotateTeam votingTeam
                        settleOnThisQuestTemplateOrLose newTeam results

                let! (settleQuest, newQuestingTeam) = settleOnThisQuestTemplateOrLose questingTeam NoVotesYet

                stdout.WriteLine ""
                stdout.WriteLine "QQQQQQQQQQQ"
                return 
                    match settleQuest with
                    | EvilVictory ->
                        (GameResult.EvilVictory, newQuestingTeam)
                    | Settle acceptedQuest ->
                        let questFate = 
                            questFateForThesePlayers
                                pickAQuestFate
                                questHistory
                                acceptedQuest
                        printfn $"{questFate.quest.name} {describeQuestFate questFate.result}ed!"
                        let newQuestHistory = questFate :: questHistory
                        (determineGameResult newQuestHistory, newQuestingTeam)
            }
            match gameResult with
            | Ok (GoodVictory, _) -> Good
            | Ok (GameResult.EvilVictory, _) -> Evil
            | Ok (StillPlaying newHistory, newTeam) ->
                questUntilVictory newHistory newTeam
            | Error e ->
                stdout.WriteLine e
                questUntilVictory questHistory questingTeam
        
        stdout.WriteLine ""
        match questUntilVictory List.empty (makeATeam players) with
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