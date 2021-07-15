module TCG

// https://github.com/bkimminich/kata-tcg

type Card = int
type PlayerChosen = Player1 | Player2

type HandInitiated = {
    Player : PlayerChosen
    Card1 : Card
    Card2 : Card
    Card3 : Card
}

type PlayerPickedACard = {
    Player : PlayerChosen
    Card : Card
}

type Cmd =
    | CreateGame
    | StartNewTurn
    | EndTurn

type Evt =
    | GameCreated
    | FirstPlayerChosen of PlayerChosen
    | HandInitiated of HandInitiated
    | PlayerPickedACard of PlayerPickedACard
    | PlayerGotMana of PlayerChosen
    | PlayerGotManaMax of PlayerChosen
    | PlayerActiveEndedTurn of PlayerChosen

type Player = {
    Deck : Card list
    Hand : Card list
    Mana : int
    Health : int
}

type Game =  {
    Player1 : Player
    Player2 : Player
    Current : PlayerChosen option
}

let pickCardFromDeck player card =
    let rec removeFirst deck =
        match deck with
        | [] -> []
        | head::tail when head = card -> tail
        | head::tail -> head::(removeFirst tail)
    
    { player with
        Hand = player.Hand@[card]
        Deck = removeFirst player.Deck }

let hydrate (events: Evt list) : Game =  
    
    let handleEvent state evt =
        match evt with
        | FirstPlayerChosen player -> { state with Current = Some player }
        | HandInitiated handInitiated ->
            let pickCards player =
                [handInitiated.Card1; handInitiated.Card2; handInitiated.Card3]
                |> List.fold pickCardFromDeck player
            
            match handInitiated.Player with
            | Player1 -> { state with Player1 = pickCards state.Player1 }
            | Player2 -> { state with Player2 = pickCards state.Player2 }
            
        | _ -> state
    
    let GameCreated::tail = events
    tail |> List.fold handleEvent {
        Player1 = { Deck = [0;0;1;1;2;2;2;3;3;3;3;4;4;4;5;5;6;6;7;8]; Hand = []; Mana = 0; Health = 30 }
        Player2 = { Deck = [0;0;1;1;2;2;2;3;3;3;3;4;4;4;5;5;6;6;7;8]; Hand = []; Mana = 0; Health = 30 }
        Current = None
    }

type CommandHandler = {
    handle : Cmd -> Evt list -> Evt list
}

let apply (user: Unit -> PlayerChosen)(cmd: Cmd)(history: Evt list) : Evt list =
    match cmd with
    | CreateGame -> [GameCreated;
        HandInitiated {
            Player = Player1
            Card1 = 2
            Card2 = 4
            Card3 = 5
        };
        HandInitiated {
            Player = Player2
            Card1 = 3
            Card2 = 1
            Card3 = 1
        };
        FirstPlayerChosen Player1;
        PlayerPickedACard {
            Player = Player2
            Card = 7
        };]
    | StartNewTurn -> [
        PlayerGotMana Player1;
        PlayerGotManaMax Player1;
        PlayerPickedACard {
            Player = Player1
            Card = 0
        }]
    | EndTurn -> [PlayerActiveEndedTurn Player1]

let createCommandHandler randomPlayer : CommandHandler = { handle = apply randomPlayer }
