module TCG

// https://github.com/bkimminich/kata-tcg

type Card = int
type Player = Player1 | Player2


type CreateGame = {
    DeckPlayer1 : Card list
    DeckPlayer2 : Card list
}

type BeginGame = {
    FirstPlayer: Player // Random
}

type Cmd =
    | CreateGame of CreateGame
    | BeginGame of BeginGame
    | StartNewTurn
    | PlayCard of Card
    | EndTurn

type PlayerPickedACard = {
    Player : Player
    Card : Card
}

type PlayerHealthReduced = { 
    Player: Player
    HealthReduced: int
}

type HandInitiated = {
    Player : Player
    Card1 : Card
    Card2 : Card
    Card3 : Card
}

type GameCreated = {
    DeckPlayer1 : Card list
    DeckPlayer2 : Card list
}

type DiscardedACard = {
    Player : Player
    Card : Card
}

type Evt =
    | GameCreated of GameCreated
    | FirstPlayerChosen of Player
    | HandInitiated of HandInitiated
    | PlayerPickedACard of PlayerPickedACard
    | PlayerGotMana of Player
    | PlayerGotManaMax of Player
    | PlayerEndedTurn of Player
    | PlayerPlayedCard of Card
    | DiscardedACard of DiscardedACard 
    | PlayerHealthReduced of PlayerHealthReduced
    | PlayerWon of Player

type PlayerState = {
    Deck : Card list
    Hand : Card list
    Mana : int
    ManaMax : int
    Health : int
}

let opponentOf = function
    | Player1 -> Player2
    | Player2 -> Player1

type GameState = {
    Player1 : PlayerState
    Player2 : PlayerState
    CurrentPlayer : Player
} with 
        
    member this.OpponentPlayer = opponentOf this.CurrentPlayer

    member this.stateOf player =
        match player with
        | Player1 -> this.Player1
        | Player2 -> this.Player2
    
    member this.CurrentPlayerState =
        this.stateOf this.CurrentPlayer
        
    member this.OpponentPlayerState =
        this.stateOf this.OpponentPlayer

type Error = {
    Message: string
}

let initialDeck = [0;0;1;1;2;2;2;3;3;3;3;4;4;4;5;5;6;6;7;8]

let hydrate (events: Evt list) : GameState =  
    
    let handleEvent state evt =

        let updatePlayerState (player: Player) (update: PlayerState -> PlayerState) = 
            match player with
            | Player1 -> { state with Player1 = update state.Player1 }
            | Player2 -> { state with Player2 = update state.Player2 }

        match evt with
        | GameCreated gameCreated ->
            { state with
                Player1 = { state.Player1 with Deck = gameCreated.DeckPlayer1 }
                Player2 = { state.Player2 with Deck = gameCreated.DeckPlayer2 }
            }
            
        | FirstPlayerChosen player -> { state with CurrentPlayer = player }
        
        | HandInitiated handInitiated ->
            let pickCardFromDeck player card =
                let rec removeFirst deck =
                    match deck with
                    | [] -> []
                    | head::tail when head = card -> tail
                    | head::tail -> head::(removeFirst tail)
                
                { player with
                    Hand = player.Hand@[card]
                    Deck = removeFirst player.Deck }
            
            let pickCards player =
                [handInitiated.Card1; handInitiated.Card2; handInitiated.Card3]
                |> List.fold pickCardFromDeck player
            
            updatePlayerState handInitiated.Player pickCards
            
        | PlayerPickedACard pickedACard ->
            updatePlayerState pickedACard.Player (fun playerState ->
                { playerState with
                    Hand = playerState.Hand@(playerState.Deck |> List.take 1)
                    Deck = playerState.Deck |> List.skip 1 })
            
        | DiscardedACard discardedCard ->
            updatePlayerState discardedCard.Player (fun playerState ->
                { playerState with Deck = playerState.Deck |> List.skip 1 })

        | PlayerGotMana player ->
            updatePlayerState player (fun playerState ->
                { playerState with ManaMax = playerState.ManaMax + 1 })
            
        | PlayerGotManaMax player ->
            updatePlayerState player (fun playerState ->
                { playerState with Mana = playerState.ManaMax })

        | PlayerPlayedCard card ->
            updatePlayerState state.CurrentPlayer (fun playerState ->
                { playerState with Mana = playerState.Mana - card })

        | PlayerEndedTurn player -> { state with CurrentPlayer = opponentOf player }
                
        | PlayerHealthReduced { Player = player; HealthReduced = healthReduced } ->
            updatePlayerState player (fun playerState ->
                { playerState with Health = playerState.Health - healthReduced })
                
        | _ -> state
   
    events |> List.fold handleEvent {
        Player1 = { Deck = []; Hand = []; Mana = 0; ManaMax = 0; Health = 30 }
        Player2 = { Deck = []; Hand = []; Mana = 0; ManaMax = 0; Health = 30 }
        CurrentPlayer = Player1
    }

type CommandHandler = {
    handle : Cmd -> Evt list -> Result<Evt list, Error>
}


let private createGame (create: CreateGame) =
    let player1Card1::player1Card2::player1Card3::_ = create.DeckPlayer1
    let player2Card1::player2Card2::player2Card3::_ = create.DeckPlayer2
    
    Result.Ok [
        GameCreated {
            DeckPlayer1 = create.DeckPlayer1
            DeckPlayer2 = create.DeckPlayer2
        }
        HandInitiated {
            Player = Player1
            Card1 = player1Card1
            Card2 = player1Card2
            Card3 = player1Card3
        }
        HandInitiated {
            Player = Player2
            Card1 = player2Card1
            Card2 = player2Card2
            Card3 = player2Card3
        }]

let private beginGame (cmd: BeginGame) (state: GameState) =
    let firstPlayer = cmd.FirstPlayer
    let opponent = opponentOf firstPlayer    
    let deck = match firstPlayer with
               | Player1 -> state.Player2.Deck
               | Player2 -> state.Player1.Deck

    let card::_ = deck
    Result.Ok [
         FirstPlayerChosen firstPlayer;
         PlayerPickedACard {
             Player = opponent
             Card = card
             }
         ]


let private reduceHealth (state: GameState) target damages =
    [
        yield PlayerHealthReduced {
            Player = target
            HealthReduced = damages
        }
        if (state.stateOf target).Health <= damages then
            yield PlayerWon (opponentOf target)
    ]
    
let private startNewTurn state =
    Result.Ok [
        yield PlayerGotMana state.CurrentPlayer
        yield PlayerGotManaMax state.CurrentPlayer
        if state.CurrentPlayerState.Deck.Length = 0 then
            yield! reduceHealth state state.CurrentPlayer 1
        elif state.CurrentPlayerState.Hand.Length < 5 then
            yield PlayerPickedACard {
                Player = state.CurrentPlayer
                Card = state.CurrentPlayerState.Deck.[0]
            }
        else
            yield DiscardedACard {
                Player = state.CurrentPlayer
                Card = state.CurrentPlayerState.Deck.[0]
            }]
    
let private playCard (card: Card) (state: GameState) =
    if state.CurrentPlayerState.Hand |> List.contains card |> not
    then Result.Error { Message = "Don't have the card"}
    elif state.CurrentPlayerState.Mana < card
    then Result.Error { Message = "Not enough mana" }
    else
        Result.Ok [
            yield PlayerPlayedCard card 
            yield! reduceHealth state state.OpponentPlayer card
        ]

let apply (cmd: Cmd) (history: Evt list) : Result<Evt list, Error> =
    
    match cmd with
    | CreateGame createCmd -> createGame createCmd

    | BeginGame beginGameCmd -> 
        let state = hydrate history
        state |> beginGame beginGameCmd

    | StartNewTurn -> 
        let state = hydrate history
        state |> startNewTurn

    | PlayCard card ->
        let state = hydrate history
        state |> playCard card

    | EndTurn -> Result.Ok [PlayerEndedTurn Player1]

let createCommandHandler : CommandHandler = {
    handle = apply
}
