module TCG

type PlayerChosen = Player1 | Player2

type Evt =
    | GameCreated
    | FirstPlayerChosen of PlayerChosen

type Card = int
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

let hydrate (events: Evt list) : Game =  
    
    let handleEvent state evt =
        match evt with
        | FirstPlayerChosen player -> { state with Current = Some player }
        | _ -> state
    
    let GameCreated::tail = events
    tail |> List.fold handleEvent {
        Player1 = { Deck = [0;0;1;1;2;2;2;3;3;3;3;4;4;4;5;5;6;6;7;8]; Hand = []; Mana = 0; Health = 30 }
        Player2 = { Deck = [0;0;1;1;2;2;2;3;3;3;3;4;4;4;5;5;6;6;7;8]; Hand = []; Mana = 0; Health = 30 }
        Current = None
    } 
