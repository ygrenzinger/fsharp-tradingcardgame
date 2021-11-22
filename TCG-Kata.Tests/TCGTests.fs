module TCGTests

open TCG
open Xunit
open Swensen.Unquote

let player1BeginHistory = [
        GameCreated {
            DeckPlayer1 = [1;4;5;1]
            DeckPlayer2 = [2;4;5;7]
        }
        HandInitiated {
            Player = Player1
            Card1 = 1
            Card2 = 4
            Card3 = 5
        }
        HandInitiated {
            Player = Player2
            Card1 = 2
            Card2 = 4
            Card3 = 5
        }
        FirstPlayerChosen Player1;
        PlayerPickedACard {
            Player = Player2
            Card = 7
        };
    ]

let beginHistory deckPlayer1 deckPlayer2 = [
        GameCreated {
            DeckPlayer1 = deckPlayer1
            DeckPlayer2 = deckPlayer2
        }
        HandInitiated {
            Player = Player1
            Card1 = deckPlayer1.[0]
            Card2 = deckPlayer1.[1]
            Card3 = deckPlayer1.[2]
        }
        HandInitiated {
            Player = Player2
            Card1 = deckPlayer2.[0]
            Card2 = deckPlayer2.[1]
            Card3 = deckPlayer2.[2]
        }
        FirstPlayerChosen Player1;
        PlayerPickedACard {
            Player = Player2
            Card = deckPlayer2.[3]
        };
    ]

let defaultCommandHandler = createCommandHandler

[<Fact>]
let ``First command CreateGame`` () =
    let commandHandler = createCommandHandler
    let cmd = CreateGame {
        DeckPlayer1 = [2; 4; 5]
        DeckPlayer2 = [2; 4; 5]
    }
    let events = commandHandler.handle cmd []
    test <@ events = Result.Ok [
        GameCreated {
            DeckPlayer1 = [2; 4; 5]
            DeckPlayer2 = [2; 4; 5]
        }
        HandInitiated {
            Player = Player1
            Card1 = 2
            Card2 = 4
            Card3 = 5
        }
        HandInitiated {
            Player = Player2
            Card1 = 2
            Card2 = 4
            Card3 = 5
        }
    ] @>
    
let ``Begin game when command BeginGame`` player1 player2 =
    let deck = [2;4;5;7]
    let history = [
        GameCreated {
            DeckPlayer1 = deck
            DeckPlayer2 = deck
        }
        HandInitiated {
            Player = Player1
            Card1 = 2
            Card2 = 4
            Card3 = 5
        }
        HandInitiated {
            Player = Player2
            Card1 = 2
            Card2 = 4
            Card3 = 5
        }]
    
    let cmd = BeginGame {
        FirstPlayer = player1
    }
    
    let commandHandler = createCommandHandler
    let events = commandHandler.handle cmd history
    test <@ events = Result.Ok [
        FirstPlayerChosen player1;
        PlayerPickedACard {
            Player = player2
            Card = 7
        };
    ] @>

[<Fact>]
let ``Begin Game with Player1 as First player`` () = ``Begin game when command BeginGame`` Player1 Player2

[<Fact>]
let ``Begin Game with Player2 as First player`` () = ``Begin game when command BeginGame`` Player2 Player1

[<Fact>]
let ``Begining of the turn the active player get mana`` () =
    // TODO : Same avec le player 2
    let beginHistory = beginHistory [0;1;2;0] [0;1;2;3]
    let events = defaultCommandHandler.handle StartNewTurn beginHistory
    test <@ events = Result.Ok [
        PlayerGotMana Player1;
        PlayerGotManaMax Player1;
        PlayerPickedACard {
            Player = Player1
            Card = 0
        }
    ] @>

[<Fact>]
let ``The active player end it's turn``() =
    let beginHistory = beginHistory [0;1;2;0] [0;1;2;3]
    let event = defaultCommandHandler.handle EndTurn (beginHistory @ [
        PlayerGotMana Player1;
        PlayerGotManaMax Player1;
        PlayerPickedACard {
            Player = Player1
            Card = 0
        }
    ])
    test <@ event = Result.Ok [
        PlayerEndedTurn Player1;
    ] @>

[<Fact>]
let ``Create a game with initial state`` () =
    let game = hydrate [GameCreated {
            DeckPlayer1 = initialDeck
            DeckPlayer2 = initialDeck
        }]
    test <@ game.Player1 = { Mana = 0; ManaMax = 0; Health = 30; Deck = [0;0;1;1;2;2;2;3;3;3;3;4;4;4;5;5;6;6;7;8]; Hand = [] } @>
    test <@ game.Player2 = { Mana = 0; ManaMax = 0; Health = 30; Deck = [0;0;1;1;2;2;2;3;3;3;3;4;4;4;5;5;6;6;7;8]; Hand = [] } @>

[<Fact>]
let ``Choose first player as current player`` () =
    let game = hydrate [GameCreated {
            DeckPlayer1 = initialDeck
            DeckPlayer2 = initialDeck
        }; FirstPlayerChosen Player1]
    test <@ game.CurrentPlayer = Player1 @>
    
[<Fact>]
let ``Player should pick cards from his deck for his initial hand`` () =
    let handInitiatedPlayer1 = HandInitiated {
        Player = Player1
        Card1 = 2
        Card2 = 4
        Card3 = 5
    }
    let handInitiatedPlayer2 = HandInitiated {
        Player = Player2
        Card1 = 3
        Card2 = 1
        Card3 = 1
    }
    let game = hydrate [GameCreated {
            DeckPlayer1 = initialDeck
            DeckPlayer2 = initialDeck
        }; handInitiatedPlayer1; handInitiatedPlayer2]
    test <@ game.Player1 = { Mana = 0; ManaMax = 0; Health = 30; Deck = [0;0;1;1;2;2;3;3;3;3;4;4;5;6;6;7;8]; Hand = [2;4;5] } @>
    test <@ game.Player2 = { Mana = 0; ManaMax = 0; Health = 30; Deck = [0;0;2;2;2;3;3;3;4;4;4;5;5;6;6;7;8]; Hand = [3;1;1] } @>
    
let isError = function
    | Ok _ -> false
    | Error _ -> true
    
[<Fact>]
let ``Impossible to play a card when not enough mana`` () =
    let beginHistory = beginHistory [0;1;5;0] [0;1;2;3]
    let history = beginHistory@[
            PlayerGotMana Player1;
            PlayerGotManaMax Player1;
            PlayerPickedACard {
                Player = Player1
                Card = 0
            }
        ]
    
    let cmd = PlayCard 5
    let result = createCommandHandler.handle cmd history
    test <@ result |> isError @>
     
[<Fact>]
let ``Possible to play a card when enough mana`` () =
    let beginHistory = beginHistory [0;1;5;0] [0;1;2;3]
    let history = beginHistory@[
            PlayerGotMana Player1;
            PlayerGotManaMax Player1;
            PlayerPickedACard {
                Player = Player1
                Card = 0
            }
        ]
    
    let cmd = PlayCard 1
    let result = createCommandHandler.handle cmd history
    test <@ result = Result.Ok [
        PlayerPlayedCard 1; 
        PlayerHealthReduced {
            Player = Player2
            HealthReduced = 1
        };
        ] @>

[<Fact>]
let ``Impossible to play a card that is not in your hand``() =
    let beginHistory = beginHistory [1;1;5;6] [0;1;2;3]
    let history = beginHistory@[
        PlayerGotMana Player1;
        PlayerGotManaMax Player1;
        PlayerPickedACard {
            Player = Player1
            Card = 6
        }
    ]

    let cmd = PlayCard 0
    let result = createCommandHandler.handle cmd history
    test <@ result |> isError @>

[<Fact>]
let ``Impossible to play a card when we have not enough mana anymore``() =
    let beginHistory = beginHistory [0;1;1;0] [0;1;2;3]
    let history = beginHistory@[
        PlayerGotMana Player1;
        PlayerGotManaMax Player1;
        PlayerPickedACard {
            Player = Player1
            Card = 1
        }
        PlayerPlayedCard 1;
        PlayerHealthReduced {
            Player = Player2
            HealthReduced = 1
        };
    ]

    let cmd = PlayCard 1
    let result = createCommandHandler.handle cmd history
    test <@ result |> isError @>

[<Fact>]
let ``The next player becomes active after previous player end its turn``() =
    let beginHistory = beginHistory [0;1;5;0] [0;1;2;3;2]
    let event = defaultCommandHandler.handle StartNewTurn (beginHistory @ [
        PlayerGotMana Player1
        PlayerGotManaMax Player1
        PlayerPickedACard {
            Player = Player1
            Card = 0
        }
        PlayerEndedTurn Player1
    ])
    test <@ event = Result.Ok [
        PlayerGotMana Player2;
        PlayerGotManaMax Player2;
        PlayerPickedACard {
            Player = Player2
            Card = 2
        }
    ] @>

[<Fact>]
let ``The game ends when a player loose all its health`` () =
    let beginHistory = beginHistory [0;1;5;0] [0;1;2;3;2]
    let history =
        beginHistory@[
            PlayerGotMana Player1
            PlayerGotManaMax Player1
            PlayerPickedACard {
                Player = Player1
                Card = 0
            }
            PlayerHealthReduced {
                 Player = Player.Player2
                 HealthReduced = 29
             }]
        
    let cmd = PlayCard 1
    let result = createCommandHandler.handle cmd history
    test <@ result = Ok [
            PlayerPlayedCard 1
            PlayerHealthReduced {
                 Player = Player.Player2
                 HealthReduced = 1
            }
            PlayerWon Player.Player1 ] @>
    
[<Fact>]
let ``Next card from deck is destroyed when player has already 5 cards in hands`` () =
    let beginHistory = beginHistory [0;1;5;0;5;8;4] [0;1;2;3;2]
    let history =
        beginHistory@[
            PlayerPickedACard {
                Player = Player1
                Card = 0
            }
            PlayerPickedACard {
                Player = Player1
                Card = 5
            }]
    let cmd = StartNewTurn
    let event = createCommandHandler.handle cmd history
    test <@ event = Result.Ok [
        PlayerGotMana Player1;
        PlayerGotManaMax Player1;
        DiscardedACard {
            Player = Player1
            Card = 8
        }
    ] @>
    
[<Fact>]
let ``The discarded card cannot be picked``() =
    let beginHistory = beginHistory [0;1;5;0;3;8;4] [0;1;2;3;2]
    let history =
        beginHistory@[
            DiscardedACard {
                Player = Player1
                Card = 0
            }
        ]
    let cmd = StartNewTurn
    let event = createCommandHandler.handle cmd history
    test <@ event = Result.Ok [
        PlayerGotMana Player1;
        PlayerGotManaMax Player1;
        PlayerPickedACard {
            Player = Player1
            Card = 3
        }
    ] @>
    
[<Fact>]
let ``The discarded card cannot be played``() =
    let beginHistory = beginHistory [0;1;5;3;8;4] [0;1;2;3;2]
    let history =
        beginHistory@[
            DiscardedACard {
                Player = Player1
                Card = 3
            }
        ]
    let cmd = PlayCard 3
    let event = createCommandHandler.handle cmd history
    test <@ event = Result.Error { Message = "Don't have the card"} @>

[<Fact>]
let ``Bleeding out : active player looses health it can't draw a card``() =
    let history = beginHistory [0;1;5] [0;1;2;3]
    let cmd = StartNewTurn
    let event = createCommandHandler.handle cmd history
    test <@ event = Result.Ok [
        PlayerGotMana Player1;
        PlayerGotManaMax Player1;
        PlayerHealthReduced {
             Player = Player.Player1
             HealthReduced = 1
        }
    ] @>
 