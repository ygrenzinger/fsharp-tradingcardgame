module TCGTests

open TCG
open Xunit
open Swensen.Unquote

let player1BeginHistory = [
        GameCreated;
        HandInitiated {
            Player = Player1
            Card1 = 1
            Card2 = 4
            Card3 = 5
        };
        HandInitiated {
            Player = Player2
            Card1 = 2
            Card2 = 4
            Card3 = 5
        };
        FirstPlayerChosen Player1;
        PlayerPickedACard {
            Player = Player2
            Card = 7
        };
    ]

let defaultCommandHandler = createCommandHandler

[<Fact>]
let ``First command CreateGame`` () =
    let commandHandler = createCommandHandler
    let cmd = CreateGame {
        PickedHand = 2, 4, 5
    }
    let events = commandHandler.handle cmd []
    test <@ events = Result.Ok [
        GameCreated;
        HandInitiated {
            Player = Player1
            Card1 = 2
            Card2 = 4
            Card3 = 5
        };
        HandInitiated {
            Player = Player2
            Card1 = 2
            Card2 = 4
            Card3 = 5
        }
    ] @>
    
let ``Begin game when command BeginGame`` player1 player2 =
    let history = [
        GameCreated;
        HandInitiated {
            Player = Player1
            Card1 = 2
            Card2 = 4
            Card3 = 5
        };
        HandInitiated {
            Player = Player2
            Card1 = 2
            Card2 = 4
            Card3 = 5
        }]
    
    let cmd = BeginGame {
        FirstPlayer = player1
        PickedCard = 7
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
    let events = defaultCommandHandler.handle StartNewTurn player1BeginHistory
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
    // TODO : Same avec le player 2
    let event = defaultCommandHandler.handle EndTurn (player1BeginHistory @ [
        PlayerGotMana Player1;
        PlayerGotManaMax Player1;
        PlayerPickedACard {
            Player = Player1
            Card = 0
        }
    ])
    test <@ event = Result.Ok [
        PlayerActiveEndedTurn Player1;
    ] @>

[<Fact>]
let ``Create a game with initial state`` () =
    let game = hydrate [GameCreated]
    test <@ game.Player1 = { Mana = 0; ManaMax = 0; Health = 30; Deck = [0;0;1;1;2;2;2;3;3;3;3;4;4;4;5;5;6;6;7;8]; Hand = [] } @>
    test <@ game.Player2 = { Mana = 0; ManaMax = 0; Health = 30; Deck = [0;0;1;1;2;2;2;3;3;3;3;4;4;4;5;5;6;6;7;8]; Hand = [] } @>

[<Fact>]
let ``Choose first player as current player`` () =
    let game = hydrate [GameCreated; FirstPlayerChosen Player1]
    test <@ game.Current = Some Player1 @>
    
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
    let game = hydrate [GameCreated; handInitiatedPlayer1; handInitiatedPlayer2]
    test <@ game.Player1 = { Mana = 0; ManaMax = 0; Health = 30; Deck = [0;0;1;1;2;2;3;3;3;3;4;4;5;6;6;7;8]; Hand = [2;4;5] } @>
    test <@ game.Player2 = { Mana = 0; ManaMax = 0; Health = 30; Deck = [0;0;2;2;2;3;3;3;4;4;4;5;5;6;6;7;8]; Hand = [3;1;1] } @>
    
let isError = function
    | Ok _ -> false
    | Error _ -> true
    
[<Fact>]
let ``Impossible to draw a card which is not in the deck`` () =
    let handInitiatedPlayer1 = HandInitiated {
        Player = Player1
        Card1 = 6
        Card2 = 7
        Card3 = 8
    }
    let handInitiatedPlayer2 = HandInitiated {
        Player = Player2
        Card1 = 3
        Card2 = 1
        Card3 = 1
    }
    
    let history = [GameCreated; handInitiatedPlayer1; handInitiatedPlayer2]
    let cmd = BeginGame { 
        FirstPlayer = PlayerChosen.Player2
        PickedCard = 8
    }
    let result = defaultCommandHandler.handle cmd history
    test <@ result |> isError @>
    
[<Fact>]
let ``Impossible to play a card when not enough mana`` () =
    let history = player1BeginHistory@[
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
    let history = player1BeginHistory@[
            PlayerGotMana Player1;
            PlayerGotManaMax Player1;
            PlayerPickedACard {
                Player = Player1
                Card = 0
            }
        ]
    
    let cmd = PlayCard 1
    let result = createCommandHandler.handle cmd history
    test <@ result = Result.Ok [PlayerPlayedCard 1; PlayerLostLife Player2] @>

[<Fact>]
let ``Impossible to play a card that is not in your hand``() =
    let history = player1BeginHistory@[
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
    let history = player1BeginHistory@[
        PlayerGotMana Player1;
        PlayerGotManaMax Player1;
        PlayerPickedACard {
            Player = Player1
            Card = 1
        }
        PlayerPlayedCard 1;
        PlayerLostLife Player2;
    ]

    let cmd = PlayCard 1
    let result = createCommandHandler.handle cmd history
    test <@ result |> isError @>
