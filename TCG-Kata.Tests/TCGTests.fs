module TCGTests

open TCG
open Xunit
open Swensen.Unquote

let eventsHistory = [
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
        PickHand = fun () -> 2, 4, 5
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
    let commandHandler = createCommandHandler
    let cmd = BeginGame {
        ChooseFirstPlayer = fun () -> player1
        PickCard = fun _ -> 7
    }
    
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
    let events = defaultCommandHandler.handle StartNewTurn eventsHistory
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
    let event = defaultCommandHandler.handle EndTurn (eventsHistory @ [
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
    test <@ game.Player1 = { Mana = 0; Health = 30; Deck = [0;0;1;1;2;2;2;3;3;3;3;4;4;4;5;5;6;6;7;8]; Hand = [] } @>
    test <@ game.Player2 = { Mana = 0; Health = 30; Deck = [0;0;1;1;2;2;2;3;3;3;3;4;4;4;5;5;6;6;7;8]; Hand = [] } @>

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
    test <@ game.Player1 = { Mana = 0; Health = 30; Deck = [0;0;1;1;2;2;3;3;3;3;4;4;5;6;6;7;8]; Hand = [2;4;5] } @>
    test <@ game.Player2 = { Mana = 0; Health = 30; Deck = [0;0;2;2;2;3;3;3;4;4;4;5;5;6;6;7;8]; Hand = [3;1;1] } @>
    
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
    test <@ true = false @>
    