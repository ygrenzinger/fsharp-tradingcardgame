module TCGTests

open TCG
open Xunit
open Swensen.Unquote

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