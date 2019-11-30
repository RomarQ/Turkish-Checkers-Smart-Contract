type players = (nat, address) map

type board = (nat, (nat, int) map) map

type storage = {
  players: players;
  currentPlayer: nat;
  winner: address option;
  isOver: bool;
  board: board;
}

type parameter = {
  origin_x: nat;
  origin_y: nat;
  target_x: nat;
  target_y: nat;
}

(* Gets a value from a board cell *)
let getPieceAt (x:nat) (y:nat) (board:board) =
  Map.find y (Map.find x board)

(* Virifies if the pawn is a King *)
let isDamaKing (x:nat) (y:nat) (board:board) = 
  if getPieceAt x y board < 0 then true else false

(* Verifies if the operation sender is the currentPlayer *)
let isCurrentPlayer (sender:address) (currentPlayer:nat) (players:players) = 
  sender = Map.find currentPlayer players

(* Verifies if the move is in the board range (0,0) to (7,7) *)
let areCoordinatesValid (p:parameter) =
  (p.origin_x >= 0n && p.origin_x < 8n) &&
  (p.origin_y >= 0n && p.origin_y < 8n) &&
  (p.target_x >= 0n && p.target_x < 8n) &&
  (p.target_y >= 0n && p.target_y < 8n)

(* Verifies if the origin position cantains a piece owned by the currentUser *)
let isOriginValid (p:parameter) (currentPlayer:nat) (board:board) = 
  abs (getPieceAt p.origin_x p.origin_y board) = currentPlayer

(* Verifies if the target position is empty *)
let isTargetValid (p:parameter) (board:board) = 
  getPieceAt p.target_x p.target_y board = 0

(* Verifies if there are any possible Jumps to the left *)
let hasLeftJumps (x:nat) (y:nat) (board:board) (currentPlayer:nat) (isKing:bool) (state:{ isValid: bool; state:int; }) =
  Map.fold board state (fun (prev:{ isValid: bool; state:int; }) (cur:(nat * (nat, int) map)) ->
    if not prev.isValid then
      prev
    else (
      let cell = abs(Map.find y cur.1) in
      let status = (
        if (cur.0 < x) && (isKing || cur.0 >= abs(x-2n)) then
          if (prev.state > -1 && prev.state < 3) && cell = 0n then 1
          else (
            if prev.state = 1 && cell = abs(3n - currentPlayer) then 2 else -1
          )
        else prev.state
      ) in {
        isValid = status <> -1;
        state = status;
      }
    )
  )

(* Verifies if there are any possible Jumps to the right *)
let hasRightJumps (x:nat) (y:nat) (board:board) (currentPlayer:nat) (isKing:bool) (state:{ isValid: bool; state:int; }) =
  Map.fold board state (fun (prev:{ isValid:bool; state:int }) (cur:(nat * (nat, int) map)) ->
    if not prev.isValid then
      prev
    else (
      let cell = abs(Map.find y cur.1) in
      let status = (
        if cur.0 <= x || (not isKing && cur.0 > abs(x+2)) then
          prev.state
        else (
          if cell = abs(3n - currentPlayer) && prev.state = 0 then
            1
          else (
            if cell = 0n && prev.state = 1 then
              2
            else (if cell = 0n then 0 else -1)
          )
        )
      ) in {
        isValid = status <> -1;
        state = status;
      }
    )
  )

(* Verifies if there are any possible Jumps to the forward *)
let hasForwardJumps (x:nat) (y:nat) (board:board) (currentPlayer:nat) (isKing:bool) (state:{ isValid: bool; state:int; }) =
  Map.fold (Map.find x board) state (fun (prev:{ isValid:bool; state:int }) (cur:(nat * int)) ->
    if prev.isValid then
      prev
    else (
      let cell = abs(cur.1) in
      let status = (
        if currentPlayer = 1n then
          if cur.0 > y && (cur.0 <= abs(y+2) || isKing) then
            prev.state
          else (
            if cell = abs(3n - currentPlayer) && prev.state = 0 then
              1
            else (
              if cell = 0n && prev.state = 1 then
                2
              else (if cell = 0n then 0 else -1)
            )
          )
        else (
          if cur.0 < y && (cur.0 <= abs(y-2n) || isKing) then
            prev.state
          else (
            if cell = abs(3n - currentPlayer) && prev.state = 0 then
              1
            else (
              if cell = 0n && prev.state = 1 then
                2
              else (if cell = 0n then 0 else -1)
            )
          )
        )
      ) in {
        isValid = status <> -1;
        state = status;
      }
    )
  )

(* Verifies if there are any possible Jumps to the backward *)
let hasBackwardJumps (x:nat) (y:nat) (board:board) (currentPlayer:nat) (isKing:bool) (state:{ isValid: bool; state:int; }) = 
  Map.fold (Map.find x board) state (fun (prev:{ isValid:bool; state:int }) (cur:(nat * int)) ->
    prev
  )
    
(* Verifies if are any possibles Jumps from a given board cell *)
let hasJumps (x:nat) (y:nat) (board:board) (currentPlayer:nat) : bool =
  let state: { isValid: bool; state:int; } = {
    isValid = true;
    state = 0;
  } in
  let isKing = isDamaKing x y board in
  let leftState = hasLeftJumps x y board currentPlayer isKing state in
  let rightState = (
    if not leftState.isValid then
      leftState
    else
      hasRightJumps x y board currentPlayer isKing leftState
  ) in
  let forwardState = (
    if not rightState.isValid then 
      rightState
    else
      hasForwardJumps x y board currentPlayer isKing rightState
  ) in
  let backwardState = (
    if not forwardState.isValid || not isKing then
      forwardState
    else    
      hasBackwardJumps x y board currentPlayer isKing forwardState
  ) in backwardState.isValid

(* Verifies if the pawn is moving in the right direction (Forward, Left or Right) *)
let isPawnMoveValid (p:parameter) (currentPlayer:nat) = false

let playerMove(p:parameter) (s:storage) : bool =
  let _ = 
    if s.isOver then failwith "The game is over!" in
  let _ = 
    if not isCurrentPlayer Current.sender s.currentPlayer s.players 
    then failwith "You are not the current player!" in
  let _ = 
    if not areCoordinatesValid p
    then failwith "Move should be in board range (0,0) to (7,7)." in
  let _ =
    if not isOriginValid p s.currentPlayer s.board
    then failwith "You can only move your own pieces." in
  let _ =
    if not isTargetValid p s.board
    then failwith "You can only move to empty cells." in
  let _ =
    if not isPawnMoveValid p s.currentPlayer
    then failwith "Pawns can only move forward, left or right (If not king)." in
  (* Get Deltas *)
  let deltaX = abs (p.target_x - p.origin_x) in
  let deltaY = abs (p.target_y - p.origin_y) in
  (* At least one of the deltas need to be 0 since movement is orthogonal *)
  let _ =
    if not ((deltaX = 0n) || (deltaY = 0n))
    then failwith "Movement should be orthogonal." in
  (* Check for Jumps *)
  let _ =
    if hasJumps p.origin_x p.origin_y s.board s.currentPlayer
    then failwith "No jumps available." in
  let newStorage:storage = {
    players = s.players;
    currentPlayer = s.currentPlayer;
    winner = s.winner;
    isOver = s.isOver;
    board = s.board;
  } in (([]:operation list) , s)
