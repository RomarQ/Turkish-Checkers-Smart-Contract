# Dama - A Turkish variation of Draughts (Checkers).

import smartpy as sp

class Dama(sp.Contract):
    def __init__(self, players, board, currentPlayer):
        sp.defaultVerifyMessage=sp.unit
        
        self.init(
            players = players,
            currentPlayer = currentPlayer,
            winner = -1,
            isOver = False,
            board = sp.matrix(board)
        )
    
    @sp.entryPoint
    def playerMove(self, params):
        # Player can only move if is his turn
        sp.verify(sp.sender == self.data.players[self.data.currentPlayer])
        
        # Player can only move if game is not over
        sp.verify(~self.data.isOver)
        
        # Move should be in board range (0,0) to (7,7)
        sp.verify((params.start_x >= 0) & (params.start_x < 8)) # Origin X
        sp.verify((params.start_y >= 0) & (params.start_y < 8)) # Origin Y
        sp.verify((params.target_x >= 0) & (params.target_x < 8)) # Target X
        sp.verify((params.target_y >= 0) & (params.target_y < 8)) # Target Y
        
        # Verify if origin position cantains a piece own by this used
        piece = self.data.board[params.start_x][params.start_y]
        sp.verify(abs(piece) == self.data.currentPlayer)
        
        # Target Position should be empty
        sp.verify(self.data.board[params.target_x][params.target_y] == 0)
        
        # Pawns can only move forward, left or right (If not king)
        sp.verify(((self.data.currentPlayer == 1) & (params.start_y <= params.target_y)) | ((self.data.currentPlayer == 2) & (params.start_y >= params.target_y)) | (self.data.board[params.start_x][params.start_y] < 0))
        
        # Get deltas
        deltaX = abs(params.target_x - params.start_x)
        deltaY = abs(params.target_y - params.start_y)
        # At least one of the deltas need to be 0 since movement is orthogonal
        sp.verify((deltaX == 0) | (deltaY == 0))
        
        # Helper variable for jump check
        shouldJump = sp.newLocal('shouldJump', False)
        enemyPlayer = sp.newLocal('enemyPlayer', abs(3 - sp.toInt(self.data.currentPlayer)))
        
        # Look for jumps on every position 
        sp.for x in sp.range(0, 8):
            sp.for y in sp.range(0, 8):
                sp.if abs(self.data.board[x][y]) == self.data.currentPlayer:
                    # LEFT
                    sp.if x > 1:
                        sp.for k in sp.range(0, x):
                            sp.if ~shouldJump & (self.data.board[k][y] == 0) & (abs(self.data.board[k+1][y]) == enemyPlayer) & (((self.data.board[x][y] > -1) & (k == x-2)) | (self.data.board[x][y] < 0)):
                                shouldJump.set(True)
                                sp.for kk in sp.range(k+2, x):
                                    sp.if self.data.board[kk][y] != 0:
                                        shouldJump.set(False)
                    # RIGHT
                    sp.if x < 6:
                        sp.for k in sp.range(x+1, 7):
                            sp.if ~shouldJump & (self.data.board[k+1][y] == 0) & (abs(self.data.board[k][y]) == enemyPlayer) & (((self.data.board[x][y] > -1) & (k == x+1)) | (self.data.board[x][y] < 0)):
                                shouldJump.set(True)
                                sp.for kk in sp.range(x+1, k):
                                    sp.if self.data.board[kk][y] != 0:
                                        shouldJump.set(False)
                    # TOP
                    sp.if (y < 6) & ((self.data.board[x][y] != 2) | (abs(self.data.board[x][y]) < 0)):
                        sp.for k in sp.range(y+1, 7):
                            sp.if ~shouldJump & (self.data.board[x][k+1] == 0) & (abs(self.data.board[x][k]) == enemyPlayer) & (((self.data.board[x][y] > -1) & (k == y+1)) | (self.data.board[x][y] < 0)):
                                shouldJump.set(True)
                                sp.for kk in sp.range(y+1, k):
                                    sp.if self.data.board[x][kk] != 0:
                                        shouldJump.set(False)
                    # BOTTOM
                    sp.if (y > 1) & ((self.data.board[x][y] != 1) | (abs(self.data.board[x][y]) < 0)):
                        sp.for k in sp.range(0, y):
                            sp.if ~shouldJump & (self.data.board[x][k] == 0) & (abs(self.data.board[x][k+1]) == enemyPlayer) & (((self.data.board[x][y] > -1) & (k == y-2)) | (self.data.board[x][y] < 0)):
                                shouldJump.set(True)
                                sp.for kk in sp.range(k+2, y):
                                    sp.if self.data.board[x][kk] != 0:
                                        shouldJump.set(False)
   
        sp.if shouldJump:
            # Obligated to jump
            # Can only jump 2 positions
            sp.verify(((deltaY < 3) & (deltaX < 3)) | (piece < 0))
        sp.else:
            # No Jumps available
            # Can only move 1 position
            sp.verify(((deltaY < 2) & (deltaX < 2)) | (piece < 0))
        
        # Move
        self.move(params.start_x, params.start_y, params.target_x, params.target_y)
        # Jump
        sp.if shouldJump:
            # Obligated to jump
            sp.if deltaX == 0:
                sp.if params.start_y < params.target_y:
                    sp.verify(abs(self.data.board[params.target_x][params.target_y - 1]) != self.data.currentPlayer)
                    sp.verify(abs(self.data.board[params.target_x][params.target_y - 1]) != 0)
                    self.data.board[params.target_x][params.target_y - 1] = 0
                sp.else:
                    sp.verify(abs(self.data.board[params.target_x][params.target_y + 1]) != self.data.currentPlayer)
                    sp.verify(abs(self.data.board[params.target_x][params.target_y + 1]) != 0)
                    self.data.board[params.target_x][params.target_y + 1] = 0
            sp.else:
                sp.if params.target_x > params.start_x:
                    sp.verify(abs(self.data.board[params.target_x - 1][params.target_y]) != self.data.currentPlayer)
                    sp.verify(abs(self.data.board[params.target_x - 1][params.target_y]) != 0)
                    self.data.board[params.target_x - 1][params.target_y] = 0
                sp.else:
                    sp.verify(abs(self.data.board[params.target_x + 1][params.target_y]) != self.data.currentPlayer)
                    sp.verify(abs(self.data.board[params.target_x + 1][params.target_y]) != 0)
                    self.data.board[params.target_x + 1][params.target_y] = 0
        
        # Become King
        sp.if (((params.target_y == 7) & (self.data.currentPlayer == 1)) | ((params.target_y == 0) & (self.data.currentPlayer == 2))) & (self.data.board[params.target_x][params.target_y] > 0):
            self.data.board[params.target_x][params.target_y] = -self.data.board[params.target_x][params.target_y]

        
        # Change Next Player
        continueMove = sp.newLocal('continueMove', False)
        # LEFT
        sp.if params.target_x > 1:
            sp.for k in sp.range(0, params.target_x):
                sp.if (~continueMove) & (self.data.board[k][params.target_y] == 0) & (abs(self.data.board[k+1][params.target_y]) == enemyPlayer) & (((self.data.board[params.target_x][params.target_y] > -1) & (k == params.target_x-2)) | (self.data.board[params.target_x][params.target_y] < 0)):
                    continueMove.set(True)
                    sp.for kk in sp.range(k+2, params.target_x):
                        sp.if self.data.board[kk][params.target_y] != 0:
                            continueMove.set(False)
        # RIGHT
        sp.if params.target_x < 6:
            sp.for k in sp.range(params.target_x, 7):
                sp.if (~continueMove) & (self.data.board[k][params.target_y] == 0) & (abs(self.data.board[k-1][params.target_y]) == enemyPlayer) & (((self.data.board[params.target_x][params.target_y] > -1) & (k == params.target_y+2)) | (self.data.board[params.target_x][params.target_y] < 0)):
                    continueMove.set(True)
                    sp.for kk in sp.range(params.target_x, k):
                        sp.if self.data.board[kk][params.target_y] != 0:
                            continueMove.set(False)
        # TOP
        sp.if params.target_y < 6:
            sp.for k in sp.range(params.target_y+1, 7):
                sp.if (~continueMove) & (self.data.board[params.target_x][k+1] == 0) & (abs(self.data.board[params.target_x][k]) == enemyPlayer) & (((self.data.board[params.target_x][params.target_y] > -1) & (k == params.target_y+1)) | (self.data.board[params.target_x][params.target_y] < 0)):
                    continueMove.set(True)
                    sp.for kk in sp.range(params.target_y+1, k):
                        sp.if self.data.board[params.target_x][kk] != 0:
                            continueMove.set(False)
        # BOTTOM
        sp.if params.target_y > 1:
            sp.for k in sp.range(0, params.target_y):
                sp.if (~continueMove) & (self.data.board[params.target_x][k] == 0) & (abs(self.data.board[params.target_x][k+1]) == enemyPlayer) & (((self.data.board[params.target_x][params.target_y] > -1) & (k == params.target_y-2)) | (self.data.board[params.target_x][params.target_y] < 0)):
                    continueMove.set(True)
                    sp.for kk in sp.range(k+2, params.target_y):
                        sp.if self.data.board[params.target_x][kk] != 0:
                            continueMove.set(False)
           
        sp.if ~(continueMove & shouldJump):                     
            sp.if self.data.currentPlayer == 1:
                self.data.currentPlayer = 2
            sp.else:
                self.data.currentPlayer = 1
            
        # Verify Game State
        player1Damas = sp.newLocal('player1Damas', 0)
        player1DamasKing = sp.newLocal('player1DamasKing', 0)
        player2Damas = sp.newLocal('player2Damas', 0)
        player2DamasKing = sp.newLocal('player2DamasKing', 0)

        sp.for x in sp.range(0, 8):
            sp.for y in sp.range(0, 8):
                sp.if self.data.board[x][y] == 1:
                    player1Damas.set(player1Damas + 1)
                    
                sp.if self.data.board[x][y] == -1:
                    player1DamasKing.set(player1DamasKing + 1)
                    
                sp.if self.data.board[x][y] == 2:
                    player2Damas.set(player2Damas + 1)
                
                sp.if self.data.board[x][y] == -2:
                    player2DamasKing.set(player2DamasKing + 1)
                    
        
        sp.if (player1Damas == 0) & (player1DamasKing == 0):
            self.data.isOver = True
            self.data.winner = 2
        
        sp.if (player2Damas == 0) & (player2DamasKing == 0):
            self.data.isOver = True
            self.data.winner = 1
            
        sp.if (player1Damas == 1) & (player1DamasKing == 0) & (player2DamasKing > 0):
            self.data.isOver = True
            self.data.winner = 2
            
        sp.if (player2Damas == 1) & (player2DamasKing == 0) & (player1DamasKing > 0):
            self.data.isOver = True
            self.data.winner = 1
        
    
    # Helpers
    def move(self, start_x, start_y, target_x, target_y):
        self.data.board[target_x][target_y] = self.data.board[start_x][start_y]
        self.data.board[start_x][start_y] = 0

# Test
@addTest(name = "Test-Black-Dama-Sequencial-Jumps-Left")
def test():
    
    scenario = sp.testScenario()
    
    playersMap = {}
    playersMap[0] = sp.address("tz1Kz6VSEPNnKPiNvhyio6E1otbSdDhVD9qB")
    playersMap[1] = sp.address("tz1f8Ybid6x2pMvJGauz3Zi8enkABSSkGPn8")
    
    currentPlayer = 1
    board = [
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,0,2,0],
        [0,1,1,0,2,0,2,0],
        [0,1,1,0,0,0,2,0],
        [0,1,1,0,2,0,2,0],
        [0,1,1,0,1,0,2,0]
    ]
    
    # Contract
    c = Dama(playersMap, board, currentPlayer)
    scenario.register(c, show = True)
    
    scenario += c.playerMove(start_x=7, start_y=4, target_x=5, target_y=4).run(sender=playersMap[1], valid = True)
    # Assert state
    scenario.verify((c.data.currentPlayer == 1) & (c.data.board == sp.matrix([
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,0,2,0],
        [0,1,1,0,2,0,2,0],
        [0,1,1,0,1,0,2,0],
        [0,1,1,0,0,0,2,0],
        [0,1,1,0,0,0,2,0]
    ])))

# Test
@addTest(name = "Test-Move-Black-King-Dama-Backwards")
def test():
    
    scenario = sp.testScenario()
    
    playersMap = {}
    playersMap[0] = sp.address("tz1Kz6VSEPNnKPiNvhyio6E1otbSdDhVD9qB")
    playersMap[1] = sp.address("tz1f8Ybid6x2pMvJGauz3Zi8enkABSSkGPn8")
    
    currentPlayer = 1
    board = [
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,2,0,2,-1]
    ]
    
    # Contract
    c = Dama(playersMap, board, currentPlayer)
    scenario.register(c, show = True)
    
    scenario += c.playerMove(start_x=7, start_y=7, target_x=7, target_y=5).run(sender=playersMap[1], valid = True)
    # Assert state
    scenario.verify((c.data.currentPlayer == 1) & (c.data.board == sp.matrix([
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,2,-1,0,0]
    ])))
      

# Test
@addTest(name = "Test-Move-Black-Dama")
def test():
    
    scenario = sp.testScenario()
    
    playersMap = {}
    playersMap[0] = sp.address("tz1Kz6VSEPNnKPiNvhyio6E1otbSdDhVD9qB")
    playersMap[1] = sp.address("tz1f8Ybid6x2pMvJGauz3Zi8enkABSSkGPn8")
    
    currentPlayer = 1
    board = [
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0]
    ]
    
    # Contract
    c = Dama(playersMap, board, currentPlayer)
    scenario.register(c, show = True)
    
    scenario += c.playerMove(start_x=0, start_y=2, target_x=0, target_y=3).run(sender=playersMap[currentPlayer], valid = True)
      
# Test
@addTest(name = "Test-Black-Dama-Sequencial-Jumps")
def test():
    
    scenario = sp.testScenario()
    
    playersMap = {}
    playersMap[1] = sp.address("tz1Kz6VSEPNnKPiNvhyio6E1otbSdDhVD9qB")
    playersMap[2] = sp.address("tz1f8Ybid6x2pMvJGauz3Zi8enkABSSkGPn8")
    
    currentPlayer = 1
    board = [
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,2,0,2,0,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0]
    ]
    
    # Contract
    c = Dama(playersMap, board, currentPlayer)
    scenario.register(c, show = True)
    
    scenario += c.playerMove(start_x=2, start_y=2, target_x=2, target_y=4).run(sender=playersMap[1], valid = True)
    # Assert state
    scenario.verify((c.data.currentPlayer == 1) & (c.data.board == sp.matrix([
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,0,0,1,2,0,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0]
    ])))
    
    scenario += c.playerMove(start_x=2, start_y=4, target_x=2, target_y=6).run(sender=playersMap[1], valid = True)
    # Assert state
    scenario.verify((c.data.currentPlayer == 2) & (c.data.board == sp.matrix([
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,0,0,0,0,1,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0]
    ])))
    
# Test
@addTest(name = "Test-White-Dama-Sequencial-Jumps")
def test():
    
    scenario = sp.testScenario()
    
    playersMap = {}
    playersMap[1] = sp.address("tz1Kz6VSEPNnKPiNvhyio6E1otbSdDhVD9qB")
    playersMap[2] = sp.address("tz1f8Ybid6x2pMvJGauz3Zi8enkABSSkGPn8")
    
    currentPlayer = 2
    board = [
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,0,1,0,1,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0]
    ]
    
    # Contract
    c = Dama(playersMap, board, currentPlayer)
    scenario.register(c, show = True)
    
    scenario += c.playerMove(start_x=2, start_y=5, target_x=2, target_y=3).run(sender=playersMap[2], valid = True)
    # Assert state
    scenario.verify((c.data.currentPlayer == 2) & (c.data.board == sp.matrix([
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,0,1,2,0,0,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0]
    ])))
    
    scenario += c.playerMove(start_x=2, start_y=3, target_x=2, target_y=1).run(sender=playersMap[2], valid = True)
    # Assert state
    scenario.verify((c.data.currentPlayer == 1) & (c.data.board == sp.matrix([
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,2,0,0,0,0,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0]
    ])))
    
# Test
@addTest(name = "Test-Black-King-Sequencial-Jumps")
def test():
    
    scenario = sp.testScenario()
    
    playersMap = {}
    playersMap[1] = sp.address("tz1Kz6VSEPNnKPiNvhyio6E1otbSdDhVD9qB")
    playersMap[2] = sp.address("tz1f8Ybid6x2pMvJGauz3Zi8enkABSSkGPn8")
    
    currentPlayer = 1
    board = [
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,-1,-2,0,0,-2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0]
    ]
    
    # Contract
    c = Dama(playersMap, board, currentPlayer)
    scenario.register(c, show = True)

    scenario += c.playerMove(start_x=2, start_y=2, target_x=2, target_y=4).run(sender=playersMap[1], valid = True)
    # Assert state
    scenario.verify((c.data.currentPlayer == 1) & (c.data.board == sp.matrix([
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,0,0,-1,0,-2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0]
    ])))
    
    scenario += c.playerMove(start_x=2, start_y=4, target_x=2, target_y=7).run(sender=playersMap[1], valid = True)
    # Assert state
    scenario.verify((c.data.currentPlayer == 2) & (c.data.board == sp.matrix([
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,0,0,0,0,0,-1],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0]
    ])))
    
# Test
@addTest(name = "Test-White-King-Sequencial-Jumps")
def test():
    
    scenario = sp.testScenario()
    
    playersMap = {}
    playersMap[1] = sp.address("tz1Kz6VSEPNnKPiNvhyio6E1otbSdDhVD9qB")
    playersMap[2] = sp.address("tz1f8Ybid6x2pMvJGauz3Zi8enkABSSkGPn8")
    
    currentPlayer = 2
    board = [
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,-1,0,0,-1,-2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0]
    ]
    
    # Contract
    c = Dama(playersMap, board, currentPlayer)
    scenario.register(c, show = True)

    scenario += c.playerMove(start_x=2, start_y=5, target_x=2, target_y=3).run(sender=playersMap[2], valid = True)
    # Assert state
    scenario.verify((c.data.currentPlayer == 2) & (c.data.board == sp.matrix([
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,-1,0,-2,0,0,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0]
    ])))
    
    scenario += c.playerMove(start_x=2, start_y=3, target_x=2, target_y=0).run(sender=playersMap[2], valid = True)
    # Assert state
    scenario.verify((c.data.currentPlayer == 1) & (c.data.board == sp.matrix([
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [-2,0,0,0,0,0,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0]
    ])))
    
# Test
@addTest(name = "Test-Black-Dama-MovingJumping-Backwards")
def test():
    
    scenario = sp.testScenario()
    
    playersMap = {}
    playersMap[1] = sp.address("tz1Kz6VSEPNnKPiNvhyio6E1otbSdDhVD9qB")
    playersMap[2] = sp.address("tz1f8Ybid6x2pMvJGauz3Zi8enkABSSkGPn8")
    
    currentPlayer = 1
    board = [
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,2,1,0,1,0,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0]
    ]
    
    # Contract
    c = Dama(playersMap, board, currentPlayer)
    scenario.register(c, show = True)

    scenario += c.playerMove(start_x=2, start_y=4, target_x=2, target_y=3).run(sender=playersMap[1], valid = False)
    scenario += c.playerMove(start_x=2, start_y=2, target_x=2, target_y=0).run(sender=playersMap[1], valid = False)
    
# Test
@addTest(name = "Test-White-Dama-MovingJumping-Backwards")
def test():
    
    scenario = sp.testScenario()
    
    playersMap = {}
    playersMap[1] = sp.address("tz1Kz6VSEPNnKPiNvhyio6E1otbSdDhVD9qB")
    playersMap[2] = sp.address("tz1f8Ybid6x2pMvJGauz3Zi8enkABSSkGPn8")
    
    currentPlayer = 2
    board = [
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,0,2,0,2,1,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0]
    ]
    
    # Contract
    c = Dama(playersMap, board, currentPlayer)
    scenario.register(c, show = True)

    scenario += c.playerMove(start_x=2, start_y=3, target_x=2, target_y=4).run(sender=playersMap[currentPlayer], valid = False)
    
    scenario += c.playerMove(start_x=2, start_y=5, target_x=2, target_y=7).run(sender=playersMap[currentPlayer], valid = False)
    
# Test
@addTest(name = "Test-Dama-Moving-Left-and-Right")
def test():
    
    scenario = sp.testScenario()
    
    playersMap = {}
    playersMap[1] = sp.address("tz1Kz6VSEPNnKPiNvhyio6E1otbSdDhVD9qB")
    playersMap[2] = sp.address("tz1f8Ybid6x2pMvJGauz3Zi8enkABSSkGPn8")
    
    currentPlayer = 2
    board = [
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,0,2,0,2,1,0],
        [0,1,0,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0]
    ]
    
    # Contract
    c = Dama(playersMap, board, currentPlayer)
    scenario.register(c, show = True)

    scenario += c.playerMove(start_x=2, start_y=3, target_x=3, target_y=3).run(sender=playersMap[currentPlayer], valid = True)
    # Assert state
    scenario.verify((c.data.currentPlayer == 1) & (c.data.board == sp.matrix([
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,0,0,0,2,1,0],
        [0,1,0,2,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0]
    ])))
    
    scenario += c.playerMove(start_x=1, start_y=2, target_x=2, target_y=2).run(sender=playersMap[1], valid = True)
    # Assert state
    scenario.verify((c.data.currentPlayer == currentPlayer) & (c.data.board == sp.matrix([
        [0,1,1,0,0,2,2,0],
        [0,1,0,0,0,2,2,0],
        [0,1,1,0,0,2,1,0],
        [0,1,0,2,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0]
    ])))
    
# Test
@addTest(name = "Test-Dama-Jumping-Left-Right")
def test():
    
    scenario = sp.testScenario()
    
    playersMap = {}
    playersMap[1] = sp.address("tz1Kz6VSEPNnKPiNvhyio6E1otbSdDhVD9qB")
    playersMap[2] = sp.address("tz1f8Ybid6x2pMvJGauz3Zi8enkABSSkGPn8")
    
    currentPlayer = 1
    board = [
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,0,2,2,0,2,0],
        [0,1,0,1,1,0,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0]
    ]
    
    # Contract
    c = Dama(playersMap, board, currentPlayer)
    scenario.register(c, show = True)

    scenario += c.playerMove(start_x=3, start_y=3, target_x=1, target_y=3).run(sender=playersMap[currentPlayer], valid = True)
    # Assert state
    scenario.verify((c.data.currentPlayer == 2) & (c.data.board == sp.matrix([
        [0,1,1,0,0,2,2,0],
        [0,1,1,1,0,2,2,0],
        [0,1,0,0,2,0,2,0],
        [0,1,0,0,1,0,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0]
    ])))
    
    scenario += c.playerMove(start_x=2, start_y=4, target_x=4, target_y=4).run(sender=playersMap[2], valid = True)
    # Assert state
    scenario.verify((c.data.currentPlayer == currentPlayer) & (c.data.board == sp.matrix([
        [0,1,1,0,0,2,2,0],
        [0,1,1,1,0,2,2,0],
        [0,1,0,0,0,0,2,0],
        [0,1,0,0,0,0,2,0],
        [0,1,1,0,2,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0]
    ])))
    
# Test
@addTest(name = "Test-King-Moving-Left-and-Right")
def test():
    
    scenario = sp.testScenario()
    
    playersMap = {}
    playersMap[1] = sp.address("tz1Kz6VSEPNnKPiNvhyio6E1otbSdDhVD9qB")
    playersMap[2] = sp.address("tz1f8Ybid6x2pMvJGauz3Zi8enkABSSkGPn8")
    
    currentPlayer = 2
    board = [
        [0,1,-1,0,0,2,2,0],
        [0,1,0,0,0,2,2,0],
        [0,1,1,0,-2,0,2,0],
        [0,1,0,0,0,2,2,0],
        [0,1,0,0,0,2,2,0],
        [0,1,1,-1,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0]
    ]
    
    # Contract
    c = Dama(playersMap, board, currentPlayer)
    scenario.register(c, show = True)

    scenario += c.playerMove(start_x=2, start_y=4, target_x=7, target_y=4).run(sender=playersMap[currentPlayer], valid = True)
    # Assert state
    scenario.verify((c.data.currentPlayer == 1) & (c.data.board == sp.matrix([
        [0,1,-1,0,0,2,2,0],
        [0,1,0,0,0,2,2,0],
        [0,1,1,0,0,0,2,0],
        [0,1,0,0,0,2,2,0],
        [0,1,0,0,0,2,2,0],
        [0,1,1,-1,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,-2,2,2,0]
    ])))
    
    scenario += c.playerMove(start_x=5, start_y=3, target_x=0, target_y=3).run(sender=playersMap[1], valid = True)
    # Assert state
    scenario.verify((c.data.currentPlayer == currentPlayer) & (c.data.board == sp.matrix([
        [0,1,-1,-1,0,2,2,0],
        [0,1,0,0,0,2,2,0],
        [0,1,1,0,0,0,2,0],
        [0,1,0,0,0,2,2,0],
        [0,1,0,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,0,2,2,0],
        [0,1,1,0,-2,2,2,0]
    ])))
    
# Test
@addTest(name = "Test-Game-Over")
def test():
    
    scenario = sp.testScenario()
    
    playersMap = {}
    playersMap[1] = sp.address("tz1Kz6VSEPNnKPiNvhyio6E1otbSdDhVD9qB")
    playersMap[2] = sp.address("tz1f8Ybid6x2pMvJGauz3Zi8enkABSSkGPn8")
    
    currentPlayer = 2
    board = [
        [0,0,0,0,0,0,2,0],
        [0,0,0,0,0,0,2,0],
        [0,0,0,0,0,0,2,0],
        [0,0,0,0,0,0,2,0],
        [0,0,0,0,0,0,2,0],
        [0,-1,0,0,0,0,-2,0],
        [0,0,0,0,0,0,2,0],
        [0,1,0,0,0,0,2,0],
    ]
    
    # Contract
    c = Dama(playersMap, board, currentPlayer)
    scenario.register(c, show = True)

    scenario += c.playerMove(start_x=5, start_y=6, target_x=5, target_y=0).run(sender= playersMap[currentPlayer], valid = True)
    # Assert state
    scenario.verify((c.data.isOver == True) & (c.data.winner == currentPlayer))