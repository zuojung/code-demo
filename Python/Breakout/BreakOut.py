# Zuojun Gong + Section K + zuojung
# Term Project

from Tkinter import *
import random
import time

class BreakOut(object):
  def init(self):
    self.blockRows = 20
    self.blockCols = 6
    self.board = self.createBoard()
    self.lives = 3
    self.isMenu = True
    self.isMakingBoard = False 
    self.isGameBegin = False
    self.isBallBegin = False
    self.isGameOver = False
    self.isHelperScreen = False
    self.isPaused = False
    self.isGameWon = False
    self.score = 0
    self.time = 30000
    self.canvasWidth = 500
    self.canvasHeight = 400
    self.windowWidth = 500 
    self.windowHeight = 300 
    self.paddleLength = 50
    self.paddleHeight = 10
    self.radius = 5
    self.blockWidth = (self.windowWidth)/len(self.board[0])
    self.blockHeight = self.blockWidth / 2.0
    self.paddleLocation = (self.windowWidth/2.0, self.windowHeight)
    self.ball = Ball(self.windowWidth/2.0,self.windowHeight-self.paddleHeight-
                     2*self.radius,"red", self.canvas)
    self.paddle = Paddle(self.windowWidth/2.0,
                         self.windowHeight-self.paddleHeight,self.paddleLength,
                         self.paddleHeight,self.canvas)
    self.powerups = []

  def keyPressed(self,event):
    if not (self.isMenu or self.isMakingBoard):
      if self.isPaused == False:
        if (event.keysym == "Left"):
          self.paddle.update("Left")
          if self.isBallBegin == False:
            self.ball.update("*L")
            self.isBallBegin = True
            self.isGameBegin = True
            self.beginTime= time.time()
            self.isPaused = False
        elif (event.keysym == "Right"):
          self.paddle.update("Right")
          if self.isBallBegin == False:
            self.ball.update("*R")
            self.isBallBegin = True
            self.isGameBegin = True
            self.beginTime= time.time()
            self.isPaused = False
      if (event.keysym == "h"):
        self.isHelperScreen = not self.isHelperScreen
      if (event.keysym == "p"):
        if self.isHelperScreen == False:
          self.isPaused = not self.isPaused
    if self.isMakingBoard:
      if not (self.checkGameWon()):
        if (event.keysym == "Return"):
          self.isMakingBoard = False
          self.isGameWon = False
    if event.keysym == "Escape":
      if self.isGameOver:
        self.init()
        self.isMenu = True
      elif self.isGameBegin and not self.isMenu:
        self.init()
        self.isMenu = True
      elif not self.isGameBegin and self.isHelperScreen and not self.isMenu:
        self.isHelperScreen = False
        self.isMenu = True
      elif not (self.isGameBegin and self.isBallBegin and self.isGameOver
                and self.isHelperScreen and self.isGamePaused and self.isMenu):
        self.isMenu = True
      

  def mousePressed(self,event):
    if self.isMenu:
      if (event.x >= self.canvasWidth/5
          and event.x < self.canvasWidth*4/5
          and event.y>=self.canvasHeight*4/5+20
          and event.y<=self.canvasHeight*4/5+60):
        self.isMenu = False
      elif (event.x >= self.canvasWidth/4+20
            and event.y >=self.canvasHeight/4+40
            and event.x<=self.canvasWidth*3/4-20
            and event.y<=self.canvasHeight/4+80):
        self.isMakingBoard = True
        self.board = [[0]*self.blockRows for block in xrange(self.blockCols)]
        self.isMenu = False
      elif (event.x >=self.canvasWidth/4+20
            and event.x <= self.canvasWidth*3/4-20
            and event.y >= self.canvasHeight/4+100
            and event.y <= self.canvasHeight/4+140):
        self.isMenu = False
        self.isHelperScreen = True
    elif self.isMakingBoard: 
      for i in xrange(len(self.board)):
        for j in xrange(len(self.board[i])): 
          if (j*self.blockWidth <=event.x 
            and event.x <= ((j+1)*self.blockWidth)
            and i*self.blockHeight <= event.y
            and event.y <= ((i+1)*self.blockHeight)):
              if self.board[i][j] == 0: 
                self.board[i][j] = Block(self.canvas)
              else: 
                self.board[i][j] = 0 

  def createBoard(self):
    blockCols = self.blockCols
    blockRows = self.blockRows
    board1 = [[Block(self.canvas)]*blockRows for block in xrange(blockCols)]
    return board1

  def drawBlocks(self):
    board = self.board
    for row in xrange(len(board)):
      for col in xrange(len(board[0])):
        x0 = col*(self.blockWidth)
        y0 = row*(self.blockHeight)
        x1 = (col+1)*(self.blockWidth)
        y1 = (row+1)*(self.blockHeight)
        if (row+col) % 3 == 0:
          color = "blue"
        elif (row+col) % 3 == 1:
          color = "green"
        else:
          color = "pink"
        if board[row][col] != 0:
          board[row][col].drawSelf(x0,y0,x1,y1,color)
        else:
          pass

  def updatePowerUps(self): 
    for pu in self.powerups[:]:
      if not(pu.isActivated):
        pu.update()
        (x, y) = pu.coordinates
        (px, py) = self.paddle.coordinates
        if y>(py+self.paddle.paddleHeight/2):
          self.powerups.remove(pu)
          continue
        pu.collision(self.paddle)
      if pu.isActivated:
        if pu.activateStepAndFinish(self):
          self.powerups.remove(pu)
          continue

  def timerFired(self):
    if not self.isGameOver and (not self.isGameWon or self.isMakingBoard):
      self.redrawAll()
      self.update()              
    delay = 10
    self.canvas.after(delay,self.timerFired)

  def update(self):
    if not (self.isPaused or self.isHelperScreen):    
      self.ball.update(0)
      self.updatePowerUps()
      if (self.time > 0 and self.isGameBegin
          and not self.isGameOver and not self.isGameWon):
        self.time -= 3

  def drawPowerUps(self):
    for i in xrange(len(self.powerups)):
      if not(self.powerups[i].isActivated):
        self.powerups[i].drawSelf()

  def addPU(self, row, col):
    x = self.blockWidth*col + (self.blockWidth/2)
    y = self.blockHeight*row + (self.blockHeight/2)
    prob = random.randint(0,99)
    if prob < 10:
      self.powerups.append(ExtraLife(x,y,self.canvas))
    elif prob < 30 and prob > 10:
      self.powerups.append(EnlargePaddle(x,y,self.canvas))
    elif prob < 45 and prob > 30:
      self.powerups.append(ShrinkPaddle(x,y,self.canvas))
    elif prob < 90 and prob > 60:
      self.powerups.append(AddBall(x,y,self.canvas))

  def redrawAll(self):
    self.canvas.delete(ALL)
    self.canvas.create_rectangle(0,0,self.windowWidth,self.windowHeight,
                                 fill = "black")
    self.drawBlocks()
    self.drawPowerUps()
    self.paddle.drawSelf()
    self.ball.drawSelf()
    self.helperScreen()
    self.drawGameWon()
    self.menu()
    self.scoreBoard()
    self.makingBoard()
    if not (self.isPaused or self.isHelperScreen):
      self.ball.collision(self.paddle,self.board)
      for row in xrange(len(self.board)):
        for col in xrange(len(self.board[0])):
          x0 = col*(self.blockWidth)
          y0 = row*(self.blockHeight)
          x1 = (col+1)*(self.blockWidth)
          y1 = (row+1)*(self.blockHeight)
          self.ball.blockCorrdinates = (x0,y0,x1,y1)
          if self.board[row][col] != 0:
            collision = self.board[row][col].collision(self.ball)
            if (collision == "Collision type 1-1"):
              self.addPU(row,col)
              self.board[row][col] = 0
              self.score += 1            
            elif (collision == "Collision type 1-2"):
              self.addPU(row,col)
              self.board[row][col] = 0
              self.score += 1
            elif (collision == "Collision type 2"):
              self.addPU(row,col)
              self.board[row][col] = 0
              self.score += 1
            elif (collision == "Collision type 3-1"):
              self.addPU(row,col)
              self.board[row][col] = 0
              self.score += 1
            elif (collision == "Collision type 3-2"):
              self.addPU(row,col)
              self.board[row][col] = 0
              self.score += 1
            elif (collision == "Collision type 4-1"):
              self.addPU(row,col)
              self.board[row][col] = 0
              self.score += 1
            elif (collision == "Collision type 4-2"):
              self.addPU(row,col)
              self.board[row][col] = 0
              print "board changed"
            elif (collision == "Collision type 5"):
              self.addPU(row,col)
              self.board[row][col] = 0
              self.score += 1
            elif (collision == "Collision type 6-1"):
              self.addPU(row,col)
              self.board[row][col] = 0
              self.score += 1
            elif (collision == "Collision type 6-2"):
              self.addPU(row+1,col)
              self.board[row+1][col] = 0
              self.score += 1
            elif (collision == "Collision type 7-1"):
              self.addPU(row,col+1)
              self.board[row][col+1] = 0
              self.score += 1
            elif (collision == "Collision type 7-2"):
              self.addPU(row+1,col)
              self.board[row+1][col] = 0
              self.score += 1
            elif (collision == "Collision type 8"):
              self.addPU(row,col)
              self.board[row][col] = 0
              self.score += 1
            elif (collision == "Collision type 9-1"):
              self.addPU(row,col)
              self.board[row][col] = 0
              self.score += 1
            elif (collision == "Collision type 9-2"):
              self.addPU(row,col)
              self.board[row][col] = 0
              self.score += 1
            elif (collision == "Collision type 10-1"):
              self.addPU(row,col-1)
              self.board[row][col-1] = 0
              self.score += 1
            elif (collision == "Collision type 10-2"):
              self.addPU(row,col)
              self.board[row][col] = 0
              self.score += 1
            elif (collision == "Collision type 11"):
              self.addPU(row,col)
              self.board[row][col] = 0
              self.score += 1
            elif (collision == "Collision type 12-1"):
              self.addPU(row,col)
              self.board[row][col] = 0
              self.score += 1
            elif (collision == "Collision type 12-2"):
              self.addPU(row,col+1)
              self.board[row][col+1] = 0
              self.score += 1
    if (self.isPaused == True and self.isHelperScreen == False
        and self.isGameBegin == True and self.isGameOver == False):
      self.canvas.create_text(self.windowWidth / 2, self.windowHeight /2 ,
                              text = "Paused",fill = "red")
    if not self.isGameOver:
      isBallDead = self.ball.collision(self.paddle,self.board)
      if self.lives > 0 and self.time >0:
        if isBallDead == "dead":
          self.lives -= 1
          self.isBallBegin = False
          self.ball = Ball(self.windowWidth/2.0,self.windowHeight-
                           self.paddleHeight-2*self.radius,"red", self.canvas)
          self.paddle = Paddle(self.windowWidth/2.0,
                               self.windowHeight-self.paddleHeight,
                               self.paddleLength,self.paddleHeight,self.canvas)
          self.powerups = []
      else:
        self.isGameOver = True
        self.canvas.create_text(self.windowWidth / 2, self.windowHeight / 2,
                    font = "Times 24 bold", text = "Game Over", fill = "red")
        self.canvas.create_text(self.windowWidth / 2,self.windowHeight / 2 + 25,
                     text = "Your Score is: %d" %(self.score), fill = "red")
        self.canvas.create_text(self.windowWidth / 2,self.windowHeight / 2 + 40,
                     text = "press esc to exit to menu", fill = "red")


  def helperScreen(self):
    if self.isHelperScreen == True:
        self.drawHelperScreen()
    

  def drawHelperScreen(self):
      self.canvas.create_rectangle(0,0,self.canvasWidth,
                              self.canvasHeight,fill = "yellow")
      self.canvas.create_text(self.windowWidth/2, 40,text  = "BreakOut",
                              font = "Helitavia 12 bold")
      self.canvas.create_text(60, 70,
                      text="Key options:", font = "Times 12 bold")
      self.canvas.create_text(110, 100,
                      text="Left - Move the Paddle to the Left",
                              font = "Times 10")
      self.canvas.create_text(self.windowWidth / 2 + 80, 100,
                      text="Right - Move the Paddle to the Right",
                              font = "Times 10")
      self.canvas.create_text(self.windowWidth / 2 - 10, 130,
                      text="Esc - return to main menu",
                              font = "Times 10")
      self.canvas.create_text(70, 130,
                      text="p - Pause the Game",
                              font = "Times 10")
      self.canvas.create_text(self.windowWidth/2 +130, 130,
                      text="h - Help Screen",
                              font = "Times 10")
      self.canvas.create_text(self.windowWidth/2, 160,
                      text  = "Move the paddle to start the game!",
                              font = "Times 12 bold")
      self.canvas.create_text(60, 190,
                      text="Power Ups:", font = "Times 12 bold")
      self.canvas.create_oval(55,215,65,225,fill = "orange")
      self.canvas.create_text(100,220,text = " - Extra Life",font="Times 10")
      self.canvas.create_rectangle(180,215,190,225,fill = "purple")
      self.canvas.create_text(self.windowWidth/2 + 10, 220,
                        text  = "- Double Paddle Length")
      self.canvas.create_rectangle(55,245,65,255,fill = "red")
      self.canvas.create_text(110,250,text = " - Half Paddle",
                              font="Times 10")
      self.canvas.create_rectangle(180,245,190,255,fill = "green")
      self.canvas.create_text(self.windowWidth/2 - 20,250,
                              text = " - Extra Ball",
                              font="Times 10")

  def scoreBoard(self):
    if not (self.isMenu or self.isHelperScreen):
      self.drawScoreBoard()

  def drawScoreBoard(self):
    self.canvas.create_rectangle(0,self.windowHeight,self.windowWidth,
                                 self.canvasHeight,  fill = "black")
    self.canvas.create_rectangle(0,self.windowHeight,self.windowWidth,
                                 self.windowHeight+10, fill = "yellow")
    self.canvas.create_text(self.windowWidth/6, self.windowHeight + 25,
                            text = "Lives: %d" %(self.lives),fill = "white")
    self.canvas.create_text(self.windowWidth/2, self.windowHeight + 25,
                            text = "Time: %d" %(self.time / 100),fill = "white")
    self.canvas.create_text(self.windowWidth*5/6, self.windowHeight + 25,
                            text = "Score: %d" %(self.score),fill = "white")
    self.canvas.create_text(self.windowWidth/2, self.windowHeight + 70,
                            text = "Press h for help screen",fill = "white")

  def checkGameWon(self):
    board = self.board
    for row in xrange(len(board)):
      for col in xrange(len(board[0])):
        if board[row][col] != 0:
          return False
    return True
    
  def drawGameWon(self):
    if self.checkGameWon():
      self.isGameWon = True
      if self.isGameWon:
        self.canvas.create_text(self.windowWidth / 2, self.windowHeight / 2,
                    font = "Times 24 bold", text = "You Win!", fill = "red")
        self.canvas.create_text(self.windowWidth / 2,self.windowHeight / 2 + 25,
                     text = "Your Score is: %d" %(self.score), fill = "red")
        self.canvas.create_text(self.windowWidth / 2,self.windowHeight / 2 + 40,
                     text = "press esc to exit to menu", fill = "red")

  def makingBoard(self): 
    if self.isMakingBoard:
      self.drawMakingBoard()

  def drawMakingBoard(self): 
    self.canvas.delete(ALL)
    self.canvas.create_rectangle(0,0,self.windowWidth,self.windowHeight,
                                 fill = "black")
    board = self.board
    for row in xrange(len(board)):
      for col in xrange(len(board[row])):
        x0 = col*(self.blockWidth)
        y0 = row*(self.blockHeight)
        x1 = (col+1)*(self.blockWidth)
        y1 = (row+1)*(self.blockHeight)
        if (row+col) % 3 == 0:
          color = "blue"
        elif (row+col) % 3 == 1:
          color = "green"
        else:
          color = "pink"
        if board[row][col] == 0:
          self.canvas.create_rectangle(x0,y0,x1,y1,outline = "white", width = 1)
        else:
          board[row][col].drawSelf(x0,y0,x1,y1,color)
    x = self.canvasWidth / 2
    y = self.windowHeight + ((self.canvasHeight-self.windowHeight)/ 2)      
    self.canvas.create_text(x, y-10,font = "Times",
                  text = "Click on the blocks to create a board.",
                            fill = "black")
    self.canvas.create_text(x, y+10, font = "Times",
            text = "Press enter when you are ready to play your creation.",
                            fill = "black")


  def menu(self):
    if self.isMenu:
      self.drawMenu()

  def drawMenu(self):
    self.canvas.create_rectangle(0,0,self.canvasWidth,
                                 self.canvasHeight,fill = "cyan")
    self.canvas.create_text(self.canvasWidth/2,self.canvasHeight/6,
                            text = "Break Out",font = "Ariel 30 bold",
                            fill = "black")
    self.canvas.create_rectangle(self.canvasWidth/4,self.canvasHeight/4,
                            (self.canvasWidth*3)/4,self.canvasHeight*3/4 -20,
                                 width = 2, fill = "skyblue")
    self.canvas.create_text(self.canvasWidth/2,self.canvasHeight/4+20,
                            text = "Options",font = "Times 12")
    self.canvas.create_rectangle(self.canvasWidth/4 + 20,self.canvasHeight/4+40,
                      self.canvasWidth*3/4-20,self.canvasHeight/4+80,
                                 fill = "yellow")
    self.canvas.create_text(self.canvasWidth/2,self.canvasHeight/4+60,
                            text = "Create Your Own Board!",
                            font = "Times 12")
    self.canvas.create_rectangle(self.canvasWidth/4+20,self.canvasHeight/4+100,
                      self.canvasWidth*3/4-20,self.canvasHeight/4+140,
                                 fill = "yellow")
    self.canvas.create_text(self.canvasWidth/2,self.canvasHeight/4+120,
                            text = "How To Play",font = "Times 12")    
    self.canvas.create_rectangle(self.canvasWidth/5,self.canvasHeight*4/5+20,
                              self.canvasWidth*4/5,(self.canvasHeight*4/5)+60,
                                 width = 1,fill = "yellow")
    self.canvas.create_text(self.canvasWidth/2,self.canvasHeight*4/5+40,
                            text = "Start Game", font = "Times 16")

  def run(self):
    root  = Tk()
    root.wm_title("Break Out! (Gong)")
    self.canvas = Canvas(root,width=500,height=400)
    self.canvas.pack()
    root.bind("<Button-1>", self.mousePressed)
    root.bind("<Key>", self.keyPressed)
    self.init()
    self.timerFired()
    root.mainloop()

class Elements(object):
  def init(self): pass
  def drawSelf(self): pass
  def collision(self): pass
  def update(self): pass

class PowerUps(Elements):
  def activateStepAndFinish(self): pass

class ExtraLife(PowerUps):
  def __init__(self, x, y, canvas):
    self.coordinates = (x,y)
    self.canvas = canvas
    self.isActivated = False

  def update(self):
    (x,y) = self.coordinates
    self.coordinates = (x,y+1)

  def drawSelf(self):
    (x, y) = self.coordinates
    self.canvas.create_oval(x - 5,y-5,x+5, y+5,fill = "orange")

  def collision(self, paddle):
    if not(self.isActivated):
      (px,py) = paddle.coordinates
      (x,y) = self.coordinates
      pWidth = paddle.paddleLength
      if (py-paddle.paddleHeight/2 < y) and (px-(pWidth/2)<= x and x<=(px+pWidth/2)):
        self.isActivated = True

  def activateStepAndFinish(self, game):
    game.lives += 1
    return True

class EnlargePaddle(PowerUps):
  def __init__(self, x, y, canvas):
    self.coordinates = (x,y)
    self.canvas = canvas
    self.duration = 500
    self.start = 500
    self.isActivated = False

  def update(self):
    (x,y) = self.coordinates
    self.coordinates = (x,y+1)

  def drawSelf(self):
    (x, y) = self.coordinates
    self.canvas.create_rectangle(x - 5,y-5,x+5, y+5,fill = "purple")

  def collision(self, paddle):
    if not(self.isActivated):
      (px,py) = paddle.coordinates
      (x,y) = self.coordinates
      pWidth = paddle.paddleLength
      if ((py-paddle.paddleHeight/2 < y)
          and (px-(pWidth/2)<= x and x<=(px+pWidth/2))):
        self.isActivated = True

  def activateStepAndFinish(self, game):
    if self.duration == self.start:
      game.paddle.paddleLength *= 2
    elif self.duration == 0:
      game.paddle.paddleLength /= 2
      return True
    self.duration -= 1
    return False

class ShrinkPaddle(PowerUps):
  def __init__(self, x, y, canvas):
    self.coordinates = (x,y)
    self.canvas = canvas
    self.duration = 500
    self.start = 500
    self.isActivated = False

  def update(self):
    (x,y) = self.coordinates
    self.coordinates = (x,y+1)

  def drawSelf(self):
    (x, y) = self.coordinates
    self.canvas.create_rectangle(x - 5,y-5,x+5, y+5,fill = "red")

  def collision(self, paddle):
    if not(self.isActivated):
      (px,py) = paddle.coordinates
      (x,y) = self.coordinates
      pWidth = paddle.paddleLength
      if ((py-paddle.paddleHeight/2 < y)
          and (px-(pWidth/2)<= x and x<=(px+pWidth/2))):
        self.isActivated = True

  def activateStepAndFinish(self, game):
    if self.duration == self.start:
      game.paddle.paddleLength /= 1.5
    elif self.duration == 0:
      game.paddle.paddleLength *= 1.5
      return True
    self.duration -= 1
    return False

class AddBall(PowerUps):
  def __init__(self, x, y, canvas):
    self.coordinates = (x,y)
    self.canvas = canvas
    self.isActivated = False
    self.newlyActive = True
    self.ball = None

  def update(self):
    (x,y) = self.coordinates
    self.coordinates = (x,y+1)

  def drawSelf(self):
    (x, y) = self.coordinates
    self.canvas.create_oval(x - 5,y-5,x+5, y+5,fill = "green")

  def collision(self, paddle):
    if not(self.isActivated):
      (px,py) = paddle.coordinates
      (x,y) = self.coordinates
      pWidth = paddle.paddleLength
      if ((py-paddle.paddleHeight/2 < y)
          and (px-(pWidth/2)<= x and x<=(px+pWidth/2))):
        self.isActivated = True

  def activateStepAndFinish(self, game):
    if self.newlyActive:
      self.newlyActive = not(self.newlyActive)
      (px,py)=game.paddle.coordinates
      self.ball = Ball(px,py-(game.paddle.paddleHeight), "maroon", game.canvas)
      self.ball.update("*R")
    self.ball.update(0)
    if not (game.isPaused or game.isHelperScreen):
      self.ball.collision(game.paddle,game.board)
      for row in xrange(len(game.board)):
        for col in xrange(len(game.board[0])):
          x0 = col*(game.blockWidth)
          y0 = row*(game.blockHeight)
          x1 = (col+1)*(game.blockWidth)
          y1 = (row+1)*(game.blockHeight)
          self.ball.blockCorrdinates = (x0,y0,x1,y1)
          if game.board[row][col] != 0:
            collision = game.board[row][col].collision(self.ball)
            if (collision == "Collision type 1-1"):
              game.addPU(row, col) 
              game.board[row][col] = 0          
            elif (collision == "Collision type 1-2"):
              game.addPU(row, col) 
              game.board[row][col] = 0
            elif (collision == "Collision type 2"):
              game.addPU(row, col) 
              game.board[row][col] = 0
            elif (collision == "Collision type 3-1"):
              game.addPU(row, col) 
              game.board[row][col] = 0
            elif (collision == "Collision type 3-2"):
              game.addPU(row, col) 
              game.board[row][col] = 0
            elif (collision == "Collision type 4-1"):
              game.addPU(row, col) 
              game.board[row][col] = 0
            elif (collision == "Collision type 4-2"):
              game.addPU(row, col) 
              game.board[row][col] = 0
            elif (collision == "Collision type 5"):
              game.addPU(row, col) 
              game.board[row][col] = 0
            elif (collision == "Collision type 6-1"):
              game.addPU(row, col) 
              game.board[row][col] = 0
            elif (collision == "Collision type 6-2"):
              game.addPU(row+1, col) 
              game.board[row+1][col] = 0
            elif (collision == "Collision type 7-1"):
              game.addPU(row, col+1) 
              game.board[row][col+1] = 0
            elif (collision == "Collision type 7-2"):
              game.addPU(row+1, col) 
              game.board[row+1][col] = 0
            elif (collision == "Collision type 8"):
              game.addPU(row, col) 
              game.board[row][col] = 0
            elif (collision == "Collision type 9-1"):
              game.addPU(row, col)
              game.board[row][col] = 0
            elif (collision == "Collision type 9-2"):
              game.addPU(row, col)
              game.board[row][col] = 0
            elif (collision == "Collision type 10-1"):
              game.addPU(row, col-1) 
              game.board[row][col-1] = 0
            elif (collision == "Collision type 10-2"):
              game.addPU(row, col) 
              game.board[row][col] = 0
            elif (collision == "Collision type 11"):
              game.addPU(row, col) 
              game.board[row][col] = 0
            elif (collision == "Collision type 12-1"):
              game.addPU(row, col) 
              game.board[row][col] = 0
            elif (collision == "Collision type 12-2"):
              game.addPU(row, col+1)
              game.board[row][col+1] = 0
    self.ball.drawSelf()
    return False

class Ball(Elements,BreakOut):
  def __init__(self,x,y,color,canvas):
    self.windowWidth = 500
    self.windowHeight = 300
    self.coordinates = (x,y)
    self.canvas = canvas
    self.radius = 5
    self.vX = 0
    self.vY = 0
    self.color = color

  def drawSelf(self):
    (x,y) = self.coordinates
    r = self.radius
    if (y<=self.windowHeight):
      self.canvas.create_oval(x-r,y-r,x+r,y+r,fill= self.color)

  def collision(self,*other):
    (x,y) = self.coordinates
    (paddle,board) = other
    (xP,yP) = paddle.coordinates
    r = self.radius
    if x-r <= 0:
      self.vX = abs(self.vX)
    elif x+r >= self.windowWidth:
      self.vX = -abs(self.vX)
    elif y-r <= 0:
      self.vY = abs(self.vY)
    elif y-r >= self.windowHeight:
      self.vX,self.vY = 0,0
      return "dead"
    elif (abs((y+r)-(yP-paddle.paddleHeight/2.0)) < 2
        and ((xP-(paddle.paddleLength/2.0))<=(x)
             and (xP+(paddle.paddleLength/2.0))>=(x))):
      if x < xP-paddle.paddleLength/5.0:# hits the left portion of the paddle
        self.vY = -abs(self.vY)
        self.vX = -abs(self.vX)
      elif (x>xP-paddle.paddleLength/5.0
            and x<xP+paddle.paddleLength/5.0):# hits the middle of the paddle
        self.vY = -abs(self.vY)
      else:# hits the right portion of the paddle
        self.vY = -abs(self.vY)
        self.vX = abs(self.vX)

  def update(self,x):
    if x == "*L":
      self.vX = -3
      self.vY = -3
    elif x == "*R":
      self.vX = 3
      self.vY = -3
    elif x == "LR":
      self.vX = -self.vX
    elif x == "UD":
      self.vY = -self.vY
    (x,y) = self.coordinates
    self.coordinates = (x+self.vX,y+self.vY)


class Paddle(Elements):
  def __init__(self,x,y,pW, pH,canvas):
    self.coordinates = (x,y)
    self.canvas = canvas
    self.paddleLength = pW 
    self.paddleHeight = pH 

  def drawSelf(self):
    (x,y) = self.coordinates
    (length,height) = (self.paddleLength,self.paddleHeight)
    self.canvas.create_rectangle(x-(length/2.0),y-(height/2.0),
                                 x+(length/2.0),y+(height/2.0),fill = "white")

  def update(self,x):
    if x == "Left" and self.coordinates[0] - (self.paddleLength/2.0) >= 0:
      (x,y)= self.coordinates
      self.coordinates = (x-6,y)
    elif x == "Right" and self.coordinates[0] + (self.paddleLength/2.0) <= 500:
      (x,y)= self.coordinates
      self.coordinates = (x+6,y)

  def collision(self,other): pass


class Block(Elements):
  def __init__(self,canvas):
    self.canvas = canvas

  def drawSelf(self,x0,y0,x1,y1,color):
    self.canvas.create_rectangle(x0,y0,x1,y1,fill = color)

  def collision(self,other):
    (x,y) = other.coordinates
    r = other.radius
    left,up,right,down = x-r,y-r,x+r,y+r
    (x0,y0,x1,y1) = other.blockCorrdinates
    if up <= y1 and down > y1: #ball collides the bottom
      poC = x # abbv for point of contact,for percise ball collision
      if left < x0 and (right >= x0 and right <= x1):
        if poC < x0:
          other.update("UD")
          return "Collision type 1-1"
        elif (poC >= x0 and poC <= x1):
          other.update("UD") #improve this when improving edge dection
          return "Collision type 1-2"
      elif ((left >= x0 and left <= x1)
          and (right >= x0 and right <= x1)):
        other.update("UD")
        return "Collision type 2"
      elif right > x1 and (left >= x0 and left <= x1):
        if poC < x1 and poC > x0:
          other.update("UD") #improve this when improving edge dection
          return "Collision type 3-1"
        elif poC >= x1:
          other.update("UD") #improve this when improving edge dection
          return "Collision type 3-2"
    elif left < x0 and right >= x0: # ball collides the left side
      poC = y
      if up < y0 and (down >= y0 and down <= y1):
        if poC < y0:
          other.update("LR")#improve this when improving edge dection
          return "Collision type 4-1"
        elif poC >= y0 and poC <= y1:
          other.update("LR")#improve this when improving edge dection
          return "Collision type 4-2"
      elif ((up >= y0 and up <= y1)
          and (down >= y0 and down <= y1)):
        other.update("LR")
        return "Collision type 5"
      elif (up >= y0 and up <= y1) and down > y1:
        if poC <y1 and poC > y0:
          other.update("LR")#improve this when improving edge dection
          return "Collision type 6-1"
        elif poC >= y1 :
          other.update("LR")#improve this when improving edge dection
          return "Collision type 6-2"
    elif right > x1 and left <= x1: # ball collides the right side
      poC = y
      if down > y1 and (up >= y0 and up <= y1):
        if poC > y0 and poC < y1: 
          other.update("LR")#improve this when improving edge dection
          return "Collision type 7-1"
        elif poC >= y1:
          other.update("LR")#improve this when improving edge dection
          return "Collision type 7-2"
      elif ((up >= y0 and up <= y1)
          and (down >= y0 and down <= y1)):
        other.update("LR")
        return "Collision type 8"
      elif (down >= y0 and down <= y1) and up < y0:
        if poC < y0:
          other.update("LR")#improve this when improving edge dection
          return "Collision type 9-1"
        elif poC >= y0 and poC < y1:
          other.update("LR")#improve this when improving edge dection
          return "Collision type 9-2"          
    elif up < y0 and down >= y0: # ball collides the top side
      poC = x
      if left < x0 and (right >= x0 and right <= x1):
        if poC < x0:
          other.update("UD")#improve this when improving edge dection
          return "Collision type 10-1"
        elif poC >= x0 and poC < x1:
          other.update("UD")#improve this when improving edge dection
          return "Collision type 10-2"
      elif ((left >= x0 and left <= x1)
          and (right >= x0 and right <= x1)):
        other.update("UD")
        return "Collision type 11"
      elif (left >= x0 and left <= x1) and right > x1:
        if poC < x1 and poC > x0:
          other.update("UD")#improve this when improving edge dection
          return "Collision type 12-1"
        elif poC >= x1:
          other.update("UD")#improve this when improving edge dection
          return "Collision type 12-2"
    
    
game = BreakOut()
game.run()
