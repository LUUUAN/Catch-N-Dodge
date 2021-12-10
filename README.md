# Catch N' Dodge

# Group Member

Wendi Luan, Yichen Liu, Yingjian (Steven) Wu, Zhaoxing Lyu

# Running Instruction
To run the game, type the following in the terminal (Inside the main folder)

``
cabal run CnD
``

To run the test, type the following in the terminal (Inside the main folder)

``
cabal run CnD-test
``

# Milestone I
deadline: 11/12

### Project Description

In this project, our group intends to use **Haskell** and **brick** library to achieve a terminal game. The goal of this game is to allow the character manipulated by users to collect as much good bricks and dodge bad bricks as possible. 

### Rule of the game

There are two types of bricks falling from the top of the terminal screen, a yellow one indicating the good brick and a red one indicating the bad brick. Blue brick indicating the character is manipulated by users to collect as much as good bricks and dodge as much as bad bricks as possible. When catching a good one, user will have 10 points and when catching a bad brick, user will lose 20 points. Users can select difficulty level to choose the number of bad bricks and good bricks on the screen.

### Game Control

The player can only use the "<-" and "->" to manipulate the character to move left or right. "r" can help restart the game and "q" helps to quit the game.

# Milestone II
deadline: 11/25

### Libraries

We intend to use below libraries for this project.

- `brick` : TUI library
- `vty` : TUI library
- `optparse-applicative`: Parse command line options
- `containers`: Provide concrete container types
- `directory`: For filesystem operations
- `filepath`: Manipulate FilePaths
- `random`: Generate pseudo-random number
- `Exit`: Exit the game when the player no longer wants to play

### Project Structure

Our project is composed of four src files, including `Main.hs`, `CnD.hs`, `PickLevel.hs` and `Game.hs`.

Inside `CnD.hs`, we define the main character and two types of bricks. We also plan to implement bricks and character movement control as well as the score calculation function in this file

we define the main character and three types of bricks that have different functionalities. We also plan to implement bricks and character movement control as well as the score calculation function in this file.

`PickLevel.hs` illustrates the basic UI when user starts the game and selects the difficult level.

`Game.hs` contains user interfaces for the game page. We design event handlers to coop with the character movement command, the pause command, and the quit game command.

`Main.hs` will handle IO events, including storing the highest score, parsing user input, and displaying messages through the command line.

### Challenges

1. Setting up the architecture of this application is the foremost challenge we meet. After several discussions, the final architecture is described above.
2. The second problem we meet is the use of the Brick and Graphics library. Since it is the first time for us to implement game engine using this library, it wasn't easy to get familiar with it and know how to use its feature. To solve this challenge, we searched the Internet and found relevant tutorials, including videos and texts. During the learning process, we understood and became familiar with the basic functions and features of this library. Additionally, we came up with new ideas about how to build our games easier with this library.
3. Another issue is the how to move character smoothly. When users move the character on the terminal to collect bricks, we need to determine how to set up the reassignment of the character to make it move smoothly. After discussions and implementation, we decided to make the reassignment happen once per tick, making the character move smoothly under the control of players.
4. At this point, we also meet problems including how to generate multiple bricks, how to generate more bad bricks depends on the difficulty level users choose, how to control the speed of different blocks and how to let users choose different difficult levels, which are waited for us to solve.

### Current Progress

We have met with certain challenges that are quite difficult to tackle, but we expect that the project could be finished by the deadline.

For the time being, we have completed the following parts of the game:

1. Keyboard bindings (left and right arrow keys to facilitate the position changes);
2. UI desgining
    1. Level of difficulty selection;
    2. Game pause and resume button;
    3. Main character, good bricks, bad bricks;
    4. Score borad of the user;

The following parts should be implemented next:

1. Default character movement 
2. Default bricks movement (good and bad bricks);
3. Collision detection and processing
4. Game Exit Triggering
5. Scoring of each round of game (measured by round lasting time)
