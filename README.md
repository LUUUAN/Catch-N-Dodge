# Running Instruction
``
stack install CnD --local-bin-path ""
``


``
./CnD
``

# Mile Stone I

### Project Description

In this project, our group intends to use **Haskell** and **brick** library to achieve a terminal game. The goal of this game is to allow the character to collect as much bricks as possible. There will be bricks falling from the top of the terminal with two colors, the blue one and the red one. If the character manipulated by users moving at the bottom of the terminal screen catch the blue one, the user will get a point. However, if the player catch a red one. The game will finish.

### Rule of the game

There are two types of bricks falling from the top of the terminal screen, a red one and a blue one. User can press <- or -> to move the character at the bottom of the terminal screen to move left or right. When user catch a blue brick, he will receive one point and when he catch a red one, the game will finish and the final score will be count. To increase the difficulty of the game, bricks are falling with different speed.

### Game Control

The player can only use the "<-" and "->" to manipulate the character to move left or right.

# Mile Stone II
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
    
