## Haskellog

### Simple Prolog interpreter written in Haskell

Hello!

This is an university project made in Haskell for Functional programming course.

#### Supported:
  * Constants*
  * Rules and facts**
  * Infinite generation

*Only atom constants are supported. Lists, numbers and aritmetic operations and not yet suppoted.

**Rules only support logical AND via "," in their body.

#### How to use:
  * Load Main module into GHCi and run main function by typing "main" in *Main> prompt
  * Load .pl file with valid Prolog code by entering its full path when prompted
    * You are now inside a Read Evaluate Print Loop (REPL) made for this version of Prolog.
    * The prompt of this REPL is *Haskellog>
  * Enter commands inside *Haskellog> prompt.

#### Supported commands

  * *Haskellog> :help
  
    * Get a list of supported commands.
    
  * *Haskellog> ??
  
    * Get a list of all rules and facts. Prints the current state of the knowledge base.
                          
  * *Haskellog> blue(sky).
  
    * Adds fact "blue(sky)." to knowledge base.
    
  * *Haskellog> p(X, Y) :- q(X, Z), r(Z, Y).
  
    * Adds rule "p(X,Y):-q(X,Z),r(Z,Y)." to knowledge base.
    
  * *Haskellog> ?- q1(X, Y, Z).
  
    * Make a query. To get more answers enter ";". Enter anything else (for example ".") to stop.
    
  * *Haskellog> ?- q1(X, Y, Z), q2(Y), q3(T).
  
    * Make multiple queries at once connected with logical AND similar to a rules body.
    
  * *Haskellog> :q
  
    * Quit interactive loop (exits out of *Haskellog> prompt).
    

Please reload Main module (*Main> :r) before running main function again after *Haskellog> :q


### Sources and inspirations for code and approach
  * Basic Parser module idea:        https://www.youtube.com/watch?v=cdHY9Kqfr-k
  * Ideas for rest of the modules:   https://github.com/propella/prolog/
    * Credit goes to Takashi Yamamiya:    https://github.com/propella/
