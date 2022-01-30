## Haskellog

### Simple Prolog interpreter written in Haskell

Hello!
This is an university project made in Haskell for course Functional programming.

#### Supported:
1. Constants*
2. Rules and facts**
3. Infinite generation

*Only atom constants are supported. Lists, numbers and aritmetic operations and not yet implemented.

**Rules only support logical AND via "," in their body.

#### How to use:
0. Load Main module into GHCi and run main function bu typing "main" in *Main> prompt
1. Load .pl file with valid Prolog code by entering its full path when prompted
You are now inside a Read Evaluate Print Loop (REPL) made for this version of Prolog.
The prompt of this REPL is *Haskellog>
2. Enter commands inside *Haskellog> prompt.

#### Supported commands


0. *Haskellog> :help                         - get a list of supported commands
1. *Haskellog> ??                            - get a list of all rules and facts ( prints the current state of the knowledge base )
2. *Haskellog> blue(sky).                    - add fact "blue(sky)."             to knowledge base
3. *Haskellog> p(X, Y) :- q(X, Z), r(Z, Y).  - add rule "p(X,Y):-q(X,Z),r(Z,Y)." to knowledge base
4. *Haskellog> ?- q1(X, Y, Z).               - make a query. To get more answers enter ";". Enter anything else (for example ".") to stop.
5. *Haskellog> ?- q1(X, Y, Z), q2(Y), q3(T). - multiple queries at once connected with logical AND similar to a rules body
6. *Haskellog> :q                            - quit interactive loop ( exit out of *Haskellog> prompt )

Please reload Main module (*Main> :r) before running main function again after *Haskellog> :q
Enjoy!

### Sources and inspirations for code and approach
0. Basic Parser module idea:        https://www.youtube.com/watch?v=cdHY9Kqfr-k
1. Ideas for rest of the modules:   https://github.com/propella/prolog/
Credit goes to Takashi Yamamia:     https://github.com/propella/
