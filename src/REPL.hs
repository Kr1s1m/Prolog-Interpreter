module REPL where

import Types( Command(..), Substitution, Program, Term, showSubstitution )
import Parser ( parseCommand, parseProgram )
import Unification ( unify', applySubstitution )
import Solver ( solve )

import Data.Maybe ( fromMaybe )
import System.IO ( IOMode(ReadMode), hGetContents, openFile, hClose)

prologREPL :: IO()
prologREPL = do
    putStrLn "Please enter a valid .pl file path:"
    filePath <- getLine

    handle <- openFile filePath ReadMode
    contents <- hGetContents handle

    let program = parseProgram contents

    if null program
    then
        putStrLn "Syntax error found inside .pl file.\nPlease fix errors and reload."
    else do
        putStrLn "File loaded and intepreted. No errors found.\n" 
        interactLine (initializeREPL program)

    hClose handle

--TODO: Causes *** Exception: <stdin>: hGetLine: illegal operation (handle is semi-closed)
--on next load of main function which requires reload of Main module to execute main again
interactLine :: (String -> String) -> IO ()
interactLine f = loop
  where
    loop = do
      s <- getContents
      putStr (f s)

initializeREPL :: Program -> (String -> String)
initializeREPL program = writeStr welcomeMsg (repl program)

repl ::  Program -> (String -> String)
repl program = writeStr "*Haskellog> " $ readLine (execute program . parseCommand)

execute :: Program -> Command -> (String -> String)
execute program (AddRule rule)      = writeStr ("\nRule added: " ++ show rule ++ "\n") (repl (program ++ [rule]))
execute program (Query query)       = getSolution query (solve program query) program
execute program ShowAllRules        = writeStr (getAllRules program) (repl program)
execute program Help                = writeStr helpMsg (repl program)
execute program NoAction            = repl program
execute program CmdError            = writeStr errorMsg (repl program)
execute program Quit                = end

getSolution :: [Term] -> [Substitution] -> Program -> (String -> String)
getSolution q [] program     = writeStr "false.\n" (repl program)
getSolution q (s:ss) program  = writeStr (result ++ "\n") (moreSolutions q ss program)
    where result = showSubstitution $ fromMaybe [] $ unify' q (applySubstitution s q)

moreSolutions :: [Term] -> [Substitution] -> Program -> (String -> String)
moreSolutions q ss program = readLine f
    where f (';':_)      = getSolution q ss program
          f _            = repl program

getAllRules :: Program -> String
getAllRules = concatMap (\r -> show r ++ "\n")

welcomeMsg :: String
welcomeMsg = "****Welcome to Prolog in Haskell (Haskellog) version 2.2.1!****\n" ++
             "****For help, please enter :help\n"
             
end :: String -> String
end _ = "Interaction ended by user. Reload main to initiate it again.\n"

helpMsg :: String
helpMsg = 
          "Define fact      -->  mammal(orca).\n" ++
          "Define rule      -->  grandparent(X, Y) :- parent(X, Z), parent(Z, Y).\n" ++
          "Query            -->  ?- mammal(X), black(X), grandparent(Y, Z).\n" ++
          "Show all         -->  ??\n" ++
          "Help             -->  :help\n" ++
          "Quit             -->  :q\n\n"

errorMsg :: String
errorMsg = "Unknown command! Enter :help in prompt for more information.\n"

-- writeStr arguments are a string to be written and next process p.
writeStr :: String -> (String -> String) -> String -> String
writeStr output p input = output ++ p input

-- readLine argument is a process p which receives a line.
readLine :: (String -> String -> String) -> String -> String
readLine p input = case nextLine input of
                        ("", [])      -> "" -- End of file
                        (line, rest) -> p line rest

nextLine :: String -> (String, String)
nextLine ""        = ("", "")
nextLine ('\n':xs) = ("\n", xs)
nextLine (x:xs)    = (x:ys, zs) 
    where (ys, zs) = nextLine xs