module Main where

import Control.Monad.State ( runState )
import qualified Lang.Simp.Syntax.Lexer as Lexer
import qualified Lang.Simp.Syntax.Parser as Parser
import Lang.Simp.Syntax.Parsec
    ( Parser(run),
      Result(Failed, Ok),
      Error,
      Progress(Empty, Consumed) )
import Lang.Simp.Syntax.AST ( Stmt, Const (..) )
import Lang.Simp.IR.MMUpDown ( Cogen(cogen) )
import Lang.Simp.IR.PseudoAssembly ( LabeledInstr )
import Lang.Simp.IR.Util ( StateInfo(StateInfo) )
import Lang.Simp.Semantics.TypeInf hiding ( Empty )
import Lang.Simp.Interpreter.SimpInt
import Lang.Simp.Backend.Wasm ( convertMod, saveMod )
import qualified Language.Wasm.Structure as WasmStruct
import System.Environment ( getArgs )
import Text.Read



main :: IO ()
main = do
    args <- getArgs
    case args of
        { (flag:srcFile:inputStr:_)
            | flag == "-i" -> do
            src <- readFile srcFile
            let intResult = do
                { input <- parseInputFromCmdLine inputStr
                ; toks  <- lexer src
                ; stmts <- parser toks
                ; tyEnv <- typeInf stmts
                ; interpret stmts input
                }
            case intResult of
            { Left err -> putStrLn err
            ; Right (IntConst v) ->
                print v
            }
        ; (flag:srcFile:wasmFile:mainFuncName:_)
            | flag == "-c" -> do
            src <- readFile srcFile
            let infResult = do
                { toks  <- lexer src
                ; stmts <- parser toks
                ; tyEnv <- typeInf stmts
                ; return stmts 
                }
            case infResult of
            { Left err -> putStrLn err
            ; Right stmts ->
                let pa = irGen stmts
                    wasmMod = codeGen mainFuncName pa
                in saveMod wasmFile wasmMod
            }
        ; _ -> putStrLn "Usage: simp -i <src.simp> <input> or simp -c <src.simp> <dst.wasm> <function_name>"
        }

parseInputFromCmdLine :: String -> Either Error Int
parseInputFromCmdLine inputStr = case readMaybe inputStr of
    Nothing -> Left "error: command line input must be an integer."
    Just v  -> Right v


lexer :: String -> Either Error [Lexer.LToken]
lexer src =
    let lenv = Lexer.LEnv src 1 1
    in case run Lexer.lex lenv of
        { Consumed (Ok (toks, lenv))
            | Lexer.eof lenv    -> Right toks
            | otherwise         -> Left "lexing failed, the remaining token stream is not empty."
        ; Consumed (Failed err) -> Left err
        ; Empty (Ok (toks, lenv))
            | Lexer.eof lenv     -> Right toks
            | otherwise         -> Left "lexing failed, no token is consumed and the remaining token stream is not empty."
        ; Empty (Failed err)    -> Left err
        }

parser :: [Lexer.LToken] -> Either Error [Stmt]
parser toks =
    let penv = Parser.PEnv toks
    in case run Parser.parser penv of
        { Consumed (Ok (stmts, penv))
            | Parser.done penv  -> Right stmts
            | otherwise         -> Left "parsing failed, the remaining token stream is not empty."
        ; Consumed (Failed err) -> Left err
        ; Empty (Ok (stmts, penv))
            | Parser.done penv  -> Right stmts
            | otherwise         -> Left "parsing failed, no token has been consumed."
        ; Empty (Failed err)    -> Left err
        }



irGen :: [Stmt] -> [LabeledInstr]
irGen stmts =
    let st = StateInfo 1 "var" 1
    in case runState (cogen stmts) st of
        (instrs, st) -> instrs


codeGen :: String -> [LabeledInstr] -> WasmStruct.Module
codeGen = Lang.Simp.Backend.Wasm.convertMod
