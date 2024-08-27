module Main where
--------------------------------------------------------------------------------
import Options.Applicative
import Sydc
--------------------------------------------------------------------------------

data Command = Compile SydCompileOptions

parser :: Parser SydOptions
parser = _

main :: IO ()
main = putStrLn "hello, worms."
