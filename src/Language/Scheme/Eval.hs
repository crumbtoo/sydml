module Language.Scheme.Eval
  ( eval
  , Env
  )
  where
--------------------------------------------------------------------------------
import Control.Lens
import Data.HashMap.Strict        qualified as H
import Language.Scheme.Syntax
import Sydc
--------------------------------------------------------------------------------

type Env = H.HashMap Symbol Sexp 

eval :: Env -> Sexp -> Either SydError Sexp
eval g e = _

