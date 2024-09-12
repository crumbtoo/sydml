module TreeSitter.SydML
  ( tree_sitter_sydml
  , getNodeTypesPath
  , getTestCorpusDir
  )
  where
--------------------------------------------------------------------------------

import Foreign.Ptr
import TreeSitter.Language
import Paths_tree_sitter_sydml
import Foreign.C.ConstPtr

foreign import ccall unsafe "src/parser.c tree_sitter_sydml"
  tree_sitter_sydml :: ConstPtr Language

getNodeTypesPath :: IO FilePath
getNodeTypesPath = getDataFileName "src/node-types.json"

getTestCorpusDir :: IO FilePath
getTestCorpusDir = getDataFileName "corpus"
