module Knowledgebase where

import Types
import Data.Map.Lazy as Map (empty, insertWith, lookup)
import Data.Maybe(fromMaybe)

createKnowledgeBase :: Maybe Program -> KnowledgeBase
createKnowledgeBase program = case program of
    Nothing -> Map.empty
    Just prog -> createKnowledgeBase' prog Map.empty

    where
        createKnowledgeBase' :: Program -> KnowledgeBase -> KnowledgeBase
        createKnowledgeBase' prog kbase = case prog of
            [] -> kbase
            (cl:cls) -> createKnowledgeBase' cls (addClause cl kbase)

addClause :: Clause -> KnowledgeBase -> KnowledgeBase
addClause cl = Map.insertWith (flip (++)) (funct (Types.head cl)) [cl]

lookup' :: Funct -> KnowledgeBase -> [Clause]
lookup' f kb = Data.Maybe.fromMaybe [] (Map.lookup f kb)