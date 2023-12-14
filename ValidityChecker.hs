module ValidityChecker where

-- Imports

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Data.List.Utils
import Data.String.Utils
import Data.List

--- Data Types
-- Data Type represting Propositions:
data Prop = Var String | Con Bool | Uno Unop Prop | Duo Duop Prop Prop deriving Show

-- Data Type representing Unary Operator Not:
data Unop = Not deriving Show

-- Data Type representing Binary Operators:
data Duop = And | Or | Xor | Implies | Iff deriving Show

-- Data Type representing an Argument, is a record type consisting of a List of Propositions as premises, and a single Proposition as a conclusion.
data Arg = Arg {
  premises :: [Prop],
  conclusion :: Prop
} deriving (Show)


--- Defining Parsers
-- Define our Language
def = emptyDef{ commentStart = "{-"
              , commentEnd = "-}"
              , identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "~&||<->"
              , opLetter = oneOf "~&||<->"
              , reservedOpNames = ["~", "!", "¬", "not", "and", "&", "&&", "∧", "·", "or", "||", "∨", "+", "implies", "->", "⇒", "=>", "⊃", "→", "iff", "<->", "↔", "=", "≡", "⇔", "<=>", "⊕", "⊻", "xor"]
              , reservedNames = ["true", "⊤", "1", "false", "⊥", "0", "nop", ",", "|=", "∴", "therefore"]
              }

-- Parses Tokens
TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def

-- Parses Propositions
exprparser :: Parser Prop
exprparser = buildExpressionParser table term <?> "proposition"
table = [ [Prefix (m_reservedOp "~" >> return (Uno Not)), Prefix (m_reservedOp "!" >> return (Uno Not)), Prefix (m_reservedOp "¬" >> return (Uno Not)), Prefix (m_reservedOp "not" >> return (Uno Not))]
	, [Infix (m_reservedOp "&&" >> return (Duo And)) AssocLeft, Infix (m_reservedOp "&" >> return (Duo And)) AssocLeft, Infix (m_reservedOp "∧" >> return (Duo And)) AssocLeft, Infix (m_reservedOp "·" >> return (Duo And)) AssocLeft, Infix (m_reservedOp "and" >> return (Duo And)) AssocLeft]
	, [Infix (m_reservedOp "||" >> return (Duo Or)) AssocLeft, Infix (m_reservedOp "∨" >> return (Duo Or)) AssocLeft, Infix (m_reservedOp "+" >> return (Duo Or)) AssocLeft, Infix (m_reservedOp "or" >> return (Duo Or)) AssocLeft]
	, [Infix (m_reservedOp "⊕" >> return (Duo Xor)) AssocLeft, Infix (m_reservedOp "⊻" >> return (Duo Xor)) AssocLeft, Infix (m_reservedOp "xor" >> return (Duo Xor)) AssocLeft]
	, [Infix (m_reservedOp "->" >> return (Duo Implies)) AssocLeft, Infix (m_reservedOp "⇒" >> return (Duo Implies)) AssocLeft, Infix (m_reservedOp "=>" >> return (Duo Implies)) AssocLeft, Infix (m_reservedOp "→" >> return (Duo Implies)) AssocLeft, Infix (m_reservedOp "⊃" >> return (Duo Implies)) AssocLeft, Infix (m_reservedOp "implies" >> return (Duo Implies)) AssocLeft]
	, [Infix (m_reservedOp "<->" >> return (Duo Iff)) AssocLeft, Infix (m_reservedOp "↔" >> return (Duo Iff)) AssocLeft, Infix (m_reservedOp "=" >> return (Duo Iff)) AssocLeft, Infix (m_reservedOp "≡" >> return (Duo Iff)) AssocLeft, Infix (m_reservedOp "⇔" >> return (Duo Iff)) AssocLeft, Infix (m_reservedOp "<=>" >> return (Duo Iff)) AssocLeft, Infix (m_reservedOp "iff" >> return (Duo Iff)) AssocLeft]
        ]

-- Various Booleans
term = m_parens exprparser
       <|> fmap Var m_identifier
       <|> (m_reserved "true" >> return (Con True))
       <|> (m_reserved "⊤" >> return (Con True))
       <|> (m_reserved "1" >> return (Con True))
       <|> (m_reserved "false" >> return (Con False))
       <|> (m_reserved "⊥" >> return (Con False))
       <|> (m_reserved "0" >> return (Con False))

-- Parse the Argument
argParser :: Parser (Maybe Arg)
argParser = do
  premList <- exprparser `sepBy` (char ',' >> spaces)
  spaces
  therefore_symbol
  spaces
  conclusionProp <- exprparser
  eof
  return (Just (Arg premList conclusionProp))

therefore_symbol :: Parser ()
therefore_symbol = do
  try (string "|=") <|> try (string "therefore") <|> try (string "∴")
  spaces
  return ()

-- Maybe wrapper around Argument Parser, also checks for Variable count to limit Long Compute Times
parseArg :: String -> Maybe Arg
parseArg input = case parse argParser "" input of
  Left _  -> Nothing
  Right result -> result


--- Code that evalutes Truth Values and Determines Validity
-- Evaluate truth value of formulas
evalProp :: [(String, Bool)] -> Prop -> Bool
evalProp _ (Con b)       = b
evalProp assignments (Var x) = case lookup x assignments of
  Just value -> value
  Nothing    -> error $ "Variable " ++ x ++ " not found in truth assignments"
evalProp assignments (Uno Not p)       = not (evalProp assignments p)
evalProp assignments (Duo And p q)     = evalProp assignments p && evalProp assignments q
evalProp assignments (Duo Or p q)      = evalProp assignments p || evalProp assignments q
evalProp assignments (Duo Xor p q)     = evalProp assignments p /= evalProp assignments q
evalProp assignments (Duo Implies p q) = not (evalProp assignments p) || evalProp assignments q
evalProp assignments (Duo Iff p q)     = evalProp assignments p == evalProp assignments q

-- Function to extract variables from a proposition
getVariables :: Prop -> [String]
getVariables (Var x)         = [x]
getVariables (Con _)         = []
getVariables (Uno _ p)       = getVariables p
getVariables (Duo _ p1 p2)   = nub (getVariables p1 ++ getVariables p2)

-- Function to extract all variables used in an argument
getArgVariables :: Arg -> [String]
getArgVariables arg = nub (concatMap getVariables (premises arg) ++ getVariables (conclusion arg))

-- Generate all possible truth assignments for a list of variables
generateTruthAssignments :: [String] -> [[(String, Bool)]]
generateTruthAssignments variables = sequence [[(var, value) | value <- [True, False]] | var <- variables]

-- Function to check if an argument is valid for all truth assignments
isValid :: Arg -> Bool
isValid arg = all (\assignment -> not (all (evalProp assignment) (premises arg)) || evalProp assignment (conclusion arg)) truthAssignments
  where
    -- Generate all possible truth assignments
    variables = nub $ concatMap getVariables (premises arg) ++ getVariables (conclusion arg)
    truthAssignments = generateTruthAssignments variables

-- Maybe wrapper on IsValid to catch errors
isMaybeValid :: Maybe Arg -> Bool
isMaybeValid marg = case marg of
    Nothing -> False
    Just arg -> isValid arg
