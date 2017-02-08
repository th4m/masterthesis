module ParseEvaluate where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import AbsCakeML
import SemanticPrimitives
import Evaluate

-- Lexer START --
languageDef =
  emptyDef { Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = ["if",
                                      "then",
                                      "else",
                                      "true",
                                      "false"
                                      ]
           , Token.reservedOpNames = ["+",
                                      "-",
                                      "*",
                                      "/",
                                      "%",

                                      "<",
                                      ">",
                                      "<=",
                                      ">=",

                                      "&&",
                                      "||"
                                     ]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

-- Lexer END --

whileParser :: Parser Exp
whileParser = whiteSpace >> exprParse

exprParse :: Parser Exp
exprParse = ifExp
            <|> appExp

ifExp :: Parser Exp
ifExp =
  do reserved "if"
     cond <- exprParse
     reserved "then"
     exp1 <- exprParse
     reserved "else"
     exp2 <- exprParse
     return $ If cond exp1 exp2

appExp :: Parser Exp
appExp =
  do op <- undefined
     e1 <- exprParse
     e2 <- exprParse
     return $ App op [e1, e2]


opParse :: Parser Op
opParse = undefined
  -- do oneOf
  --      [ string "+" >> pure Plus
  --      , string "-" >> pure Minus
    
-- exprParse :: Parser Exp
-- exprParse = buildExpressionParser table term

-- table = undefined -- [ [Infix (reservedOp "+" >> return [App (OPN Plus)]) AssocLeft]
--         -- ]

-- term = parens exprParse
--        -- <|> fmap Var identifier
--        <|> (reserved "true" >> return (Con (Just (Short "true")) []))
--        <|> (reserved "false" >> return (Con (Just (Short "false")) []))
