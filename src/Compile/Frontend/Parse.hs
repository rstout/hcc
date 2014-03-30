{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>

   The start of a parser
-}

module Compile.Frontend.Parse (parseAST) where

import Data.ByteString as BS
import Compile.Types

import Text.ParserCombinators.Parsec hiding (try)
import Control.Monad.Identity                 -- For our custom Language Definition
import Text.Parsec hiding (parse)             -- Parser Combinators
import Text.Parsec.Expr                       -- Expression Parser Generator
import Text.Parsec.Token (GenLanguageDef(..)) -- Language Definition Structure
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Prim (try)

parseAST :: SourceCode -> Either String AST
parseAST (SourceCode file code) =
  case parse astParser file code of
    Left e  -> Left $ show e
    Right a -> Right a

type C0Parser = Parsec ByteString ()

astParser :: C0Parser AST
astParser = do
  whiteSpace
  reserved "int"
  reserved "main"
  parens $ return ()
  block <- braces
           (do pos   <- getPosition
               stmts <- many1 stmt
               return $ Block stmts pos)
  eof
  return block
  <?> "block"

stmt :: C0Parser Stmt
stmt = decl <|> asgn <|> ret <?> "statement"

decl :: C0Parser Stmt
decl = do { pos <- getPosition
          ; reserved "int"
          ; ident <- identifier
          ; e <- (do semi; return Nothing) <|>
                 (do symbol "="; e <- expr; semi; return $ Just e)
          ; return $ Decl ident e pos
          } <?> "declaration"

asgn :: C0Parser Stmt
asgn = do { pos <- getPosition
          ; ident <- lvalue
          ; op <- asnOp
          ; e <- expr
          ; semi
          ; return $ Asgn ident op e pos
          } <?> "assignment"

ret :: C0Parser Stmt
ret = do { pos <- getPosition
         ; reserved "return"
         ; e <- expr
         ; semi
         ; return $ Return e pos
         } <?> "return"

lvalue :: C0Parser String
lvalue = (do parens lvalue) <|> (do i <- identifier; return i)

asnOp ::C0Parser (Maybe Op)
asnOp = (do symbol "+="; return $ Just Add) <|>
        (do symbol "*="; return $ Just Mul) <|>
        (do symbol "-="; return $ Just Sub) <|>
        (do symbol "/="; return $ Just Div) <|>
        (do symbol "%="; return $ Just Mod) <|>
        (do symbol "="; return $ Nothing)
        <?> "assignment operator"

expr :: C0Parser Expr
expr = buildExpressionParser opTable term <?> "expr"

term :: C0Parser Expr
term = do
   parens expr
   <|>
   (do p <- getPosition
       i <- identifier
       return $ Ident i p)
   <|>
   (do p <- getPosition
       i <- intConst
       return $ ExprInt i p)
   <?> "term"

intConst :: C0Parser Integer
intConst = try hex <|> dec
           <?> "integer constant"

dec :: C0Parser Integer
dec = (do string "0"
          whiteSpace
          return 0)
      <|>
      (do n <- natural
          return n)
      <?> "decimal"

hex :: C0Parser Integer
hex = do { string "0"
         ; h <- hexadecimal
         ; whiteSpace
         ; return h
         } <?> "hexadecimal"

c0Def :: GenLanguageDef ByteString () Identity
c0Def = LanguageDef
   {commentStart    = "/*",
    commentEnd      = "*/",
    commentLine     = "//", -- string "#" <|> string "//",
    nestedComments  = True,
    identStart      = letter <|> char '_',
    identLetter     = alphaNum <|> char '_',
    opStart         = oneOf "=+-*/%&^|<>!~",
    opLetter        = oneOf "=&|<>",
    reservedNames   = ["int", "char", "string", "void", "while", "for", "if",
                       "return", "break", "continue", "NULL", "alloc",
                       "alloc_array", "typedef", "struct", "else", "assert",
                       "true", "false", "bool"],
    reservedOpNames = ["+",  "*",  "-",  "/",  "%", "?", ":", "->", "."],
    caseSensitive   = True}

c0Tokens :: Tok.GenTokenParser ByteString () Identity
c0Tokens = Tok.makeTokenParser c0Def

symbol     :: String -> C0Parser String
symbol     = Tok.symbol c0Tokens
reserved   :: String -> C0Parser ()
reserved   = Tok.reserved   c0Tokens
comma      :: C0Parser ()
comma      = do _ <- Tok.comma c0Tokens; return ()
semi       :: C0Parser ()
semi       = do _ <- Tok.semi c0Tokens; return ()
identifier :: C0Parser String
identifier = Tok.identifier c0Tokens
operator   :: C0Parser String
operator   = Tok.operator   c0Tokens
braces     :: C0Parser a -> C0Parser a
braces     = Tok.braces     c0Tokens
parens     :: C0Parser a -> C0Parser a
parens     = Tok.parens     c0Tokens
reservedOp :: String -> C0Parser ()
reservedOp = Tok.reservedOp c0Tokens
natural    :: C0Parser Integer
natural    = Tok.natural    c0Tokens
hexadecimal :: C0Parser Integer
hexadecimal = Tok.hexadecimal c0Tokens
whiteSpace :: C0Parser ()
whiteSpace = Tok.whiteSpace c0Tokens
commaSep   :: C0Parser a -> C0Parser [a]
commaSep   = Tok.commaSep c0Tokens
semiSep    :: C0Parser a -> C0Parser [a]
semiSep    = Tok.semiSep c0Tokens
brackets   :: C0Parser a -> C0Parser a
brackets   = Tok.brackets c0Tokens

opTable :: [[Operator ByteString () Identity Expr]]
opTable = [[prefix "-"   (ExprUnOp  Neg)],
           [binary "*"   (ExprBinOp Mul)  AssocLeft,
            binary "/"   (ExprBinOp Div)  AssocLeft,
            binary "%"   (ExprBinOp Mod)  AssocLeft],
           [binary "+"   (ExprBinOp Add)  AssocLeft,
            binary "-"   (ExprBinOp Sub)  AssocLeft]]
{-
We used a few helper functions which are in the Parsec documentation of Text.Parsec.Expr, located at \url{http://hackage.haskell.org/packages/archive/parsec/3.1.0/doc/html/Text-Parsec-Expr.html} The functions ``binary'', ``prefix'', and ``postfix'' were taken from there and are not my work, however they are used because rewriting them would look much the same, and they do not provide any core functionality, just make my code easier to read. Type signatures and location annotations were added by me.
-}
binary :: String -> (a -> a -> SourcePos -> a) -> Assoc -> Operator ByteString () Identity a
binary  name f = Infix $ do pos <- getPosition
                            reservedOp name
                            return $ \x y -> f x y pos
prefix :: String -> (a -> SourcePos -> a) -> Operator ByteString () Identity a
prefix  name f = Prefix $ do pos <- getPosition
                             reservedOp name
                             return $ \x -> f x pos
