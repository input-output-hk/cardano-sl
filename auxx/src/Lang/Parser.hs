{-# LANGUAGE RecursiveDo #-}

module Lang.Parser
       ( parse
       , ParseError(..)
       ) where

import           Universum

import           Control.Applicative.Combinators.NonEmpty (sepBy1)
import           Control.Lens (Getting)
import           Data.Loc (Span)
import           Data.Monoid (First)
import           Text.Earley (Grammar, Parser, Prod, Report, fullParses, parser, rule, terminal,
                              (<?>))

import           Lang.Lexer (BracketSide, Token, getFilePath', tokenize, _BracketSideClosing,
                             _BracketSideOpening, _TokenAddress, _TokenBlockVersion, _TokenFilePath,
                             _TokenHash, _TokenKey, _TokenName, _TokenNumber, _TokenParenthesis,
                             _TokenPublicKey, _TokenSemicolon, _TokenSoftwareVersion,
                             _TokenStakeholderId, _TokenString)
import           Lang.Name (Name)
import           Lang.Syntax (Arg (..), Expr (..), Lit (..), ProcCall (..))

tok :: Getting (First a) Token a -> Prod r e (s, Token) a
tok p = terminal (preview $ _2 . p)

inBrackets
    :: Getting (First ()) Token BracketSide
    -> Prod r e (s, Token) a
    -> Prod r e (s, Token) a
inBrackets p r =
    tok (p . _BracketSideOpening) *> r <* tok (p . _BracketSideClosing)

gExpr :: Grammar r (Prod r Text (s, Token) (Expr Name))
gExpr = mdo
    ntName <- rule $ tok _TokenName
    ntKey <- rule $ tok _TokenKey
    ntExprLit <- rule $ ExprLit <$> asum
        [ LitNumber <$> tok _TokenNumber
        , LitString <$> tok _TokenString
        , LitAddress <$> tok _TokenAddress
        , LitPublicKey <$> tok _TokenPublicKey
        , LitStakeholderId <$> tok _TokenStakeholderId
        , LitHash <$> tok _TokenHash
        , LitBlockVersion <$> tok _TokenBlockVersion
        , LitSoftwareVersion <$> tok _TokenSoftwareVersion
        , LitFilePath . getFilePath' <$> tok _TokenFilePath
        ] <?> "literal"
    ntArg <- rule $ asum
        [ ArgKw <$> ntKey <*> ntExprAtom
        , ArgPos <$> ntExprAtom
        ] <?> "argument"
    ntExpr1 <- rule $ asum
        [ ExprProcCall <$> ntProcCall
        , ntExprAtom
        , pure ExprUnit
        ] <?> "expression"
    ntExpr <- rule $ ExprGroup <$> ntExpr1 `sepBy1` tok _TokenSemicolon
    ntProcCall <- rule $ ProcCall <$> ntName <*> some ntArg <?> "procedure call"
    ntProcCall0 <- rule $
        (\name -> ExprProcCall $ ProcCall name []) <$> ntName
        <?> "procedure call w/o arguments"
    ntExprAtom <- rule $ asum
        [ ntExprLit
        , ntProcCall0
        , inBrackets _TokenParenthesis ntExpr <?> "parenthesized expression"
        ] <?> "atom"
    return ntExpr

pExpr :: Parser Text [(s, Token)] (Expr Name)
pExpr = parser gExpr

data ParseError = ParseError
    { peSource :: Text
    , peReport :: Report Text [(Span, Token)]
    }
    deriving (Eq, Show)

parse :: Text -> Either ParseError (Expr Name)
parse str = first (ParseError str) . toEither . fullParses pExpr . tokenize $ str
  where
    toEither = \case
      ([] , r) -> Left r
      (a:_, _) -> Right a
