module Violet.Parser

import Data.List
import Text.Parser.Core
import Text.Parser
import Text.Parser.Expression

import Violet.Surface.Lexer
import public Violet.Surface.Syntax
import public Violet.Surface.Error

-- `Grammar state tok consumes ty`
-- * `state` can be defined, but didn't be used here
-- * `tok` is the token type of the language
-- * `consumes` flag is True if the language is guaranteed to consume some input on success
-- * `ty` is the returned type of parser
public export
Rule : (ty : Type) -> Type
Rule = Grammar () VToken True

tmU : Rule Raw
tmU = match VTUniverse $> RU

tmVar : Rule Raw
tmVar = RVar <$> match VTIdentifier

tmHole : Rule Raw
tmHole = do
	match VTQuestionMark
	RHole <$> match VTIdentifier

parens : Rule a -> Rule a
parens p = match VTOpenP *> p <* match VTCloseP
braces : Rule a -> Rule a
braces p = match VTOpenB *> p <* match VTCloseB

mutual
	atom : Rule Raw
	atom = tmHole <|> tmU <|> tmVar <|> (parens tm)

	spine : Rule Raw
	spine = foldl1 RApp <$> some atom

	expr : Rule Raw
	expr = buildExpressionParser [
		-- dollar sign can use to avoid parentheses
		-- the following expressions are same
		-- `a $ b c d` <=> `a (b c d)`
		-- except the arrow symbol, it's the lowest
		-- since it's about to reorganize the code
		[ Infix (RApp <$ match VTDollar) AssocRight ],
		-- To be convience, thus, arrow is the lowsest symbol
		-- For example, user might write `List a -> List b`, here `List a` is an application
		[ Infix (RPi Explicit "_" <$ match VTArrow) AssocRight ]
	] (spine <|> atom)

	tm : Rule Raw
	tm = do
		r <- bounds (tmLet <|> tmElim <|> tmLam <|> tmImplicitPi <|> tmPi <|> expr)
		pure $ RSrcPos r

	-- λ A x => x
	tmLam : Rule Raw
	tmLam = do
		match VTLambda
		names <- some $ match VTIdentifier
		match VTLambdaArrow
		body <- tm
		pure $ foldr RLam body names

	-- (a : A) -> b
	tmPi : Rule Raw
	tmPi = do
		match VTOpenP
		name <- match VTIdentifier
		match VTColon
		a <- tm
		match VTCloseP
		match VTArrow
		RPi Explicit name a <$> tm

	-- {a : A} -> b
	tmImplicitPi : Rule Raw
	tmImplicitPi = do
		match VTOpenB
		name <- match VTIdentifier
		match VTColon
		a <- tm
		match VTCloseB
		match VTArrow
		RPi Implicit name a <$> tm

	-- let x : a = t; u
	tmLet : Rule Raw
	tmLet = do
		match VTLet
		name <- match VTIdentifier
		match VTColon
		a <- tm
		match VTAssign
		t <- tm
		match VTSemicolon
		u <- tm
		pure $ RLet name a t u

	patRule : Rule (List1 Name)
	patRule = some (match VTIdentifier)
	caseRule : Rule (List (List1 Name), Raw)
	caseRule = do
		match VTVerticalLine
		ps <- sepBy (match VTComma) patRule
		match VTLambdaArrow
		(ps,) <$> tm
	-- elim n
	-- | C x => x
	-- | z => z
	tmElim : Rule Raw
	tmElim = do
		match VTElim
		pure $ RElim !(sepBy (match VTComma) tm) !(many caseRule)

ttmData : Rule TopLevelRaw
ttmData = do
	match VTData
	name <- match VTIdentifier
	caseList <- many pCase
	pure $ TData name [] caseList
	where
		pCase : Rule (Name, List RTy)
		pCase = do
			match VTVerticalLine
			name <- match VTIdentifier
			a <- many tm
			pure (name, a)

-- def x (xi : Ti) : T =>
--   u
ttmDef : Rule TopLevelRaw
ttmDef = do
	match VTDef
	name <- match VTIdentifier
	raw_tele <- many $ try (parens (bindGroup Explicit)) <|> (braces (bindGroup Implicit))
	match VTColon
	a <- tm
	match VTLambdaArrow
	t <- tm
	pure $ TDef name (transform raw_tele) a t
	where
		transform : List (List (Mode, Name, RTy)) -> RTelescope
		transform nns = foldl (\x, scm => x ++ scm) [] nns

		bindGroup : Mode -> Rule (List (Mode, Name, RTy))
		bindGroup mode = do
			ns <- some $ match VTIdentifier
			match VTColon
			ty <- tm
			pure $ map (\n => (mode, n, ty)) $ forget ns

ttm : Rule TopLevelRaw
ttm = TSrcPos <$> bounds (ttmData <|> ttmDef)

export
ruleTm : Rule Raw
ruleTm = tm

export
ruleModule : Rule ModuleRaw
ruleModule = do
	match VTModule
	name <- match VTIdentifier
	bindings <- many ttm
	pure $ MkModuleRaw (MkModuleInfoRaw name) bindings

export
parseViolet : Rule a -> String -> Either PError a
parseViolet rule source =
	case lexViolet source of
		Just toks => parseTokens toks source
		Nothing => Left LexFail
	where
		parseTokens : List (WithBounds VToken) -> (source : String) -> Either PError a
		parseTokens toks source =
			let toks' = filter (not . ignored) toks
			in case parse rule toks' of
				Right (l, []) => Right l
				Right (_, leftTokens) => Left $ TokensLeave source leftTokens
				Left es => Left $ CollectPError $ map (\(Error msg bounds) => SinglePError source bounds msg) $ forget es
			where
				ignored : WithBounds VToken -> Bool
				ignored (MkBounded (Tok VTIgnore _) _ _) = True
				ignored _ = False
