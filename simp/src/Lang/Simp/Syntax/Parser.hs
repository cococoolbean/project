{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Lang.Simp.Syntax.Parser where


import Lang.Simp.Syntax.Parsec
import Lang.Simp.Syntax.SrcLoc
import Lang.Simp.Syntax.AST ( Const(..), Exp, Var(..), Stmt(..) )
import Lang.Simp.Syntax.Lexer

-- | The `PEnv` datatype defines a parser environment
newtype PEnv = PEnv {
    toks :: [LToken]
}

-- | check whether the parsing is done based on the list of tokens left. 
done :: PEnv -> Bool
done penv = null $ toks penv

instance ParserEnv PEnv LToken where
    getCol penv = case toks penv of
        { [] -> -1
        ; (tok:_) -> case srcLoc tok of
            { SrcLoc ln col -> col }
        }
    getLine penv = case toks penv of
        { [] -> -1
        ; (tok:_) -> case srcLoc tok of
            { SrcLoc ln col -> ln }
        }
    setTokens ts penv = penv{toks = ts}
    setLine ln penv = penv --  "setLine for PEnv has no write permission."
    setCol col penv = penv --  "setCol for PEnv ihas no write permission."
    isNextTokNewLine penv = case getTokens penv of
           { ((WhiteSpace _ c):_) ->  c == '\n'
           ; _ -> False
           }
    getTokens = toks


-- | The function `parser` is the top level parsing function; 
parser :: Parser PEnv [Stmt]
parser = pStmts

-- | The function `pStmts` parses zero or more statements separated by some spaces.
pStmts :: Parser PEnv [Stmt]
pStmts = many pOne
    where
        pOne = do
        { pSpaces
        ; s <- pStmt
        ; pSpaces
        ; return s
        }

-- | The function `pStmt` parses one statement 
pStmt :: Parser PEnv Stmt
pStmt = choice pAssign (choice pRet (choice pNop (choice pIfElse pWhile)))

-- | The function `pNop` parses a nop statement 
pNop :: Parser PEnv Stmt
pNop = do
    _ <- sat (\tok -> case tok of { NopKW src -> True ; _ -> False}) "expecting a NOP keyword but none is found."
    pSpaces
    pSemiColon
    return Nop

-- | The function `pAssign` parses an assignment statement 
pAssign :: Parser PEnv Stmt
pAssign = do
    x <- pVar
    pSpaces
    pEqual
    pSpaces
    e <- pExp
    pSpaces
    pSemiColon
    return (Assign x e)

-- | The function `pRet` parses a return statement
pRet :: Parser PEnv Stmt
pRet = do
    pReturnKW
    pSpaces
    x <- pVar
    pSpaces
    pSemiColon
    return (Ret x)


-- | The function`pIfElse` parses an if-else statement
pIfElse :: Parser PEnv Stmt
pIfElse = do
    pIfKW
    pSpaces
    e <- pExp
    pSpaces
    pLBrace
    s1 <- pStmts
    pRBrace
    pSpaces
    pElseKW
    pSpaces
    pLBrace
    s2 <- pStmts
    pRBrace
    return (If e s1 s2)

pWhile :: Parser PEnv Stmt
pWhile = do
    pWhileKW
    pSpaces
    e <- pExp
    pLBrace
    s <- pStmts
    pRBrace
    return (While e s)


-- Lab 1 Task 1.1 

-- | the `pSpace` function parses / skips a space token
pSpace :: Parser PEnv LToken
pSpace = sat (\x -> case x of
                (WhiteSpace _ _) -> True
                _ -> False
            ) "Expecting one whitespace!"

-- | The `pSpaces` function parses / skips zero or more space tokens
pSpaces :: Parser PEnv [LToken]
pSpaces = many pSpace

-- Lab 1 Task 1.1 end 


{-  Lab 1 Task 1.2 
    Parsing an expression
    Note that 
    E ::= E Op E | X | C | (E) contains left recursion
-}

{- 
    << Left Recursion Eliminated >>
    << Grammar >>
    E  = X E'
    E  = C E'
    E  = (E) E'
    E' = OP E E'
    E' = Epsilon
-}

data ExpLE = VarExp Var ExpLEP
    | ConstExp Const ExpLEP
    | ParenExp ExpLE ExpLEP
    deriving (Show, Eq)

data ExpLEP = Plus ExpLE ExpLEP
    | Minus ExpLE ExpLEP
    | Mult ExpLE ExpLEP
    | DEqual ExpLE ExpLEP
    | LThan ExpLE ExpLEP
    | Epsilon 
    deriving (Show, Eq)


-- | The `pExp` function parses an expression
pExp :: Parser PEnv Exp
pExp = do
    ele <- pExpLE
    return (fromExpLE ele)


pExpLE :: Parser PEnv ExpLE
pExpLE = do
    x  <- pTerm
    e' <- pExpP
    return (ExpLE x e')


pExpP :: Parser PEnv ExpLEP
pExpP = do
    result <- optional pOpExpP
    case result of 
        Left _ -> empty
        Right e -> return e

pOpExpP :: Parser PEnv ExpLEP
pOpExpP = do
    op <- pOp 
    e  <- pExp
    e' <- pExpP
    return (ExpLEP e e')

-- | The `pTerm` function parses either a variable (X), constant (C), or a parenthesized expression `(E)`
pTerm :: Parser PEnv ExpLEP
pTerm = do
    result <- optional pX
    case result of
        Right x -> return x
        Left _ -> do
            resultC <- optional pC
            case resultC of
                Right c -> return c
                Left _ -> pParenExp

pX :: Parser PEnv ExpLE
pX = do
    x <- pVariable
    return (VarExp x Epsilon)

pC :: Parser PEnv ExpLE
pC = do
    c <- pConstant
    return (ConstExp c Epsilon)

pParenExp :: Parser PEnv Exp
pParenExp = do
    _ <- pLeftParenTok
    e <- pExpLE 
    _ <- pRightParenTok
    return (ParenExp e)

pLeftParenTok :: Parser PEnv LToken
pLeftParenTok = sat (\x -> case x of
    {
        LParen v  -> True;
        _         -> False 
    }) "pLeftParenTok failed, expecting '('."

pRightParenTok :: Parser PEnv LToken
pRightParenTok = sat (\x -> case x of
    {
        RParen v  -> True;
        _         -> False 
    }) "pRightParenTok failed, expecting ')'."

pVariable :: Parser PEnv Exp
pVariable = do
    x <- pVar
    return (VarExp x) 

pConstant :: Parser PEnv Exp
pConstant = do
    c <- pConst
    return (ConstExp c)

pOp :: Parser PEnv Exp
pOp = sat (\x -> case x of
    PlusSign v      -> True
    MinusSign v     -> True
    AsterixSign v   -> True
    _               -> False
    )


-- fromExpLE :: ExpLE -> Exp
-- fromExpLE (VarExp var expLEP)     = fromExpLEP (VarExp var) expLEP  
-- fromExpLE (ConstExp const expLEP) = fromExpLEP (ConstExp const) expLEP 
-- fromExpLE (ParenExp expLE expLEP) = fromExpLEP (ParenExp (fromExpLE expLEP)) expLEP


-- fromExpLEP :: ExpLE -> ExpLEP -> ExpLE
-- fromExpLEP exp Epsilon          = exp  -- No more expression, return the current expression
-- fromExpLEP exp (Plus e eP)      = fromExpLEP (Plus exp e) eP     -- '+'
-- fromExpLEP exp (Minus e eP)     = fromExpLEP (Minus exp e) eP    -- '-'
-- fromExpLEP exp (Mult e eP)      = fromExpLEP (Mult exp e) eP     -- '*'
-- fromExpLEP exp (DEqual e eP)    = fromExpLEP (DEqual exp e) eP   -- '=='
-- fromExpLEP exp (LThan e eP)     = fromExpLEP (LThan exp e) eP    -- '<'



-- Lab 1 Task 1.2 end 


pPlus :: Parser PEnv LToken
pPlus = sat (\tok -> case tok of
    { PlusSign _ -> True
    ; _ -> False
    }) "expecting a + token but none is found."


pMinus :: Parser PEnv LToken
pMinus = sat (\tok -> case tok of
    { MinusSign _ -> True
    ; _ -> False
    }) "expecting a - token but none is found."


pMult :: Parser PEnv LToken
pMult = sat (\tok -> case tok of
    { AsterixSign _ -> True
    ; _ -> False
    }) "expecting a * token but none is found."


pLThan :: Parser PEnv LToken
pLThan = sat (\tok -> case tok of
    { LThanSign _ -> True
    ; _ -> False
    }) "expecting a < token but none is found."


pDEqual :: Parser PEnv LToken
pDEqual = sat (\tok -> case tok of
    { DEqSign _ -> True
    ; _ -> False
    }) "expecting a == token but none is found."


pEqual :: Parser PEnv LToken
pEqual = sat (\tok -> case tok of
    { EqSign _ -> True
    ; _ -> False
    }) "expecting a = token but none is found."



pVar :: Parser PEnv Var
pVar = do
    tok <- sat (\tok -> case tok of
        { IdTok src v -> True
        ; _ -> False
        }) "expecting an identifier but none is found."
    name <- justOrFail tok (\t-> case t of
        { IdTok src v -> Just v
        ; _ -> Nothing
        }) "expecting an identifier but none is found."
    return (Var name)

pConst :: Parser PEnv Const
pConst = choice pTrue (choice pFalse pInt)


pTrue :: Parser PEnv Const
pTrue = do
    tok <- sat (\tok -> case tok of
        { TrueKW src -> True
        ; _ -> False
        }) "expecting a true keyword but none is found."
    return (BoolConst True)



pFalse :: Parser PEnv Const
pFalse = do
    tok <- sat (\tok -> case tok of
        { FalseKW src -> True
        ; _ -> False
        }) "expecting a false keyword but none is found."
    return (BoolConst False)


pInt :: Parser PEnv Const
pInt = do
    tok <- sat (\tok -> case tok of
        { IntTok src v -> True
        ; _ -> False
        }) "expecting an integer but none is found."
    i <- justOrFail tok (\t -> case t of
        { IntTok srv v -> Just v
        ; _ -> Nothing
        }) "expecting an integer but none is found."
    return (IntConst i)


-- parsing keywords 

pReturnKW :: Parser PEnv LToken
pReturnKW = sat (\tok -> case tok of
    { RetKW src -> True
    ; _ -> False
    }) "expecting a return keyword but none is found."

pIfKW :: Parser PEnv LToken
pIfKW = sat (\tok -> case tok of
    { IfKW src -> True
    ; _ -> False
    }) "expecting an if keyword but none is found."

pElseKW :: Parser PEnv LToken
pElseKW = sat (\tok -> case tok of
    { ElseKW src -> True
    ; _ -> False
    }) "expecting an else keyword but none is found."


pWhileKW :: Parser PEnv LToken
pWhileKW = sat (\tok -> case tok of
    { WhileKW src -> True
    ; _ -> False
    }) "expecting a while keyword but none is found."


-- parsing symbols

pLBrace :: Parser PEnv LToken
pLBrace = sat (\tok -> case tok of
    { LBrace src -> True
    ; _ -> False
    }) "expecting a { but none is found."



pRBrace :: Parser PEnv LToken
pRBrace = sat (\tok -> case tok of
    { RBrace src -> True
    ; _ -> False
    }) "expecting a } but none is found."


pLParen :: Parser PEnv LToken
pLParen = sat (\tok -> case tok of
    { LParen src -> True
    ; _ -> False
    }) "expecting a ( but none is found."



pRParen :: Parser PEnv LToken
pRParen = sat (\tok -> case tok of
    { RParen src -> True
    ; _ -> False
    }) "expecting a ) but none is found."




pSemiColon = sat (\tok -> case tok of
    { SemiColon src -> True
    ; _ -> False
    }) "expecting a ; but none is found."
