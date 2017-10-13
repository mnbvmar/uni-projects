-- Main module for typing the program.
-- The program is typed using a slightly modified Hindley-Milner inference
-- algorithm. It allows the recursive and polymorphic types.
--
-- Short sketch of the algorithm:
--   * We read the definitions and their already defined types and then
--      store them in the environment.
--   * The environment holds the types in the following format:
--      \forall [t1, t2, ..., tn] (type). To use the type, change each
--      "forall" type into a fresh variable. To store the type in
--      the environment, find the free variables and then put them in "forall".
--   * The monad consists of three parts: Except (error handling),
--      Reader (environment - types of variables, defined datatypes and
--      their constructors) and State (number of variables and current
--      substitutions). It also allows adding more substitutions, creating
--      new type variables and so on.
--   * Each 'standard' construct (lambda, let, application, ...) is handled
--      almost exactly like in default H-M description;
--      the substitutions are released globally into the state and are not
--      returned with the unification.
--   * Type unification checks for occur checks.
--   * To type a single 'case' option, we recursively find the types for
--      each subexpression and then turn them into the function returning
--      the data type. Then try to unify it with the analoguous function
--      describing the data constructor.
--   * Moreover, each 'case' expression is checked for its exhaustiveness.
--   * To type multiple definitions, create a fresh variable type for each
--      of the definitions and then type the definitions one by one.
--      If it's impossible to type the expressions, the unification
--      should fail.

{-# LANGUAGE FlexibleContexts #-}

module Types (tryInferProgram, InferenceError) where


import AbsLang
import PrintLang
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
-- import Debug.Trace


-- "for-all" type.
data ForAllType = ForAll [IdentVar] Type deriving (Eq, Ord, Show, Read)
-- Environment is a mapping from variables, constructor types and constructors
-- into their "for-all" types.
type VarMap = M.Map IdentVar ForAllType
type ConsMap = M.Map IdentCons ForAllType
type ConsListMap = M.Map IdentCons [(IdentCons, Int)]
data Env = Env { vars :: VarMap
               , cons :: ConsMap
               , consFuncs :: ConsMap
               , consFuncsList :: ConsListMap
               } deriving Show
-- The state is (number of variables, current substitution).
type TypeState = (Int, Substitution)
-- The error is held in a String.
type InferenceError = String
type InferenceM a = ExceptT InferenceError (ReaderT Env (State TypeState)) a
type Substitution = M.Map IdentVar Type


-- Tries to infer the types in the whole program. It sweeps through the
-- whole program, looking for constructors, definitions and declarations.
-- Then, runs the main inference.
tryInferProgram :: Prog -> Either InferenceError ()
tryInferProgram (Prog d) =
  let m1 = runExceptT (tryInferProgramM (Prog d))
      m2 = runReaderT m1 $ Env M.empty M.empty M.empty M.empty
      m3 = runState m2 (0, M.empty)
  in fst m3

tryInferProgramM :: Prog -> InferenceM ()
tryInferProgramM (Prog d) = do
  (cons, consFuncs, consFuncsList) <- foldM addConsM (M.empty, M.empty, M.empty) d
  defTypes <- foldM addDeclToMapM M.empty d
  defTypes <- foldM addDefM defTypes d
  let decls = reverse $ foldl addDecl [] d
      env = Env {
              vars = defTypes,
              cons = cons,
              consFuncs = consFuncs,
              consFuncsList = consFuncsList
            }
  local (const env) $ procDecls decls

-- Generalization of the type (independent of the environment, or more
-- precisely, assuming the environment is empty). It's the list of free
-- variables in the type.
generalizeType :: Type -> ForAllType
generalizeType t = ForAll (S.toList $ freeVars t) t


-- Finds a non-unique element in list, if there is any.
findNonUnique :: Ord t => [t] -> Maybe t
findNonUnique lst = findNonUnique' lst S.empty
  where findNonUnique' [] _ = Nothing
        findNonUnique' (t:ts) prefix = if S.member t prefix
                                       then Just t
                                       else findNonUnique' ts (S.insert t prefix)

-- Asserts that the variables defined in the constructor context are unique.
-- It disallows the constructions like
--   data X [a, a] = ...;
-- Here variable 'a' is repeated.
assertIdentsUnique :: [String] -> InferenceM ()
assertIdentsUnique ids =
  case (findNonUnique ids) of
    Nothing -> return ()
    Just x -> throwError $ "Type variable '" ++ x ++ "' repeated"

-- Asserts that the type variable 'id' is known in the constructor context.
assertHasIdent :: S.Set String -> String -> InferenceM ()
assertHasIdent knownIds id =
  if S.member id knownIds
  then return ()
  else throwError $ "Type variable '" ++ id ++ "' not found"

-- Asserts that each type variable is known in the constructor context.
-- It disallows the constructions like
--   data X [a] = Y [b];
-- where variable 'b' is unknown.
assertTypeHasIdents :: S.Set String -> Type -> InferenceM ()
assertTypeHasIdents knownIds (TCons _) = return ()
assertTypeHasIdents knownIds (TVar (IdentVar x)) = assertHasIdent knownIds x
assertTypeHasIdents knownIds (TApp t1 t2) = do
  assertTypeHasIdents knownIds t1
  assertTypeHasIdents knownIds t2
assertTypeHasIdents knownIds (TFun t1 t2) = do
  assertTypeHasIdents knownIds t1
  assertTypeHasIdents knownIds t2


-- A helper function adding the constructors into the environment.
-- For example,
-- the constructor List [a] = Nil [] | Elem[a, List a] is turned into
-- data type List a. Additionally, the functions Nil :: List a and
-- Elem :: a -> List a -> List a are created.
addConsM :: (ConsMap, ConsMap, ConsListMap) -> Def ->
            InferenceM (ConsMap, ConsMap, ConsListMap)
addConsM (cons, consFuncs, consFuncList) (DefCons ident vars options) =
  addContext ("when processing constructor " ++ printTree ident) $ do
    let typeVars = map (\(IdentVar x) -> x) vars
        typeVarsSet = S.fromList typeVars
    assertIdentsUnique typeVars
    mapM_ (\(Cons _ types) -> mapM_ (assertTypeHasIdents typeVarsSet) types) options
    let consType = foldl (\cur nxt -> TApp cur (TVar nxt)) (TCons ident) vars
        getFunc (Cons idCons args) = (idCons, (ForAll vars $ foldr TFun consType args))
        newFuncs = M.fromList $ map getFunc options
        getOptionsList = map (\(Cons idCons args) -> (idCons, length args)) options

    return (M.insert ident (ForAll vars consType) cons,
            M.union consFuncs newFuncs,
            M.insert ident getOptionsList consFuncList)
addConsM c _ = return c

-- A helper function adding the type definitions into the environment.
-- It generalizes the provided type and puts it in the env.
addDefM :: VarMap -> Def -> InferenceM VarMap
addDefM vars (DefType ident t) =
  addContext ("when processing definition " ++ printTree ident) $ do
    if M.member ident vars
    then return $ M.insert ident (generalizeType t) vars
    else throwError $ "Type of '" ++ printTree ident ++ "' provided, but undefined"
addDefM vars _ = return vars

-- A helper function turning the declarations into the map of their types.
-- Each declaration is assigned a temporary type (forall x. x), which then
-- is to be unified.
addDeclToMapM :: VarMap -> Def -> InferenceM VarMap
addDeclToMapM vars (DefDecl (Decl ident _ _)) =
  addContext ("when processing declaration " ++ printTree ident) $ do
    if M.member ident vars
    then throwError $ "Identifier '" ++ printTree ident ++ "' redeclared"
    else return $ M.insert ident (ForAll [(IdentVar "_x")] (TVar $ IdentVar "_x")) vars
addDeclToMapM vars _ = return vars

-- A helper function adding the declarations into the environment.
addDecl :: [(IdentVar, Exp)] -> Def -> [(IdentVar, Exp)]
addDecl decls (DefDecl (Decl ident args body)) =
  (ident, ELam args body):decls
addDecl decls _ = decls

-- A main monadic function processing the list of expressions along with
-- their identifiers and trying to infer their types. For each declaration,
-- it fetches its current type, gets the type of its corresponding
-- expression and then tries to process the remaining expression.
procDecls :: [(IdentVar, Exp)] -> InferenceM ()
procDecls [] = return () -- do

procDecls ((ident, body):decls) = do
  bodyType <- addContext
              ("when typing named expression '" ++ printTree ident ++ "'") $ do {
    vars <- askVars;
    identType <- getInstance $ vars M.! ident;
    bodyType <- typeExp body;
    unifyTypes body identType bodyType;
    applySubstM bodyType;
  }
    -- Remember to add the information about the newly found type.
  replaceVars (M.insert ident (generalizeType bodyType)) (procDecls decls)


-- Adds a fresh type variable to the environment.
newVar :: InferenceM Type
newVar = do
  (nvars, subst) <- get
  put (nvars + 1, subst)
  return $ TVar (IdentVar ("_t" ++ show nvars))


-- Puts a substitution into the environment.
addSubst :: Substitution -> InferenceM ()
addSubst s = do
  (nvars, subst) <- get
  put (nvars, mergeSubst subst s)
  return ()

-- Empty substitution (no changes done).
emptySubst :: Substitution
emptySubst = M.empty

-- Applies the substitution to the type. Should always succeed,
-- assuming the substitutions are correct.
applySubst :: Substitution -> Type -> Type
applySubst _ t@(TCons _) = t
applySubst s t@(TVar x) = case M.lookup x s of
  Just t' -> t'
  Nothing -> t
applySubst s (TApp f x) = TApp (applySubst s f) (applySubst s x)
applySubst s (TFun x f) = TFun (applySubst s x) (applySubst s f)

-- Merges two substitutions into one.
mergeSubst :: Substitution -> Substitution -> Substitution
mergeSubst sEarlier sLater = M.union sEarlier' sLater
  where sEarlier' = M.map (applySubst sLater) sEarlier


-- A helper function returning a non-generalized type.
singleType :: Type -> ForAllType
singleType = ForAll []


-- Function returning the free variables in the expression. A variable
-- is free iff it occurs at least once in the expression.
freeVars :: Type -> S.Set IdentVar
freeVars (TCons _) = S.empty
freeVars (TVar x) = S.singleton x
freeVars (TApp f x) = S.union (freeVars f) (freeVars x)
freeVars (TFun x f) = S.union (freeVars f) (freeVars x)



-- Applies the substitutions to the "for-all" functions. The extra
-- care is needed as we can't substitute the generalized variables.
applySubstForAll :: Substitution -> ForAllType -> ForAllType
applySubstForAll s (ForAll xs t) = ForAll xs (applySubst reducedS t)
  where reducedS = foldl (\m name -> M.delete name m) s xs


-- The monadic version of the applySubst function. Reads the current
-- set of substitutions and applies it to the current type.
applySubstM :: Type -> InferenceM Type
applySubstM t = do
  (_, subst) <- get
  return (applySubst subst t)


-- Gets the instance of the "for-all" type with its "for-all" variables
-- changed into the fresh variables.
getInstance :: ForAllType -> InferenceM Type
getInstance (ForAll vars t) = do
  freeTypes <- mapM (const newVar) vars
  let subst = M.fromList (zip vars freeTypes)
  return (applySubst subst t)

-- Generalizes a type by changing its free (in the current environment)
-- variables into the "for-all" variables.
getGeneralization :: Type -> InferenceM ForAllType
getGeneralization t = do
  vars <- askVars
  let fvars = freeVars t
      envFreeVars = S.unions (map (\(ForAll xs _) -> S.fromList xs) (M.elems vars))
      useVars = S.difference fvars envFreeVars
  return (ForAll (S.toList useVars) t)


-- A helper function replacing the variable set in the environment
-- and then running a monadic function.
replaceVars :: (VarMap -> VarMap) -> InferenceM a -> InferenceM a
replaceVars f m = local (\(Env vars c1 c2 c3) -> Env (f vars) c1 c2 c3) m

-- A helper function returning the current variables.
askVars :: InferenceM VarMap
askVars = do
  Env vars _ _ _ <- ask
  return vars

-- A helper function replacing all the environment types using the available
-- substitutions and then running a monadic function.
runWithSubstitution :: InferenceM a -> InferenceM a
runWithSubstitution func = do
  (_, s) <- get
  replaceVars (M.map $ applySubstForAll s) func


-- A helper function checking if the occur check occured. It happens
-- if the substitution for 't' contains 't' in its body.
occurCheck :: IdentVar -> Type -> Bool
occurCheck name t = S.member name (freeVars t)


-- Unification of the types. The substitutions are emitted to the state.
unifyTypes :: Print a => a -> Type -> Type -> InferenceM ()
unifyTypes e t1 t2 = addContext
  ("when unifying types '" ++ printTree t1 ++ "' and '" ++ printTree t2
                           ++ "' of expression '" ++ printTree e ++ "'")
  $ unifyTypesRec t1 t2

unifyTypesRec :: Type -> Type -> InferenceM ()

-- Unification of two constructors. It works only if they are the same.
unifyTypesRec (TCons x) (TCons y) = do
  when (x /= y) (throwError $ "Conflicting constructors: '" ++ printTree x
                               ++ "' and '" ++ printTree y ++ "'")
  return ()

-- Unification of a variable and something else. We have three cases:
--   * The variables are equal - we don't do anything.
--   * The occur check failed.
--   * Nothing of the above - a single substitution is emitted.
unifyTypesRec x@(TVar nx) y =
  if x == y
    then return ()
    else if occurCheck nx y
      then throwError $ "Occur check"
      else do
        (_, subst) <- get
        case M.lookup nx subst of
          Nothing -> addSubst $ M.singleton nx y
          Just t -> unifyTypesRec t y

unifyTypesRec x y@(TVar ny) = unifyTypesRec y x


-- Unification of the application of types or the function type. We
-- unify the corresponding subtypes.
unifyTypesRec (TApp f1 x1) (TApp f2 x2) = do
  unifyTypesRec f1 f2
  x1 <- applySubstM x1
  x2 <- applySubstM x2
  unifyTypesRec x1 x2

unifyTypesRec (TFun x1 f1) (TFun x2 f2) = do
  unifyTypesRec x1 x2
  f1 <- applySubstM f1
  f2 <- applySubstM f2
  unifyTypesRec f1 f2

-- Fallback - we obviously cannot match the types.
unifyTypesRec a b = throwError $ "Couldn't match '"
                              ++ printTree a
                              ++ "' with '"
                              ++ printTree b
                              ++ "'"


-- Checks the type of the expression.
typeExp :: Exp -> InferenceM Type

typeExp e = addExpressionContext e $ typeExpRec e


typeExpRec :: Exp -> InferenceM Type

-- Type of integer constant :: Int.
typeExpRec (EInt _) = return intType

-- Type of constructor. We can read it using the helper function.
typeExpRec (ECons name) = getConsFunType name

-- Type of variable. It must be in the environment.
typeExpRec (EVar name) = do
  vars <- askVars
  case (M.lookup name vars) of
    Nothing -> throwError $ "Variable '" ++ show name ++ "' not found"
    Just allT -> do
      s <- getInstance allT
      return s

-- Type of application. Refer to the desription of H-M system for details.
typeExpRec e@(EApp f x) = do
  typeF <- runWithSubstitution $ typeExp f
  typeX <- runWithSubstitution $ typeExp x
  typeRes <- newVar
  unifyTypes f (TFun typeX typeRes) typeF
  typeRes <- applySubstM typeRes
  return typeRes

-- Type of function. Refer to the desription of H-M system for details.
typeExpRec (ELam [] f) = typeExp f
typeExpRec (ELam (x:xs) f) = do
  typeArg <- newVar
  typeF <- replaceVars (M.insert x (singleType typeArg))
              $ runWithSubstitution $ typeExpRec (ELam xs f)
  typeArg <- applySubstM typeArg
  return (TFun typeArg typeF)

-- Type of let-construct. Refer to the desription of H-M system for details.
typeExpRec (ELet (Decl name args body) e) = do
  typeDecl <- newVar
  typeDecl' <- replaceVars (M.insert name (ForAll [] typeDecl))
                  $ runWithSubstitution $ typeExpRec (ELam args body)
  unifyTypes (ELam args body) typeDecl typeDecl'
  typeDecl' <- applySubstM typeDecl'
  generalType <- getGeneralization typeDecl'
  typeE <- replaceVars (M.insert name generalType)
              $ runWithSubstitution $ typeExp e
  return typeE

-- Type of the conditional. In "if a then b else c", we should have
-- type(a) == Bool and type(b) == type(c).
typeExpRec (EIf eCond eFalse eTrue) = do
  typeC <- runWithSubstitution $ typeExp eCond
  typeT <- runWithSubstitution $ typeExp eFalse
  typeF <- runWithSubstitution $ typeExp eTrue
  unifyTypes eFalse typeT typeF
  unifyTypes eCond typeC boolType
  resType <- applySubstM typeT
  return resType

-- Type of the const-list expression. Each element should have the same
-- type 't', and the resulting type is 'List a'.
typeExpRec (EList (ConstList exps)) = do
  t <- newVar
  mapM_ (\x -> typeExp x >>= (unifyTypes x t)) exps
  t <- applySubstM t
  return (TApp (TCons $ IdentCons "List") t)

-- Type of list operator application (:). In "a : b", if type(a) == t then
-- type(b) = List t. The resulting type is List t.
typeExpRec (EOp eLhs OpLst eRhs) = do
  typeL <- runWithSubstitution $ typeExp eLhs
  typeR <- runWithSubstitution $ typeExp eRhs
  unifyTypes (EApp (EApp (ECons $ IdentCons "Elem") eLhs) eRhs)
             (TApp (TCons $ IdentCons "List") typeL) typeR
  typeR <- applySubstM typeR
  return typeR

-- Other operators. Each of the operators is either Int -> Int -> Int
-- or Int -> Int -> Bool.
typeExpRec (EOp eLhs op eRhs) = do
  typeL <- runWithSubstitution $ typeExp eLhs
  typeR <- runWithSubstitution $ typeExp eRhs
  let (typeArgLhs, typeArgRhs, typeRes) = getOpType op
  unifyTypes eLhs typeL typeArgLhs
  unifyTypes eRhs typeR typeArgRhs
  typeRes <- applySubstM typeRes
  return typeRes

  where getOpType x = if (elem x [OpMul, OpDiv, OpAdd, OpSub])
                        then (intType, intType, intType)
                        else (intType, intType, boolType)


-- Type of the case-expression. For each binding case a -> b, the
-- argument should be unified with a and the result should be
-- unified with b.
typeExpRec (ECase e cases) = do
  typeE <- runWithSubstitution $ typeExp e
  typeRes <- newVar
  (typeRes, _) <- foldM addBindCase (typeRes, typeE) cases
  typeE <- applySubstM typeE
  verifyExhaustiveCase BindAny $ map (\(Bind binding _) -> binding) cases
  return typeRes


-- Helper function adding a single binding case in foldM. Takes
-- the current (result type, argument type) and processes it to
-- return the new types.
addBindCase :: (Type, Type) -> BindCase -> InferenceM (Type, Type)
addBindCase (tRes, tExpr) (Bind binding bindE) = do
  (typeBound, vars) <- runWithSubstitution $ procBinding binding
  tBindE <- replaceVars (M.union vars) $ runWithSubstitution $ typeExp bindE
  unifyTypes bindE tRes tBindE
  unifyTypes binding tExpr typeBound
  tBindE <- applySubstM tBindE
  typeBound <- applySubstM typeBound
  return (tBindE, typeBound)


-- A helper function turning a binding into a pair (type of argument,
-- the types of variables bound to this binding case).
procBinding :: Binding -> InferenceM (Type, VarMap)

-- Binding to anything (_). Can have any type of argument and binds to nothing.
procBinding BindAny = do
  t <- newVar
  return (t, M.empty)

-- Binding to the variable. Any type of argument, binds to this variable.
procBinding (BindVar name) = do
  t <- newVar
  return (t, M.singleton name (singleType t))

-- Binding to the constructor. The argument should match the constructor one,
-- and the bound variables are collected in the recursive calls.
procBinding (BindCons name binds) = do
  infos <- mapM procBinding binds
  let (types, envs) = unzip infos
  consType <- getConsType name
  consFunType <- getConsFunType name
  let bindType = foldr TFun consType types
  unifyTypes name bindType consFunType
  consType <- applySubstM consType

  let needLen = foldl (+) 0 (map M.size envs)
      finalEnv = M.unions envs
  if M.size finalEnv /= needLen
    then throwError "A variable cannot be bound more than once"
    else return (consType, finalEnv)


consFromConsFun :: IdentCons -> InferenceM IdentCons
consFromConsFun ident = do
  let recoverData t = case t of TFun _ res -> recoverData res
                                TApp res _ -> recoverData res
                                TCons res -> res

  identFunc <- getConsFunType ident
  return $ recoverData identFunc

-- Gets the type of a constructor type by its constructor name.
-- First, the constructor type name is recovered from the constructor type
-- (the constructor have types like a -> b -> c -> Cons d e f), and then
-- the environment is asked about this constructor type.
getConsType :: IdentCons -> InferenceM Type
getConsType ident = do
  dataIdent <- consFromConsFun ident
  Env _ eCons _ _ <- ask

  case M.lookup dataIdent eCons of
    Just t -> getInstance t
    Nothing -> throwError $ "Could not find type '" ++ printTree dataIdent ++ "'"


-- Gets the type of the constructor function type.
getConsFunType :: IdentCons -> InferenceM Type
getConsFunType ident = do
  Env _ _ eConsFun _ <- ask
  case M.lookup ident eConsFun of
    Just t -> getInstance t
    Nothing -> throwError $ "Could not find constructor '" ++ printTree ident ++ "'"



-- Exhaustiveness check.
-- To check if the 'case' expression exhaustively matches expression E, the following
-- algorithm is used:
--
--   1. Try to match E with any of the bindings on the list, starting from the
--       first.
--   2. If no bindings succeed, E is (trivially) not exhaustively matched.
--   3. If a binding is found whose substitutions only permute the variables, E
--       is (trivially) exhaustively matched.
--   4. If a binding is found which substitutes one of the variables (x) with the
--       constructor (Cons), we find all possible constructors constructing the same
--       datatype: (Cons1/a1, Cons2/a2, ..., Cons(n)/a(n) - that is, Cons(k) has
--       arity a(k)). Then, recursively try to match E with x substituted for each of
--       Cons(k)(y1, y2, ..., y_a(k)).
--
-- We can prove that the execution of the algorithm always finishes.
--


-- Result of a recursive matching of expression E.
--   * BindFail: E couldn't have been bound (step 2 in algorithm above).
--   * BindOK: E has been bound with substitution permuting the variables
--       (step 3 above).
--   * BindCases(bindings): E has been bound, but a constructor has been substituted;
--       'bindings' contains all expressions E' to be called recursively.
data BindingRes = BindFail | BindOK | BindCases [Binding]


-- Runs the recursive algorithm described above. Tries to bind the bindings one by
-- one and checks the result of binding. Note that each variable in pattern is
-- called BindAny.
verifyExhaustiveCase :: Binding -> [Binding] -> InferenceM ()
verifyExhaustiveCase pattern [] =
  throwError $ "Found a non-exhaustive case study: " ++ printTree pattern

verifyExhaustiveCase pattern (bind:binds) = do
  bindRes <- checkBind pattern bind
  case bindRes of
    BindOK -> return ()
    BindFail -> verifyExhaustiveCase pattern binds
    BindCases cases -> mapM_ (\bCase -> verifyExhaustiveCase bCase (bind:binds)) cases


-- Tries to do a single binding step. Given an expression to match P ('pattern')
-- and a binding option B, tries to find out which of three cases happen.
checkBind :: Binding -> Binding -> InferenceM BindingRes

-- If B can bind to anything, then any permutation of P's variables can match B.
checkBind _ BindAny = return BindOK
checkBind _ (BindVar _) = return BindOK

-- If P and B are constructors:
--   1) If P and B have different constructors, they cannot match.
--   2) If the constructors have different length, they cannot match.
--   3) If the constructors have zero length, they trivially match.
--   4) In the opposite case, try to bind the first arguments of the constructors.
--        If it fails or we find a constructor substitution, we can immediately
--        break the execution. Otherwise, try to bind the rest of the arguments
--        by stripping the first one. Some care is needed to handle all the cases.
checkBind (BindCons (IdentCons c) []) (BindCons (IdentCons c') []) =
  return $ if c == c' then BindOK else BindFail

checkBind (BindCons (IdentCons c) (p:ps)) (BindCons (IdentCons c') (b:bs)) =
  if c /= c' then return BindFail else do
    resHead <- checkBind p b
    case resHead of
      BindFail -> return BindFail
      BindOK -> do
        resTail <- checkBind (BindCons (IdentCons c) ps) (BindCons (IdentCons c) bs)
        case resTail of
          BindFail -> return BindFail
          BindOK   -> return BindOK
          BindCases cases ->
            let pack (BindCons (IdentCons c) xs) = BindCons (IdentCons c) (p:xs)
            in return $ BindCases (map pack cases)
      BindCases cases -> let pack bindCase = (BindCons (IdentCons c) (bindCase:ps))
                         in return $ BindCases (map pack cases)

checkBind (BindCons _ _) (BindCons _ _) = return BindFail

-- If P is a variable and B is a constructor, nondeterminism is created. We find
-- all the options the current type can take, and then create all expression cases.
checkBind BindAny binding@(BindCons consFunc _) = do
  Env _ _ _ consFuncsList <- ask
  cons <- consFromConsFun consFunc
  case M.lookup cons consFuncsList of
    Nothing -> throwError $ "Could not find constructor '" ++ printTree cons ++ "'"
    Just options ->
      let optionToBinding (ident, argc) = BindCons ident (replicate argc BindAny)
      in return $ BindCases (map optionToBinding options)


-- Adds the error context to monad. When an error occurs while doing 'op',
-- 'context' is appended to the end of the error message.
addContext :: String -> InferenceM a -> InferenceM a
addContext context op = op `catchError` handler
  where handler err = throwError $ err ++ "\n\n" ++ context

addExpressionContext :: Exp -> InferenceM a -> InferenceM a
addExpressionContext e = addContext
  ("when processing expression: '" ++ printTree e ++ "'")


-- Type constants.
intType = TCons $ IdentCons "Int"
boolType = TCons $ IdentCons "Bool"
