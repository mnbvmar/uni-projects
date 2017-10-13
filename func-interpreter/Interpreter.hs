-- The main interpreting module. It evaluates the value of 'main' and returns
-- it in 'interpret' function.
module Interpreter (interpret, Result) where

import AbsLang
import ErrM
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map as M


type ErrorType = String
type Result = Err String
type Env = M.Map String Value

-- The monad interpreting the program is a reader on the environment (the
-- currently known values). It is surrounded by an Except monad gathering
-- the runtime errors.
type EnvM = ExceptT String (Reader Env)


-- Each element is either a constant (integer or bool), a function,
-- a data constructor or a yet unevaluated expression.
data Value = VInt Integer
           | VBool Bool
           | VFunc (Value -> EnvM Value)
           | VCons String [Value]
           | VUnevaluated Exp

instance Show Value where
  show (VInt x) = ("VInt " ++ show x)
  show (VBool b) = ("VBool " ++ show b)
  show (VFunc _) = ("VFunc ...")
  show (VCons s val) = ("VCons " ++ s ++ " " ++ show val)
  show (VUnevaluated e) = ("uneval")



interpret :: Prog -> Result
interpret (Prog defs) =
  let e1 = runExceptT (doInterpret defs)
      e2 = runReader e1 M.empty
  in case e2 of
       Left msg -> Bad msg
       Right (VInt x) -> Ok $ show x
       Right (VBool x) -> Ok $ show x
       Right (VFunc _) -> Ok "function"
       Right (VCons name _) -> Ok $ name ++ " constructor"
       Right (VUnevaluated _) -> Ok $ "unevaluated"


-- Main interpreting method. Appends the definitions to the environment
-- one by one and then runs "main" function.
doInterpret :: [Def] -> EnvM Value
doInterpret [] = do
  env <- ask
  evalNameDecl "main"
doInterpret (def:defs) = do
  newEnv <- addDefM def
  local (const newEnv) $ doInterpret defs


-- Turns a data constructor into a function taking its arguments and
-- then returning a typed value. The arguments are appended backwards,
-- so we need to reverse the argument list before applying the args.
--
-- The arguments are held in a pseudo-constructor called _Cons
-- (underline assures that no user-defined constructor shares the name).
getConsType :: String -> [Type] -> EnvM Value
getConsType name [] = do
  env <- ask
  let (VCons _ args) = M.findWithDefault (VCons name []) "_Cons" env
  return $ VCons name (reverse args)

getConsType name (_:args) = do
  env <- ask
  let (VCons _ cur) = M.findWithDefault (VCons name []) "_Cons" env
  let f = \x -> local (const $ M.insert "_Cons" (VCons name (x:cur)) env) $ getConsType name args
  return $ VFunc f



-- Adds a definition to the environment.
--   * Types can be ignored.
--   * Declarations are saved as the unevaluated expressions.
--   * Constructors are changed into functions using getConsType.
addDefM :: Def -> EnvM Env

addDefM (DefType _ _) = ask

addDefM (DefDecl (decl@(Decl (IdentVar name) _ _))) = do
  env <- ask
  return (M.insert name (declToVal decl) env)

addDefM (DefCons _ _ []) = ask

addDefM (DefCons types ident (consType:consTypes)) = do
  env <- ask
  let (Cons (IdentCons name) argTypes) = consType
  cons <- getConsType name argTypes
  local (const (M.insert name cons env)) $ addDefM (DefCons types ident consTypes)


-- Turns a declaration into an unevaluated expression.
declToVal :: Decl -> Value
declToVal (Decl _ [] e) = VUnevaluated e
declToVal (Decl _ args e) = VUnevaluated (ELam args e)

-- Evaluates a possibly unevaulated expression.
evalVal :: Value -> EnvM Value
evalVal (VUnevaluated e) = evalExp e
evalVal v = return v

-- Finds a declaration by its name and then evaluates it.
evalNameDecl :: String -> EnvM Value
evalNameDecl name =
  if name == "undefined"
    then throwError "Exception: undefined"
    else do 
      env <- ask
      case M.lookup name env of
        Just val -> evalVal val
        Nothing -> throwError $ "Identifier '" ++ name ++ "' not found"



-- Expression evaluation method.
evalExp :: Exp -> EnvM Value

-- Integer evaluation - we can just return the value.
evalExp (EInt x) = return (VInt x)

-- Variable or constructor evaluation - it should be saved in the environment.
evalExp (EVar (IdentVar x)) = evalNameDecl x
evalExp (ECons (IdentCons x)) = evalNameDecl x

-- Operator evaluation. The type depends on the kind of the operation done:
--   * Arithmetic: Int -> Int -> Int,
--   * Comparison: Int -> Int -> Bool,
--   * List operator (:): a -> List a -> List a.
evalExp (EOp lhs nameOp rhs) = do
  lhs' <- evalExp lhs
  rhs' <- evalExp rhs
  return (op lhs' rhs')
  where op = case nameOp of
                OpAdd -> liftVInt (+)
                OpSub -> liftVInt (-)
                OpMul -> liftVInt (*)
                OpDiv -> liftVInt div
                OpLeq -> liftVIntToBool (<=)
                OpLt  -> liftVIntToBool (<)
                OpGeq -> liftVIntToBool (>=)
                OpGt  -> liftVIntToBool (>)
                OpEq  -> liftVIntToBool (==)
                OpNeq -> liftVIntToBool (/=)
                OpLst -> (\a b -> VCons "Elem" [a, b])
        liftVInt f (VInt a) (VInt b) = VInt (f a b)
        liftVIntToBool f (VInt a) (VInt b) = VBool (f a b)

-- Lambda evaluation. Empty lambda is just an expression.
evalExp (ELam [] e) = evalExp e

-- To append an argument to the lambda, append it to the saved environment.
evalExp (ELam ((IdentVar arg):args) e) = do
  env <- ask
  let func val = local (const $ M.insert arg val env) (evalExp (ELam args e))
  return (VFunc func)

-- Conditional evaluation.
evalExp (EIf b eTrue eFalse) = do
  VBool b' <- evalExp b
  if b' then evalExp eTrue else evalExp eFalse

-- Let evaluation. Extends the environment and evaluates the expression.
evalExp (ELet d@(Decl (IdentVar name) _ _) e) =
  local (\env -> M.insert name (declToVal d) env) (evalExp e)

-- Function application.
evalExp (EApp eFun eVal) = do
  VFunc f <- evalExp eFun
  val <- evalExp eVal
  f val

-- Case study. Tries to bind the value to the subsequent cases.
evalExp (ECase eCased cases) = do
  val <- evalExp eCased
  let matchCase _ [] = throwError $ "Non-exhaustive case study"
      matchCase x ((Bind binding exp):opts) = do
        bindingResult <- tryBind val binding
        case bindingResult of
          Just env -> local (const env) $ evalExp exp
          Nothing -> matchCase x opts
  matchCase val cases

-- Constant list expression.
evalExp (EList (ConstList elems)) = do
  results <- mapM evalExp elems
  return $ foldr (\res acc -> VCons "Elem" [res, acc]) (VCons "Nil" []) results


-- The function trying the bind the value to the binding. Returns:
--   * Just env, if the binding succeeded and the extended environment is env.
--   * Nothing, if the binding didn't succeed.
tryBind :: Value -> Binding -> EnvM (Maybe Env)

-- Binding to any element (_). Always succeeds, doesn't extend the environment.
tryBind _ BindAny = do
  env <- ask
  return $ Just env

-- Binding to the variable. Always succeeds, extends the environment.
tryBind value (BindVar (IdentVar var)) = do
  env <- ask
  return $ Just (M.insert var value env)

-- Binding to the constructor. Succeeds only if:
--   * The constructor names and the parameter count are identical,
--   * each of the value's arguments can be bound to the constructor's
--      arguments.
-- The second requirement is checked recursively by binding the first argument
-- and then matching the shrunk constructors.
tryBind (VCons consVar varArgs) (BindCons (IdentCons consName) bindArgs) = do
  env <- ask
  if (consVar /= consName) then return Nothing
  else if (length varArgs) /= (length bindArgs) then return Nothing
  else if (length varArgs) == 0 then return (Just env)
  else do
    nxt <- tryBind (head varArgs) (head bindArgs)
    case nxt of
      Nothing -> return Nothing
      Just resNext -> local (const resNext) $ tryBind (VCons consVar (tail varArgs))
                                    (BindCons (IdentCons consName) (tail bindArgs))
    
tryBind _ (BindCons _ _) = return Nothing

