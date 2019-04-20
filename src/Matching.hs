{-# LANGUAGE EmptyDataDecls, FlexibleContexts, FlexibleInstances, GADTs, Rank2Types, ScopedTypeVariables, TypeApplications, UndecidableInstances #-}

module Matching (
    Meetable(..)

  , MonadMatchable(..)
  , refreshVar
  , Match
  , matchChoose
  , runMatch
  , runMatchFirst
  , runMatchUnique

  , Pattern(..)
  , Matchee(..)

  , Matchable(..)
  , matchList
  , refreshVarsList
  , fillMatchList
  , module MatchEffect
  , runMatchEffect

  , EmptyState(..)
  ) where

import Control.Monad ( MonadPlus(..), guard, forM_, (=<<), msum )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.State ( MonadState(..), StateT, evalStateT, modify )
import Control.Monad.Trans ( lift )

import Data.Foldable ( fold )
import Data.Map ( Map, (!) )
import qualified Data.Map as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Typeable ( Typeable, cast )

import Control.Monad.Logic ( LogicT(..), runLogicT, observeAllT )

import Configuration
import Debug
import LangBase
import MatchEffect
import Term
import Var

------------------------------------------------------------------
------
------              Thoughts on Abstract Matching/Rewriting
------              (a.k.a.: Jimmy justifying why he doesn't need to do work)
------
------ A matching engine for type x is given fundamentally by one function:
------    match :: Pattern x -> x -> m (Pattern x -> m x)
------
------ This is weaker than the current match/fillMatch interface (and less compositional). Note that getVars is only used internally
------ (i.e.: would be a protected method in Java), and refreshVars is just me not knowing how to do higher-order matching
------ (which I think should still sit behind the same interface).
------
------ In abstract interpretation, there are functions alpha :: x -> y and gamma :: y -> x satisfying the usual lattice laws.
------ For terms, you can take x to be the powerset of terms, and lift match through the usual powerset monad.
------ Then, abstractMatch has type:  Pattern x -> y -> m (Pattern x -> m y), satisfying:
------         (gamma <$> (abstractMatch p1 (alpha t) <*> p2)) >= (match p1 t <*> p2)
------
------ I haven't read the paper on Galois transformers.....but I think match might need to be one. At least, it should be
------ an endofunctor on the category of lattices (i.e.: preserves monotone maps).
------
------ An example of abstract terms: terms where some subtrees have been replaced with variables, together with logical
------ constraints on those variables. For instance, in the case where the constraints are of the form cx+dy<=b, where
------ c,d \in {-1, 1}, then this is the octagon domain, and rewrites are just octagon updates.
------
------ The main example of interest: terms where some subtrees are replaced by the special node Star, representing
------ any well-sorted subtree. Matching is performed where Star matches any pattern, and binds all pattern variables
------ to Star. This does not yield the most precise transformer because it loses equalities between variables.
------
------ My plan is to introduce a Star node to all Matchable's. Just as non-pattern terms are currently given by the type Term l
------ with the implicit constraint that there are no free variables, non-abstract terms (including pattern terms)
------ will be given by Term l with the implicit constraint that there are no Star's (or free variables). Star's may also
------ not appear in negative position. It's easy to see that this is valid abstraction, and the abstract match is a valid transformer.
------ It's nice because matching is easy to implement; I've basically used a quantified version of subtyping to write a common
------ matcher that works on both.
------
------ To extend my current implementation to the more arbitrary abstractions, I need to: make pattern/matchee types
------ truly distinct (make Matchable take a "p" parameter probably, and merge match/fillMatch into the match given above
------ for type-inference reasons), and.....I think that's it. So, it's basically a matter of rewriting the matchers
------ + tweaking the callers. At the present level of design, I do not see anything that makes this change much harder
------ to do later vs. now.
------
------ Also, runCompFunc will need a variant for abstract terms. Will take some threading for each semantics,
------ and breaking apart the runCompFunc impl per language.
------
------ The main barrier I see is in case I do want to allow abstractions in negative position,
------ where terms are used as patterns (and vice versa?). I accept that I don't understand this that well,
------ don't know how to deal with it, and am essentially propagating the assumption that negative terms
------ are a subtype of patterns.
------
-----------------------------------------------
------
------ Questions:
------   1) How to implement fake associative and ACI matching for Star nodes?
------
------      A: Associative is easy, since we're restricting to only one list variable.
------         For our restricted deterministic ACI matching, we must make sure that there are no Star nodes
------         in keys. A map variable may be bound to a (map star)+ list of terms.
------         ^^ But this is assuming that a * node stands for a single element, not a list (must specify which).
------         If it can stand for a list, as it may in the case we care about (where it can mean an
------         arbitrary number of stack frames), then this is full-on associative unification.
------
------   2) How to reduce terms? Example:
------           Rule:    < v | [\x -> x+e]. K> -> let n = x+e in <n | K>
------           AbsTerm: <* | * . ctx > should -> let n = *+* in <n | ctx>
------     A: Bind (fillMatch v) to x before evaluating the RHS. Oh hey, guess what: I already do this
------        in a way that will work.
------
------   3) How do the star node plans compare to the val node plans? Can they be merged?
------
------     A: Kinda. You can have terms that contain abstract vars which stand for arbitrary subtrees,
------        where some equalities are known. However, when building the graph, all equalities between vars should
------        be forgotten, and equality between states should be modulo alpha-equivalence. Indeed, I'm already
------        relying on poor-man's alpha equivalence for my plan to work (all matching frames should have matching
------        variable names).
------
------        In other words: I may have a notion of abstract states that remembers variable equalities and uses
------        different variable names when doing reductions, but most of my CFGs of interest will project them
------        down to something which is basically just having a Star node. So, if I can get a good plan for
------        projections, then I can do this.
------
------        Oh yeah, and stars can be lists, whereas val nodes can't. See below.
------
------   4) How will projections work? Will they work? If not, how can I create statement or block level CFGs?
------
------      A: The only way I think it can work is if you first compute a full graph using an abstraction,
------         and then blow it down.
------
------         Relation between abs CFG and transition system: the abstraction of every path in the transition system
------         is a path in the CFG. There's a graph homomorphism between all possible states of the transition system
------         to the abs CFG. Whereas, between the abs CFG and the projected CFG, there's just a graph homomorphism
------         from the set of abstract states *seen so far*. This is much easier to come by, and justifies having
------         a separate projection step as an additional source of expressivity.
------
------         Can you create statement level CFG without projections?
------
------         With an expression-irrelevance abstraction, all expression-eval rules become stutter steps,
------         all expression congruence-enter rules become pushing a new stack frame (that does nothing), and all congruence-exit
------         rules become popping off one of those do-nothing stack frames. But if you take those identity-frames
------         and replace them with a Star frame, then it works; you get separate nodes for evaling a statement
------         vs. evalling some arbitrary expression within that statement. And remember: *.*.K == *.K .
------
------         This means: Yes, I need real-ish associative and ACI matching. Not just matching; it's really
------         unification where one side must be linear (Star nodes are like distinct variables). This means
------         that my matching interface must allow backtracking/multiple matches. Good thing I realized this now.
------
------         For basic blocks: Yes, if you turn all sequences of assignments into a *. Unsure what happens with
------         function calls.
------
------   5) What about ValVar's?
------
------     A: For matching, they are just like having an extra Val constructor. But to chain together rules, the
------        infrastructure needs to be able to turn a pattern var into a val, whereas it can't currently. So, yes, these
------        must exist. Also, you cannot prove the needed axiom unless you can mark non-vals; otherwise,
------        fusing rules (even without discarding anything) can eliminate valid transition sequences.
------
------
------
------ Other comment: Non-vals do not automatically become vals. So, pairs do need separate pair values / mkPair
------ constructors.
------
------
------
------------------------------------------------------------------

-- TODO: Find a proper algebraic structure to use here and incorporate it into the code properly
-- TODO: Put this somewhere proper in the code
-- This is deadline design
class (Eq m) => Meetable m where
  meet :: m -> m -> Maybe m

instance {-# OVERLAPPABLE #-} (Eq m) => Meetable m where
  meet a b = if a == b then Just a else Nothing

instance Meetable (Term l) where
  -- TODO: What to do if there's a var?
  meet x y
    | x == y = Just x
  meet (GStar mt1) (GStar mt2) = GStar <$> matchTypeMeet mt1 mt2
  meet t s@(GStar _) = meet s t
  meet (GStar mt) v@(Val  _ _) = if matchTypeCompat ValueOnly mt  then Just v else Nothing
  meet (GStar mt) t@(Node _ _) = if matchTypeCompat NonvalOnly mt then Just t else Nothing
  meet Star       x            = Just x
  meet _          _            = Nothing

data AnyMatchable where
  AnyMatchable :: (Matchable m, Meetable m) => m -> AnyMatchable

instance Show AnyMatchable where
  showsPrec d (AnyMatchable x) = showsPrec d x

data MatchState = MatchState { ms_varMap   :: Map MetaVar AnyMatchable
                             , ms_varAlloc :: VarAllocator
                             }

modVarMap :: (Map MetaVar AnyMatchable -> Map MetaVar AnyMatchable) -> MatchState -> MatchState
modVarMap f m = m { ms_varMap = f (ms_varMap m) }


-- LogicT is on inside; means state will reset on backtrack
type Match = StateT MatchState (LogicT IO)

runMatch :: Match a -> IO [a]
runMatch m = observeAllT $ evalStateT m (MatchState Map.empty mkPositiveVarAllocator)

runMatchFirst :: Match a -> IO (Maybe a)
runMatchFirst m = runLogicT (evalStateT m (MatchState Map.empty mkPositiveVarAllocator)) (const . return . Just) (return Nothing)

runMatchUnique :: Match a -> IO (Maybe a)
runMatchUnique m = do xs <- runMatch m
                      case xs of
                        []  -> return Nothing
                        [a] -> return (Just a)
                        _   -> error "Called runMatchUnique, but had many results"

runMatchEffect :: MatchEffect a -> Match a
runMatchEffect (MatchEffect x) = lift x

instance MonadVarAllocator Match where
  allocVarM = do ms <- get
                 let (mv, va') = allocVar (ms_varAlloc ms)
                 put $ ms { ms_varAlloc = va' }
                 return mv

class (MonadPlus m, MonadVarAllocator m, MonadIO m) => MonadMatchable m where
  hasVar :: MetaVar -> m Bool
  putVar :: (Matchable a, Meetable a) => MetaVar -> a -> m ()
  clearVar :: MetaVar -> m ()
  overrideVar :: (Matchable a, Meetable a) => MetaVar -> a -> m ()
  getVarMaybe :: Matchable a => MetaVar -> (a -> m b) -> m b -> m b
  getVarDefault :: Matchable a => MetaVar -> m a -> m a
  getVar :: Matchable a => MetaVar -> m a

  modifyVars :: (forall a. (Matchable a, Meetable a) => MetaVar -> a -> m a) -> m ()

  withSubCtx   :: m x -> m x
  withFreshCtx :: m x -> m x

  withVarAllocator :: VarAllocator -> m x -> m x

  debugVars :: m ()

  overrideVar m x = clearVar m >> putVar m x
  getVarDefault v = getVarMaybe v return
  getVar var = getVarDefault var mzero

matchChoose :: (MonadMatchable m) => [a] -> m a
matchChoose as = msum (map return as)

withModCtxState :: (MonadState MatchState m) => (MatchState -> MatchState) -> m x -> m x
withModCtxState f m = do old <- get
                         modify f
                         res <- m
                         put old
                         return res


hoistMaybe :: MonadPlus m => Maybe a -> m a
hoistMaybe = maybe mzero return

instance {-# OVERLAPPABLE #-} (MonadState MatchState m, MonadVarAllocator m, MonadIO m, MonadPlus m) => MonadMatchable m where
  hasVar var = Map.member var <$> ms_varMap <$> get
  putVar var val = do curVal <- getVarMaybe var (return.Just) (return Nothing)
                      case curVal of
                        Nothing   -> do debugM $ "Setting var " ++ show var
                                        modify (modVarMap $ Map.insert var (AnyMatchable val))
                        Just val' -> do newVal <- hoistMaybe $ meet val val'
                                        modify (modVarMap $ Map.insert var (AnyMatchable newVal))

  clearVar var = modify (modVarMap $ Map.delete var)

  getVarMaybe var f def = do maybeAM <- Map.lookup var <$> ms_varMap <$> get
                             case (maybeAM :: Maybe AnyMatchable) of
                               Nothing              -> def
                               Just (AnyMatchable x) -> maybe def f (cast x)

  modifyVars f = do m <- get
                    vm' <- Map.traverseWithKey (\v (AnyMatchable x) -> AnyMatchable <$> f v x) (ms_varMap m)
                    put (m {ms_varMap = vm' })

  withSubCtx          = withModCtxState id
  withFreshCtx        = withModCtxState (\ms -> ms { ms_varMap   = Map.empty })
  withVarAllocator va = withModCtxState (\ms -> ms { ms_varAlloc = va})

  debugVars = debugM =<< (show <$> ms_varMap <$> get)

-- Hack to prevent eagerly matching the above instance
data UnusedMonad a
instance {-# OVERLAPPING #-} (MonadPlus UnusedMonad, MonadVarAllocator UnusedMonad, MonadIO UnusedMonad) => MonadMatchable UnusedMonad where
  hasVar = error "Using UnusedMonad"
  putVar = error "Using UnusedMonad"
  clearVar = error "Using UnusedMonad"
  getVarMaybe = error "Using UnusedMonad"
  withSubCtx = error "Using UnusedMonad"
  withFreshCtx = error "Using UnusedMonad"
  withVarAllocator = error "Using UnusedMonad"
  debugVars = error "Using UnusedMonads"

refreshVar :: (MonadMatchable m, Matchable a, Meetable a) => (MetaVar -> a) -> MetaVar -> m MetaVar
refreshVar f v = do v' <- allocVarM
                    putVar v (f v')
                    return v'

-- These exist to make arguments more clear now that we're not putting Open/Closed in the types
-- We do have the option of giving these a smart constructor which checks closed-ness for a more sophisticated definition
-- of closedness which includes binders
-- TODO: "Matchee?" There is a standard name for this, right?
newtype Matchee f = Matchee f
newtype Pattern f = Pattern f

-- NOTE: I think I need to write SYB/Uniplate infrastructure for vars-containing terms
class (Show f, Typeable f) => Matchable f where
  -- | Returns all meta-syntactic variables contained in an `f`
  getVars :: f -> Set MetaVar

  -- | `match p m` matches the pattern p against m in the current match context.
  -- It uses the failure effect on failure. On success, it binds all metavars in `p` to the corresponding subterms
  -- of `m`.
  --
  --
  -- Extra precondition:
  -- Variables in an open term may be in "template" or "pattern" position. This distinction is not made syntactially.
  -- Currently, the only example of a template variable is the LHSs of bindings in SimpEnvMap.
  --
  -- Prior to calling match, all template variables should be filled in, either because there were none, or
  -- by calling fillMatch. This includes recursive calls to match in match instances.
  --

  -- While you can call (match t1 t2) where t2 is open, if you do, all variables in t2 will be considered closed,
  -- i.e.: distinctly numbered variables will be assumed to stand for distinct terms. In other words,
  -- the term f(x,y) will not match the pattern f(z,z).
  match :: (MonadMatchable m) => Pattern f -> Matchee f -> m ()

  -- | Renames all meta-syntactic variables in the argument with newly allocated variables, binding the old
  -- variables to the new ones
  --
  -- This is used when wrapping a term inside a binder, to prevent the bound variable from shadowing
  -- existing variables. It is particularly used when converting an SOS rule to PAM rules. Consider the following SOS rule:
  --
  --    plus-cong-1:
  --    step(+(0t, 1)) = let 2 = step(0t) in +(2, 1)
  --
  -- Naively, it would be converted into the following two PAM rules:
  --
  --   plus-cong-1-1:
  --   <+(0t, 1) | 9> down  ---->  <0t | [\2 -> +(2, 1)].3> down
  --
  --   plus-cong-1-2:
  --    <2 | [\2 -> +(2, 1)].3> up  ---->  <+(2, 1) | 3> up
  --
  -- This is problematic, because the bound variable "2" in the context on the LHS shadows the variable "2"
  -- used to match the term. Instead, a new name is generated for the bound variable, yielding the following PAM rule:
  --
  --   plus-cong-1-2:
  --    <2 | [\4 -> +(4, 1)].3> up  ---->  <+(2, 1) | 3> up
  refreshVars :: (MonadMatchable m) => f -> m f

  -- | Replaces all metavariables in the argument with their matched values in the current match context
  --
  -- If all metavariables in the argument have been bound, then the return value
  -- will be closed
  fillMatch :: (MonadMatchable m) => f -> m f

matchList :: (Matchable f, MonadMatchable m) => Pattern [f] -> Matchee [f] -> m ()
matchList (Pattern xs1) (Matchee xs2) = sequence_ $ zipWith match (map Pattern xs1) (map Matchee xs2)

refreshVarsList :: (Matchable f, MonadVarAllocator m, MonadMatchable m) => [f] -> m [f]
refreshVarsList = mapM refreshVars

fillMatchList :: (Matchable f, MonadMatchable m) => [f] -> m [f]
fillMatchList = mapM fillMatch


fillMatchTermGen :: (MonadMatchable m, Typeable l) => (MetaVar -> MatchType -> m (Term l)) -> Term l -> m (Term l)
fillMatchTermGen f (Node s ts)     = Node s <$> (mapM (fillMatchTermGen f) ts)
fillMatchTermGen f (Val  s ts)     = Val s <$> (mapM (fillMatchTermGen f) ts)
fillMatchTermGen f (IntNode s i)   = return (IntNode s i) -- This could just be unsafeCoerce....or hopefully just coerce
fillMatchTermGen f (StrNode s x)   = return (StrNode s x) -- This could just be unsafeCoerce....or hopefully just coerce
fillMatchTermGen f (GMetaVar v mt) = f v mt
fillMatchTermGen f (GStar mt)      = return (GStar mt)

instance (Typeable l) => Matchable (Term l) where
  getVars (Node _ ts)    = fold $ map getVars ts
  getVars (Val _ ts)     = fold $ map getVars ts
  getVars (IntNode _ _)  = Set.empty
  getVars (StrNode _ _)  = Set.empty
  getVars (GMetaVar v _) = Set.singleton v
  getVars (GStar _)      = Set.empty

  match (Pattern (Node s1 ts1))   (Matchee (Node s2 ts2))
    | (s1 == s2)                              = matchList (Pattern ts1) (Matchee ts2)
  match (Pattern (Val  s1 ts1))   (Matchee (Val  s2 ts2))
    | (s1 == s2)                              = matchList (Pattern ts1) (Matchee ts2)
  match (Pattern (IntNode s1 i1)) (Matchee (IntNode s2 i2))
    | (s1 == s2) && (i1 == i2)                = return ()
  match (Pattern (StrNode s1 x1)) (Matchee (StrNode s2 x2))
    | (s1 == s2) && (x1 == x2)                = return ()

  -- TODO: There has to be a cleaner way than this
  match (Pattern (ValVar v))       (Matchee t@(Val      _ _)) = putVar v t
  match (Pattern (ValVar v))       (Matchee t@(ValVar   _  )) = putVar v t
  match (Pattern (NonvalVar v))    (Matchee t@(Node     _ _)) = putVar v t
  match (Pattern (NonvalVar v))    (Matchee t@(NonvalVar  _)) = putVar v t
  match (Pattern (MetaVar v))      (Matchee t@(Val      _ _)) = putVar v t
  match (Pattern (MetaVar v))      (Matchee t@(Node     _ _)) = putVar v t
  match (Pattern (MetaVar v))      (Matchee t@(GMetaVar _ _)) = putVar v t
  match (Pattern (MetaVar v))      (Matchee t@(IntNode  _ _)) = putVar v t
  match (Pattern (MetaVar v))      (Matchee t@(StrNode  _ _)) = putVar v t
  match (Pattern (GMetaVar v mt1)) (Matchee t@(GStar    mt2)) = guard (matchTypeCompat mt1 mt2) >> putVar v t

  match (Pattern (Node _ _))      (Matchee ValStar   ) = mzero
  match (Pattern (Node _ ts))     (Matchee (GStar mt)) = forM_ ts $ \ t -> match (Pattern t) (Matchee (GStar mt))
  match (Pattern (Val _ _))       (Matchee NonvalStar) = mzero
  match (Pattern (Val _ ts))      (Matchee (GStar mt)) = forM_ ts $ \ t -> match (Pattern t) (Matchee (GStar mt))
  match (Pattern (IntNode _ _))   (Matchee (GStar _))  = return ()
  match (Pattern (StrNode _ _))   (Matchee (GStar _))  = return ()

  match _                         _                                  = mzero

  refreshVars = fillMatchTermGen (\v mt -> getVarMaybe v return (refresh v mt))
    where
      refresh :: (MonadMatchable m, MonadVarAllocator m, Typeable l) => MetaVar -> MatchType -> m (Term l)
      refresh v mt = GMetaVar <$> refreshVar (\v' -> GMetaVar @l v' mt) v <*> pure mt

  fillMatch = fillMatchTermGen (\v mt -> getVarMaybe v (guardValMatches mt) (return $ GMetaVar v mt))
    where
      guardValMatches :: (MonadMatchable m, Typeable l) => MatchType -> Term l -> m (Term l)
      guardValMatches TermOrValue t                 = return t
      guardValMatches ValueOnly   t@(Val       _ _) = return t
      guardValMatches ValueOnly   t@(ValVar      _) = return t
      guardValMatches ValueOnly   t@(GStar mt)      = guard (matchTypeCompat ValueOnly mt) >> return t

      guardValMatches NonvalOnly  t@(Node      _ _) = return t
      guardValMatches NonvalOnly  t@(NonvalVar   _) = return t
      guardValMatches NonvalOnly  t@(GStar mt)      = guard (matchTypeCompat ValueOnly mt) >> return t
      guardValMatches _           _                 = mzero


instance {-# OVERLAPPABLE #-} (Typeable l, Matchable (Term l), Matchable s) => Matchable (GConfiguration s l) where
  getVars (Conf t s) = getVars t `Set.union` getVars s

  match (Pattern (Conf t1 s1)) (Matchee (Conf t2 s2)) = do
      match (Pattern t1) (Matchee t2)
      match (Pattern s1) (Matchee s2)

  refreshVars (Conf t s) = Conf <$> refreshVars t <*> refreshVars s
  fillMatch   (Conf t s) = Conf <$> fillMatch   t <*> fillMatch   s

-- Hack to prevent over-eagerly expanding (Matchable (Configuration l)) constraints
instance {-# OVERLAPPING #-} (Matchable s) => Matchable (GConfiguration s UnusedLanguage) where
  getVars = error "Matching UnusedLanguage"
  match = error "Matching UnusedLanguage"
  refreshVars = error "Matching UnusedLanguage"
  fillMatch = error "Matching UnusedLanguage"

instance Matchable EmptyState where
  getVars EmptyState = Set.empty
  match _ _ = return ()
  refreshVars EmptyState = return EmptyState
  fillMatch EmptyState = return EmptyState

instance Matchable () where
  getVars () = Set.empty
  match _ _ = return ()
  refreshVars () = return ()
  fillMatch () = return ()


-------------------------------------- Matching on SimpEnv ------------------------------------------

-- | Description: We implemented a restricted version of ACI
-- matching specialized to the kinds of patterns used in operational semantics,
-- e.g.: \Gamma, x=v. Our implementation imposes the following restrictions:
-- the match pattern must be linear, all but one pattern variable may only match one element,
-- and all bindings in the match pattern must have a concrete key.
--
-- These restrictions are actually desirable because general ACI-matching is NP-hard,
-- and operational semantics are generally written to not require solving NP-hard problems
-- to compute a single step of reduction. With these restrictions, on the other hand,
-- assuming constant-time map operations, matching can easily be implemented in time linear
-- in the size of the matched term and pattern.
--
-- This restricted version is not sufficient to encode type systems such as the basic presentation of the linear lambda calculus,
-- which may match a linear context against a pattern of the form \Delta, \Delta' (i.e.: try an arbitrary context split).
-- However, it is sufficient to implement the "resource-tracking" version of the type system of the linear lambda calculus,
-- as well as the operational semantics of the linear lambda calculus (which are almost the same as that of the
-- simply-typed lambda calculus with sums and products).


-- |
-- Important notes on matching maps:
--
-- If you fail to close all keys before matching, then you'll get weird behavior

instance (Matchable a, Matchable b) => Matchable (SimpEnvMap a b) where
  getVars (SimpEnvMap m) = fold (map getVars keys) `Set.union` fold (map getVars vals)
    where
      (keys, vals) = unzip (Map.toList m)

  match (Pattern (SimpEnvMap m1)) (Matchee (SimpEnvMap m2)) = do
    m1' <- mapKeysM fillMatch m1
    if Map.keys m1' /= Map.keys m2 then
      mzero
    else
      -- Use (!) because guaranteed has key
      sequence_ $ Map.mapWithKey (\k v -> match (Pattern v) (Matchee (m2 ! k))) m1'

  refreshVars (SimpEnvMap m) = SimpEnvMap <$> (mapKeysM refreshVars =<< mapM refreshVars m)
  fillMatch   (SimpEnvMap m) = SimpEnvMap <$> (mapKeysM fillMatch   =<< mapM fillMatch   m)


-- | Used as a hint to type inference
declareTypesEq :: (Monad m) => a -> a -> m ()
declareTypesEq _ _ = return ()

instance (Matchable a, Matchable b, Typeable a) => Matchable (SimpEnv a b) where
  getVars (SimpEnvRest v m) = Set.insert v (getVars m)
  getVars (JustSimpMap m) = getVars m

  match (Pattern p@(SimpEnvRest v m1)) (Matchee m@(JustSimpMap m2)) = do
    vFilled <- getVarMaybe v (return.Just) (return Nothing)
    case vFilled of
      Just (SimpEnvMap vm) -> do declareTypesEq (SimpEnvMap vm) m1
                                 p' <- fillMatch p
                                 match (Pattern p') (Matchee m)
      _ -> do
        m1' <- fillMatch m1
        let (mp1, mp2) = (getSimpEnvMap m1', getSimpEnvMap m2)
        let diff = Map.difference mp2 mp1
        debugM $ "Matching maps " ++ show mp1 ++ " and " ++ show mp2
        debugM $ "Binding var " ++ show v ++ " to " ++ show diff
        putVar v (JustSimpMap $ SimpEnvMap diff)
        match (Pattern m1) (Matchee (SimpEnvMap $ Map.intersection mp2 mp1))

  match (Pattern (JustSimpMap m1)) (Matchee (JustSimpMap m2)) = match (Pattern m1) (Matchee m2)

  -- FIXME: WTF is this doing? Why is it not matching v1 also to m2-m1
  match (Pattern (SimpEnvRest v1 m1)) (Matchee (SimpEnvRest v2 m2)) = putVar v1 (WholeSimpEnv @a @b v2) >> match (Pattern m1) (Matchee m2)

  -- TODO: Do we need a case for matching SimpMap with EnvRest? (Beware infinite recursion if so)

  refreshVars (JustSimpMap m)   = JustSimpMap <$> refreshVars m
  refreshVars (SimpEnvRest v m) = SimpEnvRest <$> refreshVar (\v -> WholeSimpEnv @a @b v) v <*> refreshVars m

  fillMatch (SimpEnvRest v m1) = do
    filledVar <- getVarMaybe v (return.Just) (return Nothing)
    m1' <- fillMatch m1

    -- In the below:
    -- Order of Map.union is important.
    -- When there is conflict, will prefer the explicitly given keys
    case filledVar of
      (Just (SimpEnvRest v' m2)) -> return $ SimpEnvRest v' $ SimpEnvMap (Map.union (getSimpEnvMap m1') (getSimpEnvMap m2))
      (Just (JustSimpMap m2))    -> return $ JustSimpMap $ SimpEnvMap (Map.union (getSimpEnvMap m1') (getSimpEnvMap m2))
      Nothing -> return $ SimpEnvRest v m1'

  fillMatch (JustSimpMap m) = JustSimpMap <$> fillMatch m


