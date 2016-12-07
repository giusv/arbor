{-# LANGUAGE TypeOperators #-}
module Language.Elements.HTable where
import Language.Commons
import Language.Expression
import Language.Query
-- import Language.Filter
-- import Language.Element
import Language.Environment
import Language.Transition
import Language.Utils

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Text.PrettyPrint
import Text.Printf
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map
import Debug.Trace

data HTable a = HTable Identifier (Query Rel) (Rel -> SElement a)
instance (Show a) => Show (HTable a) where
    -- show (HTable id q f) = "HTable " ++ " (" ++ show id ++ " " ++ show (evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)) ++ ")"
    show (HTable id _ _) = "(hTable " ++ show id ++ ")"
    
instance Eq (HTable a) where
    (HTable id1 _ _) == (HTable id2 _ _) = id1 == id2
    
instance Alternable (HTable a)

htable :: (Alternable a, HList a) => Query Rel -> (Rel -> SElement a) -> SElement (HTable a)
htable q f = do pref <- fmap first3 get
                i <- nextIndex
                return $ HTable (pref ++ "hTable" ++ show i) q f

data HNil = HNil deriving (Eq,Show,Read)
data HCons e l = HCons e l deriving (Eq,Show,Read)
type e :*: l = HCons e l -- type level constructor
infixr 6 .*.
(.*.) :: HList l => e -> l -> HCons e l
(.*.) = HCons

class HList l
instance HList HNil
instance HList l => HList (HCons e l)

instance Renderable HNil where
    directives  _ = return []   
    template    _ = return empty
    controller  _ = return empty
    references  _ = return []   
    identifiers _ = return []   
    toElementList _ = []

instance (Renderable e, Renderable l) => Renderable (HCons e l) where
    directives (HCons e l) = liftM2 (++) (directives e) (directives l)
    template (HCons e l) = do
        e' <- template e
        l' <- template l
        return $  text "+ '<td>'"
               $$ e'
               $$ text "+ '</td>'"
               $$ l'
    controller (HCons e l) = liftM2 ($$) (controller e) (controller l)
    references (HCons e l) = liftM2 (++) (references e) (references l)
    identifiers (HCons e l) = liftM2 (++) (identifiers e) (identifiers l)
    toElementList (HCons e l) = toElementList e ++ toElementList l

instance Alternable HNil
instance (Alternable e, Alternable l) => Alternable (HCons e l)

instance Filterable HNil where
    contains _ _ = False
instance (Renderable e, Renderable l, 
          Eq e, Eq l,
          Show e, Show l,
          Presentable e, Presentable l,
          Filterable e, Filterable l) => Filterable (HCons e l) where
    children (HCons e l) = Element e : children l
    contains i (HCons e l) = contains i e || contains i l
    
    
    
instance Presentable HNil
    where present _ = return empty
instance (Presentable e, Presentable l) => Presentable (HCons e l)
    where present _ = return empty

instance (Submittable a) => Submittable (HTable a) where
    submission (HTable id q f) =
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
            elem = evalState (f rel) (id ++ "",0,Map.empty)
        in  submission elem
        
instance (Renderable a) => Renderable (HTable a) where
                                        
    references (HTable id q f) = do
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
        let e = evalState (f rel) (id ++ "",0,Map.empty)
        references e
    
    identifiers (HTable id q f) = do
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
        let e = evalState (f rel) (id ++ "",0,Map.empty)
        identifiers e
    
    directives (HTable id q f) = do
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
        -- let helems = evalState (f rel) (id ++ "",0,Map.empty)
        let (helems,(_,_,transitions)) = runState (f rel) (id ++ "",0,Map.empty)
        let update = \(Environment m p c s) -> (Environment (Map.unionWith (++) m transitions) p (ListContext id) s)
        temp <- local update (template helems)
        ctrl <- local update (controller helems)
        ctx <- fmap getContext ask
        classes <- fmap getClasses ask
        let dir = text (printf "angular.module('%s', ['pose','data'])" id) -- traceShow ("in directive HTable: " ++ show (model helems)) $ 
               $$ nest 4 (text (printf ".directive('dir%s', function() {" (capitalize id))
               $$ nest 4 (text "return {"
               $$ nest 4 (text "scope: {context: '='},"
               $$ text "template: '<table class=\"table table-striped\"" <+> hsep (map text classes) <> text "\">'"
               $$ nest 4 (text "+ '<thead>'"
               $$ text "+ '</thead>'")
               $$ nest 4 (text "+ '<tbody>'"
               $$ nest 4 (text "+ '<tr ng-repeat=\"obj in context\">'"
               $$ nest 4 temp
               $$ text "+ '</tr>'")
               $$ text "+ '</tbody>'") <> comma
               $$ text "replace: true,"
               $$ text (printf "controller: '%sController'," id)
               $$ text "controllerAs: 'ctrl'")
               $$ text "};")
               $$ text "})"
               $$ text (printf ".controller('%sController', function($pose,$data,$scope,$resource) {" id)
               $$ nest 4 (text "$scope." <> text "go = function(pose) { $pose.go(pose);}"
               $$ text "$scope." <> text "match = function(url) {return $pose.match(url);};"
               $$ text "$scope." <> text "get = function(pose,key) { console.log(pose + \" \" + key + \" \" + $pose.get(pose,key)); return $pose.get(pose,key);}"
               $$ ctrl)
               $$ text "});")
        dirs <- local update (directives helems)
        return (dir:dirs)

    template (HTable id q f) = do
        ctx <- fmap getContext ask
        case ctx of 
            EmptyContext -> return $ text "+ '<dir-" <> text id <+> text "context=\""<> text id <> text "\">" <> text "</dir-" <> text id <> text ">'"
            ListContext _ -> return $ text "+ '<dir-" <> text id <+> text "context=\""<> text id <> text "[$index]\">" <> text "</dir-" <> text id <> text ">'"
                                 
    controller (HTable id q f) = do
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
        q' <-  generateQuery id pq
        ctx <- fmap getContext ask
        case ctx of 
            EmptyContext -> return $ q' <> semi
            ListContext _ -> return $ text "$scope.$watchCollection('context', function(newContext,oldContext){"
                                   $$ nest 4 (q' <> semi)
                                   $$ text "});"
            -- ListContext _ -> return $ text "$scope.source = function (index) {"
                                   -- $$ nest 4 (text "return" <+> q' <> semi)
                                   -- $$ text "}"
    modules (HTable id q f) = id : (modules $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty))
    toElementList (HTable id q f) = toElementList $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
-- instance (Modellable a) => Modellable (HTable a) where
    -- model (HTable id q f) = 
        -- let (rel,(_,pq)) = runState q (0,EmptyQuery)
            -- elem = evalState (f rel) (0,Map.empty) in
        -- arrayfy $ model elem 
-- instance (Filterable a) => Filterable (HTable a) where
    -- item (HTable id q f) = item $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
instance (Filterable a) => Filterable (HTable a) where
    isNamed n (HTable id q f) = isNamed n $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
    parameters (HTable id q f) = parameters $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
    children (HTable id q f) = children $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
    contains i (HTable id q f) = contains i $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
instance (Presentable a) => Presentable (HTable a) where
    present (HTable id q f) = do
        e' <- present (evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty))
        return $ text "Goto" <> (parens . doubleQuotes) (text id)
              $$ text "Say" <> (parens . doubleQuotes) (text "this is " <> text id)
              $$ text "; Sleep, 1000"
              $$ e'