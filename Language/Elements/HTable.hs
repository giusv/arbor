{-# LANGUAGE TypeOperators #-}
module Language.Elements.Htable where
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

data Htable a = Htable Identifier (Query Rel) (Rel -> SElement a)
instance (Show a) => Show (Htable a) where
    -- show (Htable id q f) = "Htable " ++ " (" ++ show id ++ " " ++ show (evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)) ++ ")"
    show (Htable id _ _) = "(htable " ++ show id ++ ")"
    
instance Eq (Htable a) where
    (Htable id1 _ _) == (Htable id2 _ _) = id1 == id2
    
instance Alternable (Htable a)

htable :: (Alternable a, HList a) => Query Rel -> (Rel -> SElement a) -> SElement (Htable a)
htable q f = do pref <- fmap first3 get
                i <- nextIndex
                return $ Htable (pref ++ "htable" ++ show i) q f

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
    modules     _ = []
    toElementList _ = []

instance (Renderable e, Renderable l) => Renderable (HCons e l) where
    directives (HCons e l) = liftM2 (++) (directives e) (directives l)
    template (HCons e l) = do
        e' <- template e
        l' <- template l
        return $  text "+ '<div class=\"table-cell\">'"
               $$ nest 4 e'
               $$ text "+ '</div>'"
               $$ l'
    controller (HCons e l) = liftM2 ($$) (controller e) (controller l)
    references (HCons e l) = liftM2 (++) (references e) (references l)
    identifiers (HCons e l) = liftM2 (++) (identifiers e) (identifiers l)
    modules (HCons e l) = modules e ++ modules l
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

instance (Submittable a) => Submittable (Htable a) where
    submission (Htable id q f) =
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
            elem = evalState (f rel) (id ++ "",0,Map.empty)
        in  submission elem
        
instance (Renderable a) => Renderable (Htable a) where
                                        
    references (Htable id q f) = do
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
        let e = evalState (f rel) (id ++ "",0,Map.empty)
        references e
    
    identifiers (Htable id q f) = do
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
        let e = evalState (f rel) (id ++ "",0,Map.empty)
        identifiers e
    
    directives (Htable id q f) = do
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
        -- let helems = evalState (f rel) (id ++ "",0,Map.empty)
        let (helems,(_,_,transitions)) = runState (f rel) (id ++ "",0,Map.empty)
        let update = \(Environment m p c s) -> (Environment (Map.unionWith (++) m transitions) p (ListContext id) s)
        temp <- local update (template helems)
        ctrl <- local update (controller helems)
        ctx <- fmap getContext ask
        classes <- traceShow rel $ fmap getClasses ask
        let headers = let (Rel r) = rel in vcat $ map (\(h,_) -> text "+ '<div class=\"table-cell\">'"
                                               $$ nest 4 (text ("+ '" ++  h ++ "'"))
                                               $$ text "+ '</div>'") r
        let dir = text (printf "angular.module('%s', ['pose','data'])" id) -- traceShow ("in directive Htable: " ++ show (model helems)) $ 
               $$ nest 4 (text (printf ".directive('dir%s', function() {" (capitalize id))
               $$ nest 4 (text "return {"
               $$ nest 4 (text "scope: {context: '='},"
               $$ text "template: '<div class=\"table" <+> hsep (map text classes) <> text "\">'"
               $$ nest 4 (text "+ '<div class=\"table-row\">'"
               $$ nest 4 headers
               $$ text "+ '</div>'")
               -- $$ nest 4 (text "+ '<tbody>'"
               $$ nest 4 (text "+ '<div class=\"table-row\" ng-repeat=\"obj in context\">'"
               $$ nest 4 temp
               $$ text "+ '</div>'")
               $$ text "+ '</div>'"
               -- $$ text "+ '</tbody>'") 
               <> comma
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

    template (Htable id q f) = do
        ctx <- fmap getContext ask
        case ctx of 
            EmptyContext -> return $ text "+ '<dir-" <> text id <+> text "context=\""<> text id <> text "\">" <> text "</dir-" <> text id <> text ">'"
            ListContext _ -> return $ text "+ '<dir-" <> text id <+> text "context=\""<> text id <> text "[$index]\">" <> text "</dir-" <> text id <> text ">'"
                                 
    controller (Htable id q f) = do
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
    modules (Htable id q f) = id : (modules $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty))
    toElementList (Htable id q f) = toElementList $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
-- instance (Modellable a) => Modellable (Htable a) where
    -- model (Htable id q f) = 
        -- let (rel,(_,pq)) = runState q (0,EmptyQuery)
            -- elem = evalState (f rel) (0,Map.empty) in
        -- arrayfy $ model elem 
-- instance (Filterable a) => Filterable (Htable a) where
    -- item (Htable id q f) = item $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
instance (Filterable a) => Filterable (Htable a) where
    isNamed n (Htable id q f) = isNamed n $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
    parameters (Htable id q f) = parameters $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
    children (Htable id q f) = children $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
    contains i (Htable id q f) = contains i $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
instance (Presentable a) => Presentable (Htable a) where
    present (Htable id q f) = do
        e' <- present (evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty))
        return $ text "Goto" <> (parens . doubleQuotes) (text id)
              $$ text "Say" <> (parens . doubleQuotes) (text "this is " <> text id)
              $$ text "; Sleep, 1000"
              $$ e'