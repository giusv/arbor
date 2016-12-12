module Language.Elements.Chart where
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

                  
data Chart a b = Chart Identifier (Query Rel) (Rel -> (Expression a, Expression b))
instance Show (Chart a b) where
    show (Chart id q f) = "(Chart " ++ show id ++ ")"
    
instance Eq (Chart a b) where
    (Chart id1 _ _) == (Chart id2 _ _) = id1 == id2
instance Alternable (Chart a b)
instance Identifiable (Chart a b) where
    identifier (Chart id q f) = id
chart :: Query Rel -> (Rel -> (Expression a, Expression b)) -> SElement (Chart a b)
chart q f = do pref <- fmap first3 get
               i <- nextIndex
               return $ Chart (pref ++ "chart" ++ show i) q f


instance (Renderable (Chart a b)) where
    references (Chart id q f) = 
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
            (Expression a,Expression b) = f rel
            a' = getId a
            b' = getId b
        in return $ a' ++ b'
        where getId (IdExpr id) = [id]
              getId _ = []
    identifiers (Chart id q f) = return [id]
    
    directives (Chart id q f) = return []
        
    template (Chart id q f) = do
        return $ text "+ '<canvas id=\"" <> text id <> text "\" width=\"400\" height=\"300\">'"
              $$ text "+ '</canvas>'"
         
    controller (Chart id q f) = do
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
        let (Expression x,Expression y) = f rel
        x' <- generateCtrlExpr x
        y' <- generateCtrlExpr y
        q' <-  generateQuery id pq
        ctx <- fmap getContext ask
        return $ text "var ctx = document.getElementById(\"" <> text id <> text "\").getContext(\"2d\");"
              $$ text "var myChart = new Chart(ctx, {"
              $$ text "  type: \"polarArea\","
              $$ text "  data: {"
              $$ text "    labels: [\"v1\", \"v2\", \"v3\", \"v4\", \"v5\", \"v6\"],"
              $$ text "    datasets: [{"
              $$ text "      backgroundColor: ["
              $$ text "        \"#2ecc71\","
              $$ text "        \"#3498db\","
              $$ text "        \"#95a5a6\","
              $$ text "        \"#9b59b6\","
              $$ text "        \"#f1c40f\","
              $$ text "        \"#e74c3c\","
              $$ text "      ],"
              $$ text "      data: [12, 19, 3, 17, 28, 24]"
              $$ text "    }]"
              $$ text "  }"
              $$ text "});"
    modules (Chart id q f) = []
    toElementList e = [Element e]
-- instance (Modellable a) => Modellable (Chart a) where
    -- model (Chart id q f) = 
        -- let (rel,(_,pq)) = runState q (0,EmptyQuery)
            -- elem = evalState (f rel) (0,Map.empty) in
        -- arrayfy $ model elem 
-- instance Filterable (Chart a b) where
    -- item  (Chart id q f) = ItemLeaf id

instance Filterable (Chart a b) where
    contains i j = identifier i == identifier j

instance Presentable (Chart a b) where
    present (Chart id q f) = do
        return $ text "Goto" <> (parens . doubleQuotes) (text id)
              $$ text "Say" <> (parens . doubleQuotes) (text "this is " <> text id)
              $$ text "; Sleep, 1000"
