module Language.Elements.Panel where
import Language.Commons
import Language.Expression
import Language.Query
import Language.Elements.Link
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


-- <div class="panel panel-primary">
                            -- <div class="panel-heading">
                                -- <div class="row">
                                    -- <div class="col-xs-3">
                                        -- <i class="fa fa-file-pdf-o fa-5x"></i>
                                    -- </div>
                                    -- <div class="col-xs-9 text-right">
                                        -- <h4>Nuovo Report</h4>
                                    -- </div>
                                -- </div>
                            -- </div>
                            -- <a href="#/report" data-ui-sref="report({type:'new'})">
                                -- <div class="panel-footer">
                                    -- <span class="pull-left">Vai alla Pagina</span>
                                    -- <span class="pull-right"><i class="fa fa-arrow-circle-right"></i></span>
                                    -- <div class="clearfix"></div>
                                -- </div>
                            -- </a>
                        -- </div>
                        
                        
                        
type Color = String
data Panel a = Panel Identifier Color Link (Expression a) deriving (Eq)

instance Show (Panel a) where
    show (Panel id c l e) = "(Panel " ++ id ++ ")"
    
instance Renderable (Panel a) where
    references (Panel _ _ _ (Expression (IdExpr id))) = return [id]
    references (Panel _ _ _ (Expression _)) = return []
    
    identifiers (Panel id _ _ _) = return [id]
    
    directives _ = return []
                               
    template (Panel id color link (Expression expr)) = do
        transMap <- fmap getTransitions ask
        transitions' <- sequence $ map generateViewTransition (fromMaybe [] $ Map.lookup id transMap)
        ctx <- fmap getContext ask
        classes <- fmap getClasses ask
         -- traceShow ("in template Panel before generateViewExpr: expr=" ++ show expr ++ " ctx: " ++ show ctx) $ 
        expr' <- generateViewExpr False expr
        link' <- template link
        -- expr' <- generateViewExpr True expr
        -- return $ text "+ '<input type=\"text\" ng-model=\"" <> text id <> text "\" class=\"arbor-Panel" <+> hsep (map text classes) <> text "\""
             -- <+> hsep transitions'
             -- -- <+> text "ng-bind=\"" <> expr' <> text "\"></Panel>'"
             -- <+> text "value=\"" <> expr' <> text "\"/>'"
        return $ text "+ '<div id=\"" <> text id <> text "\" class=\"panel panel-" <> text color <+> hsep (map text classes) <> text "\"" <+> hsep transitions' <> text ">'"
              $$ text "+ '    <div class=\"panel-heading\">'"
              $$ text "+ '        <div class=\"row\">'"
              $$ text "+ '            <div class=\"col-xs-3\">'"
              $$ text "+ '                <i class=\"fa fa-file-pdf-o fa-5x\"></i>'"
              $$ text "+ '            </div>'"
              $$ text "+ '            <div class=\"col-xs-9 text-right\">'"
              $$ text "+ '                <h4>" <> expr' <> text "</h4>'"
              $$ text "+ '            </div>'"
              $$ text "+ '        </div>'"
              $$ text "+ '    </div>'"
              $$ link'
              $$ text "+ '</div>'"
            
    controller (Panel id color link (Expression expr)) = -- return empty
        do  link' <- controller link
            return $ link' 
    -- template (Panel id (Expression expr)) = do
        -- transMap <- fmap getTransitions ask
        -- ctx <- fmap getContext ask
        -- classes <- fmap getClasses ask
        -- let suffix = case ctx of
                        -- EmptyContext -> empty
                        -- ListContext _ -> text "[$index]"
                        -- PlainFormContext _ -> error "PlainFormContext not allowed in input"
        -- transitions' <- sequence $ map generateViewTransition (fromMaybe [] $ Map.lookup id transMap)
        -- return $ text "+ '<input id=\"" <> text id <> text "\" type=\"text\" class=\"" <+> hsep (map text classes) <> text "\""
             -- <+> text "ng-model=\"" <> text id <> suffix <> text "\""
             -- <+> hsep transitions'
             -- <+> text "name=" <> doubleQuotes (text id)
             -- <+> text "style=\"outline:none;border-color:inherit;-webkit-box-shadow: none;box-shadow: none;\""
              -- <> text "/>'"

    -- controller (Panel id (Expression expr)) = -- return empty
        -- do  expr' <- generateCtrlExpr expr
            -- transMap <- fmap getTransitions ask
            -- ctx <- fmap getContext ask
            -- let init = case ctx of 
                            -- EmptyContext -> text "$scope." <> text id <+> equals <+> expr' <> semi 
                            -- ListContext _-> text "$scope.$watchCollection('context', function(newContext,oldContext){"
                                   -- $$ nest 4 (text "$scope." <> text id <+> equals 
                                  -- <+> text "newContext.map(function(obj,ind) {return " <> expr' <> semi <> text"});")
                                   -- $$ text "});"
                            -- PlainFormContext _-> error ("no PlainFormContext allowed for Input")
            -- return init
    toElementList e = [Element e]
-- instance Modellable (Panel a) where
    -- model (Panel m e) = m
instance Alternable (Panel a)
instance Filterable (Panel a) where
    contains i j = identifier i == identifier j
instance Identifiable (Panel a) where
    identifier (Panel id c l e) = id

panel :: Color -> Link -> Expression a -> SElement (Panel a)
panel col ln exp =  do pref <- fmap first3 get
                       i <- nextIndex 
                       return $ Panel (pref ++ "Panel" ++ show i) col ln exp
-- instance Filterable (Panel a) where
    -- item (Panel id _) = ItemLeaf id
    
instance Presentable (Panel a) where
    present (Panel id c l e) = do
        return $ text "Goto" <> (parens . doubleQuotes) (text id)
              $$ text "Say" <> (parens . doubleQuotes) (text "this is " <> text id)
              $$ text "; Sleep, 1000"
              