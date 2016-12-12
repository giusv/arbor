module Language.Elements.Hpanel where
import Language.Commons
import Language.Expression
import Language.Query
-- import Language.Filter
-- import Language.Element
import Language.Environment
import Language.Transition
import Language.Utils
-- import Language.Element
-- import Language.Filter
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Text.PrettyPrint
import Text.Printf
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map
import Debug.Trace

instance Alternable (Hpanel a b c)
instance (Submittable a, Submittable b) => Submittable (Hpanel a b c) where
    submission (Hpanel z i a b c) = submission a ++ submission b
 -- where
    -- validator (Hpanel _ e f) = validator e ++ validator f

type Hcolor = String
type HIcon = String
data Hpanel a b c = Hpanel Hcolor HIcon a b c deriving (Eq)
instance (Show a, Show b, Show c) => Show (Hpanel a b c) where
    show (Hpanel z i a b c) = "(Hpanel " ++ show a ++ " " ++ show b ++ " " ++ show c ++ ")"


-- infoPanel :: (Renderable a, Renderable c) => Hcolor -> a -> Rel -> [(Attribute,Attribute)] -> c -> SElement (Hpanel a b c)
-- infoPanel z i a r attrs c = 
hpanel :: (Renderable a, Renderable b, Renderable c) => Hcolor -> HIcon -> a -> b -> c -> SElement (Hpanel a b c)
hpanel z i a b c = return $ Hpanel z i a b c

instance (Renderable a, Renderable b, Renderable c, 
          Eq a, Eq b, Eq c,
          Show a, Show b, Show c,
          Presentable a, Presentable b, Presentable c,
          Filterable a, Filterable b, Filterable c) => Filterable (Hpanel a b c) where
    children (Hpanel z i a b c) = [Element a,Element b]
    contains i (Hpanel z _ a b c) = contains i a || contains i b

instance (Renderable a, Renderable b, Renderable c) => Renderable (Hpanel a b c) where
    references (Hpanel z i a b c) = do
        a' <- references a 
        b' <- references b 
        c' <- references c 
        return $ a' ++ b' ++ c'
    identifiers (Hpanel z i a b c) = do
        a' <- identifiers a 
        b' <- identifiers b 
        c' <- identifiers c 
        return $ a' ++ b' ++ c'
    check s@(Hpanel z i a b c) = do 
        a' <- check a 
        b' <- check b
        c' <- check c
        ids <- identifiers s
        refs <- references s
        let test = all (\i -> i `elem` ids) refs
        return $ a' && b' && c' && test

    directives (Hpanel z i a b c) = 
        do  dira <- directives a
            dirb <- directives b
            dirc <- directives c
            return $ dira ++ dirb ++ dirc
    
    template (Hpanel z i f g h)
        = do f' <- template f 
             g' <- template g
             h' <- template h
             return $ text "+ '<div class=\"panel panel-" <> text z <> text "\">'"
                   $$ nest 4 (text "+ '<div class=\"panel-heading \">'"
                   $$ nest 4 (text "+ '<div class=\"row\">'"
                   $$ nest 4 (text "+ '<div class=\"col-xs-3\">'"
                   $$ nest 4 (text "+ '<i class=\""<> text i <> text"\"></i>'")
                   $$ text "+ '</div>'"
                   $$ text "+ '<div class=\"col-xs-9 text-right\">'"
                   $$ nest 4 (text "+ '<h4>'"
                   $$ nest 4 f'
                   $$ text "+ '</h4>'")
                   $$ text "+ '</div>'")
                   $$ text "+ '</div>'")
                   $$ text "+ '</div>'"
                   $$ text "+ '<div class=\"panel-body\">'"
                   $$ nest 4 g'
                   $$ text "+'</div>'"
                   $$ text "+ '<div class=\"panel-footer\">'"
                   $$ nest 4 h'
                   $$ text "+ '</div>'")
                   $$ text "+ '</div>'"          
    controller (Hpanel z i f g h) = 
        do  f' <- controller f
            g' <- controller g
            h' <- controller h
            return (f' $$ g' $$ h')
            
    width (Hpanel z i f g h) = width f
    modules (Hpanel z i f g h) =  modules f ++ modules g ++ modules h
    toElementList (Hpanel z i f g h) = toElementList f ++ toElementList g ++ toElementList h

instance (Presentable a, Presentable b, Presentable c) => Presentable (Hpanel a b c) where
    present (Hpanel z i a b c) = do  
        pa <- present a
        pb <- present b
        pc <- present c
        return $ pa
              $$ pb
              $$ pc