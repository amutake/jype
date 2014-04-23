{-# LANGUAGE OverloadedStrings #-}

module Text.Jype.Html where

import Control.Monad

import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

import Data.Jype.Types

html :: [Decl] -> Html
html decls = docTypeHtml $ do
    H.head $ do
        link ! rel "stylesheet" ! type_ "text/css" ! href "./jype.css"
    body $ do
        h1 "jype-doc"
        mapM_ declHtml decls

declHtml :: Decl -> Html
declHtml (Decl name body) = H.div ! A.id (toValue $ typeNameConstr name) ! class_ "decl" $ do
    h2 $ toHtml $ show name
    case body of
        Object fields -> obj fields
        Choice valueTypes -> choice valueTypes
        Primitive -> prim
  where
    obj fields = do

        table $ do
            tr $ do
                th "key"
                th "type"
                th "description"
            forM_ fields $ \field -> tr $ do
                td $ toHtml $ fieldKey field
                td $ typeLink $ fieldType field
                td "none"
    choice valueTypes = ul $ do
        mapM_ (either (li . toHtml . show) (li . typeLink)) valueTypes
    prim = p "<primitives>"

typeLink :: ConcreteType -> Html
typeLink ty@(ConcreteType name _) = a ! href (toValue $ "#" ++ name) $ toHtml $ show ty
