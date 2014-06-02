{-# LANGUAGE OverloadedStrings #-}

module Text.Html.Jype
    ( html
    ) where

import Control.Monad
import Data.Foldable (toList)
import Data.Monoid
import Data.List
import Data.Text (Text)

import Text.Blaze.Html5 hiding (html, body, map)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (name)
import qualified Text.Blaze.Html5.Attributes as A

import Data.Jype.Syntax

html :: [Decl] -> Html
html decls = docTypeHtml $ do
    H.head $ do
        link ! rel "stylesheet" ! type_ "text/css" ! href "http://cdnjs.cloudflare.com/ajax/libs/semantic-ui/0.16.1/css/semantic.min.css"
        script "" ! type_ "text/javascript" ! src "http://cdnjs.cloudflare.com/ajax/libs/semantic-ui/0.16.1/javascript/semantic.min.js"
    H.body $ do
        h1 "jype-html" ! class_ "ui center aligned header purple"
        H.div ! class_ "ui grid" $ do
            H.div "" ! class_ "three wide column"
            H.div ! class_ "ten wide column" $ mapM_ declHtml decls
            H.div "" ! class_ "three wide column"

declHtml :: Decl -> Html
declHtml (Decl name body descs) = H.div ! A.id (toValue $ typeNameConstr name) ! class_ "ui segment" $ do
    h2 ! class_ "ui left floated header" $ toHtml $ show name
    H.div "" ! class_ "ui clearing divider"
    p ! class_ "sub header" $ concatDescs descs
    case body of
        Object fields -> obj fields
        Choice choices -> choice choices
        Primitive -> prim
  where
    obj fields = do
        h3 "Record Type"
        table ! class_ "ui celled table segment" $ do
            tr $ do
                th "key"
                th "type"
                th "description"
            forM_ fields $ \field -> tr $ do
                td $ toHtml $ fieldKey field
                td $ typeLink $ fieldType field
                td $ concatDescs $ toList (fieldDescription2 field) ++ fieldDescription1 field
    choice choices = do
        h3 "Union Type"
        table ! class_ "ui celled table segment" $ do
            tr $ do
                th "type/value"
                th "description"
            forM_ choices $ \ch -> tr $ do
                td $ toHtml $ either (toHtml . show) typeLink $ choiceEither ch
                td $ concatDescs $ toList (choiceDescription2 ch) ++ choiceDescription1 ch
    prim = h3 "Primitive Type"

typeLink :: ConcreteType -> Html
typeLink (ConcreteType name params) = do
    a ! href (toValue $ "#" ++ name) $ toHtml name
    when (params /= []) $ do
        "["
        sequence_ $ intersperse ", " $ map typeLink params
        "]"

concatDescs :: [Text] -> Html
concatDescs = mconcat . intersperse br . map toHtml
