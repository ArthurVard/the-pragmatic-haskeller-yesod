{-# LANGUAGE OverloadedStrings #-}

module Handler.NewRecipe where

import Import
import Control.Lens
import Data.Aeson
import Handler.DSL.Parser
import qualified Data.Text as T (pack, unlines, unpack ) --already imported in Import module
import qualified Data.ByteString.Char8 as BC (unpack, unlines, pack)
import Text.Parsec (parse)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding (decodeUtf8)

initialDsl :: Text
initialDsl = T.unlines  [
                      "\"Տորթ\" is made with\r",
                      "    250 gr of \"Flour\"\r",
                      "    250 gr of \"Sugar\"\r",
                      "    130 ml of \"Sunflower Oil\"\r",
                      "    130 ml of \"Water\"\r",
                      "    3 \"Eggs\"\r",
                      "\r",
                      "  prepared by\r",
                      "    \"Mixing everything\" and\r",
                      "    \"Cooking in oven at 200 degrees\" for 40 minutes"]

getNewRecipeR :: HandlerT App IO Html
getNewRecipeR = defaultLayout $(widgetFile "new_dsl")
        {--[whamlet|
        <h6>
         <form method=post action=@{NewRecipeR} enctype="type">
           #{initialDsl}
           <input type=submit value="Save">
           <br>
         <form method=post action=@{NewRecipeR} enctype="type">
           AAA
           <input type=submit value="To Json">
            
        |]--}

--recipe2json :: Recipe -> Text
--recipe2json = decodeUtf8 . toStrict . encodePretty

makeLenses ''Step
makeLenses ''Recipe
-- Uses lenses to incrementally increase the order
--correctOrder :: Recipe -> Recipe
--correctOrder r = r { _steps = newSteps (_steps r)}
-- where newSteps s = zipWith (over _order) (const <$> [1..length s]) s



--bParser = parse recipe "" initialDsl

--parseRecipe = parse recipe "" initialDsl
--postNewRecipeR :: Yesod site => HandlerT site IO Html
postNewRecipeR = do --defaultLayout [whamlet| get RecipeR|]
                  dsl <- runInputPost $ ireq textareaField "dsl"
                  maybe (defaultLayout [whamlet| Dsl can't be empty! |])(\s ->
                    case parse recipe "" s of
                      Left e -> defaultLayout [whamlet| #{show e} |]
                      Right r -> do  _ <- runDB $ insert r
                                     defaultLayout [whamlet| #{T.pack $show r} |]
                          ) (Just (T.unpack $ unTextarea dsl))


                               

    



