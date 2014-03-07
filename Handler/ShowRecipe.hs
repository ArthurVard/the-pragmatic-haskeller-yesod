module Handler.ShowRecipe where

import Import

import Handler.JSON.Parser
import Data.Aeson
import qualified Data.ByteString.Lazy as BL

{-===========================================================================-}
{-                                  HANDLERS                                 -}
{-===========================================================================-}

getShowRecipeR =  do
    toParse <- liftIO $ BL.readFile "recipe.json"
    let recipeJson = eitherParse toParse
    defaultLayout 
        [whamlet|
        <h6>
         <form method=post action=@{ShowRecipeR} enctype="type">
           #{recipeJson}
           <input type=submit value="Save">
            
        |]
   where eitherParse tp = case (eitherDecode' tp :: Either String Object) of
                           Left e -> show e
                           Right r -> show $ r

--getShowRecipeR :: Handler Html
postShowRecipeR = do
    toParse <- liftIO $ BL.readFile "recipe.json"
    eitherParse toParse
    
    where eitherParse tp = case (eitherDecode' tp :: Either String Recipe) of
                           Left e -> return ()
                           Right r -> do  _ <- runDB $ insert r 
                                          setMessage "Save Success!."
                                          redirect ShowRecipeR
    
    

    --defaultLayout 
                  --  [whamlet|
                  --      Post page
                 --   |]