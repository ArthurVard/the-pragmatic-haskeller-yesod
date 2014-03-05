module Handler.StoreRecipe where

import Import

--getStoreRecipeR :: Handler Html
--getStoreRecipeR = error "Not yet implemented: getStoreRecipeR"

--postStoreRecipeR :: Handler Html
-- postStoreRecipeR = return ()
postStoreRecipeR = return ()
--parseRecipe :: BL.ByteString -> Either String Object
-- parseRecipe = eitherDecode'

-------------------------------------------------------------------------------
{-- storeRecipe :: BL.ByteString -> AppHandler (Either String Object)
storeRecipe recipe = case parseRecipe recipe of
      Left f -> return $ Left f
      Right r -> do
        res <- eitherWithDB $ insert "recipes" $ toBson r
        case res of
          Left _ -> return $ Left "Failed to store the recipe."
          Right _ -> return $ Right r--}

