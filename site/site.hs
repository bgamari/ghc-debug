--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

import           Control.Applicative
import           Control.Monad
import           Data.List           (delete)
import           Data.Maybe
import           Data.IORef

--------------------------------------------------------------------------------
main :: IO ()
main = do
  -- Each chart needs a unique ID
  globalCounter <- newIORef 0
  hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "data/*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    create ["docs.html"] $ do
        route idRoute
        compile $ do
            posts <- loadAllSnapshots "data/*" "content"
            let archiveCtx =
                    listField "posts" defaultContext (return posts) `mappend`
                    constField "title" "Documentation"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/docs.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.md" $ do
        route $ setExtension "html"
        compile $ do
            posts <- take 5 <$> (recentFirst =<< loadAllSnapshots "posts/*" "content")
            let indexCtx =
                    listField "posts" defaultContext (return posts) `mappend`
                    defaultContext

            pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

