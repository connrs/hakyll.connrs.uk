--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.List   (isSuffixOf)
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------

config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }

main :: IO ()
main = hakyllWith config $ do
    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.markdown", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default-post.html" postCtx
            >>= relativizeUrls

    match "CNAME" $ do
        route   idRoute
        compile copyFileCompiler

    -- create ["posts.html"] $ do
    --     route idRoute
    --     compile $ do
    --         posts <- recentFirst =<< loadAll "posts/*"
    --         let archiveCtx =
    --                 listField "posts" postCtx (return posts) `mappend`
    --                 constField "title" "Posts"               `mappend`
    --                 boolField "is_posts" (const True)        `mappend`
    --                 defaultContext

    --         makeItem ""
    --             >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
    --             >>= loadAndApplyTemplate "templates/default.html" archiveCtx
    --             >>= relativizeUrls

    match "posts.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let postsCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate postsCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match ("templates/*" .||. "partials/*") $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%es %B, %Y" `mappend`
    defaultContext
