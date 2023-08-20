--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.List           (isSuffixOf)
import           Data.Monoid         (mappend)
import           Hakyll
import           Hakyll.Web.Pandoc
import           Text.Pandoc.Options
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

    match (fromList ["about.markdown", "contact.markdown", "404.markdown"]) $ do
        route   $ setExtension "html"
        compile $ myPandocCompiler -- pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ myPandocCompiler -- pandocCompiler
            >>= saveSnapshot "content"
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


    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderAtom myFeedConfiguration feedCtx posts

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%e %B, %Y" `mappend`
    defaultContext

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Posts"
    , feedDescription = "A list of my most recent posts"
    , feedAuthorName  = "connrs"
    , feedAuthorEmail = "me@connrs.uk"
    , feedRoot        = "http://connrs.uk"
    }

myHakyllWriterOptions :: WriterOptions
myHakyllWriterOptions = defaultHakyllWriterOptions { writerExtensions = myHakyllWriterExtensions }

myHakyllWriterExtensions :: Extensions
myHakyllWriterExtensions =
    let def = writerExtensions defaultHakyllWriterOptions
    in disableExtension Ext_smart def

myHakyllReaderOptions :: ReaderOptions
myHakyllReaderOptions = defaultHakyllReaderOptions { readerExtensions = myHakyllReaderExtensions }

myHakyllReaderExtensions :: Extensions
myHakyllReaderExtensions =
    let def = readerExtensions defaultHakyllReaderOptions
    in disableExtension Ext_smart def

myPandocCompiler :: Compiler (Item String)
myPandocCompiler = pandocCompilerWith myHakyllReaderOptions myHakyllWriterOptions
