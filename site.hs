--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.List (isSuffixOf)
import Data.Monoid (mappend)
import Hakyll
import Hakyll.Web.Pandoc
import System.FilePath
import Text.Pandoc.Options

--------------------------------------------------------------------------------

config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "docs"
    }

main :: IO ()
main = hakyllWith config $ do
  match "img/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "fonts/**" $ do
    route idRoute
    compile copyFileCompiler

  match "_headers" $ do
    route idRoute
    compile copyFileCompiler

  match (fromList ["about.markdown", "contact.markdown", "404.markdown"]) $ do
    route $ setExtension "html"
    compile
      $ myPandocCompiler -- pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile
      $ myPandocCompiler -- pandocCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= loadAndApplyTemplate "templates/default-post.html" postCtx
      >>= relativizeUrls

  {-match "CNAME" $ do
      route   idRoute
      compile copyFileCompiler-}

  match "posts.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let postsCtx =
            listField "posts" postCtx (return posts)
              `mappend` defaultContext

      getResourceBody
        >>= applyAsTemplate postsCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts)
              `mappend` defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match ("templates/*" .||. "partials/*") $ compile templateBodyCompiler

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx `mappend` bodyField "description"
      posts <-
        fmap (take 10)
          . recentFirst
          =<< loadAllSnapshots "posts/*" "content"
      renderAtom myFeedConfiguration feedCtx posts

  create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
      -- load and sort the posts
      posts <- recentFirst =<< loadAll "posts/*"

      -- load individual pages from a list (globs DO NOT work here)
      singlePages <- loadAll (fromList ["posts.html"])
      -- singlePages <- loadAll (fromList ["about.rst", "contact.markdown"])

      -- mappend the posts and singlePages together
      let pages = posts <> singlePages

          -- create the `pages` field with the postCtx
          -- and return the `pages` value for it
          sitemapCtx =
            constField "root" root
              <> listField "pages" postCtx (return pages)

      -- make the item and apply our sitemap template
      makeItem ""
        >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

  create ["robots.txt"] $ do
    route idRoute
    compile $ do
      let sitemapCtx = constField "root" root

      -- make the item and apply our sitemap template
      makeItem ""
        >>= loadAndApplyTemplate "templates/robots.txt" sitemapCtx

--------------------------------------------------------------------------------
-- postCtx :: Context String
-- postCtx =
-- dateField "date" "%e %B, %Y"
-- `mappend` defaultContext
postCtx :: Context String
postCtx =
  mconcat
    [ dateField
        "date"
        "%e %B, %Y"
    , dateField
        "date_ymd"
        "%Y-%m-%d"
    , niceUrlField
        "url"
    , defaultContext
    , constField "root" root
    ]

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration =
  FeedConfiguration
    { feedTitle = "Posts"
    , feedDescription = "A list of my most recent posts"
    , feedAuthorName = "connrs"
    , feedAuthorEmail = "me@conn.rs"
    , feedRoot = root
    }

myHakyllWriterOptions :: WriterOptions
myHakyllWriterOptions = defaultHakyllWriterOptions{writerExtensions = myHakyllWriterExtensions}

myHakyllWriterExtensions :: Extensions
myHakyllWriterExtensions =
  let def = writerExtensions defaultHakyllWriterOptions
   in disableExtension Ext_smart def

myHakyllReaderOptions :: ReaderOptions
myHakyllReaderOptions = defaultHakyllReaderOptions{readerExtensions = myHakyllReaderExtensions}

myHakyllReaderExtensions :: Extensions
myHakyllReaderExtensions =
  let def = readerExtensions defaultHakyllReaderOptions
   in disableExtension Ext_smart def

myPandocCompiler :: Compiler (Item String)
myPandocCompiler = pandocCompilerWith myHakyllReaderOptions myHakyllWriterOptions

-- url field without /index.html
niceUrlField :: String -> Context a
niceUrlField key = field key niceItemUrl

niceItemUrl :: Item a -> Compiler String
niceItemUrl =
  fmap (maybe "" (dropExtensions . removeIndexStr . toUrl)) . getRoute . itemIdentifier
  where
    removeIndexStr url = case splitFileName url of
      (dir, "index.html") -> dir
      _ -> url

root :: String
root = "https://conn.rs"