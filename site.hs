{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

{-

  The general idea of a Hakyll website is trivial: convert markdown files to HTML (using Pandoc under the hood)
  and put the results, as well as the static assets, in the output directory, `_site`.
  However, there are two non-trivial tasks with our site:

  1. Generating `chapters.html`, which is a table of contents (TOC) for all chapters, including subsections.
  2. Back and forward links between chapters.

  To solve both, we first use Pandoc to parse all chapter markdown files to extract chapter numbers and titles
  from the front matter (a piece of YAML at the top of file), as well as the list of sections for that chapter;
  this is done by the `buildChapterList` function, which produces a list of `ChapterInfo` records.
  Then, we construct Hakyll contexts for (1) (see `chaptersCtx` under `create ["chapters.html"]`) and
  (2) (see the `chapterCtx` function). Hakyll contexts are used to populate the HTML templates.

-}

import Hakyll
import Data.List (sortOn)
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Pandoc.Definition
import Text.Pandoc.Walk as Pandoc (query)
import Text.Pandoc.Shared as Pandoc (stringify)
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Options (readerExtensions, Extension(Ext_implicit_figures), ReaderOptions)
import Text.Pandoc.Extensions (disableExtension)
import System.Directory (listDirectory)
import Control.Monad (forM_)
import System.FilePath ((</>), replaceExtension, takeFileName)

-- Paths
sourceMdDir :: FilePath
sourceMdDir = "source_md"

staticDir :: FilePath
staticDir = "static"

templatesDir :: FilePath
templatesDir = "templates"

defaultTemplate :: FilePath
defaultTemplate = templatesDir </> "default.html"

chaptersTemplate :: FilePath
chaptersTemplate = templatesDir </> "chapters.html"

-- Data type for chapter metadata
data ChapterInfo = ChapterInfo
    { chapterFile :: FilePath
    , chapterNumber :: Int
    , chapterTitle :: String
    , chapterSections :: [Section]
    }
    deriving Show

-- Data type for section with anchor and title
data Section = Section
    { sectionAnchor :: String
    , sectionTitle :: String
    }
    deriving Show

-- Helper route to strip source_md/ directory and set .html extension
stripSourceMdRoute :: Routes
stripSourceMdRoute = customRoute (takeFileName . toFilePath) `composeRoutes` setExtension "html"

main :: IO ()
main = hakyll $ do
    -- Copy static assets to the destination
    match (fromGlob $ staticDir </> "**") $ do
        route $ gsubRoute (staticDir ++ "/") (const "")
        compile copyFileCompiler
    
    -- Pre-complile templates
    match (fromGlob defaultTemplate) $ compile templateBodyCompiler
    match (fromGlob chaptersTemplate) $ compile templateBodyCompiler
    
    -- Collect all chapters with their metadata using Pandoc
    chapterFiles <- buildChapterList
    
    -- Convert chapter markdown files to HTML using foraward/back-link info from a Hakyll context (`chapterCtx`)
    let chapterTriples = zipPrevNext chapterFiles

    forM_ chapterTriples $ \(mprev, ChapterInfo{chapterFile}, mnext) -> do
        match (fromGlob $ sourceMdDir </> chapterFile) $ do
            route stripSourceMdRoute
            compile (customPandocCompiler
                >>= loadAndApplyTemplate (fromFilePath defaultTemplate) (chapterCtx mprev mnext))
    
    -- Generate chapters.html (TOC)
    create ["chapters.html"] $ do
        route idRoute
        compile $ do
            -- Build Hakyll context with nested lists of fields for chapters and sections inside chapters.
            let sectionContext = 
                    field "link" (\item -> do
                        let (chFile, sec) = itemBody item
                        -- Build full URL from chapter file and section anchor
                        return $ replaceExtension chFile ".html" ++ "#" ++ sectionAnchor sec) <>
                    field "title" (return . sectionTitle . snd . itemBody)
                
                makeSectionItem :: ChapterInfo -> Section -> Item (FilePath, Section)
                makeSectionItem ch sec = Item (fromFilePath $ chapterFile ch) (chapterFile ch, sec)
                
                chapterItemContext = 
                    field "htmlname" (return . flip replaceExtension ".html" . chapterFile . itemBody) <>
                    field "title" (return . chapterTitle . itemBody) <>
                    field "number" (return . show . chapterNumber . itemBody) <>
                    listFieldWith "sections" sectionContext (\item -> 
                        let ch = itemBody item
                        in return $ map (makeSectionItem ch) $ chapterSections ch)
                
                makeChapterItem :: ChapterInfo -> Item ChapterInfo
                makeChapterItem ch = Item (fromFilePath $ chapterFile ch) ch

                -- the final nesting context
                chaptersCtx =
                    constField "title" "Learn You a Haskell for Great Good!" <>
                    listField "chapters" chapterItemContext (return $ map makeChapterItem chapterFiles) <>
                    defaultContext
            
            makeItem ""
                >>= loadAndApplyTemplate (fromFilePath chaptersTemplate) chaptersCtx
                >>= loadAndApplyTemplate (fromFilePath defaultTemplate) chaptersCtx
    
    -- Generate faq.html
    match (fromGlob $ sourceMdDir </> "faq.md") $ do
        route stripSourceMdRoute
        compile $ do
            customPandocCompiler
                >>= loadAndApplyTemplate (fromFilePath defaultTemplate)
                        (constField "faq" "true" <>
                         defaultContext)


-- List of chapters sorted by chapter number from YAML metadata
buildChapterList :: Rules [ChapterInfo]
buildChapterList = preprocess $ do
    files <- listDirectory sourceMdDir
    maybeChapters <- mapM getChapterData files
    return $ sortOn chapterNumber (catMaybes maybeChapters)
  where
    getChapterData :: FilePath -> IO (Maybe ChapterInfo)
    getChapterData fname = do
        let fullPath = sourceMdDir </> fname
        content <- readFile fullPath
        
        -- Extract chapter number and other metadata from Pandoc's parsed metadata
        pandoc <- runIO $ readMarkdown customReaderOptions (T.pack content)
        return $ case pandoc of
            Right (Pandoc meta blocks) -> do
                -- If there's no `chapter` field, it's not a chapter file (e.g., FAQ), and we return Nothing
                chapterMeta <- M.lookup "chapter" (unMeta meta)
                return ChapterInfo
                    { chapterFile = fname
                    , chapterNumber = read . T.unpack $ Pandoc.stringify chapterMeta
                    -- RE fromJust below: every chapter is ought to have a title field
                    , chapterTitle = T.unpack . Pandoc.stringify . fromJust $ M.lookup "title" (unMeta meta)
                    , chapterSections = Pandoc.query getSections blocks
                    }
            Left err -> error $ "Failed to parse " ++ fullPath ++ ": " ++ show err

    getSections :: Block -> [Section]
    getSections (Header 2 (anchor, _, _) inlines) =
        [Section (T.unpack anchor) (T.unpack $ Pandoc.stringify inlines)]
    getSections _ = []

-- Helper function to build chapter context with optional prev/next navigation
chapterCtx :: Maybe ChapterInfo -> Maybe ChapterInfo -> Context String
chapterCtx mprev mnext =
    constField "footdiv" "true" <>
    maybeChapterContext "prev" mprev <>
    maybeChapterContext "next" mnext <>
    defaultContext
  where
    maybeChapterContext :: String -> Maybe ChapterInfo -> Context String
    maybeChapterContext prefix mchapter =
        maybe mempty (\ChapterInfo{chapterFile, chapterTitle} ->
            constField (prefix ++ "_filename") (replaceExtension chapterFile ".html") <>
            constField (prefix ++ "_title") chapterTitle) mchapter

-- Custom pandoc compiler that uses our custom reader options
customPandocCompiler :: Compiler (Item String)
customPandocCompiler = pandocCompilerWith customReaderOptions defaultHakyllWriterOptions

-- Custom reader options that disable implicit_figures extension
customReaderOptions :: ReaderOptions
customReaderOptions = defaultHakyllReaderOptions
  { readerExtensions = disableExtension Ext_implicit_figures 
                      (readerExtensions defaultHakyllReaderOptions)
  }

-- Helper function to pair each element with its previous and next elements
zipPrevNext :: [a] -> [(Maybe a, a, Maybe a)]
zipPrevNext xs = zip3 (Nothing : map Just xs) xs (map Just (drop 1 xs) ++ [Nothing])
