{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

module DOI
  where

import qualified BibTeX as B1
import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bool
import           Data.Char
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.String.Utils hiding (join)
import           Data.Time.Format
import           Data.Time.LocalTime
import           Data.URLEncoded
import           Options.Applicative hiding (action)
import           Safe
import           System.Console.Haskeline
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import qualified System.IO.Strict as S
import           System.IO.Temp
import           System.Process
import           Text.BibTeX.Entry
import           Text.BibTeX.Format
import           Text.HTML.TagSoup
import           Text.Parsec.Prim hiding ((<|>))
import           Text.Printf
import           Text.Read
import           Text.Regex
import           Text.Regex.Base.RegexLike
import           Text.Regex.TDFA

-- * hard coded global config (bad)
dst = "/home/data/promotion/Literatur"
-- ^ destination folder

pdfSubDir = "doi"
-- ^ subdirectory for PDFs

bibFile = dst </> "literatur.bib"           
-- ^ BibTeX entries are appended to this file 

-- | Url of the bibsonomy scraper (e.g. their public version of self hosted)
--
-- Self hosted: needs to be installed in ROOT app (tested on tomcat8)
--
-- @  cd \/tmp && wget http:\/\/dev.bibsonomy.org\/maven2\/org\/bibsonomy\/bibsonomy-scrapingservice\/3.3.0\/bibsonomy-scrapingservice-3.3.0.war && sudo mv bibsonomy-scrapingservice-3.3.0.war \/var\/lib\/tomcat8\/webapps\/ROOT.war @
bibsonomyUrl = if True then
  "http://scraper.bibsonomy.org/service?format=bibtex&selection=&url="
  else
  "http://localhost:8080/service?format=bibtex&selection=&"
  -- needs to be install in ROOT app (tested on tomcat8)
  -- cd /tmp && wget http://dev.bibsonomy.org/maven2/org/bibsonomy/bibsonomy-scrapingservice/3.3.0/bibsonomy-scrapingservice-3.3.0.war && sudo mv bibsonomy-scrapingservice-3.3.0.war /var/lib/tomcat8/webapps/ROOT.war

-- * The types and the program
   
data Options = Options
  { oTarget :: String
  , oFile :: Maybe String
  , oKey :: Maybe String }
  deriving Show

optP :: Parser Options
optP = Options
       <$> (on (<|>) (strArgument . metavar) "URL" "DOI")
       <*> optional (strOption
                     ( short 'f'
                       <> metavar "PATH"
                       <> help "Path to possibly pre-existing pdf" ))
       <*> optional (strOption
                     ( short 'k'
                       <> help "Provide a BibTeX Key" ))

main :: IO ()
main = customExecParser (prefs showHelpOnError) (info (helper <*> optP) i) >>= action
  where i = fullDesc
            <> header "retrieve BibTeX information and PDFs from a DOI or URL" 


test4 = " @article{Armada_2007, title={A modified finite-lived American exchange option methodology applied to real options valuation}, volume={17}, ISSN={1044-0283}, url={http://dx.doi.org/10.1016/j.gfj.2006.05.006}, DOI={10.1016/j.gfj.2006.05.006}, number={3}, journal={Global Finance Journal}, publisher={Elsevier BV}, author={Armada, Manuel Rocha and Kryzanowski, Lawrence and Pereira, Paulo Jorge}, year={2007}, month={Mar}, pages={419\8211\&438}}\n"


test = "http://www.sciencedirect.com/science/article/pii/S1044028306000603"
test2 ="http://dx.doi.org/10.1016/j.gfj.2006.05.006"
test3 = "http://dx.doi.org/10.2139/ssrn.1709599"
       
parseOrError name x y = either (error . unlines . (:[y]) . show) id $ parse x name y
parseBib name bib = f . parseOrError name (B1.skippingLeadingSpace $ B1.skippingSpace B1.file)
                    . sr "^@comment" "comment" $ bib
  where f [x] = Just x
        f [] = Nothing -- todo: propagate error, but fail only
        -- if there are no bibtex entries at all (also from other
        -- sources)
        f x = error $ printf "BibTeX %s parser returns wrong number of entries:\n%s\n%sEOF\n" name (show x) bib

doiFromBibsonomy :: Maybe T -> String
doiFromBibsonomy b =  f $ find ((=="doi") . fmap toLower . fst) =<< fmap fields b
  where f = fromMaybe (error $ printf "Bibsonomy returned no DOI:\n%s\n" $ maybe "" entry b)
            . (extractDoi . snd =<<)
                    
existingKey Nothing = return ()
existingKey (Just key) = (isInfixOf key <$> readFile bibFile) >>= flip when
                         (printf "Key '%s' already present in '%s'\n" key bibFile >> exitFailure)

-- | first arg: doi or url
--  second arg: filename for preexisting file (which will be moved to doi location)
--              or value for the 'file' field
action (Options raw path key) = do
  existingKey key
  let edoi = extractDoi raw
      url = maybe raw doiUrl edoi
  bibson <- async $ downloadBibTeX url bibsonomy
  doi <- fromMaybe (doiFromBibsonomy <$> wait bibson) $ return <$> edoi
  -- print bibson
  -- print doi  
  bibs <- (fmap catMaybes . mapM wait . (bibson:)) =<<
          mapM (async . downloadBibTeX (doiUrl doi)) [crossref2]
  -- crossref not as up-to-date e.g. for SSRN
  file <- maybeToList . fmap ((,) "file") <$> getFile doi path
  timestamp <- formatTime defaultTimeLocale "%FT%T" <$> getZonedTime
  let mod bib = bib { fields =
                          fields bib ++ file ++ [("timestamp",timestamp)]
                    , identifier = fromMaybe  (identifier bib) key}
      str = entry $ mod $ merge bibs
  putStrLn str
  appendFile bibFile $ "\n" ++ str


-- combine several bibtex entries into one. the result with contain
-- the fields of both entries and thus contain duplicates.
merge :: [T] -> T
merge xs = (headNote "empty list in 'merge'" xs) {
  fields = sortBy (comparing fst) $ nub $
           fmap (normalizeDoi . first (fmap toLower) . second strip) $ concatMap fields xs }

normalizeDoi (f,v) = (,) f $
                     if f=="doi" then fromMaybe v $ extractDoi v
                     else v

getFile :: String -- ^ DOI
        -> Maybe String -- ^ Existing File if available
        -> IO (Maybe FilePath)
getFile doi f = do e <- doesFileExist target
                   if e then do pErr "File already exists: %s" target
                                return $ Just filename
                     else g f 
  where g (Just f) = do
          e <- doesFileExist f
          if e then withTarget (fmap Just . copyFile f)
            else do pErr "File %s does not exist. Using it literally." f
                    return $ Just f
        g Nothing = do r <- selectLink . extractLinks =<< doi2html doi
                       withTarget $ fmap join . forM r . downloadPdf
        filename = pdfSubDir </> sr "/" "SLASH" doi <.> "pdf"
        target = dst </> filename
        withTarget :: (String -> IO (Maybe ())) -> IO (Maybe FilePath)
        withTarget ac = do
          createDirectoryIfMissing True $ fst $ splitFileName target
          (fmap $ const filename) <$> ac target

extractDoi :: String -> Maybe String
extractDoi input = -- maybe (error $ printf "Could not extract DOI from '%s'"  input)
                   ((!!3).getAllTextSubmatches) <$>
                   input =~~ "^(http://(dx\\.)?doi\\.org/|doi:)?([^./]+\\.[^/]+/.*)"

doiUrl doi = "http://dx.doi.org/" ++ doi
    
selectLink :: [String] -> IO (Maybe String)
selectLink = runInputT defaultSettings . f . fmap transformUrl
  where f [] = do
          r <- fromJust <$> getInputChar "No Pdf Link Found, leave empty? [y/N] "
          if r == 'y' then return Nothing
            else error "not implemented, use -f argument instead"
        f [x] = return $ Just x
        f xs = do liftIO $ sequence $ reverse $ zipWith (printf "%2d: %s\n\n")
                    [(0::Int)..] ("(skip PDF download)":xs)
                  either ((>> f xs) . liftIO . putStrLn)
                    (return . selected)
                    . readEither . fromJust =<< getInputLine "Select PDF: "
          where selected 0 = Nothing
                selected x = Just $ xs !! (pred x)
        transformUrl = sr "(.*jstor.*)" "\\1?acceptTC=true"


pErr :: HPrintfType r => [Char] -> r
pErr x = hPrintf stderr $ "\n" ++ x ++ "\n\n"

extractLinks = mapMaybe mPdf . fmap (fromAttrib "href" . head)
               . sections (~== "<a href>") . parseTags

mPdf x = x =~~ "[^#].*pdf.*" :: Maybe String
mPdf2 x = (headNote "empty list in 'mPdf2'")
  <$> matchRegex (mkRegexWithOpts "^[^#].*pdf.*" False False) x

-- | get Location header
-- doi2url url = either (error.show) f <$> simpleHTTP (getRequest url) 
--   where f = fromMaybe (error $ "No Location Header in "++ url ) . findHeader HdrLocation

downloadBibTeX url (name,io) = do
  pErr "Downloading %s BibTeX for %s" name url
  a <- io url
  print a
  return $ parseBib name a :: IO (Maybe T)

bibsonomy = (,) "Bibsonomy" $ \url -> do
  readProcess2 "curl"
    ["-L"
     , bibsonomyUrl
       ++ export (importList [("url", url)])
     ]
    ""

readProcess2 a b c = do pErr $ sr "%" "%%" $ showCommandForUser a b
                        readProcess a b c
  
crossref = (,) "CrossRef" $ \url -> do
  readProcess2 "curl"
    (["-LH", "Accept: text/bibliography; style=bibtex", url])
    ""
  
crossref2 = (,) "CrossRef" $ \url -> do
  readProcess2 "curl"
    (["-LH", "Accept: application/x-bibtex", url])
    ""

uA = "Mozilla/5.0 (Windows NT 6.3; rv:36.0) Gecko/20100101 Firefox/36.0"

doi2html :: String -> IO String
doi2html url = do
  pErr "Downloading Website for %s" url
  withSystemTempFile "wget_doi" $ \file h -> do
    putStrLn file
    hClose h
    readProcess "wget"
      (["-U",uA,"-O",file,"-k"
       ] ++ [doiUrl url])
      ""
    S.readFile file
  -- readProcess "curl"
  --          ["-LH",uA
  --          ,doiUrl url]
  --          ""
           

downloadPdf :: String -> String -> IO (Maybe ())
downloadPdf target url = do
  pErr "Downloading PDF from for %s" url
  callProcess "wget"
    ["-U",uA
    ,"-O",target
    ,url]
  ft <- readProcess "xdg-mime"
    ["query","filetype",target]
    ""
  if isInfixOf "pdf" $ fmap toLower ft
    then return $ Just ()
    else do pErr "Filetype not PDF: %s" ft
            return Nothing

-- | substitute regex  
sr :: String -- ^ regex
   -> String -- ^ replacement
   -> String -- ^ input
   -> String
sr regex replacement input = subRegex (mkRegex regex) input replacement
