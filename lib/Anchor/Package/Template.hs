{-# LANGUAGE RecordWildCards #-}

module Anchor.Package.Template where

import           Control.Arrow
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set               as S
import           Data.Time.Clock
import           Data.Time.Format
import           System.Locale
import           System.Process

import           Anchor.Package.Types

generateSpecFile :: Packager String
generateSpecFile = generateTemplate "/usr/share/haskell2package/TEMPLATE.spec"

generateControlFile :: Packager String
generateControlFile = generateTemplate "/usr/share/haskell2package/control"

generateTemplate :: FilePath -> Packager String
generateTemplate templatePath = do
    m4Defs <- generateM4
    liftIO $ readProcess "m4" ["-", templatePath] m4Defs

-- | Generates the M4 defines to generate a specfile
generateM4 :: Packager String
generateM4 = do
    PackagerInfo{..} <- ask
    let CabalInfo{..} = cabalInfo
    currentTime <- liftIO getCurrentTime
    let dateString = formatTime defaultTimeLocale "%a %b %d %Y" currentTime
    let (srcStrings, setupStrings) = generateSetupAndSourceStrings $ S.toList anchorDeps
    let changelogHeading = generateChangelogHeading maintainerString versionString buildNoString dateString
    let defines = map generateDefineStatement
            [ ("NAME",        name)
            , ("PKGNAME",     fromMaybe name packageName)
            , ("VERSION",     versionString)
            , ("BUILDNO",     buildNoString)
            , ("SUMMARY",     synopsisString)
            , ("DESCRIPTION", descriptionString)
            , ("DEB_DESC",    concatMap (' ':) $ lines descriptionString)
            , ("SRCS",        srcStrings)
            , ("SETUP",       setupStrings)
            , ("COPYS",       generateCopyStrings executableNames)
            , ("FILES",       generateFileStrings executableNames)
            , ("ADD_SRCS",    generateSandboxStrings $ S.toList anchorDeps)
            , ("BUILD_REQS",  generateBuildReqs      $ S.toList sysDeps)
            , ("RUN_REQS",    generateRunReqs        $ S.toList sysDeps)
            , ("DEB_DEPS",    concatMap (<> ",")     $ S.toList sysDeps)
            , ("CHANGELOG_HEADING", changelogHeading)
            ]
    return $ unlines $ m4Header <> defines
  where
    m4Header :: [String]
    m4Header = [ "dnl -*- m4 -*-"
               , "changequote(<<, >>)dnl"
               , "dnl"
               ]
    generateCopyStrings :: [String] -> String
    generateCopyStrings = unlines . map (\x -> "cp -v dist/build/" <> x <> "/" <> x <> " %{buildroot}%{_bindir}")

    generateFileStrings :: [String] -> String
    generateFileStrings = unlines . map (\x -> "%{_bindir}/" <> x)

    generateSandboxStrings :: [String] -> String
    generateSandboxStrings = unlines . map (\x -> "cabal sandbox add-source ../" <> x)

    generateSetupAndSourceStrings :: [String] -> (String, String)
    generateSetupAndSourceStrings xs =
        let f = unlines . map (\(x, ix) -> "Source" <> show ix <> ":\t" <> x <> ".tar.gz")
            g = unlines . map (\(x, ix) -> "%setup -n " <> x <> " -T -D -b " <> show ix)
        in  (f &&& g) (zip xs [1..])

    generateBuildReqs :: [String] -> String
    generateBuildReqs = unlines . map (\x -> "BuildRequires:\t" <> x <> "-devel")

    generateRunReqs :: [String] -> String
    generateRunReqs = unlines . map (\x -> "Requires:\t" <> x)

    generateChangelogHeading :: String -> String -> String -> String -> String
    generateChangelogHeading maint ver buildNumber dateString =
        dateString <> " " <> maint <> " " <> ver <> "-0.0anchor" <> buildNumber

    generateDefineStatement :: (String, String) -> String
    generateDefineStatement (macroName, value) = "define(<<" <> macroName <> ">>, <<" <> value <> ">>)dnl"
