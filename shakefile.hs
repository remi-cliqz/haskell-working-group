import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
  want $ map (\x -> x </> "slides.html")["session1", "session2"]

  phony "clean" $ do
    putNormal "cleaning output"
    removeFilesAfter "." ["*/*.html"]
 
  "*/slides.html" %> \out -> do
    let src = takeDirectory1 out
    cmd (Cwd src) "pandoc" "--self-contained" "-V slidy-url=../slidy" "-s" "-t" "slidy" "-o" "slides.html" "slides.md"

