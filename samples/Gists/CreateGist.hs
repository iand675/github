module CreateGist where

import qualified Github.Gists as Github

main = do
  result <- Github.createGist Github.PublicGist (Just "creating a gist") [
     Github.NewGistFile "create_a_gist.sh"
                     "curl -d@create_a_gist https://api.github.com/gists"
    ,Github.NewGistFile "create_a_gist.hs" "createGist PublicGist ..."
    ]
  case result of
    Left error       -> putStrLn $ "Error: " ++ (show error)
    Right gistDetail -> putStrLn $ Github.gistDetailHtmlUrl gistDetail
