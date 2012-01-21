-- | The gists API as described at <http://developer.github.com/v3/gists/>.
module Github.Gists (
 gists
,gist
,createGist
,module Github.Data
) where

import Github.Data
import Github.Private

-- | The list of all public gists created by the user.
--
-- > gists "mike-burns"
gists :: String -> IO (Either Error [Gist])
gists userName = githubGet ["users", userName, "gists"]

-- | A specific gist, given its id.
--
-- > gist "225074"
gist :: String -> IO (Either Error Gist)
gist gistId = githubGet ["gists", gistId]

-- | Create a gist from file data.
--
-- > createGist PublicGist (Just "creating a gist") [
-- >   GistFile "create_a_gist.sh"
-- >            "curl -d@create_a_gist https://api.github.com/gists"
-- >  ,GistFile "create_a_gist.hs" "createGist PublicGist ..."
-- >  ]
createGist :: GistPublicity -> Maybe String -> [NewGistFile] -> IO (Either Error GistDetail)
createGist publicity possibleDescription files =
  githubPost ["gists"] $ NewGist publicity possibleDescription files
