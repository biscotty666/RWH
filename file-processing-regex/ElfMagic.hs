import qualified Data.ByteString.Lazy as L

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
    where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

isElfFile :: FilePath -> IO Bool
isElfFile path = do
    content <- L.readFile path
    return (hasElfMagic content)

-- ghci> isElfFile "/home/biscotty/.nix-profile/bin/firefox"
-- False
-- ghci> isElfFile "/nix/store/psjqqbj19n3fqssn38hgz4cv7b7a9alp-findutils-4.10.0/bin/find"
-- True
