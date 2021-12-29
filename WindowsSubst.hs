module WindowsSubst (Disk, Command, STDIN, STDOUT, STDERR, disksGet, diskDel, diskCreate, disksClear) where

import System.Process (readCreateProcessWithExitCode, shell)
import System.Exit (ExitCode(..))

type Disk     = Char
type Command  = String
type Argument = String
type STDIN    = String
type STDOUT   = String
type STDERR   = String

tp1 (x, _, _) = x
tp2 (_, x, _) = x
tp3 (_, _, x) = x

cmdBool = return . (== ExitSuccess) . tp1

exprDelCreate' d a = shellRunArgs "subst" [d:":", a] "" >>= cmdBool

shellRun :: Command -> STDIN -> IO (ExitCode, STDOUT, STDERR)
shellRun cmd = readCreateProcessWithExitCode (shell cmd)

shellRunArgs :: Command -> [Argument] -> STDIN -> IO (ExitCode, STDOUT, STDERR)
shellRunArgs cmd args =
    let cmd' = foldl ((++) . (++ " ")) cmd args
    in shellRun cmd'

disksGet :: IO [Disk]
disksGet = shellRun "subst" "" >>= return . tp2 >>= \text -> return $ [head line | line <- lines text]

diskDel :: Disk -> IO Bool
diskDel = \d -> exprDelCreate' d "/D"

diskCreate :: Disk -> FilePath -> IO Bool
diskCreate = exprDelCreate'

disksClear :: IO ()
disksClear = disksGet >>= mapM_ diskDel
