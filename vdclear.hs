import WindowsSubst (Disk, disksGet, disksClear)

putDisks :: [Disk] -> String
putDisks []     = "{}"
putDisks [d]    = ['{', d, '}'];
putDisks (d:ds) =
    let middle = foldl (\s a -> s ++ (", " ++ [a])) ['{', d] ds
    in middle ++ "}"

main = do
    disks <- disksGet
    putStrLn ("clear virtual disk: " ++ putDisks disks)
    disksClear
