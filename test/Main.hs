main :: IO ()
main = putStrLn "111"
-- store a >> retrieve = store a >> store a >> retrieve    <- hard to check
-- store a >>= retrieve = a
--
-- store a >> collectGarbage >> retrieve = error
-- store a >> markRoot a >> collectGarbage >> retrieve = a
--
-- runCasperT act1 >> runCasperT act2
-- runCasperT (act1 >> act2)
--
-- Something with references
