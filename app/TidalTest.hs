import Sound.Tidal.Context

-- compile: stack build
-- run: stack exec parseini-exe

setupTidalStream :: IO (OscPattern -> IO ())
setupTidalStream = do
  putStrLn "Starting Tidal Dirt Software Synth"
  d1 <- dirtStream
  return d1

playSimplePattern :: (OscPattern -> t) -> String -> t
playSimplePattern s x = s $ sound (p x)

playAndWait s x = do
  playSimplePattern s x
  getLine

main :: IO ()
main = do
  d1 <- setupTidalStream
  let ps = ["bd","bd cp","[bd bd] cp","[bd*4] [cp ~ arpy*2 bd]"]
  putStrLn "Press enter to advance pattern until finished"
  getLine
  mapM (playAndWait d1) ps
  putStrLn "Finished"
