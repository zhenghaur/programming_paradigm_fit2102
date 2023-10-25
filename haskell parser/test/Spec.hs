import           System.FilePath.Glob           ( glob )
import           Test.DocTest                   ( doctest )
import           System.FSNotify
import           Watch                          ( watch )
import           System.Console.ANSI
import           Control.Exception
import           Control.Concurrent

resetScreen :: IO ()
resetScreen = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0

colorMsg :: Color -> String -> IO ()
colorMsg c s = do
    setSGR [SetColor Foreground Vivid c]
    putStrLn $ s ++ "\ESC[0m" -- resets the colour, because the following doesn't seem to always work.
    setSGR [Reset]

runDoctest :: [String] -> IO ()
runDoctest files = do
    resetScreen
    colorMsg Cyan $ "testing " ++ show files ++ "..."
    finally
        (       (do
                    doctest $ "-XStandaloneDeriving" : files
                    colorMsg Green $ "Tests Passed for " ++ show files
                )
        `catch` (\(SomeException _) -> colorMsg Red "Error from doctest!")
        )
        (colorMsg Cyan "waiting for file change...")

forkDoctest :: Event -> IO ()
forkDoctest (Modified f _ _) = do
    -- file <- glob "src/Pointfree.hs"
    _ <- forkIO $ runDoctest [f]
    return ()
forkDoctest _ = error "Need modified value"

main :: IO ()
main = do
    files <- glob "submission/**/*.hs"
    runDoctest files
    watch forkDoctest "submission"
