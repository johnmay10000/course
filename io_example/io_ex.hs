import System.Environment
import Control.Applicative
import Control.Monad

{-

Functions --
	getArgs :: IO [String]
	putStrLn :: IO String
	readFile :: filePath -> IO String
	lines :: String -> [String]
    void :: IO a -> IO ()  

	abstractions --
			Applicative, Monad
		(<$>) :: (a -> b) -> IO a -> IO b
		(<*>) :: IO (a -> b) -> IO a -> IO b
		(>>=) :: IO a -> (a -> IO b) -> IO b
		(=<<) :: (a -> IO b) -> IO a -> IO b
		pure :: a -> IO a
-}
main::IO()
main = getArgs >>= \args -> case args of
							[x]-> run x
							Nil-> putStrLn "Usage : "
-- Hint getArgs and run
{- 

Hint : use getFiles and printFiles

run :: String -> IO () 
run = undefined

getFiles :: [Strings] -> IO [(String, String)]
getFiles = undefined

getFile :: String -> IO (String,String)
getFile = undefined

printFiles :: [(String, String)] -> IO ()
-}



	
	 
