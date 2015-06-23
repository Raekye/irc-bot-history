import Data.List
import Network
import System.IO
import Control.Monad.Reader
import System.Exit
import Control.Exception
import Text.Printf
import Text.Regex.Posix

server = "irc.esper.net"
port = 6667
chan = "#gamocosm"
nick = "hmmmbot"

putStrLnStderr :: String -> IO ()
putStrLnStderr str = hPutStrLn stderr str

main :: IO ()
main = bracket connect hClose run

connect :: IO Handle
connect = do
	h <- connectTo server (PortNumber (fromIntegral port))
	hSetBuffering h NoBuffering
	return h

sock_write :: Handle -> String -> String -> IO ()
sock_write h s t = do
	hPrintf h "%s %s\r\n" s t
	putStrLnStderr $ printf "< %s %s" s t

sock_read :: Handle -> IO String
sock_read h = hGetLine h

run :: Handle -> IO ()
run h = do
	sock_write h "NICK" nick
	sock_write h "USER" $ printf "%s 0 * :%s" nick nick
	forever $ do
		line <- sock_read h
		deal_with_it h line join_room

ping :: String -> Bool
ping line | "PING :" `isPrefixOf` line = True
ping _ = False

pong :: Handle -> String -> IO ()
pong h line = do
	putStrLnStderr line
	sock_write h "PONG" (':' : drop 6 line)

deal_with_it :: Handle -> String -> (Handle -> String -> IO ()) -> IO ()
deal_with_it h line _ | ping line = pong h line
deal_with_it h line fn = fn h line

join_room :: Handle -> String -> IO ()
join_room h line | (printf ":%s MODE %s" nick nick) `isPrefixOf` line = do
	sock_write h "JOIN" chan
	forever $ do
		line <- sock_read h
		deal_with_it h line process
join_room _ line = putStrLnStderr line

process :: Handle -> String -> IO ()
process h line =
	let
		process' _ ((_:user:msg:_):_) = do
			putStrLn $ printf "> %s: %s" user (init msg)
			hFlush stdout
		process' line _ = putStrLnStderr line
	in process' line (line =~ ((printf ":(.*)!.* PRIVMSG %s :(.*)" chan) :: String) :: [[String]])
