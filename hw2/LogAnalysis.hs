{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- Exercise 1
parseMessage :: String -> LogMessage
-- parseMessage s = case (take 1 s) of
--                     "I" -> LogMessage Info (read(unwords(drop 1 (take 2 (words s)))) :: Int) (unwords (drop 2( words s)))
--                     "W" -> LogMessage Warning (read(unwords(drop 1 (take 2 (words s)))) :: Int) (unwords (drop 2( words s)))
--                     "E" -> LogMessage (Error (read(unwords(drop 1 (take 2 (words s)))) :: Int)) (read (unwords(drop 2 (take 3 (words s)))) :: Int) (unwords (drop 3( words s)))
--                     _ -> Unknown s
parseMessage('I':s) = LogMessage Info (read(unwords(take 1 (words s))) :: Int) (unwords (drop 1( words s)))
parseMessage('W':s) = LogMessage Warning (read(unwords(take 1 (words s))) :: Int) (unwords (drop 1( words s)))
parseMessage('E':s) = LogMessage (Error (read(unwords(take 1 (words s))) :: Int)) (read (unwords(drop 1 (take 2 (words s)))) :: Int) (unwords (drop 2( words s)))
parseMessage s = Unknown s

parse :: String -> [LogMessage]
parse [] = []
parse s = parseMessage(head(lines s)) : parse(unlines(drop 1 (lines s)))

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert logMessage@(LogMessage _ stamp _) (Node leftChild rootMessage@(LogMessage _ parentStamp _) rightChild)
    | stamp < parentStamp = case leftChild of
        Leaf -> Node (Node Leaf logMessage Leaf) rootMessage rightChild
        _ -> Node (insert logMessage leftChild) rootMessage rightChild
    | otherwise  = case rightChild of
        Leaf -> Node leftChild rootMessage (Node Leaf logMessage Leaf)
        _ -> Node leftChild rootMessage (insert logMessage rightChild)

-- Exercise 3
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

--Exercise 4
-- smallest to biggest
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftChild rootMessage rightChild) = inOrder(leftChild) ++ [rootMessage] ++ inOrder(rightChild)

-- Exercise 5
-- takes an unsorted list of LogMessages, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . inOrder . build . filter isSevereError
  where getMessage (LogMessage _ _ message) = message
        isSevereError (LogMessage (Error level) _ _) = level >= 50
        isSevereError _ = False
