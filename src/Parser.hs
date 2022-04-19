module Parser where
    
type Parser a = String -> Maybe (a ,String)
    
parseCharExcept:: String -> Parser Char
parseCharExcept e (h:r)
                | h `elem` e = Nothing
                | otherwise = Just (h , r)
parseCharExcept _ _ = Nothing

parseAllCharExcept:: String -> Parser String
parseAllCharExcept e  = parseMany (parseCharExcept e)  


applyTwoTime :: Parser a -> String -> String -> Parser (a , a )
applyTwoTime p s s' rest = case p s of
    Just (r , _) -> case p s' of
        Just (r', _) -> Just ((r, r'), rest)
        Nothing -> Nothing
    Nothing -> Nothing

parseTuple :: Parser a -> Parser (a , a )
parseTuple p s = case fisrtPart s of
        Just ((_, a1), s') ->  case sndPart s' of
            Just ((_, a2), s'') ->  applyTwoTime p a1 a2 (tail s'')
            Nothing -> Nothing
        Nothing -> Nothing    
        where fisrtPart = parseAnd (parseChar '(') (parseAllCharExcept ",") 
              sndPart = parseAnd (parseChar ',') (parseAllCharExcept ")")


parseInt :: Parser Int
parseInt "" =  Nothing 
parseInt s = case reads s ::[(Int, String)] of 
            [(x, s)]-> Just(x, s)
            _ -> Nothing

parseDouble :: Parser Double
parseDouble "" =  Nothing 
parseDouble s = case reads s ::[(Double, String)] of 
            [(x, s)]-> Just(x, s)
            _ -> Nothing

parseUInt :: Parser Int
parseUInt s = case parseInt s of
    Just (i, s) -> Just (abs i , s)
    Nothing -> Nothing

parseChar :: Char -> Parser Char
parseChar a  str | a == head str = Just (a, tail str)
                 |  otherwise = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar target str
    | i `elem` target =  Just (i, tail str)
    |  otherwise = Nothing
        where i = head str

purre :: a -> Parser a
purre x s =  Just (x, s)

parsOr :: Parser a -> Parser a -> Parser a
parsOr p1 p2 s = case p1 s of
    Just (r, s') -> Just (r, s')
    Nothing -> case p2 s of
        Just (r, s') -> Just (r, s')
        Nothing -> Nothing

parseAnd::Parser a -> Parser b -> Parser(a, b)
parseAnd  p1 p2 s = case p1 s of
    Just( r1 , "") -> Nothing
    Just( r1 , s') -> case p2 s' of
        Just (r2 , s'') -> Just ((r1, r2), s'')
        Nothing -> Nothing
    Nothing -> Nothing

parseAndWith::(a ->b ->c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 s = case parseAnd p1 p2 s of
    Just ((r1, r2), s') -> Just (f r1 r2, s')
    Nothing -> Nothing
    
parseMany:: Parser a -> Parser [a]
parseMany p s = case p s of 
    Just (r, s') -> case  parseMany p s' of
        Just (r' , s'') -> Just (r:r', s'')
        Nothing -> Just([r], s')
    Nothing -> purre [] s

parseSome:: Parser a -> Parser [a]
parseSome p s = case parseMany p s of 
    Just ([], s') -> Nothing
    Just (r, s') -> Just (r, s')
    Nothing -> Nothing


eraseAll:: String -> Parser String
eraseAll c = eraseAll' c []

eraseAll':: String -> String -> Parser String
eraseAll' c res [] = Just (reverse res, "")
eraseAll' c res (a:r)
        | a `elem` c = eraseAll' c res r
        | otherwise = eraseAll' c (a:res) r 

--(<.>)::Parser a -> Parser b -> Parser(a, b)    
--(<.>)  p1 p2 s = case p1 s of
--    Just( r1 , "") -> Nothing    
--    Just( r1 , s') -> case p2 s' of
--        Just (r2 , s'') -> Just ((r1, r2), s'')    
--        Nothing -> Nothing
--    Nothing -> Nothing

--extract:: Parser a -> a
--extract s   = case p1 s of
--    Just (r , s') -> r    
--    Nothing -> 
--

--(<$>) :: (a -> b) -> Parser a -> Parser b
--(<$>) f  p1 s =  case p1 s of
--    Just (r1 , s') -> Just (f r1, s')    
--    Nothing ->  Nothing
--
--(<*>) :: Parser(a -> b) -> Parser a -> Parser b
--(<*>) p1  p2 s = case p1 s of
--    Just (f, s') -> case p2 s' of
--        Just (a, s'') -> Just (f a, s'')    
--        Nothing       -> Nothing
--    Nothing -> Nothing
