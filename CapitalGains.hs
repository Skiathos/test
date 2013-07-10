module CapitalGains where

import System.IO
import Data.List
import Data.List.Split

data BuyOrSell = Buy | Sell deriving (Eq)

data Match = Match { mshares :: Int 
                   , dateSold :: String 
                   , dateBought :: String 
                   , amountReceived :: Double 
                   , costBasis :: Double }

data Trade = Trade { date :: String
                   , shares :: Int
                   , payment :: Double
                   , kind :: BuyOrSell }
         
breakTrade :: Trade -> Int -> (Trade, [Trade])
breakTrade t n
  | n < shares t = let matchedPayment = (payment t) * (fromIntegral n)/(fromIntegral (shares t))
                       matched = t {shares = n, payment = matchedPayment}
                       rest = t {shares = shares t - shares matched, payment = payment t - payment matched}
                   in  (matched, [rest])
  | otherwise    = (t, [])

pairTrades :: Trade -> Trade -> (Match, [Trade])
pairTrades a b
  | shares b >= shares a =
      let (matched, remaining) = breakTrade b (shares a)
          m = if kind b == Sell 
                then Match (shares matched) (date b) (date a) (payment matched) (payment a)
                else Match (shares matched) (date a) (date b) (payment a) (payment matched)
      in  (m, remaining)
  | otherwise = pairTrades b a

processBuysSells :: [Trade] -> [Trade] -> [Match] -> ([Match], [Trade])
processBuysSells buys sells matches =
  case (buys, sells) of
    (_, [])      -> (matches, buys)
    (b:bs, s:ss) -> case pairTrades b s of
                      (m, [])  ->  processBuysSells  bs ss (m:matches)
                      (m, [t]) ->  if kind t == Sell 
                                     then processBuysSells  bs (t:ss) (m:matches)
                                     else processBuysSells  (t:bs) ss (m:matches) 

processTrades :: [Trade] -> ([Match], [Trade])
processTrades trades = processBuysSells buys sells []
  where
    (buys, sells) = partition (\t -> (kind t == Buy)) trades

---------------------------------------------------------
--       CSV file processing
---------------------------------------------------------

csvLineToTrade :: String -> Trade
csvLineToTrade info = Trade date shares payment kind 
  where
    d = splitOn "," info
    date = d!!0
    signedShares = read (d!!1) :: Int
    shares = abs(signedShares)
    payment = read (d!!2) :: Double
    kind = if signedShares > 0 then Buy else Sell

csvTextToTrades :: String -> [Trade]
csvTextToTrades info = map csvLineToTrade (tail (lines info))

matchToCSVline :: Match -> String
matchToCSVline m = ms ++ "," ++ ds ++ "," ++ db ++ "," ++ ar ++ "," ++ cb ++ "\n"
  where
    ms = show (mshares m)
    ds = dateSold m
    db = dateBought m
    ar = show (amountReceived m)
    cb = show (- (costBasis m))

matchesToCSV :: [Match] -> String
matchesToCSV ms = concat (map matchToCSVline ms)

-----------------------------------------------------------

main = do
  let fileIn = "csv\\TIP.csv"
  let fileOut = "csv\\TIPresult.csv"
  contents <- readFile fileIn
  let (matched, _) = processTrades (csvTextToTrades contents)
  writeFile fileOut (matchesToCSV matched)
