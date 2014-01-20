{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-

  CMPS 122 HW1
  Morgan McDermott
  Professor Miller

  Credit & Sources:
  - English word count data file: 
      http://norvig.com/google-books-common-words.txt
  - Information on english language statistics:
      http://norvig.com/mayzner.html
  - Papers about using MCMC to break ciphers
    + Decrypting Classical Cipher Text Using Markov Chain Monte Carlo, Jian Chen
      www.utstat.toronto.edu/wordpress/WSFiles/technicalreports/1005.pdf‎
    + Simulation and Solving Substitution Codes, Stephen Connor
      www.cse.iitk.ac.in/users/jayant/mcmc/ref/conner-thesis.pdf‎
-}

import System.Environment
import Data.List
import Data.Char (isAlpha, ord, chr)
import Data.Monoid ( (<>) )
import Data.Maybe (fromJust, fromMaybe)
import Data.Ord (Ordering (..))
import Debug.Trace
import Data.IORef
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.HashMap.Strict as HM
import qualified System.Random.Mersenne as MRS

{- Letter & Word & 2-gram Frequencies from norvig -} 
letterFreqFile = "letter-frequencies.txt"
wordFreqFile = "google-books-common-words.txt"
wordFreqTotal = 743842922321
ngrams2File = "ngrams2.txt"
ngrams2Total = 2819662855499
warAndPeaceOrig = "war-and-peace.txt"
warAndPeaceSpaces = "war-and-peace-spaces.txt"
warAndPeaceFile = "war-and-peace-processed.txt"

intFolder hm l = HM.insert (l !! 0) ((read . T.unpack $ (l !! 1))::Int) hm
doubleFolder hm l = HM.insert (l !! 0) ((read . T.unpack $ (l !! 1))::Double) hm

tchar = T.singleton
alphabet = map chr $ [65..90]
sndSort (a, f1) (b, f2) = compare f1 f2

fileFold :: FilePath
   -> ((HM.HashMap T.Text a) -> [T.Text] -> HM.HashMap T.Text a) 
   -> IO (HM.HashMap T.Text a)
fileFold file folder = do
  z <- TIO.readFile file
  return $ foldl' folder HM.empty $ map T.words $ T.lines $ z

adjustDefault f k def hm = HM.adjust f k (HM.insertWith (\new old -> old) k def hm)

letterFreqs :: T.Text -> HM.HashMap T.Text Int
letterFreqs txt = T.foldl' (\hm c -> adjustDefault (\x -> x + 1) (tchar c) 0 hm) HM.empty txt

ngram2Freqs :: T.Text -> HM.HashMap T.Text Int
ngram2Freqs txt = fst $ T.foldl' folder (HM.empty, ' ') txt
 where 
   folder (hm, c0) c1 = (adjustDefault (\x -> x + 1) (T.pack [c0, c1]) 0 hm, c1)

type Code = HM.HashMap T.Text T.Text

decodeMA :: T.Text -> Code -> T.Text
decodeMA cipherText code = T.foldl' (\str char -> str <> 
   (fromMaybe " " $ (HM.lookup (tchar char) code))) "" cipherText

scoreLetterFreq :: T.Text -> HM.HashMap T.Text Double -> Double
scoreLetterFreq plaintext letterFreqs = 
 T.foldl' (\score c -> score + scoreOf (HM.lookup (tchar c) letterFreqs)) 0 plaintext
 where 
   scoreOf Nothing = 0
   scoreOf (Just f) = log f

-- | This will be used as our pi() for MCMC
--   We'll use the log probability of the current configurations of
--   2-grams in the text as the (unscaled) probability of that configuration.
scoreNgramFreq' :: HM.HashMap T.Text Double -> T.Text -> Double
scoreNgramFreq' refFreqs plaintext = 
    HM.foldlWithKey' (\n k v -> n + ((+) 1 $ scoreOf $ (HM.lookup k refFreqs)) * (1 + v)) 1 textFreqs
 where 
   textFreqs = HM.map fromIntegral $ ngram2Freqs plaintext
   scoreOf Nothing = 0
   scoreOf (Just f) = log f 

-- | This will be our initial state for monoalphabetic MCMC
--   Seems like a good idea to start with the most probable state for 1-grams
--   But for now just using the identity mapping (A -> A etc)
initState englishLetterFreqs ngram2Freqs cipherText = 
  HM.fromList $ zipWith (,) (map fst $ HM.toList englishLetterFreqs) (map fst $ HM.toList englishLetterFreqs) 
  where 
    engFreqList = map fst $ sortBy sndSort $ HM.toList englishLetterFreqs
    cipherFreqList =  map fst $ sortBy sndSort $ HM.toList $ letterFreqs cipherText

-- | This samples from our symmetric probability density q(x, y) where
--   x is the current state and y is a proposed state
--   In this case we swap n mappings at random (now only using n = 1)
--   Which is symmetric, and similar states are more likely.
sampleQ_MA :: Int -> MRS.MTGen -> Code -> IO Code
sampleQ_MA nswaps rg code = do
    go nswaps (return code)
    where
      l = HM.size code
      keys = map fst $ HM.toList code
      go n hm = case n > 0 of
                  False -> hm
                  True -> do
                    r <- (MRS.random rg :: IO Double)
                    k1 <- fmap (\x->keys !! (floor(x * fromIntegral l ))) 
                          $ (MRS.random rg :: IO Double)
                    k2 <- fmap (\x->keys !! (floor(x * fromIntegral l ))) 
                          $ (MRS.random rg :: IO Double)
                    h <- hm
                    let v1 = fromJust $ HM.lookup k1 h
                        v2 = fromJust $ HM.lookup k2 h
                    go (n - 1) $ return $ HM.adjust (const v1) k2 $
                           HM.adjust (const v2) k1 h

-- | Sampling from q(x, y) for vignere ciphers
--   Our state space is all possible keys {A..Z}^n
--   We sample by changing one character at random in the key
sampleQ_VG :: Int -> MRS.MTGen -> String -> IO String
sampleQ_VG nswaps rg key = do
    go nswaps (return key)
    where
      l = length key
      go n str = case n > 0 of
                  False -> str
                  True -> do
                    r <- (MRS.random rg :: IO Double)
                    idx <- fmap (\x-> floor $ x * fromIntegral l) $ (MRS.random rg::IO Double)
                    val <- fmap (\x->alphabet !! (floor $ x * 25.0) )
                            $ (MRS.random rg:: IO Double)
                    k <- str
                    go (n - 1) $ return $ take (idx) k ++ val : drop (idx + 1) k

mcmc :: (Show a) => MRS.MTGen 
   -> (MRS.MTGen -> a -> IO a) -- ^ Sample from q(x, y)
   -> (a -> Double) -- ^ log pi(-) [ Scoring function ]
   -> (a, Double) -- ^ (State_i, log pi_i)
   -> (a, Double) -- ^ (State_best, log pi_best)
   -> Double -- ^ MCMC scaling factor `p`
   -> Int    -- ^ Number of iterations
   -> IO a
mcmc rg !sampleQ !pi !(state_i, pi_i) !(state_best, pi_b) !p !iterations = do
  state_j <- sampleQ rg state_i
  let pi_j = pi state_j
  u <- MRS.random rg :: IO Double
  -- alpha = pi_j / pi_i  ==> alpha = exp ( log pi_j - log pi_i )
  let alpha = exp ( min 0 (pi_j - pi_i) )
  z <- case iterations `mod` 100 == 0 of 
         True -> do 
           putStrLn $ show state_i
           putStrLn $ "MCMC Iteration "++ show iterations ++"; pi_i : " ++ show pi_i
           putStrLn $ "\t pi_j: " ++ show pi_j ++ " alpha: " ++ show alpha
           putStrLn $ "\t u: " ++ show u
         False -> return ()
  let state_best' = case pi_j > pi_b of
                     True -> (state_j, pi_j)
                     False -> (state_best, pi_b)
  case iterations < 1 of
    True -> return $ fst state_best'
    False -> case u < alpha ** p of
               True -> mcmc rg sampleQ pi (state_j, pi_j) state_best'
                        p (iterations - 1) 
               False -> mcmc rg sampleQ pi (state_i, pi_i) state_best'
                        p (iterations - 1) 

monoAlphaMCMC :: MRS.MTGen -> FilePath -> HM.HashMap T.Text Double 
                 -> HM.HashMap T.Text Double -> Int -> IO ()
monoAlphaMCMC rg cipherFile englishLetterFreqs wapngrams2 chainIters = do

  putStrLn "Attempting to break ciphertext as monoalphabetic substitution cipher"
  cipherText <-fmap ( T.replace " " "" . T.replace "\n" "") $ TIO.readFile cipherFile
  let state0 = initState englishLetterFreqs wapngrams2 cipherText
  putStrLn $ "CIPHERTEXT"
  putStrLn $ T.unpack cipherText
  putStrLn $ "Frequencies"
  putStrLn $ show $ reverse $ sortBy sndSort $ HM.toList $ letterFreqs cipherText
  putStrLn $ show $ ngram2Freqs cipherText
  putStrLn $ "Decoding.."
  let state0 = initState englishLetterFreqs wapngrams2 cipherText
  code <- mcmc rg (sampleQ_MA 1) 
             (\x -> scoreNgramFreq' wapngrams2 (decodeMA cipherText x)) 
             (state0, 0) (state0, 0) 0.8 chainIters

  let decoded = decodeMA cipherText code
  putStrLn $ "Decoded"
  putStrLn $ T.unpack decoded
  putStrLn $ "Letter score: " ++ (show $ scoreLetterFreq decoded englishLetterFreqs)
  putStrLn $ "Ngram score: " ++ (show $ scoreNgramFreq' wapngrams2 decoded)
  putStrLn $ "Ngram freqs: " ++ (show $ ngram2Freqs decoded)
  TIO.writeFile (cipherFile ++ ".decrypt") $ "Decoded\n" <> decoded 
         <> "\n" <> (T.pack $ show code)
  putStrLn $ "Results written to " ++ cipherFile ++ ".decrypt"


indexOfCoincidence :: T.Text -> Double 
indexOfCoincidence t = freqSqSum / 
  (( fromIntegral $ T.length t * ( T.length t - 1 )) / 26)
  where 
     freqSqSum = HM.foldl' (+) 0
          $ HM.map (\v -> fromIntegral (v * (v - 1))) $ letterFreqs t

-- | Index of coincidence of text when wrapped into columns of length `shift`
indexOfCoincidenceKey :: T.Text -> Int -> Double
indexOfCoincidenceKey cipherText shift = 
   avg $ map indexOfCoincidence $ T.transpose $ (\(x, y, z) -> x) $ T.foldl' folder ([], ("" :: T.Text), 0) cipherText
  where
    avg xs = (foldl (+) 0 xs) / fromIntegral (length xs)
    folder (res, row, i) char = case i `mod` shift == 0 of
      True -> (res ++ [row], T.singleton char, i + 1)
      False -> (res, row <> (T.singleton char), i + 1)

-- ord 'A' = 65, ord 'Z' = 90
opAlphas :: (Int -> Int -> Int) -> Char -> Char -> Char
opAlphas op c1 c2 = chr $ (+) 65 $ ((ord c1 - 65) `op` (ord c2 - 65)) `mod` 26

-- | Encode & decode for Vegnere ciphers
codeVG :: (Int -> Int -> Int) -> T.Text -> String -> T.Text
codeVG op cipherText key = 
  snd $ T.foldl' (\(i, t) c -> (i + 1, t <> T.singleton (opAlphas op c (key !! (i `mod` lt))))) (0, "") cipherText
  where lt = length key

vignereMCMC rg cipherFile englishLetterFreqs wapngrams2 chainLength = do
  putStrLn "Attempting to break ciphertext as Vignere cipher"
  cipherText <- fmap (T.toUpper . T.filter isAlpha) $ TIO.readFile cipherFile
  putStrLn $ "\n\nCiphertext"
  putStrLn $ T.unpack cipherText
  putStrLn $ "\n\nLetter frequencies"
  putStrLn $ show $ letterFreqs cipherText
  putStrLn $ "\n\nIOC cipherText: " ++ show (indexOfCoincidence cipherText)
  let iocKey i = putStrLn $ "IC "++show i++": " ++ (show $ indexOfCoincidenceKey cipherText i)
--  mapM_  iocKey [1..40]
  let z = last $ sortBy sndSort $ map (\x -> (x, indexOfCoincidenceKey cipherText x)) [1..20]
  putStrLn $ "Highest IOC in first 20: " ++ show z
  putStrLn $ "Code Test"
  let plaintext = "ATTACKATDAWN"
  let key = "AXFGVFBEKUUH"
--  let cipherText = codeVG (+) (T.pack plaintext) key
  let decrypted = codeVG (-) cipherText key
  putStrLn $ "Plaintext:\t\t " ++ plaintext
  putStrLn $ "Key \t\t: "++key
  putStrLn $ "cipherText \t\t: "++ T.unpack cipherText
  putStrLn $ "decrypted \t\t: "++ T.unpack decrypted
  putStrLn $ "\n\nRunning MCMC"
  let state0 = take (fst z) $ repeat 'A'
  
  code <- mcmc rg (sampleQ_VG 1) 
             (\x ->   
     let dec = (codeVG (-) cipherText x) in
     --(-0.5) * scoreLetterFreq dec englishLetterFreqs +
     scoreNgramFreq' wapngrams2 dec)
          (state0, 0) (state0, 0) 0.8 chainLength
  let decoded = codeVG (-) cipherText code
  putStrLn $ "\n\nDECODED"
  putStrLn $ T.unpack decoded
  putStrLn $ "Score" ++ show (scoreNgramFreq' wapngrams2 decoded)
  putStrLn $ "\nKEY: " ++ show code
  return ()

usage = putStrLn "mcmc-cipher cipherFile { -ma | -vg } chainIterations"

main = do
 args <- getArgs
 case length args >= 2 of
   False -> usage
   True -> do
         englishLetterFreqs <- fileFold letterFreqFile doubleFolder  
         wordFreqs <- fmap (HM.map (\x -> sqrt $ (x / wordFreqTotal))) 
                      $ fileFold wordFreqFile doubleFolder 
         ngrams2 <-   fmap (HM.map (\x -> (x / ngrams2Total))) 
                      $ fileFold ngrams2File doubleFolder 
         wap <- TIO.readFile warAndPeaceFile
         putStrLn "Extracting digram frequencies from War and Peace..."
         let wapngrams2 = HM.map fromIntegral $ ngram2Freqs $ wap
         let chainIter = case length args >= 3 of
                           True -> read $ args !! 2
                           False -> 10000
         rg <- MRS.newMTGen Nothing
         case args !! 1 of
           "-ma" -> monoAlphaMCMC rg (args !! 0) englishLetterFreqs 
                     wapngrams2 chainIter
           "-vg" -> vignereMCMC rg (args !! 0) englishLetterFreqs
                     wapngrams2 chainIter
           _ -> usage
