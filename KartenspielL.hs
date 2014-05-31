import Control.Monad.Trans.State
import Control.Monad.IO.Class

places :: Int
places = 4

-- State:
type Cav = Int
type Inf = Int
type CNr = Int
type ANr = Int
type Corps = (Inf,Cav) 
type Reserves = Corps
-- own, enemy
type Place = (Corps,Corps)
type LastKnown = Place
type Battlefield = [(Place,LastKnown)]
-- Bool: isAttacking, isCharging
type Stance = (Bool,Bool)
type Field = StateT Battlefield IO
data BrokenStatus = Both | Enemy | Own | None
-- init = CombatPlaces, last = Reserves

--main = execStateT testT testBF

-- subfunctions:
    
directReserves :: Bool -> Inf -> Cav -> CNr -> Field String    
directReserves isPlayer i c nc = state $ \bf -> 
    case directReserves' isPlayer i c nc bf of
        Right ((remI,remC),nS) -> ("Remaining reserves: " ++ (show remI) ++ " | " ++ (show remC), nS)
        Left err               -> (err, bf)

        

directReserves' :: Bool -> Inf -> Cav -> CNr -> Battlefield -> Either String (Reserves,Battlefield)
directReserves' isPlayer i c nc bf = if isValid
                            then Left "Incorrect Input"
                            else if isPlayer 
                                then Right ((fst $ fst newReserves), newBF)
                                else Right ((snd $ fst newReserves), newBF)
    where
        isValid = if isPlayer 
            then (i > oldRIP || c > oldRCP || 0 > oldRIP || 0 > oldRCP || nc > (places-2) || 0 > nc) 
            else (i > oldRIA || c > oldRCA || 0 > oldRIA || 0 > oldRCA || nc > (places-2) || 0 > nc) 
        ((oldRP@(oldRIP,oldRCP),oldRA@(oldRIA,oldRCA)),estimR) = last bf
        ((oldCP@(oldCIP,oldCCP),oldCA@(oldCIA,oldCCA)),estimC) = bf !! nc
        newCorps = if isPlayer
            then (((oldCIP+i,oldCCP+c),oldCA),estimC)
            else ((oldCP,(oldCIA+i,oldCCA+c)),estimC)
        newReserves = if isPlayer
            then (((oldRIP-i,oldRCP-c),oldRA),estimR)
            else ((oldRP,(oldRIA-i,oldRCA-c)),estimR)
        beforeC = take nc bf
        betweenCR = drop (nc+1) $ init bf
        newBF = beforeC ++ [newCorps] ++ betweenCR ++ [newReserves]
        
       
getCurrentDeployment :: Field String
getCurrentDeployment = state $ \bf -> (showCurrentState bf, bf)       

showCurrentState :: Battlefield -> String
showCurrentState bf = 
    concatMap printPlace (zip [1..] bf)
    where
        printPlace :: (Int,(Place,LastKnown)) -> String
        printPlace (n,(((i,c),_),(_,(eI,eC)))) =
            "Sector " ++ (show n) ++ ": " ++ (show i) ++ " ID and " ++ (show c) ++ " CvD against " ++ (show eI) ++ "(?) ID and " ++ (show eC) ++ "(?) CvD" ++ "\n"

checkBroken :: Battlefield -> BrokenStatus
checkBroken = checkBroken' None 
    where 
        checkBroken' :: BrokenStatus -> Battlefield -> BrokenStatus
        checkBroken' s [] = s
        checkBroken' s ((x,_):xs) = case s of
            Both  -> Both
            Enemy -> case (checkBrokenPlace x) of
                Both  -> Both
                Own   -> Both
                _     -> checkBroken' Enemy xs
            Own   -> case (checkBrokenPlace x) of
                Both  -> Both
                Enemy -> Both
                _     -> checkBroken' Own xs
            None  -> checkBroken' (checkBrokenPlace x) xs    
           
checkBrokenPlace :: Place -> BrokenStatus
checkBrokenPlace (cP,cE) = if isBrokenP 
        then if isBrokenE
            then Both
            else Own
        else if isBrokenE
            then Enemy
            else None
    where
    isBrokenE = checkBrokenCorps cE
    isBrokenP = checkBrokenCorps cP 
    
checkBrokenCorps :: Corps -> Bool
checkBrokenCorps (i,c) = (i <= 0) && (c <= 0)               
                
--main :: StateT Battlefield IO ()
--main = put testBF
  {-
testT :: Field String
testT = do
    put testBF
    testT'
  
testT' :: Field String
testT' = do
    reinforcePlayer
    reinforceEnemy
    resolvePlaces
    bf <- get
    case (checkBroken bf) of
        Both -> do
            liftIO $ print "Both lines have been broken!"
            return ""
        Enemy -> do
            liftIO $ print "Enemy line has been broken!"
            return ""
        Own   -> do
            liftIO $ print "Our line has been broken!"
            return ""
        None  -> do
            liftIO $ print "Next Turn"
            testT'  

    
reinforcePlayer ::  Field String
reinforcePlayer = do
    liftIO $ print "select corps to reinforce"
    nc <- liftIO $ getLine
    liftIO $ print "select number of Infantry divisions"
    i <- liftIO $ getLine
    liftIO $ print "select number of Cavalry divisions"
    c <- liftIO $ getLine
    state <- directReserves True (read i) (read c) (read nc)
    liftIO $ print state
    liftIO $ print "done?"
    done <- liftIO $ getLine
    case done of 
        "y" -> do
                bf <- get
                liftIO $ print $ "Current Deployment:" ++ (show $ fst bf)
                return ""
        _   -> reinforcePlayer
    
-- currently dumb    
reinforceEnemy :: Field String
reinforceEnemy = do
    liftIO $ print "Enemy reinforcing"
    return ""
    
resolvePlaces :: Field String
resolvePlaces = do
    resolvePlace 1 
    resolvePlace 2 
    resolvePlace 3 
    resolvePlace 4 

    
resolvePlace :: Int -> Field String
resolvePlace n = do
    liftIO $ print $ "Attack with Corps " ++ (show n) ++ "?"
    plA <- liftIO $ getLine
    bf <- get
    case (chooseA n bf) of
        enA -> do 
            liftIO $ print $ announceA n (read plA) enA
            if ((read plA) || enA) 
                then resolvePlaceCombat n (read plA) enA
                else do 
                     liftIO $ print $ "No combat in the sector of Corps " ++ (show n)    
                     return ""
            return ""
    
    
    
-- needs rework    
-- Attack
chooseA :: CNr -> Battlefield -> Bool
chooseA n bf = False

-- needs rework
-- charge
chooseC :: CNr -> Bool -> Battlefield -> Bool 
chooseC n plA bf = False  

announceA :: CNr -> Bool -> Bool -> String
announceA n plA enA = 
    "Attacks in Sector " ++ (show n) ++ ": you are " ++ (modeA plA) ++ ", the enemy is " ++ (modeA enA)
    where
        modeA :: Bool -> String
        modeA m = if m then "attacking"
                       else "defending"   

announceC :: CNr -> Bool -> Bool -> String
announceC n plA enA = 
    "Charges in Sector " ++ (show n) ++ ": you are " ++ (modeC plA) ++ ", the enemy is " ++ (modeC enA)
    where
        modeC :: Bool -> String
        modeC m = if m then "charging"
                       else "not charging"                         
                 
-- CNr -> plA -> enA

resolvePlaceCombat :: CNr -> Bool -> Bool -> Field String
resolvePlaceCombat nc plA enA = do
    liftIO $ print "Charge?"
    plC <- liftIO $ getLine 
    bf <- get
    case (chooseC nc (read plC) bf) of
        enC -> do 
            liftIO $ print $ announceA nc (read plC) enC
            state <- resolveCombat nc (plA,(read plC)) (enA,enC) 
            liftIO $ print state
            return ""

resolveCombat :: CNr -> Stance -> Stance -> Field String
resolveCombat nc plS plA = state $ \bf -> (resolveCombat' nc plS plA bf)
         
resolveCombat' :: CNr -> Stance -> Stance -> Battlefield -> (String, Battlefield)
resolveCombat' nc plS enS (a,b) = 
    case nc of 
        1 -> (losses, ((nca,a2,a3,a4,a5),(ncb,b2,b3,b4,b5)))
        2 -> (losses, ((a1,nca,a3,a4,a5),(b1,ncb,b3,b4,b5)))
        3 -> (losses, ((a1,a2,nca,a4,a5),(b1,b2,ncb,b4,b5)))
        _ -> (losses, ((a1,a2,a3,nca,a5),(b1,b2,b3,ncb,b5)))
    where        
        a5@(a51,a52) = fifth a
        a4@(a41,a42) = fourth a
        a3@(a31,a32) = third a
        a2@(a21,a22) = second a
        a1@(a11,a12) = first a
        b5@(b51,b52) = fifth b
        b4@(b41,b42) = fourth b
        b3@(b31,b32) = third b
        b2@(b21,b22) = second b
        b1@(b11,b12) = first b
        ca = case nc of
            1 -> a1
            2 -> a2
            3 -> a3
            _ -> a4
        cb = case nc of
            1 -> b1
            2 -> b2
            3 -> b3
            _ -> b4 
        (losses, (nca,ncb)) = resolveCombat'' ca plS cb enS

            
        
-- not called if both defending  
resolveCombat'' :: Corps -> Stance -> Corps -> Stance -> (String, (Corps, Corps))
resolveCombat'' ca (plA,plC) cb (enA,enC) = 
    if plA 
        then case calcLosses sa sb (plA && enA) of
            (la, lb) -> ("Losses: our forces: " ++ (show la) ++ ", enemy forces: " ++ (show lb),(takeLosses ca plC la, takeLosses cb enC lb))
        else case calcLosses sb sa (plA && enA) of
            (lb, la) -> ("Losses: our forces: " ++ (show la) ++ ", enemy forces: " ++ (show lb),(takeLosses ca plC la, takeLosses cb enC lb))       
    where     
        sa = calcStrength ca plC
        sb = calcStrength cb enC   

        
calcStrength :: Corps -> Bool -> Int
calcStrength (i,c) charging = if charging
    then i+2*c
    else i+c
   
   
-- Attacker -> Defender -> BothAttacking -> (lossA, lossD)            
calcLosses :: Int -> Int -> Bool -> (Int, Int)
calcLosses a d bothAttacking = if bothAttacking
    then if (a > d) 
        then let (l1, l2) = calcLosses a d False 
            in (l1, l2+1)
        else let (l1, l2) = calcLosses d a False
            in (l2+1, l1)
    else if (a > d) 
        then if (a > (2*d)) 
            then if (a > 2)
                then (1,2)
                else (0,2)
            else if (a > 2)
                then (1,1)
                else (0,1)
        else if (a > 2)
            then (1,0)
            else (0,0)

            
takeLosses :: Corps -> Bool -> Int -> Corps
takeLosses (i,c) wasCharging n =
    if wasCharging 
        then if (c < n)
            then (i-(n-c),0)
            else (i,c-n)
        else if (i < n)
            then (0,c-(n-i))
            else (i-n,c)
      -}
testCorps = (10,10)
testReserve = (40,40)
testPlace = (testCorps,testCorps)
testSector = (testPlace,testPlace)
     
      
testBF = [(((10,10),(10,10)),((10,10),(10,10))),
          (((10,10),(10,10)),((10,10),(10,10))),
          (((10,10),(10,10)),((10,10),(10,10))),
          (((10,10),(10,10)),((10,10),(10,10))),
          (((40,40),(40,40)),((40,40),(40,40)))] :: Battlefield


 