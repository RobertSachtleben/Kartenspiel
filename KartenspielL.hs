import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad
import System.Random

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
            then (i > oldRIP || c > oldRCP || 0 > oldRIP || 0 > oldRCP || nc > places || 0 > nc) 
            else (i > oldRIA || c > oldRCA || 0 > oldRIA || 0 > oldRCA || nc > places || 0 > nc) 
        ((oldRP@(oldRIP,oldRCP),oldRA@(oldRIA,oldRCA)),estimR) = last bf
        ((oldCP@(oldCIP,oldCCP),oldCA@(oldCIA,oldCCA)),estimC) = bf !! (nc-1)
        newCorps = if isPlayer
            then (((oldCIP+i,oldCCP+c),oldCA),estimC)
            else ((oldCP,(oldCIA+i,oldCCA+c)),estimC)
        newReserves = if isPlayer
            then (((oldRIP-i,oldRCP-c),oldRA),estimR)
            else ((oldRP,(oldRIA-i,oldRCA-c)),estimR)
        beforeC = take (nc-1) bf
        betweenCR = drop (nc) $ init bf
        newBF = beforeC ++ [newCorps] ++ betweenCR ++ [newReserves]
        
       
getCurrentDeployment :: Field String
getCurrentDeployment = state $ \bf -> (showCurrentState bf, bf)       

showCurrentState :: Battlefield -> String
showCurrentState bf = 
    "Current Deployment: \n" ++ (concat $ (map printPlace (zip [1..] (init bf))) ++ [(printReserves (last bf))])
    where
        printPlace :: (Int,(Place,LastKnown)) -> String
        printPlace (n,(((i,c),_),(_,(eI,eC)))) =
            "Sector  " ++ (show n) ++ ": " ++ (show i) ++ " ID and " ++ (show c) ++ " CvD against " ++ (show eI) ++ "(?) ID and " ++ (show eC) ++ "(?) CvD" ++ "\n"
        printReserves :: (Place,LastKnown) -> String
        printReserves (((i,c),_),(_,(eI,eC))) =
            "Reserves " ++ ": " ++ (show i) ++ " ID and " ++ (show c) ++ " CvD against " ++ (show eI) ++ "(?) ID and " ++ (show eC) ++ "(?) CvD" ++ "\n"  

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
                
main = execStateT testT testBF  
  
testT :: Field String
testT = do
    put testBF
    testT'
  
testT' :: Field String
testT' = do
    depl <- getCurrentDeployment
    liftIO $ putStrLn depl
    reinforcePlayer 
    reinforceEnemy
    resolvePlaces
    bf <- get
    case (checkBroken (init bf)) of
        Both -> do
            depl' <- getCurrentDeployment
            liftIO $ putStrLn depl'    
            liftIO $ print "Both lines have been broken!"
            return ""
        Enemy -> do
            depl' <- getCurrentDeployment
            liftIO $ putStrLn depl'
            liftIO $ print "Enemy line has been broken!"
            return ""
        Own   -> do
            depl' <- getCurrentDeployment
            liftIO $ putStrLn depl'
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
    case (read done) of 
        True  -> do
            depl <- getCurrentDeployment
            liftIO $ putStrLn depl
            return ""
        False -> reinforcePlayer

-- Nr of Places
resolvePlaces :: Field String
resolvePlaces = do
    bf <- get
    liftIO $ print "Declare attacks:"
    attackChance <- liftIO $ getStdRandom (randomR (1::Int,100::Int))
    declareAttacks <- liftIO $ getLine
    if ((length $ ((read declareAttacks) ::[Bool])) == places)
        then let
                sA = (read declareAttacks) ::[Bool]
                sB = chooseA bf attackChance
            in  do
                liftIO $ putStrLn $ printA sA sB
                resolvePlaces' sA sB            
        else error "wrong input"

       

-- both list must be of equal length        
resolvePlaces' :: [Bool] -> [Bool] -> Field String
resolvePlaces' [] [] = return ""
resolvePlaces' (x:xs) (y:ys) = do
        resolvePlace nc x y
        resolvePlaces' xs ys
    where
        nc = places - (length xs)
        
-- both list must be of equal length
printA :: [Bool] -> [Bool] -> String
printA [] [] = ""
printA (x:xs) (y:ys) = (announceA nc x y)  ++ "\n" ++ (printA xs ys)
    where
        nc = places - (length (xs))
        announceA :: CNr -> Bool -> Bool -> String
        announceA n plA enA = 
            "Results: Sector " ++ (show n) ++ ": you are " ++ (modeA plA) ++ ", the enemy is " ++ (modeA enA)
            where
                modeA :: Bool -> String
                modeA m = if m then "attacking"
                               else "defending" 
    
resolvePlace :: Int -> Bool -> Bool -> Field String
resolvePlace n plA enA = do
    liftIO $ print $ "Combat in Sector " ++ (show n)
    if (plA || enA) 
        then resolvePlaceCombat n plA enA
        else do 
            liftIO $ print $ "No combat in the sector of Corps " ++ (show n)    
            return ""
        

resolvePlaceCombat :: Int -> Bool -> Bool -> Field String        
resolvePlaceCombat n plA enA = do
    liftIO $ print "Charge?"
    plC <- liftIO $ getLine 
    chargeChance <- liftIO $ getStdRandom (randomR (1::Int,100::Int))
    bf <- get
    case (chooseC (bf !! (n-1)) plA enA chargeChance) of
        enC -> do 
            liftIO $ print $ announceC n (read plC) enC
            state <- resolveCombat n (plA,(read plC)) (enA,enC) 
            liftIO $ putStrLn state
            return ""


resolveCombat :: CNr -> Stance -> Stance -> Field String
resolveCombat nc plS plA = state $ \bf -> (resolveCombat' nc plS plA bf)

resolveCombat' :: CNr -> Stance -> Stance -> Battlefield -> (String, Battlefield)
resolveCombat' n plS enS bf = (losses, newBF)
    where 
        ((ca,cb),(expCB,expCA)) = bf !! (n-1)
        ((ra,rb),(expRB,expRA)) = last bf
        (losses, (nca,ncb)) = resolveCombat'' ca plS cb enS
        nPlace = ((nca,ncb),(nca,ncb)) :: (Place, LastKnown)
        nRA = calcKnownReserve expCA cb expRA
        nRB = calcKnownReserve expCB ca expRB
        nReserve = ((ra,rb),(nRB,nRA))
        beforeC = take (n-1) bf
        betweenCR = drop (n) $ init bf
        newBF = beforeC ++ [nPlace] ++ betweenCR ++ [nReserve]
         
calcKnownReserve :: Corps -> Corps -> Reserves -> Reserves -- expCorps -> realCorps -> oldR -> newR
calcKnownReserve (ei,ec) (ri,rc) (oldRI,oldRC) =
    -- real always at least expected
    (oldRI - (ri - ei), oldRC - (rc - ec)) 

-- not called if both defending  
resolveCombat'' :: Corps -> Stance -> Corps -> Stance -> (String, (Corps, Corps))
resolveCombat'' ca (plA,plC) cb@(ei,ec) (enA,enC) = 
    if plA 
        then case calcLosses sa sb (plA && enA) of
            (la, lb) -> (before ++ "Losses: our forces: " ++ (show la) ++ ", enemy forces: " ++ (show lb),(takeLosses ca plC la, takeLosses cb enC lb))
        else case calcLosses sb sa (plA && enA) of
            (lb, la) -> (before ++ "Losses: our forces: " ++ (show la) ++ ", enemy forces: " ++ (show lb),(takeLosses ca plC la, takeLosses cb enC lb))       
    where     
        sa = calcStrength ca plC
        sb = calcStrength cb enC 
        before = "Revealed enemy strength: " ++ (show ei) ++ " ID and " ++ (show ec) ++ " CvD! \n"  

announceC :: CNr -> Bool -> Bool -> String
announceC n plA enA = 
    "Charges in Sector " ++ (show n) ++ ": you are " ++ (modeC plA) ++ ", the enemy is " ++ (modeC enA)
    where
        modeC :: Bool -> String
        modeC m = if m then "charging"
                       else "not charging"                         

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
            
            
{---------"AI"-functions-------------------}

-- currently dumb    
reinforceEnemy :: Field String
reinforceEnemy = do
    liftIO $ print "Enemy reinforcing..."
    -- check if any less than 3
    -- -> with random first check to avoid always reinforcing first sector first
    -- check strongest expected enemy
    -- identify current enemy weakness (any less than 2)
    -- identify worthy targets (any expectedly less than own) 
    chance <- liftIO $ getStdRandom (randomR (1::Int,100::Int))
    reinforce3 chance
    return ""
{-
reinforceEnemy' :: Field String
reinforceEnemy' = do
    bf <- get 
    chance <- liftIO $ getStdRandom (randomR (1::Int,100::Int))
    case (last bf) of 
        ((_,rr@(ri,rc)),_) -> do
-}
        

reinforce3 :: Int -> Field String
reinforce3 chance = 
    let
        --k bf' = div chance (length bf')
        k bf' = chance * (length bf')
        result bf' = reinforce3' chance ((rotateL (k bf') $ init bf'), last bf')
    in
        state $ \bf -> ("",(rotateR (k bf) $ init (result bf)) ++ [(last (result bf))])
    
        
         
rotateL :: Int -> [a] -> [a]
rotateL n xs =  take (length xs) (drop k (cycle xs))
    where
        k = mod n (length xs)

rotateR :: Int -> [a] -> [a]
rotateR n xs =  take (length xs) (drop ((length xs) - k) (cycle xs))
    where
        k = mod n (length xs)

f :: Int -> [a] -> [a]
f n xs = rotateL ((length xs) - k) $ rotateL k xs
    where k = div n (length xs)
        
-- input-BF does not include reserves, output does
reinforce3' :: Int -> (Battlefield, (Place, LastKnown)) -> Battlefield
reinforce3' _ ([],r) = [r]
reinforce3' chance ((x@((cp,ce@(i,c)),lk):xs), rsv@((rp,(ri,rc)),lkr)) = 
    if (str < 3) 
        then if (ri+str >= 3)
            then if (chance < 96)
                then [((cp,(i+(3-str),c)),lk)] ++ (reinforce3' chance (xs, ((rp,(ri-(3-str),rc)),lkr)))
                else next
            else if (rc+str >= 3)
                then if (chance < 92)
                    then [((cp,(i,c+(3-str))),lk)] ++ (reinforce3' chance (xs, ((rp,(ri,rc-(3-str))),lkr)))
                    else next
                else next
        else next
    where
        str = calcStrength ce False 
        next = [x] ++ (reinforce3' chance (xs, rsv))
        
       
chooseA :: Battlefield -> Int -> [Bool]  
chooseA bf chance = map (chooseA' chance) $ init bf             
            
chooseA' :: Int -> (Place,LastKnown) -> Bool
chooseA' chance ((_,rce),(ecp,_)) = if (strengthEnC > (2*strengthPC))
    then (chance < 80)        
    else if (strengthEnC > strengthPC)
        then (chance < 60)
        else if (strengthEC > strengthPC)
            then (chance < 30)
            else if (strengthEC > strengthPnC)
                then (chance < 20)
                else (chance < 5)
    where
        strengthEC = calcStrength rce True
        strengthEnC = calcStrength rce False
        strengthPC = calcStrength ecp True
        strengthPnC = calcStrength ecp False
        

-- needs rework
-- charge
chooseC :: (Place,LastKnown) -> Bool -> Bool -> Int -> Bool 
chooseC ((_,rce),(ecp,_)) plA enA chance = if enA
    then if (strengthEnC > (2*strengthPC))
        then (chance < 80) 
        else if (strengthEC > (2*strengthPC))
            then (chance < 70) 
            else if (strengthEC > strengthPC)
                then (chance < 40) 
                else (chance < 10)
    else if (strengthPC > strengthEnC)
        then if (strengthPC > (2*strengthEC)) 
            then (chance < 3)
            else if (strengthPC > (2*strengthEnC)) 
                then (chance < 1)
                else (chance < 10)
        else (chance < 50)
    where
        strengthEC = calcStrength rce True
        strengthEnC = calcStrength rce False
        strengthPC = calcStrength ecp True
        strengthPnC = calcStrength ecp False
             
            
            

      
testCorps = (10,10)
testReserve = (40,40)
testPlace = (testCorps,testCorps)
testSector = (testPlace,testPlace)
     
      
testBF = [(((2,0),(3,0)),((2,0),(3,0))),
          (((2,0),(2,0)),((2,0),(2,0))),
          (((2,0),(2,0)),((2,0),(2,0))),
          (((2,0),(2,0)),((2,0),(2,0))),
          (((10,10),(10,10)),((5,5),(5,5)))] :: Battlefield


 