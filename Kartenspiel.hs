import Control.Monad.Trans.State
import Control.Monad.IO.Class

-- State:
type Cav = Int
type Inf = Int
type CNr = Int
type ANr = Int
type Corps = (Inf,Cav) 
type Reserves = Corps
type Army = (Corps, Corps, Corps, Corps, Reserves)
-- 4 Corps, 5th Reserve
type Battlefield = (Army, Army)
-- should be changed to ((corps,corps),...)

-- Bool: True if Attacking, Charging
type Stance = (Bool,Bool)

type Field = StateT Battlefield IO

data BrokenStatus = Both | Enemy | Own | None

fifth  (_,_,_,_,c) = c
fourth (_,_,_,c,_) = c
third  (_,_,c,_,_) = c
second (_,c,_,_,_) = c
first  (c,_,_,_,_) = c

main = execStateT testT testBF

-- subfunctions:
    
directReserves :: Bool -> Inf -> Cav -> CNr -> Field String    
directReserves isPlayer i c nc = state $ \bf -> 
    case directReserves' isPlayer i c nc bf of
        Right ((remI,remC),nS) -> ("Remaining reserves: " ++ (show remI) ++ " | " ++ (show remC), nS)
        Left err               -> (err, bf)

        

directReserves' :: Bool -> Inf -> Cav -> CNr -> Battlefield -> Either String (Reserves,Battlefield)
directReserves' isPlayer i c nc bf = if (i > r51 || c > r52 || 0 > r51 || 0 > r52 || nc > 4 || 0 > nc) 
                            then Left "Incorrect Input"
                            else if (isPlayer) then Right (((r51-i),(r52-c)),(newArm, b))
                                               else Right (((r51-i),(r52-c)),(b, newArm))
    where
        r5@(r51,r52) = fifth a
        r4@(r41,r42) = fourth a
        r3@(r31,r32) = third a
        r2@(r21,r22) = second a
        r1@(r11,r12) = first a
        a = if (isPlayer) then fst bf
                          else snd bf
        b = if (isPlayer) then snd bf
                          else fst bf
        newArm = case nc of
            1 -> (((r11+i),(r12+c)),r2,r3,r4,((r51-i),(r52-c)))
            2 -> (r1,((r21+i),(r22+c)),r3,r4,((r51-i),(r52-c)))
            3 -> (r1,r2,((r31+i),(r32+c)),r4,((r51-i),(r52-c)))
            4 -> (r1,r2,r3,((r41+i),(r42+c)),((r51-i),(r52-c)))
            _ -> error "still nope"
        
getCurrentDeployment :: Field String
getCurrentDeployment = state $ \bf -> (show $ fst bf, bf)       

-- resolveBattle 
--   resolvePlace
--     setStance -- must be done before to allow night
--     resolveCombat

-- resolveCombat :: (Corps,Stance) -> (Corps,Stance) -> ??


-- resolveTurn
--   setStances
--   resolvePlace
--     setCharge (?)
--     resolveCombat

--main :: StateT Battlefield IO ()
--main = put testBF
  
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
               
-- needs Fix to fold
-- new idea for battlefield: [(Corps,Corps)] -> Last is Reserve, Init are forces -> better               
checkBroken :: Battlefield -> BrokenStatus
checkBroken (a,b) 
    | ownBroken && enemyBroken = Both
    | enemyBroken = Enemy
    | ownBroken = Own
    | otherwise = None
    where
        a4 = fourth a
        a3 = third a
        a2 = second a
        a1 = first a
        b4 = fourth b
        b3 = third b
        b2 = second b
        b1 = first b
        ownBroken = ((checkBrokenCorps a1) || (checkBrokenCorps a2) || (checkBrokenCorps a3) || (checkBrokenCorps a4))
        enemyBroken = ((checkBrokenCorps b1) || (checkBrokenCorps b2) || (checkBrokenCorps b3) || (checkBrokenCorps b4))
        
checkBrokenCorps :: Corps -> Bool
checkBrokenCorps (i,c) = (i <= 0) && (c <= 0)        

    
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
      
      
testA1 = ((10,10),(10,10),(10,10),(10,10),(40,40)) :: Army
testA2 = ((2,2),(10,10),(10,10),(10,10),(40,40)) :: Army
testBF = (testA1,testA2) :: Battlefield


 