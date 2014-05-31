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

-- Bool: True if Attacking, Charging
type Stance = (Bool,Bool)

type Field = StateT Battlefield IO

fifth  (_,_,_,_,c) = c
fourth (_,_,_,c,_) = c
third  (_,_,c,_,_) = c
second (_,c,_,_,_) = c
first  (c,_,_,_,_) = c

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
    reinforcePlayer
    reinforceEnemy
    resolvePlaces
    
    
    
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
    liftIO $ print "Attack with Corps I?"
    stance1A <- liftIO $ getLine    
    liftIO $ print "Attack with Corps II?"
    stance2A <- liftIO $ getLine    
    liftIO $ print "Attack with Corps III?"
    stance3A <- liftIO $ getLine
    liftIO $ print "Attack with Corps IV?"
    stance4A <- liftIO $ getLine
    resolvePlace 1 ((read stance1A),(read stance1C))
    resolvePlace 2 ((read stance2A),(read stance2C))
    resolvePlace 3 ((read stance3A),(read stance3C))
    resolvePlace 4 ((read stance4A),(read stance4C))

    
resolvePlace :: Int -> Stance -> Field String
resolvePlace n plS@(plA, plC) = do
    bf <- get
    case (chooseStanceAI n bf) of
        enS@(enA,enC) -> do 
            liftIO $ print $ announceStances n plS enS
            if plA 
                then if enA 
                    then undefined
                    else undefined
                else if enA 
                    then undefined
                    else undefined                
            return ""
    
    
    
-- needs rework    
-- Attack, Charge
chooseStanceAI :: Int -> Battlefield -> Stance
chooseStanceAI n bf = (False,False)    

announceStances :: Int -> Stance -> Stance -> String
announceStances n pl en = 
    "Combat in Sector " ++ (show n) ++ ": you are " ++ (mode pl) ++ ", the enemy is " ++ (mode en)
    where
        mode :: Stance -> String
        mode (a,c) = (modeA a) ++ (modeC c)
        modeA :: Bool -> String
        modeA m = if m then "attacking"
                       else "defending"
        modeC :: Bool -> String
        modeC m = if m then "(charging)"
                       else "(not charging)"              
                   
--resolveCombat :: 
        
testA1 = ((10,10),(10,10),(10,10),(10,10),(40,40)) :: Army
testA2 = ((10,10),(10,10),(10,10),(10,10),(40,40)) :: Army
testBF = (testA1,testA2) :: Battlefield


 