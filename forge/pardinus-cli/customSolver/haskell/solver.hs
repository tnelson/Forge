{-# LANGUAGE LambdaCase #-}
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Foldable                  ( find )
import qualified Data.Map                      as Map
import           Data.Time
import           Text.Printf
import           Control.Exception
import           System.Environment
import           System.Random
import           Debug.Trace
import           Control.DeepSeq

-- Debug build
debug :: a -> String -> a
debug = flip trace


-- Data definitions

type Variable = Int
data Literal = Literal { literalVar :: Variable
                       , literalSign :: Bool
                       } deriving (Ord, Eq, Show)
type Clause = Set Literal
type SATInstance = Set Clause
data Result = Assignment [(Variable, Bool)] | Unsat


instance Show Result where
    show Unsat             = "UNSAT"
    show (Assignment assn) = foldl
        (\str (var, bool) -> str ++ (if bool then "-" else "") ++ (show var) ++ " ")
        ""
        assn


instance NFData Result where
    rnf Unsat            = ()
    rnf (Assignment lst) = lst `deepseq` ()


-- Functions for defined data types

showBool :: Bool -> String
showBool b = if b then "true" else "false"

-- Functions to generate SAT Instance from DIMACS file

makeLiteral :: Int -> Literal
makeLiteral litInt =
    let absLit = abs litInt
    in
        (if litInt > 0
            then Literal absLit True
            else if litInt < 0
                then Literal absLit False
                else
                    error
                        "Error: Literal must be either postive or negative number"
        )

makeLiteralFromStr :: String -> Literal
makeLiteralFromStr litStr = makeLiteral (read litStr)

negateLiteral :: Literal -> Literal
negateLiteral l = Literal (literalVar l) (not (literalSign l))

makeClause :: [String] -> Clause
makeClause = Set.fromList . map makeLiteralFromStr . init

litToVarSigned :: Literal -> Variable
litToVarSigned l =
    if literalSign l then literalVar l else negate (literalVar l)

parseCNF :: String -> SATInstance
parseCNF input =
    let allLines     = lines input
        -- tokenize and remove comment lines
        tokLines     = map words (filter (/= "") allLines)
        contentLines = filter (\tokLine -> head tokLine /= "c") tokLines
        -- make sure first line is problem line
        pLine        = if head (head contentLines) /= "p"
            then error "Error: DIMACS file does not have problem line"
            else head contentLines
        -- Create a set of clauses from the content lines
        cnf = Set.fromList . map makeClause . tail $ contentLines
    in  assert (pLine !! 1 == "cnf")
            . assert (all (\clause -> last clause == "0") . tail $ contentLines)
            $ cnf


-- Functions to implement DPLL algorithm

removeClausesWithLiteral :: Literal -> SATInstance -> SATInstance
removeClausesWithLiteral literal = Set.filter (literal `Set.notMember`)

removeNegatedLiteral :: Literal -> SATInstance -> SATInstance
removeNegatedLiteral literal = Set.map (Set.filter (/= negateLiteral literal))

compactify :: SATInstance -> [[Variable]]
compactify = Set.toList . Set.map (Set.toList . Set.map litToVarSigned)

unitClauseElim :: Result -> SATInstance -> (Result, SATInstance)
unitClauseElim Unsat cnf = (Unsat, cnf)
unitClauseElim res@(Assignment assn) cnf
    | Set.null cnf -- empty cnf
    = (Assignment assn, cnf)
    | -- `debug` "UCE: null, called with " ++ show (assn, compactify cnf)
      otherwise -- get first unit clause, eliminate it, and recur
    = let unitClauses = Set.unions (Set.filter (\c -> Set.size c == 1) cnf)
      in  if Set.null unitClauses
              then (Assignment assn, cnf)
              else
                  let new_assn =
                          [ (literalVar literal, literalSign literal)
                          | literal <- Set.toList unitClauses
                          ]
                  in  ( assignmentAppend res (Assignment new_assn)
                      , (foldl (\cnf_inst func -> func cnf)
                               cnf
                               (map removals (Set.toList unitClauses))
                        )
                      )
  where
    removals =
        (\lit -> removeClausesWithLiteral lit . removeNegatedLiteral lit)

assignmentAppend :: Result -> Result -> Result
assignmentAppend Unsat _     = Unsat
assignmentAppend _     Unsat = Unsat
assignmentAppend (Assignment assn) (Assignment new_assn) =
    Assignment (assn ++ new_assn)

sameSignElim :: Result -> SATInstance -> (Result, SATInstance)
sameSignElim Unsat cnf = (Unsat, cnf)
sameSignElim res@(Assignment _) cnf
    | Set.null cnf -- empty cnf
    = (res, cnf)
    |
    -- `debug` "SSE: null, called with " ++ show
    --     (assn, compactify cnf)
      otherwise -- get literal with same sign, eliminate it
    = -- trace ("SSE: othw, called with " ++ show (assn, compactify cnf)) $
      let vars = concatMap (map litToVarSigned . Set.toList) $ Set.toList cnf
      in
          let
              sameSignLits = Set.fromList
                  (map
                      makeLiteral
                      (filter
                          (\x ->
                              (length . filter (\v -> v == x || v == negate x))
                                      vars
                                  == 1
                          )
                          vars
                      )
                  )
          in
              let
                  new_assn = Assignment
                      (( Set.toList
                       . Set.map ((\v -> (v, v >= 0)) . litToVarSigned)
                       )
                          sameSignLits
                      )
              in  ( assignmentAppend res new_assn
                  , Set.filter
                      (\s -> Set.null $ Set.intersection s sameSignLits)
                      cnf
                  )
                      -- `debug` "SSE: sameSignLits "
                      -- ++      show (Set.map litToVarSigned sameSignLits)


-- Random functions 

randPop :: RandomGen g => g -> Set a -> (a, Set a)
randPop randGen s
    | Set.null s
    = error "Error: Empty set passed to pop from"
    | otherwise
    = let (rand_num, _) = randomR (0, length s - 1) randGen
      in  (Set.elemAt rand_num s, Set.deleteAt rand_num s)

randomHeuristic :: RandomGen g => g -> SATInstance -> (Literal, SATInstance)
randomHeuristic randGen cnf =
    let (clause       , restCNF   ) = randPop randGen cnf
        (poppedLiteral, restClause) = randPop randGen clause
    in  (poppedLiteral, Set.insert restClause restCNF)


isEmptyCNF :: SATInstance -> Bool
isEmptyCNF = Set.null

hasEmptyClause :: SATInstance -> Bool
hasEmptyClause = (any Set.null) . Set.toList

solveWithAssn :: RandomGen g => Result -> g -> SATInstance -> Result
solveWithAssn Unsat _ cnf = Unsat -- `debug` "SOL: got unsat" ++ show (cnf)
solveWithAssn res@(Assignment _) randGen cnf
    | isEmptyCNF cnf
    = res -- `debug` ("SOL: cnf empty " ++ show (res, compactify cnf))
    | hasEmptyClause cnf
    = Unsat -- `debug` ("SOL: cnf has empty " ++ show (res, compactify cnf))
    | otherwise -- TODO: something wonky going on in inference here
    = let
          (newRes@(Assignment newAssn), newCNF) = comp
              -- trace ("SOL: after round " ++ show (compactify . snd $ comp))
              --    $ comp
              where comp = uncurry sameSignElim . unitClauseElim res $ cnf
          (rg1, rg2) = split randGen
      in
          if isEmptyCNF newCNF
              then newRes
              -- `debug` "SOL: ncnf empty " ++ show (newRes, newCNF)
              else if hasEmptyClause newCNF
                  then Unsat
                  -- `debug` "SOL: ncnf has empty " ++ show
                      -- (newRes, newCNF)
                  else
                      let
                          (literal, restCNF) = randomHeuristic randGen newCNF
                          resultT =
                              (solveWithAssn
                                      (Assignment
                                          ((literalVar literal, True) : newAssn)
                                      )
                                      rg1
                                      restCNF
                                  )
                                  -- `debug` (  "SOL: trying true "
                                  --         ++ show (litToVarSigned literal)
                                  --         )
                          resultF =
                              (solveWithAssn
                                      (Assignment
                                          ((literalVar literal, False) : newAssn)
                                      )
                                      rg2
                                      restCNF
                                  )
                                  -- `debug` (  "SOL: trying false "
                                  --         ++ show (litToVarSigned literal)
                                  --         )
                      in
                          case resultT of
                              tAssn@(Assignment _) ->
                                  tAssn
                                      -- `debug` (  "SOL: was t "
                                      --         ++ show (resultT, resultF)
                                      --         )
                              Unsat -> case resultF of
                                  fAssn@(Assignment _) ->
                                      fAssn
                                          -- `debug` ("SOL: was f " ++ show
                                          --             (resultT, resultF)
                                          --         )

                                  Unsat ->
                                      Unsat
                                          -- `debug` ("SOL: was unsat " ++ show
                                          --             (resultT, resultF)
                                          --         )


solve :: StdGen -> SATInstance -> Result
solve = solveWithAssn (Assignment [])


-- has a bug
solutionChecker :: Result -> SATInstance -> Maybe Bool
solutionChecker Unsat _ = Nothing
solutionChecker res@(Assignment assn) cnf =
    let assn_map = Map.fromList assn
    in  Just
            (all
                (== True)
                (Set.map
                    ( any
                            (\case
                                Nothing -> True
                                Just b  -> b
                            )
                    . Set.map ((Map.!?) assn_map . literalVar)
                    )
                    cnf
                )
            )


checkUnsat :: Result -> String
checkUnsat Unsat = "UNSATISFIABLE"
checkUnsat _ = "SATISFIABLE"


formatOutput :: String -> NominalDiffTime -> Result -> String
formatOutput file runTime res =
    "s " ++ (checkUnsat res) ++ "\n" ++ "v " ++ (show res)

numVar :: SATInstance -> Int 
numVar cnf = length . concat . map Set.toList $ (Set.toList cnf)

main :: IO ()
main = do
    args <- getArgs
    let file = head args
    contents <- readFile file
    let cnf = parseCNF contents
    -- print (compactify cnf)
    randGen <- getStdGen
    start   <- getCurrentTime
    let result = (solve randGen) cnf  -- parse and solve
    end <- result `deepseq` getCurrentTime  -- force the computation and get end time
    putStr (formatOutput file (diffUTCTime end start) result) -- print it out
