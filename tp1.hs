-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
--
-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Évaluateur (la partie que vous devez compléter)
-- - Pretty printer

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Libraire d'analyse syntaxique (et lexicale).
import Data.Char        -- Conversion de Chars de/vers Int et autres
-- import Numeric               -- Pour la fonction showInt
import System.IO                -- Pour stdout, hPutStr
-- import Data.Maybe            -- Pour isJust and fromJust

---------------------------------------------------------------------------
-- 1ère représentation interne des expressions de notre language         --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3) == (+ . (2 . (3 . ())))
--         ==> Scons (Ssym "+")
--                   (Scons (Snum 2)
--                          (Scons (Snum 3) Snil))
--
-- (/ (* (- 68 32) 5) 9)
--     ==>
-- Scons (Ssym "/")
--       (Scons (Scons (Ssym "*")
--                     (Scons (Scons (Ssym "-")
--                                   (Scons (Snum 68)
--                                          (Scons (Snum 32) Snil)))
--                            (Scons (Snum 5) Snil)))
--              (Scons (Snum 9) Snil))

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                pChar '\n'; return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment); return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(quote E)"
pQuote :: Parser Sexp
pQuote = do { pChar '\''; pSpaces; e <- pSexp;
              return (Scons (Ssym "quote") (Scons e Snil)) }

-- Une liste est de la forme:  ( {e} [. e] )
pList :: Parser Sexp
pList  = do { pChar '('; pSpaces; pTail }
pTail :: Parser Sexp
pTail  = do { pChar ')'; return Snil }
     <|> do { pChar '.'; pSpaces; e <- pSexp; pSpaces;
              pChar ')' <|> error ("Missing ')' after: " ++ show e);
              return e }
     <|> do { e <- pSexp; pSpaces; es <- pTail; return (Scons e es) }

pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pSpaces;
                pList <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _p s = case parse pSexp "" s of
                       Left _ -> []
                       Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Pretty Printer                                                        --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) =
    let showTail Snil = showChar ')'
        showTail (Scons e1' e2') =
            showChar ' ' . showSexp' e1' . showTail e2'
        showTail e = showString " . " . showSexp' e . showChar ')'
    in showChar '(' . showSexp' e1 . showTail e2

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de Hugs).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire                                          --
---------------------------------------------------------------------------

type Var = String

data Ltype = Lint
           | Llist Ltype
           | Larw Ltype Ltype
           deriving (Show, Eq)
    
data Lexp = Lnum Int            -- Constante entière.
          | Lvar Var            -- Référence à une variable.
          | Lhastype Lexp Ltype -- Annotation de type.
          | Lfun Var Lexp    -- Fonction anonyme prenant un argument.
          | Lcall Lexp Lexp      -- Appel de fonction, avec un argument.
          | Lnil                -- Constructeur de liste vide.
          | Lcons Lexp Lexp     -- Constructeur de liste.
          | Lcase Lexp Var Var Lexp Lexp -- Expression conditionelle.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Lletrec [(Var, Maybe Ltype, Lexp)] Lexp
          deriving (Show, Eq)

-- Première passe simple qui analyse un Sexp et construit une Lexp équivalente.

s2letrecfuntype :: Ltype -> Sexp -> Ltype 
--Type de la fonction, liste des args avec leurs types, 
--Resultat type (hastype Larw funct.....)
s2letrecfuntype ftype (Scons (Scons (Ssym _) t1) xs) = 
  case xs of
    Snil -> Larw (s2t t1) ftype
    _ -> Larw (s2t t1) (s2letrecfuntype ftype xs)
s2letrecfuntype _ _ = error ""

s2letrecfun :: Lexp -> Sexp -> Lexp
s2letrecfun body (Scons (Scons (Ssym x) _ ) xs) =
  case xs of
    Snil -> Lfun x body
    _ -> Lfun x (s2letrecfun body xs)
s2letrecfun _ _ = error ""

-- map for the let arguments
s2letmap :: (Sexp -> letarg1) -> Sexp -> [letarg1]
s2letmap _ Snil = []
s2letmap f (Scons x xs) = f x : s2letmap f xs
s2letmap _ e = error ("Did not recieve an Scons: "++ (show e))

-- Construction des arguments
s2w :: Sexp -> (Var, Maybe Ltype, Lexp)
s2w (Scons (Ssym x) v) = case s2w' v of (mt, e) -> (x, mt, e)
-- patch for the last case
s2w (Scons (Scons (Ssym f) (Scons (Scons (Ssym x) t1) xs)) 
  (Scons ft ( Scons body Snil))) = 
    let args = (Scons (Scons (Ssym x) t1) xs) in 
      (f, Just (s2t ft), (Lhastype(s2letrecfun (s2l body) args ) 
      (s2letrecfuntype (s2t ft) args)))
s2w e = error ("Could not treat this expression" ++ (show e))

s2w' :: Sexp -> (Maybe Ltype, Lexp)
s2w' (Scons e Snil) = (Nothing, s2l e)
s2w' (Scons t (Scons e Snil)) = (Just (s2t t), s2l e)
s2w' (Scons (Scons (Ssym x) (Scons t Snil)) d) =
  case s2w' d of
    (Nothing, _) -> error "Expected a type, got Nothing"
    (Just t', e) -> (Just (Larw (s2t t) t'), Lfun x e)
s2w' e = error ("Did not recieve right expression"++(show e))

s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym "nil") = Lnil
s2l (Ssym s) = Lvar s
s2l (Scons (Ssym "cons") (Scons e1 (Scons e2 Snil))) =
  Lcons (s2l e1) (s2l e2)
s2l (Scons (Ssym "fn") (Scons (Scons (Ssym arg) Snil) (Scons body Snil))) =
  Lfun arg (s2l body)
-- Sucre syntaxique des fonctions
s2l (Scons (Ssym "fn") (Scons (Scons (Ssym x) xs) body)) =
  Lfun x (s2l (Scons (Ssym "fn") (Scons xs body)))

s2l (Scons (Ssym "hastype") (Scons e (Scons t Snil))) =
  Lhastype (s2l e) (s2t t)

s2l (Scons (Ssym "list") (Scons x Snil))  = Lcons (s2l x) Lnil
s2l (Scons (Ssym "list") (Scons x xs)) = 
  Lcons (s2l x) (s2l (Scons (Ssym "list") xs))

s2l (Scons (Ssym "let") (Scons s (Scons body Snil))) =
    Lletrec (s2letmap s2w s) (s2l body)

s2l (Scons (Ssym "case") (Scons e (Scons (Scons (Ssym "nil") (Scons en Snil)) 
  (Scons (Scons (Scons (Ssym "cons") 
    (Scons (Ssym x) (Scons (Ssym xs) Snil))) (Scons ec Snil)) Snil)))) = 
      Lcase (s2l e) x xs (s2l ec) (s2l en)

s2l (Scons e1 (Scons e2 Snil)) = Lcall (s2l e1) (s2l e2)
-- Sucre syntaxique des expressions
s2l (Scons f (Scons x xs)) = 
  s2l (Scons (Scons f (Scons x Snil)) xs)
s2l se = error ("Malformed Slip: " ++ (showSexp se)) 

-- Fonction helper pour les types du hastype
s2t :: Sexp -> Ltype
s2t (Ssym "Int") = Lint
s2t (Scons t Snil) = s2t t
s2t (Scons (Ssym "List") (Scons body Snil)) = Llist (s2t body)
s2t (Scons t1 (Scons (Ssym "->") (Scons t2 Snil))) = Larw (s2t t1) (s2t t2)
s2t (Scons t1 t2) = Larw (s2t t1) (s2t t2)
s2t _ = error "Your type Slipped (I know... I'm funny...)"

---------------------------------------------------------------------------
-- Vérification des types                                                --
---------------------------------------------------------------------------

type TEnv = Var -> Ltype

tenv0 :: TEnv
tenv0 "+" = Larw Lint (Larw Lint Lint)
tenv0 "-" = Larw Lint (Larw Lint Lint)
tenv0 "*" = Larw Lint (Larw Lint Lint)
tenv0 "/" = Larw Lint (Larw Lint Lint)
tenv0 x   = error ("Unknown type variable: " ++ show x)

-- Modification de l'environnement de type
tlookup :: TEnv -> Var -> Ltype
tlookup tenv = tenv

tinsert :: TEnv -> Var -> Ltype -> TEnv
tinsert tenv x t = \y -> if (y == x) then t else tlookup tenv y

-- Pour le let, modification de l'environnement de type
createT1 :: TEnv -> [(Var, Maybe Ltype ,Lexp)] -> TEnv
createT1 tenv ((x, mt, e):xs) =
  case xs of
    [] -> case mt of
      Nothing -> (tinsert tenv x (infer tenv e))
      _ -> tenv
    _ -> case mt of
      Nothing -> createT1 (tinsert tenv x (infer tenv e)) xs
      _ -> createT1 tenv xs
createT1 _ _ = error "not my format"

createT2 :: TEnv -> [(Var, Maybe Ltype, Lexp)] -> TEnv
createT2 tenv ((x, mt, _):xs) =
  case xs of
    [] -> case mt of
      Just mt' -> (tinsert tenv x mt')        
      Nothing -> tenv
    _ -> case mt of
      Just mt' -> createT2 (tinsert tenv x mt') xs        
      Nothing -> createT2 tenv xs
createT2 _ _ = error "not my format"

-- Fonctionne comme check, mais pour l'exp et le type du Letrec
verif :: TEnv -> [(Var, Maybe Ltype, Lexp)] -> Bool
verif tenv ((_, mt, e1):xs) =
  case xs of
    [] -> case mt of
      Just mt' -> let typ = mt' in 
        check tenv e1 typ
      Nothing -> True

    _ -> case mt of
      Nothing -> verif tenv xs  
      Just mt' -> 
        case check tenv e1 mt' of
          True -> verif tenv xs
          False -> error "error"
verif _ _ = error "not my format"

infer :: TEnv -> Lexp -> Ltype
infer _ (Lnum _) = Lint
infer tenv (Lvar v) = tenv v
infer tenv (Lhastype e t) = 
  case check tenv e t of
    True -> t
    False -> error "Did not get the right type"

infer tenv (Lcall e1 e2) = 
  case infer tenv e1 of
    Larw t1 t2 -> if check tenv e2 t1 then t2
                    else error "Wrong starting type"
    t -> error ("Wrong Ltype given"++(show t))

infer tenv (Lcons e1 e2) =
  case check tenv e2 (Llist (infer tenv e1)) of
    True  -> Llist (infer tenv e1)
    False -> error "List arguments are of wrong types"

infer tenv (Lletrec ((x, mt, e1):xs) e2) =
    let tenv1 = createT1 tenv ((x, mt, e1):xs) in
        case infer tenv1 e1 of
            _ -> let tenv2 = createT2 tenv1 ((x, mt, e1):xs) in
                case verif tenv2 ((x, mt, e1):xs) of
                    True -> infer tenv2 e2
                    False -> error "error cannot infer Lletrec"

infer _ e = error ("Can't infer type of " ++ (show e))

check :: TEnv -> Lexp -> Ltype -> Bool
check _ Lnil (Llist _) = True
check tenv (Lfun x e) (Larw t1 t2) = check (tinsert tenv x t1) e t2

check tenv (Lcase e x xs ec en) t =
  let t' = infer tenv e
  in case check tenv en t of
       True -> case check tenv' ec t of
                 True -> case check tenv'' ec t of
                           True -> True
                           _ -> error ("Body not a Llist of type "++(show t'))
                         where tenv'' = tinsert tenv' xs (Llist t')
                 _ -> error ("Head not of type "++(show t'))
               where tenv' = tinsert tenv x t'
       _ -> error ((show en)++" is not of type "++(show t))

check tenv e t = let t' = infer tenv e
                 in if t == t' then True
                  else if t' == Llist t then True
                    else error ("Type mismatch "
                                ++ (show t) ++ " != " ++ (show t'))

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

data Value = Vnum Int
           | Vnil
           | Vcons Value Value
           | Vfun (Value -> Value)

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec _p Vnil = showString "[]"
    showsPrec p (Vcons v1 v2) =
        let showTail Vnil = showChar ']'
            showTail (Vcons v1' v2') =
                showChar ' ' . showsPrec p v1' . showTail v2'
            showTail v = showString " . " . showsPrec p v . showChar ']'
        in showChar '[' . showsPrec p v1 . showTail v2
    showsPrec _p _ = showString "<function>"

type Env = Var -> Value 

      
-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: Env
env0 "+" = Vfun (\v1 -> case v1 of
    Vnum x -> Vfun (\v2 -> case v2 of
        Vnum y -> Vnum (x + y)
        _ -> error "Expected a number for the second argument in +")
    _ -> error "Expected a number for the first argument in +")

env0 "*" = Vfun (\v1 -> case v1 of
    Vnum x -> Vfun (\v2 -> case v2 of
        Vnum y -> Vnum (x * y)
        _ -> error "Expected a number for the second argument in *")
    _ -> error "Expected a number for the first argument in *")

env0 "/" = Vfun (\v1 -> case v1 of
    Vnum x -> Vfun (\v2 -> case v2 of
        Vnum y -> Vnum (x `div` y)
        _ -> error "Expected a number for the second argument in /")
    _ -> error "Expected a number for the first argument in /")

env0 "-" = Vfun (\v1 -> case v1 of
    Vnum x -> Vfun (\v2 -> case v2 of
        Vnum y -> Vnum (x - y)
        _ -> error "Expected a number for the second argument in -")
    _ -> error "Expected a number for the first argument in -")

env0 x = error ("Unknown env: " ++ show x)


elookup :: Env -> Var -> Value
elookup env = env

createsenv :: Env -> [(Var, Maybe Ltype, Lexp)] -> Env 
createsenv env ((x, _, e1):xs) =
    case xs of 
        [] -> einsert env x (eval env e1)
        
        _ -> einsert (createsenv env xs) x (eval env e1)
createsenv env [] = env

einsert :: Env -> Var -> Value -> Env
einsert env var val = \x -> if (x == var) then val else elookup env x

-- La fonction d'évaluation principale.
eval :: Env -> Lexp -> Value

eval _env (Lnum n) = Vnum n
eval env (Lvar x) = elookup env x

eval env (Lhastype e _) = eval env e
eval env (Lfun x e) = Vfun f where f val = eval (einsert env x val) e
eval env (Lcall fun arg) =
  case eval env fun of
        Vfun x               -> x (eval env arg)
        _                 -> error "why the fuck"
eval _ Lnil = Vnil
eval env (Lcons e1 e2) = Vcons (eval env e1) (eval env e2)

eval env (Lcase e x xs en ec) =
  case eval env e of
    Vnil -> eval env ec
    Vfun _ -> error "cannot eval on Vfun"
    Vnum _ -> error "cannot eval on Vnum"
    Vcons v1 v2 -> eval (einsert (einsert env x v1) xs v2) en

eval env (Lletrec [] e) = eval env e
eval env (Lletrec ((x, mlt, e1):xs) e) = 
    let senv = createsenv env ((x, mlt, e1):xs) in
        eval senv e
---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do s <- readFile filename
       (hPutStr stdout . show)
           (let sexps s' = case parse pSexps filename s' of
                            Left _ -> [Ssym "#<parse-error>"]
                            Right es -> es
            in map (\ sexp -> let { lexp = s2l sexp
                                 ; ltyp = infer tenv0 lexp
                                 ; val = eval env0 lexp }
                             in (val, ltyp))
                   (sexps s))

-- Don't use type classes for `read'.
test :: String -> Ltype
test = infer tenv0 . s2l . read