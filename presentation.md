%title: Wolne monady w praktyce
%author: Wojciech Wiśniewski
%date 2021-01-26

-> Problem <-
=============

Potrzebujemy zarejestrować użytkownika w systemie.

Aby to zrobić trzeba wykonać sekwencję kroków:
- sprawdzić czy w serwisie A istnieje konto użytkownika
- założyć konto w systemie B (lub użyć konto już istniejące)
- używając identyfikatora konta z systemu A wysłać do użytkownika SMS z PINem
- odebrać potwierdzenie PINu od użytkownika
- ...

-----------------------------------------------------------------------------

-> Problem cd. <-
=================

Każdy z tych kroków może się nie powieść (błędy sieciowe, restart serwisu,
błędy w kodzie...). Dodatkowo nasz program może zostać zrestartowany
w dowolnym momencie.

Chcielibyśmy móc wznowić naszą procedurę od momentu w którym została przerwana.

Chcielibyśmy, żeby nasz restartowalny program nie różnił się zbytnio od zwykłego
programu pisanego w IO: dopuszczamy, żeby operacje restartowalne wymagały anotacji,
ale oprócz tego chcemy używać zwyczajnej do-notacji.

-----------------------------------------------------------------------------

-> Pierwsze rozwiązanie <-
==========================

Zdefiniujemy typ *Restartable a*, dla którego zdefiniujemy instancję *Monad*

Zdefiniujemy też funkcję *step :: String -> IO a -> Restartable a*,
przy pomocy której będziemy oznaczać operacje restartowalne.

Zróbmy klasę i funkcje pozwalające zapisywać i odczytywać dane z pliku:

    class Persistent a where
      serialize :: a -> String  
      deserialize :: String -> a

    save :: Persistent a => Handle -> a -> IO ()
    save handle a = hPutStrLn handle $ serialize a

    restore :: Persistent a => Handle -> IO (Maybe a)
    restore handle = do
      eof <- hIsEOF handle
      if eof then
        pure Nothing
      else
        Just . deserialize <$> hGetLine handle

-----------------------------------------------------------------------------

-> Pierwsze rozwiązanie cd. <-
==============================

Zdefiniujemy też funkcję *runRestartable :: FilePath -> Restartable a -> IO a*,
uruchamiającą nasz kod w *IO*

-----------------------------------------------------------------------------

-> Własny monad Restartable <-
================================

Opis operacji restartowalnej:

    data Restartable a where
      Step :: Persistent a => String -> IO a -> Restartable a

    step :: String -> IO a -> Restartable a
    step = Step

-----------------------------------------------------------------------------

-> Własny monad Restartable: instancje <-
========================================

Dodajemy konstruktory, które "łapią" metody z klas *Monad* i *Applicative*:

    data Restartable a where
      Step :: Persistent a => String -> IO a -> Restartable a
      Pure :: a -> Restartable a
      Bind :: Restartable x -> (x -> Restartable a) -> Restartable a

    instance Applicative Restartable where
      pure = Pure

    instance Monad Restartable where
      (>>=) = Bind

    step :: String -> IO a -> Restartable a
    step = Step

-----------------------------------------------------------------------------

-> Własny monad Restartable: instancje cd. <-
=============================================

Pozostałe wymagane metody korzystają z instancji *Monad*:

    data Restartable a where
      Step :: Persistent a => String -> IO a -> Restartable a
      Pure :: a -> Restartable a
      Bind :: Restartable x -> (x -> Restartable a) -> Restartable a

    instance Functor Restartable where
      fmap = liftM

    instance Applicative Restartable where
      pure = Pure
      (<*>) = ap

    instance Monad Restartable where
      (>>=) = Bind

    step :: String -> IO a -> Restartable a
    step = Step

-----------------------------------------------------------------------------

-> Własny monad Restartable: interpreter <-
===========================================

Funkcja *runRestartable* (zwróćmy uwagę, że funkcja *go* jest rekurencyjna):

    runRestartable :: forall a . FilePath -> Restartable a -> IO a
    runRestartable path restartable = withFile path ReadWriteMode run
      where
        run :: Handle -> IO a
        run handle = go restartable
          where
            go :: Restartable b -> IO b
            go = \case
              Step name act -> do
                maybeA <- restore handle
                case maybeA of
                  Just a -> do
                    putStrLn $ "step " <> name <> " already completed"
                    pure a
                  Nothing -> do
                    putStrLn $ "running step " <> name
                    a <- act
                    save handle a
                    pure a
              Pure a ->
                pure a
              Bind act f ->
                go act >>= (go . f)

-----------------------------------------------------------------------------

-> Dygresja o prawach <-
========================

Proste "łapanie" metod klasy może powodować powstanie instancji, które nie
spełniają praw. Zobaczmy to na prostszym przykładzie wolnego monoidu:

    class Monoid a where
      mempty :: a
      mappend :: a -> a -> a

    data FreeMonoid a = Embed a | Mempty | Mappend (FreeMonoid a) (FreeMonoid a)

    instance Monoid (FreeMonoid a) where
      mempty = Mempty
      mappend = Mappend

-----------------------------------------------------------------------------

-> Dygresja o prawach cd. <-
============================

Nasz *FreeMonoid* nie spełnia praw, np.

    (a `mappend` b) `mappend` c /= a `mappend` (b `mappend` c)

```
         *            *
        / \          / \
       *   c   /=   a   *
      / \              / \
     a   b            b   c
```

Pamięta on "za dużo": interesuje nas tylko kolejność liści w drzewie, a nie jak ono
powstawało. Free monoid spełniający prawa to lista:

    instance Monoid [a] where
      mempty = []
      mappend = (++)

-----------------------------------------------------------------------------

-> Dygresja o prawach cd. cd. <-
================================

Nasz *Restartable* też nie spełnia praw, np.

    pure a >>= f /= f a

-----------------------------------------------------------------------------

-> Free monad <-
================

Lawfull monad.

Oddzielamy operacje potrzebne w każdym monadzie (*pure*, *>>=*) od
tych specyficznych dla naszego monadu.

    data Free f a = Pure a
                  | Impure (f (Free f a))

    instance Functor f => Functor (Free f) where
      fmap = liftM

    instance Functor f => Applicative (Free f) where
      pure = Pure
      (<*>) = ap

    instance Functor f => Monad (Free f) where
      (Pure a) >>= f = f a
      (Impure i) >>= f = Impure ((>>= f) <$> i)

    liftF :: Functor f => f a -> Free f a
    liftF command = Impure (Pure <$> command)

    interpret :: (Functor f, Monad m) => (forall x. f x -> m x) -> Free f a -> m a
    interpret nt = go
      where
        go = \case
          Pure a    -> pure a
          Impure fx -> nt fx >>= go

-----------------------------------------------------------------------------

-> Free monad: operacja specificzna dla Restartable <-
======================================================

Spróbujmy zdefiniować nasz typ *RestartableF*:

    type Restartable = Free RestartableF

    data RestartableF a where
      Step :: Persistent a => String -> IO a -> Restartable a

    instance Functor RestartableF where
      fmap f = ???

Instancja *Monad* dla *Free f* wymaga, żeby *f* było funktorem,
ale nie możemy zrobić funktora z *RestartableF* ze względu na
constraint *Persistent a*.

Nie da się?

-----------------------------------------------------------------------------

-> Make functor from any type with this one weird trick <-
==========================================================

Powtórzmy ten sam trick: "złapmy" funkcję, jaka została zaaplikowane przez *fmap*:

    data RestartableF next where
      Step :: Persistent a => String -> IO a -> (a -> next) -> Restartable next

    instance Functor RestartableF where
      fmap g (Step name act f) = Step name act (g . f)

    step :: Persistent a => String -> IO a -> Restartable a
    step name act = liftF $ Step name act id

Trick ten ma uczoną nazwę co-Yoneda. Jest to wariant continuation passing style
(zauważmy, jak funkcja *step* ustawia pustą kontynuację *id*).

-----------------------------------------------------------------------------

-> Free monad: interpreter <-
=============================

Zauważmy, że *go* nie jest rekurencyjna:

    runRestartable :: forall a . FilePath -> Restartable a -> IO a
    runRestartable path restartable = withFile path ReadWriteMode run
      where
        run :: Handle -> IO a
        run handle = interpret go restartable
          where
            go :: RestartableF b -> IO b
            go = \case
              Step name act cont -> do
                maybeA <- restore handle
                case maybeA of
                  Just a -> do
                    putStrLn $ "step " <> name <> " already completed"
                    pure $ cont a
                  Nothing -> do
                    putStrLn $ "running step " <> name
                    a <- act
                    save handle a
                    pure $ cont a

-----------------------------------------------------------------------------

-> Freer monad <-
=================

Możemy trick z co-Yoneda wbudować w nasz typ *Free(r)*. Dodatkowe korzyści:
- pozbywamy się wymagania, że nasze *f* musi być funktorem
- pozbywamy sie continuation passing style w naszym kodzie

    data Freer f a where
      Pure   :: a -> Freer f a
      Impure :: f x -> (x -> Freer f a) -> Freer f a

    instance Monad (Freer f) where
      (Pure a)      >>= g = g a
      (Impure fx f) >>= g = Impure fx (f >=> g)

    liftF :: f a -> Freer f a
    liftF command = Impure command Pure

    interpret :: Monad m => (forall x. f x -> m x) -> Freer f a -> m a
    interpret nt = go
      where
        go = \case
          Pure a         -> pure a
          Impure fx cont -> nt fx >>= (go . cont)

-----------------------------------------------------------------------------

-> Freer monad: operacje specyficzne dla Restartable <-
=======================================================

*RestartableF* nie musi już być funktorem i pozbywamy kontunuacji z naszego kodu:

    data RestartableF a where
      Step :: Persistent a => String -> IO a -> RestartableF a

    type Restartable = Freer RestartableF

    step :: Persistent a => String -> IO a -> Restartable a
    step name act = liftF $ Step name act

-----------------------------------------------------------------------------

-> Freer monad: interpreter <-
==============================

    runRestartable :: forall a . FilePath -> Restartable a -> IO a
    runRestartable path restartable = withFile path ReadWriteMode run
      where
        run :: Handle -> IO a
        run handle = interpret go restartable
          where
            go :: RestartableF b -> IO b
            go = \case
              Step name act -> do
                maybeA <- restore handle
                case maybeA of
                  Just a -> do
                    putStrLn $ "step " <> name <> " already completed"
                    pure a
                  Nothing -> do
                    putStrLn $ "running step " <> name
                    a <- act
                    save handle a
                    pure a


-----------------------------------------------------------------------------

-> Free monad: analogia z listą / drzewem <-
============================================

Free monad jest trochę podobny do listy:

    data List a = Nil | Cons a (List a)

Zamieńmy 2 argumenty do *Cons* na jedną parę:

    data List a = Nil | Cons (a, List a)

*(a,)* jest funktorem: sparametryzujmy go:

    data ListF f a = Nil | Cons (f (ListF f a))
    type List a = ListF (a,) a

*ListF* jest już bardzo podobne do *Free*. Można więc o *Free* myśleć jak
o liście lub lepiej drzewie. Wartości *Pure* znajdują się w liściach.
Węzły mają taki stopień rozgałęzienia, ile "dziurek" zawiera znajdujący się
w nim konstruktor *f*. *ma >>= f* podmienia wszystkie liście w *ma*
na wyniki aplikacji *f*.

-----------------------------------------------------------------------------

-> Free monad: problemy z wydajnością <-
========================================

Ze względu na "drzewopodobność" *Free*, złożoność pojednynczej
operacji *>>=* jest liniowa ze względu na rozmiar programu
(analogicznie do dokładania elementu na końcu listy), a konstrukcja całego programu
ma złożoność kwadratową.

-----------------------------------------------------------------------------

-> Dygresja: Church encoding <-
===============================

Każdą strukturę danych można zakodować jako funkcję:
- wynikiem funkcji jest dowolny (polimorficzny) typ *r* (Rank2Type)
- funkcja ta ma tyle argumentów, ile jest konstruktorów w typie
- każdy z tych argumentów jest funkcją biorącą tyle argumentów, ile brał konstruktor
- wynikiem każdego z argumentów jest typ *r*
- w szczególności, jeśli konstruktor był bezparametrowy, to argumentem jest
  pojedyncza wartość typu r
- argumenty rekurencyjne w konstruktorach są zastępowane przez typ *r*

-----------------------------------------------------------------------------

-> Dygresja: Church encoding cd. <-
===================================

Przykłady:

    data Bool = False | True
    type ChurchBool = forall r. r -> r -> r
    toChurch :: Bool -> ChurchBool
    toChurch = \case
      False -> \onFalse onTrue -> onFalse
      True  -> \onFalse onTrue -> onTrue
    fromChurch :: ChurchBool -> Bool
    fromChurch f = f False True
    
    data Either a b = Left a | Right b
    type ChurchEither a b = forall r. (a -> r) -> (b -> r) -> r
    toChurch :: Either a b -> ChurchEither a b
    toChurch = \case
      Left a  -> \onLeft onRight -> onLeft a
      Right b -> \onLeft onRight -> onRight b
    fromChurch :: ChurchEither a b -> Either a b
    fromChurch f = f Left Right

    data List a = Cons a (List a) | Nil
    type ChurchList a = forall r. (a -> r -> r) -> r -> r
    toChurch :: List a -> ChurchList a
    toChurch = \case
      Cons a lst -> \onCons onNil -> onCons a ((toChurch lst) onCons onNil)
      Nil        -> \onCons onNil -> onNil
    fromChurch :: ChurchList a -> List a
    fromChurch f = f (:) []

-----------------------------------------------------------------------------

-> Free monad: problemy z wydajnością cd. <-
============================================

Zastosowanie Chuch encodingu do *Free* znacznie podnosi wydajność:

    newtype ChurchFree f a =
      ChurchFree { runChurch :: forall r. (a -> r) -> (f r -> r) -> r }

    instance Monad (ChurchFree f) where
      ChurchFree m >>= f = ChurchFree (\kp kf -> m (\a -> runChurch (f a) kp kf) kf)

-----------------------------------------------------------------------------------

-> Systemy efektów: mtl <-
==========================

Definiujemy efekty jako klasy: *MonadA*, *MonadB*, *MonadC*

Definiujemy transformery dostarczające podstawowe instancje:

    data AT m a = ...
    instance MonadA (AT m) where ...

    data BT m a = ...
    instance MonadB (BT m) where ...

    data CT m a = ...
    instance MonadC (CT m) where ...

Definiujemy instancje "na krzyż":

    instance MonadB m => MonadB AT m where ...
    instance MonadC m => MonadC AT m where ...

    instance MonadA m => MonadA BT m where ...
    instance MonadC m => MonadC BT m where ...

    instance MonadA m => MonadA CT m where ...
    instance MonadB m => MonadB CT m where ...

----------------------------------------------------------------------------

-> Systemy efektów: mtl cd. <-
==============================

Logikę biznesową piszemy "pod interfejs":

    doImportantStuff :: (MonadA m, MonadB m) => Int -> String -> m Double

Kod uruchamiamy ostatecznie pod stosem transformerów:

   newtype AppM a = AppM { runAppM :: AT (BT (CT IO) ) a }
     deriving (MonadA, MonadB, MonadC)

----------------------------------------------------------------------------

-> Systemy efektów: mtl: wady i zalety <-
========================================

Zalety:
- dobra wydajność (podobno)
- obsługa higher order effects
    (np. *catch :: MonadError e m => m a -> (e -> m a) -> m a)

Wady:
- trudność definiowania nowych efektów: kwadratowa liczba instancji
- trudność definiowania nowych interpretacji: stosy transformerów instancje

----------------------------------------------------------------------------

-> Systemy efektów: freer monad <-
==================================

Definiujemy będący unią wszyskich efektów, których zamierzamy użyć:

    data OpenUnion (lst :: [* -> *]) (a :: *) = ...
    inject :: Member f lst => f a -> OpenUnion lst a
    split :: Member f lst => OpenUnion lst a -> Either (f a) (OpenUnion (Remove f lst) a)

Nasze interpretery mogą usuwać efekty z listy lub zamieniać
jedne efekty na liście na inne. Składamy interpretery aż lista efektów będzie pusta,
(wtedy mamy obliczenie pure), lub będzie zawierała jeden trywialny efekt
*newtype Embed m a = Embed (m a)* (wtedy mamy obliczenie w monadzie *m*)

----------------------------------------------------------------------------

-> Systemy efektów: freer monad: zalety i wady <-
=================================================

Systemy efektów to obszar gwałtownego rozwoju w Haskellu.

Chcemy, żeby system efektów:
- pozwalał na łatwe definiowanie nowych efektów i interpreterów (low boilerplate)
- obsługiwał higher order effects
- był wydajny

Na ile mi wiadomo, żadna z obecnie dostępnych bibliotek
(simple-effects, fused-effects, polysemy)
nie spełnia wszystkich 3 wymagań
