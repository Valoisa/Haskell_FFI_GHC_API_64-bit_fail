{-# LANGUAGE ForeignFunctionInterface #-}
module Compilation where

-- GHC API
import InteractiveEval (setContext, getNamesInScope, runDecls, compileExpr)
import GHC (Ghc, InteractiveImport(..), Name, runGhc, getSessionDynFlags, setSessionDynFlags, mkModuleName, defaultErrorHandler, noLoc)
import GhcMonad (liftIO)
import DynFlags (DynFlags, HscTarget(..), GhcLink(..), WarningFlag(..), wopt_set, xopt_set, defaultFlushOut, defaultFatalMessager, hscTarget, ghcLink)
import GHC.Hs.ImpExp (IE(..), IEWrappedName(..), ImportDecl(..), simpleImportDecl)
import GHC.Hs.Extension (NoExtField(..))
import RdrName (mkRdrUnqual)
import OccName (mkVarOcc)
import GHC.Paths (libdir)
import GHC.Magic (inline)
import GHC.LanguageExtensions.Type (Extension(..))

-- Foreign Function Interface
import Foreign.C.String (CWString, peekCWString, newCWString)
import Foreign.StablePtr (StablePtr, deRefStablePtr, newStablePtr, freeStablePtr)
import Foreign.Marshal.Alloc (free)

-- Base
import Control.Exception (SomeException(..), AssertionFailed(..), displayException, evaluate, toException, try)
import Control.Exception.Safe (tryAny)
import Control.DeepSeq (NFData(..))
import Control.DeepSeq (($!!))
import Data.List.Split.Internals (splitOn)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (catch)

-- Foreign export declarations
foreign export ccall c_create_context :: IO (StablePtr (Either SomeException (Ghc [Name])))
foreign export ccall c_bring_decls_to_context :: StablePtr (Either SomeException (Ghc [Name])) -> IO (StablePtr (Either SomeException (Ghc [Name])))
foreign export ccall c_execute :: StablePtr (Either SomeException (Ghc a)) 
                                                    -> CWString 
                                                    -> IO (StablePtr (Either SomeException String))

foreign export ccall c_ghc_has_exception :: StablePtr (Either SomeException (Ghc [Name])) -> IO CWString
foreign export ccall c_has_pref_error :: StablePtr (Either SomeException String) -> IO CWString
foreign export ccall c_show_result :: StablePtr (Either SomeException String) -> IO CWString

-- Memory deallocation
foreign export ccall c_free_stable_ptr :: StablePtr a -> IO ()
foreign export ccall c_free_cwstring :: CWString -> IO ()

--  Checks if the previous manipulation with the context caused an exception. 
--  Params: context
ghcHasException :: Either SomeException (Ghc a) -> IO String
ghcHasException res = 
    case res of
        Right ghcmon -> do
            eithIO <- tryEvaluate $ runGhc (Just libdir) ghcmon
            case eithIO of
                Left mod_err -> return $ ("Exception: " ++ (displayException mod_err))
                Right _      -> return ""
        Left err     -> return $ displayException err

--  Checks if function result evaluation caused an exception
hasPrefError :: (Either SomeException String) -> IO String
hasPrefError eith = 
    case eith of
        Left excep -> return $ displayException excep
        Right smth -> do
            res <- safePrefix smth
            case res of
                (xs, Nothing)  -> return ""
                (xs, Just exc) -> return ("Exception: " ++ (displayException exc) ++ ". Partial eval: " ++ xs)

--  Gets out the Right constructor value of the Either monad.
--  Patterns is non-exhaustive, but I apply this function only in case there are no exceptions 
showRigth :: Either SomeException a -> a
showRigth (Right smth) = smth

tryEvaluate = tryAny . evaluate

--  Partially evaluates the result (if there is some exception, but we can't handle it because of the lazy-evaluation
safePrefix :: String -> IO (String, Maybe SomeException)
safePrefix s = do
   r1 <-try (evaluate s)
   case r1 of
      Left exc -> return ("", Just exc)
      Right "" -> return ("", Nothing)
      Right (x:xs) -> do
         r2 <- try (evaluate x)
         case r2 of
            Left exc -> return ("", Just exc)
            Right x' -> do
               (p, exc) <- safePrefix xs
               return (x':p, exc)

--  Creates a context: loads standart modules (Prelude and Control.DeepSeq)
createContext :: Ghc [Name]
createContext = let exts_list = [ScopedTypeVariables, DatatypeContexts, ExplicitForAll]
                    warns_list = [Opt_WarnMissingMethods]
                       in do
                            dflags <- fmap (flip (foldl wopt_set) warns_list) $ fmap (flip (foldl xopt_set) exts_list) getSessionDynFlags
                            setSessionDynFlags $ dflags { hscTarget  = HscInterpreted
                                                        , ghcLink    = LinkInMemory }
                            setContext (map mkModuleImport ["Prelude", "Control.DeepSeq"])
                            getNamesInScope

--  Make a simple import of a module or import specific name
mkModuleImport :: String -> InteractiveImport
mkModuleImport mod_info = let info = splitOn " " mod_info
                          in case (length info) of
                                1 -> IIDecl $ simpleImportDecl $ mkModuleName mod_info
                                _ -> let simple_import = simpleImportDecl $ mkModuleName (head info)
                                         string_names  = tail info
                                         names         = map (noLoc . (IEVar NoExtField) . noLoc . IEName . noLoc . mkRdrUnqual .mkVarOcc) string_names
                                     in IIDecl $ ImportDecl { ideclExt       = ideclExt simple_import
                                                            , ideclSourceSrc = ideclSourceSrc simple_import
                                                            , ideclName      = ideclName simple_import
                                                            , ideclPkgQual   = ideclPkgQual simple_import
                                                            , ideclSource    = ideclSource simple_import
                                                            , ideclSafe      = ideclSafe simple_import
                                                            , ideclQualified = ideclQualified simple_import
                                                            , ideclImplicit  = ideclImplicit simple_import
                                                            , ideclAs        = ideclAs simple_import
                                                            , ideclHiding    = Just (False, noLoc names) }
    
--  Safe verion of createContext
tryCreateContext :: IO (Either SomeException (Ghc [Name]))
tryCreateContext = tryEvaluate createContext 

--  Interpretes the Haskell code and bring the names to the context
bringDeclsToCtxt :: Ghc [Name] -> Ghc [Name]
bringDeclsToCtxt ctxt = ctxt >> do 
                                  runDecls $ "instance Num ()\n\nroot :: String\nroot = \"Hello, world!\"\n"

--  Safe version of bringDeclsToCtxt
tryBringDeclsToCtxt :: Either SomeException (Ghc [Name]) -> IO (Either SomeException (Ghc [Name]))
tryBringDeclsToCtxt ctxt = case ctxt of
                                    (Right ctx) -> tryEvaluate $ bringDeclsToCtxt ctx
                                    (Left exc)  -> return $ Left exc 

--  Takes an expression (String) and compliles it
runExpression :: String -> Ghc b
runExpression exp = do
    dflags <- fmap ((flip xopt_set) RankNTypes) getSessionDynFlags
    setSessionDynFlags dflags
    act <- unsafeCoerce <$> compileExpr exp
    liftIO act

--  Takes a context, which can be created by a function above, and a list of
--  expressions to be compiled
compileExps :: String -> Ghc a -> [String] -> IO b
compileExps mode ctxt xs = defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $ (>>) ctxt $ foldl1 (>>) $ map runExpression
                         $ map (\x -> mode ++ x ++ ")) :: IO String") xs

tryCompileExps :: String -> Either SomeException (Ghc a) -> [String] -> IO (Either SomeException String)
tryCompileExps mode ctxt xs = case ctxt of
                                 (Right ctx) -> (try :: IO a -> IO (Either SomeException a))
                                    $ compileExps mode ctx xs
                                 (Left exc)  -> return $ Left exc 

{-***** Functions to be exported to C++ *****-}
c_create_context :: IO (StablePtr (Either SomeException (Ghc [Name])))
c_create_context = do
    eith <- tryCreateContext
    newStablePtr eith

c_bring_decls_to_context :: StablePtr (Either SomeException (Ghc [Name])) -> IO (StablePtr (Either SomeException (Ghc [Name])))
c_bring_decls_to_context ctxt = do
    cont <- deRefStablePtr ctxt
    eith <- tryBringDeclsToCtxt cont
    newStablePtr eith

c_execute :: StablePtr (Either SomeException (Ghc a)) 
                  -> CWString
                  -> IO (StablePtr (Either SomeException String))
c_execute ctxt str = do
    cont  <- deRefStablePtr ctxt
    exprs <- fmap (splitOn ";") $ peekCWString str
    tuple <- tryCompileExps "(return $ show (" cont exprs
    newStablePtr tuple

c_ghc_has_exception :: StablePtr (Either SomeException (Ghc [Name]))
                   -> IO CWString
c_ghc_has_exception res = do
    eith <- deRefStablePtr res
    err  <- ghcHasException eith
    newCWString err

c_has_pref_error :: StablePtr (Either SomeException String) -> IO CWString
c_has_pref_error res = do
    tuple <- deRefStablePtr res
    err <- hasPrefError tuple
    newCWString err

c_show_result :: StablePtr (Either SomeException String) -> IO CWString
c_show_result res = do
    eith <- deRefStablePtr res
    newCWString $ showRigth eith

c_free_stable_ptr :: StablePtr a -> IO ()
c_free_stable_ptr ptr = freeStablePtr ptr

c_free_cwstring :: CWString -> IO()
c_free_cwstring str = free str