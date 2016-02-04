{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
module IRTS.CodegenJava (codegenJava) where

import           Debug.Trace
import           IRTS.CodegenCommon
import           IRTS.Java.ASTBuilding
import           IRTS.Java.JTypes
import           IRTS.Java.Mangling
import           IRTS.Java.Pom (pomString)
import           IRTS.Lang
import           IRTS.Simplified
import           IRTS.System
import           Idris.Core.TT hiding (mkApp)
import           Util.System

import           Control.Applicative hiding (Const)
import           Control.Arrow
import           Control.Monad
import           Control.Monad.Except
import qualified Control.Monad.Trans as T
import           Control.Monad.Trans.State
import           Data.List (foldl', isSuffixOf)
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector.Unboxed as V
import           Data.Maybe
import           Language.Java.Parser
import           Language.Java.Pretty
import           Language.Java.Syntax hiding (Name)
import qualified Language.Java.Syntax as J
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process


-----------------------------------------------------------------------
-- Main function
codegenJava :: CodeGenerator
codegenJava cg = codegenJava' [] (simpleDecls cg) (outputFile cg)
                                 (includes cg) (compileLibs cg)
                                 (outputType cg)

codegenJava' :: [(Name, SExp)] -> -- initialization of globals
                [(Name, SDecl)] -> -- decls
                FilePath -> -- output file name
                [String] -> -- headers
                [String] -> -- libs
                OutputType ->
                IO ()
codegenJava' globalInit defs out hdrs libs exec = do
  withTgtDir exec out (codegenJava' exec)
  where
    codegenJava' :: OutputType -> FilePath -> IO ()
    codegenJava' Raw tgtDir = do
        srcDir <- prepareSrcDir exec tgtDir
        generateJavaFile globalInit defs hdrs srcDir out
    codegenJava' MavenProject tgtDir = do
      codegenJava' Raw tgtDir
      generatePom tgtDir out libs
    codegenJava' Object tgtDir = do
      codegenJava' MavenProject tgtDir
      invokeMvn tgtDir "compile"
      copyClassFiles tgtDir out
      cleanUpTmp tgtDir
    codegenJava' Executable  tgtDir = do
        codegenJava' MavenProject tgtDir
        invokeMvn tgtDir "package";
        copyJar tgtDir out
        makeJarExecutable out
        cleanUpTmp tgtDir

-----------------------------------------------------------------------
-- Compiler IO

withTgtDir :: OutputType -> FilePath -> (FilePath -> IO ()) -> IO ()
withTgtDir Raw out action = action (dropFileName out)
withTgtDir MavenProject out action = createDirectoryIfMissing False out >> action out
withTgtDir _ out action = withTempdir (takeBaseName out) action

prepareSrcDir :: OutputType -> FilePath -> IO FilePath
prepareSrcDir Raw tgtDir = return tgtDir
prepareSrcDir _ tgtDir = do
  let srcDir = (tgtDir </> "src" </> "main" </> "java")
  createDirectoryIfMissing True srcDir
  return srcDir

javaFileName :: FilePath -> FilePath -> FilePath
javaFileName srcDir out =
  either error (\ (Ident clsName) -> srcDir </> clsName <.> "java") (mkClassName out)

generateJavaFile :: [(Name, SExp)] -> -- initialization of globals
                    [(Name, SDecl)] -> -- definitions
                    [String] -> -- headers
                    FilePath -> -- Source dir
                    FilePath -> -- output target
                    IO ()
generateJavaFile globalInit defs hdrs srcDir out = do
    let code = either error
                      (prettyPrint)-- flatIndent . prettyPrint)
                      (evalStateT (mkCompilationUnit globalInit defs hdrs out) mkCodeGenEnv)
    writeFile (javaFileName srcDir out) code

pomFileName :: FilePath -> FilePath
pomFileName tgtDir = tgtDir </> "pom.xml"

generatePom :: FilePath -> -- tgt dir
               FilePath -> -- output target
               [String] -> -- libs
               IO ()
generatePom tgtDir out libs =
  do writeFile (pomFileName tgtDir) execPom
--     writeFile (pomFileName "/Users/br-gaster/dev/") execPom
  where
    (Ident clsName) = either error id (mkClassName out)
    execPom = pomString clsName (takeBaseName out) libs


invokeMvn :: FilePath -> String -> IO ()
invokeMvn tgtDir command = do
   mvnCmd <- getMvn
   let args = ["-f", pomFileName tgtDir]
   (exit, mvout, err) <- readProcessWithExitCode mvnCmd (args ++ [command]) ""
   when (exit /= ExitSuccess) $
     error ("FAILURE: " ++ mvnCmd ++ " " ++ command ++ "\n" ++ err ++ mvout)

classFileDir :: FilePath -> FilePath
classFileDir tgtDir = tgtDir </> "target" </> "classes"

copyClassFiles :: FilePath -> FilePath -> IO ()
copyClassFiles tgtDir out = do
  classFiles <- map (\ clsFile -> classFileDir tgtDir </> clsFile)
                . filter ((".class" ==) . takeExtension)
                <$> getDirectoryContents (classFileDir tgtDir)
  mapM_ (\ clsFile -> copyFile clsFile (takeDirectory out </> takeFileName clsFile)) classFiles

jarFileName :: FilePath -> FilePath -> FilePath
jarFileName tgtDir out = tgtDir </> "target" </> (takeBaseName out) <.> "jar"

copyJar :: FilePath -> FilePath -> IO ()
copyJar tgtDir out =
  copyFile (jarFileName tgtDir out) out

makeJarExecutable :: FilePath -> IO ()
makeJarExecutable out = do
  handle <- openBinaryFile out ReadMode
  contents <- TIO.hGetContents handle
  hClose handle
  handle <- openBinaryFile out WriteMode
  TIO.hPutStr handle (T.append (T.pack jarHeader) contents)
  hFlush handle
  hClose handle
  perms <- getPermissions out
  setPermissions out (setOwnerExecutable True perms)

removePom :: FilePath -> IO ()
removePom tgtDir = removeFile (pomFileName tgtDir)

cleanUpTmp :: FilePath -> IO ()
cleanUpTmp tgtDir = do
  invokeMvn tgtDir "clean"
  removePom tgtDir

-----------------------------------------------------------------------
-- Jar and Pom infrastructure

jarHeader :: String
jarHeader =
  "#!/usr/bin/env sh\n"
  ++ "MYSELF=`which \"$0\" 2>/dev/null`\n"
  ++ "[ $? -gt 0 -a -f \"$0\" ] && MYSELF=\"./$0\"\n"
  ++ "java=java\n"
  ++ "if test -n \"$JAVA_HOME\"; then\n"
  ++ "  java=\"$JAVA_HOME/bin/java\"\n"
  ++ "fi\n"
  ++ "exec \"$java\" $java_args -jar $MYSELF \"$@\"\n"
  ++ "exit 1\n"

-----------------------------------------------------------------------
-- Code generation environment

data CodeGenerationEnv
  = CodeGenerationEnv
  { globalVariables :: [(Name, ArrayIndex)]
  , localVariables  :: [[(Int, Ident)]]
  , localVarCounter :: Int
  }

type CodeGeneration = StateT (CodeGenerationEnv) (Either String)

mkCodeGenEnv :: CodeGenerationEnv
mkCodeGenEnv = CodeGenerationEnv [] [] 0

varPos :: LVar -> CodeGeneration (Either ArrayIndex Ident)
varPos (Loc i) = do
  vars <- (concat . localVariables) <$> get
  case lookup i vars of
    (Just varName) -> return (Right varName)
    Nothing -> throwError $ "Invalid local variable id: " ++ show i
varPos (Glob name) = do
  vars <- globalVariables <$> get
  case lookup name vars of
    (Just varIdx) -> return (Left varIdx)
    Nothing -> throwError $ "Invalid global variable id: " ++ show name

pushScope :: CodeGeneration ()
pushScope =
  modify (\ env -> env { localVariables = []:(localVariables env) })

popScope :: CodeGeneration ()
popScope = do
  env <- get
  let lVars = tail $ localVariables env
  let vC = if null lVars then 0 else localVarCounter env
  put $ env { localVariables = tail (localVariables env)
            , localVarCounter = vC }

setVariable :: LVar -> CodeGeneration (Either ArrayIndex Ident)
setVariable (Loc i) = do
  env <- get
  let lVars = localVariables env
  let getter = localVar $ localVarCounter env
  let lVars' = ((i, getter) : head lVars) : tail lVars
  put $ env { localVariables = lVars'
            , localVarCounter = 1 + localVarCounter env}
  return (Right getter)
setVariable (Glob n) = do
  env <- get
  let gVars = globalVariables env
  let getter = globalContext @! length gVars
  let gVars' = (n, getter):gVars
  put (env { globalVariables = gVars' })
  return (Left getter)

pushParams :: [Ident] -> CodeGeneration ()
pushParams paramNames =
  let varMap = zipWith (flip (,)) paramNames [0..] in
  modify (\ env -> env { localVariables = varMap:(localVariables env)
                       , localVarCounter = (length varMap) + (localVarCounter env) })

flatIndent :: String -> String
flatIndent (' ' : ' ' : xs) = flatIndent xs
flatIndent (x:xs) = x:flatIndent xs
flatIndent [] = []

-----------------------------------------------------------------------
-- Maintaining control structures over code blocks

data BlockPostprocessor
  = BlockPostprocessor
  { ppInnerBlock :: [BlockStmt] -> Exp -> CodeGeneration [BlockStmt]
  , ppOuterBlock :: [BlockStmt] -> CodeGeneration [BlockStmt]
  }

ppExp :: BlockPostprocessor -> Exp -> CodeGeneration [BlockStmt]
ppExp pp exp = ((ppInnerBlock pp) [] exp) >>= ppOuterBlock pp

addReturn :: BlockPostprocessor
addReturn =
  BlockPostprocessor
  { ppInnerBlock = (\ block exp -> return $ block ++ [jReturn exp])
  , ppOuterBlock = return
  }

ignoreResult :: BlockPostprocessor
ignoreResult =
  BlockPostprocessor
  { ppInnerBlock = (\ block exp -> return block)
  , ppOuterBlock = return
  }

ignoreOuter :: BlockPostprocessor -> BlockPostprocessor
ignoreOuter pp = pp { ppOuterBlock = return }

throwRuntimeException :: BlockPostprocessor -> BlockPostprocessor
throwRuntimeException pp =
  pp
  { ppInnerBlock =
       (\ blk exp -> return $
         blk ++ [ BlockStmt $ Throw
                    ( InstanceCreation
                      []
                      (toClassType runtimeExceptionType)
                      [exp]
                      Nothing
                    )
                ]
       )
  }


rethrowAsRuntimeException :: BlockPostprocessor -> BlockPostprocessor
rethrowAsRuntimeException pp =
  pp
  { ppOuterBlock =
      (\ blk -> do
          ex <- ppInnerBlock (throwRuntimeException pp) [] (ExpName $ J.Name [Ident "ex"])
          ppOuterBlock pp
            $ [ BlockStmt $ Try
                  (Block blk)
                  [Catch (FormalParam [] exceptionType False (VarId (Ident "ex"))) $
                    Block ex
                  ]
                  Nothing
              ]
      )
  }

-----------------------------------------------------------------------
-- File structure

mkCompilationUnit :: [(Name, SExp)] -> [(Name, SDecl)] -> [String] -> FilePath -> CodeGeneration CompilationUnit
mkCompilationUnit globalInit defs hdrs out = do
  clsName <- mkClassName out
  CompilationUnit Nothing ( [ ImportDecl False idrisRts True
                            , ImportDecl True idrisPrelude True
                            , ImportDecl False bigInteger False
                            , ImportDecl False runtimeException False
                            , ImportDecl False byteBuffer False
                            ] ++ otherHdrs
                          )
                           <$> mkTypeDecl clsName globalInit defs
  where
    idrisRts = J.Name $ map Ident ["org", "idris", "rts"]
    idrisPrelude = J.Name $ map Ident ["org", "idris", "rts", "Prelude"]
    bigInteger = J.Name $ map Ident ["java", "math", "BigInteger"]
    runtimeException = J.Name $ map Ident ["java", "lang", "RuntimeException"]
    byteBuffer = J.Name $ map Ident ["java", "nio", "ByteBuffer"]
    otherHdrs = map ( (\ name -> ImportDecl False name False)
                      . J.Name
                      . map (Ident . T.unpack)
                      . T.splitOn (T.pack ".")
                      . T.pack)
                $ filter (not . isSuffixOf ".h") hdrs

-----------------------------------------------------------------------
-- Main class

invertNamespace :: (Name, SDecl) -> (Name, SDecl)
invertNamespace ((NS name nss),decl) = ((NS name (reverse nss)),decl)
invertNamespace (n,decl) = (n,decl)

mkTypeDecl :: Ident -> [(Name, SExp)] -> [(Name, SDecl)] -> CodeGeneration [TypeDecl]
mkTypeDecl name globalInit defs =
  (\ body -> [ClassTypeDecl $ ClassDecl [ Public
                                        ,  Annotation $ SingleElementAnnotation
                                           (jName "SuppressWarnings")
                                           (EVVal . InitExp $ jString "unchecked")
                                        ]
              name
              []
              Nothing
              []
              body])
  <$> mkRootClass globalInit (collectDecls (map invertNamespace defs))

-- mkClassBody :: [(Name, SExp)] -> [(Name, SDecl)] -> CodeGeneration ClassBody
-- mkClassBody globalInit defs =
--   (\ globals defs -> ClassBody . (globals++) . addMainMethod {- .  mergeInnerClasses -} $ defs)
--   <$> mkGlobalContext globalInit
--   <*> mapM mkDecl defs

mkGlobalContext :: [(Name, SExp)] -> CodeGeneration [Decl]
mkGlobalContext [] = return []
mkGlobalContext initExps = do
  pushScope
  varInit <-
    mapM (\ (name, exp) -> do
           pos <- setVariable (Glob name)
           mkUpdate ignoreResult (Glob name) exp
         ) initExps
  popScope
  return [ MemberDecl $ FieldDecl [Private, Static, Final]
                                      (array objectType)
                                      [ VarDecl (VarId $ globalContextID). Just . InitExp
                                        $ ArrayCreate objectType [jInt $ length initExps] 0
                                      ]
         , InitDecl True (Block $ concat varInit)
         ]

addMainMethod :: [Decl] -> [Decl]
addMainMethod decls
  | findMainMethod decls = mkMainMethod : decls
  | otherwise = decls
  where
    findMainMethod ((MemberDecl (MethodDecl _ _ _ name [] _ _)):_)
      | name == mangle' (sMN 0 "runMain") = True
    findMainMethod (_:decls) = findMainMethod decls
    findMainMethod [] = False

mkMainMethod :: Decl
mkMainMethod =
  simpleMethod
    [Public, Static]
    Nothing
    "main"
    [FormalParam [] (array stringType) False (VarId $ Ident "args")]
    $ Block [ BlockStmt . ExpStmt
              $ call "idris_initArgs" [ jConst "args" ]
            , BlockStmt . ExpStmt $ call (mangle' (sMN 0 "runMain")) []
            ]

data NamespaceClass = NamespaceClass [SDecl] (Map.Map T.Text NamespaceClass)

data RootClass = RootClass NamespaceClass (Maybe SDecl) (Maybe SDecl) Bool

collectDecls :: [(Name, SDecl)] -> RootClass
collectDecls = foldl' accumDecl (RootClass (NamespaceClass [] Map.empty) Nothing Nothing False)
  where
    accumDecl :: RootClass -> (Name,SDecl) -> RootClass
    accumDecl (RootClass nsCls apply eval haveMain) (MN 0 (T.unpack-> "APPLY"),decl) =
      RootClass nsCls (Just decl) eval haveMain
    accumDecl (RootClass nsCls apply eval haveMain) (MN 0 (T.unpack-> "EVAL"),decl) =
      RootClass nsCls apply (Just decl) haveMain
    accumDecl (RootClass nsCls apply eval haveMain) pair@(MN 0 (T.unpack-> "runMain"),decl) =
      RootClass (insertDecl nsCls pair) apply eval True
    accumDecl (RootClass nsCls apply eval haveMain) pair =
      RootClass (insertDecl nsCls pair) apply eval haveMain

    insertDecl :: NamespaceClass -> (Name,SDecl) -> NamespaceClass
    insertDecl (NamespaceClass decls nsMap) ((NS name (ns:nss)),decl) =
      let nsCls = Map.findWithDefault (NamespaceClass [] Map.empty) ns nsMap
          nsCls' = insertDecl nsCls (NS name nss,decl)
      in (NamespaceClass decls (Map.insert ns nsCls' nsMap))
    insertDecl (NamespaceClass decls nsMap) (_,decl) =
      NamespaceClass (decl:decls) nsMap

mkInnerClass :: (T.Text, NamespaceClass) -> CodeGeneration Decl
mkInnerClass (ns,nsCls) =
  (\body -> (MemberDecl (MemberClassDecl (ClassDecl [Public, Static] (Ident (T.unpack ns)) [] Nothing [] body))))
  <$> mkNamespaceClass nsCls

mkNamespaceClass :: NamespaceClass -> CodeGeneration ClassBody
mkNamespaceClass (NamespaceClass decls nsMap) =
  (\decls decls' -> ClassBody (decls ++ decls'))
  <$> mapM mkDecl decls
  <*> mapM mkInnerClass (Map.toList nsMap)

concatMapM  :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs  = liftM concat (mapM f xs)



rangeList  :: [a] -> [Int]
rangeList xs =
  rangeList' 0 xs
    where
    rangeList'  :: Int -> [a] -> [Int]
    rangeList' _ [] = []
    rangeList' i (x:xs) = i : rangeList' (i + 1) xs

mkApplyEval :: String -> SDecl -> CodeGeneration [Decl]
mkApplyEval prefix (SFun name fparams stackSize (SChkCase var alts)) = do
  applyFn <- mkApplyEvalFn
  altFns <- concatMapM mkAltFn alts
  pure (applyFn:altFns)
  where
    altFnName :: Int -> Ident
    altFnName consIndex = Ident (prefix ++ "_0$" ++ show consIndex)

    mkAltFn :: SAlt -> CodeGeneration [Decl]
    mkAltFn cs@(SConCase parentStackPos consIndex _ params branchExpression) = do
        methodParams <- mapM mkFormalParam fparams
        paramNames <- mapM mangle fparams
        pushParams paramNames
        pushScope
        caseBranch <- mkCaseBinding addReturn var parentStackPos params branchExpression
        popScope
        popScope
        pure
         [MemberDecl
          (MethodDecl [Public,Static] [] (Just objectType)
           (altFnName consIndex)
          methodParams [] (MethodBody (Just (Block caseBranch))))]
    mkAltFn _ = pure []

    mkApplyEvalFn :: CodeGeneration Decl
    mkApplyEvalFn = do
      mname <- mangle name
      methodParams <- mapM mkFormalParam fparams
      paramNames <- mapM mangle fparams
      pushParams paramNames
      switchExp <- mkGetConstructorId False var
      caseCalls <- mapM mkCaseCall alts
      popScope
      pure (MemberDecl
              (MethodDecl [Public,Static] [] (Just objectType)
              mname
              methodParams []
              (MethodBody
                  (Just
                  (Block [BlockStmt $ Switch switchExp caseCalls])))))
            where
                mkArg :: Int -> CodeGeneration Argument
                mkArg i = Nothing <>@! Loc i

                mkCaseCall :: SAlt -> CodeGeneration SwitchBlock
                mkCaseCall cs@(SConCase _ consIndex _ _ _) = do
                 args <- mapM mkArg (rangeList fparams)
                 pure
                  (SwitchBlock (SwitchCase $ jInt consIndex)
                      [BlockStmt
                      (Return
                      (Just
                          (MethodInv
                          (MethodCall (J.Name [altFnName consIndex]) args))))])
                mkCaseCall (SDefaultCase exp) = do
                    pushScope
                    stmt <- mkExp addReturn exp
                    popScope
                    pure (SwitchBlock Default stmt)
mkApplyEval _ _ = pure []

mkRootClass :: [(Name, SExp)] -> RootClass -> CodeGeneration ClassBody
mkRootClass globalInit (RootClass (NamespaceClass decls nsMap) mApply mEval haveMain) = do
  globals <- mkGlobalContext globalInit
  decls <- mapM mkDecl decls
  innerClasses <- mapM mkInnerClass (Map.toList nsMap)
  apply <- mapM (mkApplyEval "APPLY") mApply
  eval <- mapM (mkApplyEval "EVAL") mEval
  let main = if haveMain
                then [mkMainMethod]
                else []
  pure (ClassBody
        (globals ++
         (addMainMethod decls) ++
         innerClasses ++
         fromMaybe [] apply ++
         fromMaybe [] eval ++
         main))

mkDecl :: SDecl -> CodeGeneration Decl
mkDecl (SFun name params stackSize body) = do
  (Ident methodName) <- mangle name
  methodParams <- mapM mkFormalParam params
  paramNames <- mapM mangle params
  pushParams paramNames
  methodBody <- mkExp addReturn body
  popScope
  return $
    simpleMethod [Public, Static] (Just objectType) methodName methodParams
      (Block methodBody)

mkFormalParam :: Name -> CodeGeneration FormalParam
mkFormalParam name =
  (\ name -> FormalParam [Final] objectType False (VarId name))
  <$> mangle name

-----------------------------------------------------------------------
-- Expressions

-- | Compile a simple expression and use the given continuation to postprocess
-- the resulting value.
mkExp :: BlockPostprocessor -> SExp -> CodeGeneration [BlockStmt]
-- Variables
mkExp pp (SV var) =
  (Nothing <>@! var) >>= ppExp pp

-- Applications
mkExp pp (SApp pushTail name args) =
  mkApp pushTail name args >>= ppExp pp

-- Bindings
mkExp pp (SLet    var newExp inExp) =
  mkLet pp var newExp inExp
mkExp pp (SUpdate var@(Loc i) newExp) = -- can only update locals
  mkUpdate pp var newExp
mkExp pp (SUpdate var newExp) =
  mkExp pp newExp

-- Objects
mkExp pp (SCon _ conId _ args) =
  mkIdrisObject conId args >>= ppExp pp

-- Case expressions
mkExp pp (SCase up var alts) = mkCase pp True var alts
mkExp pp (SChkCase var alts) = mkCase pp False var alts

-- Projections
mkExp pp (SProj var i) =
  mkProjection var i >>= ppExp pp

-- Constants
mkExp pp (SConst c) =
  ppExp pp $ mkConstant c

-- Foreign function calls
mkExp pp f@(SForeign t fname args) = mkForeign pp t fname args

-- Primitive functions
mkExp pp (SOp LFork [arg]) =
  (mkThread arg) >>= ppExp pp
mkExp pp (SOp LPar [arg]) =
  (Nothing <>@! arg) >>= ppExp pp
mkExp pp (SOp LNoOp args) =
  (Nothing <>@! (last args)) >>= ppExp pp

mkExp pp (SOp op args) =
  (mkPrimitiveFunction op args) >>= ppExp pp

-- Empty expressions
mkExp pp (SNothing) = ppExp pp $ Lit Null

-- Errors
mkExp pp (SError err) = ppExp (throwRuntimeException pp) (jString err)

-----------------------------------------------------------------------
-- Variable access

(<>@!) :: Maybe J.Type -> LVar -> CodeGeneration Exp
(<>@!) Nothing var =
  either ArrayAccess (\ n -> ExpName $ J.Name [n]) <$> varPos var
(<>@!) (Just castTo) var =
  (castTo <>) <$> (Nothing <>@! var)

-----------------------------------------------------------------------
-- Application (wrap method calls in tail call closures)

mkApp :: Bool -> Name -> [LVar] -> CodeGeneration Exp
mkApp False name args =
  (\ methodName params ->
    (idrisClosureType ~> "unwrapTailCall") [call methodName params]
  )
  <$> mangleFull name
  <*> mapM (Nothing <>@!) args
mkApp True name args = mkMethodCallClosure name args

mkMethodCallClosure :: Name -> [LVar] -> CodeGeneration Exp
mkMethodCallClosure name args =
  (\ name args -> closure (call name args))
  <$> mangleFull name
  <*> mapM (Nothing <>@!) args

-----------------------------------------------------------------------
-- Updates (change context array) and Let bindings (Update, execute)

mkUpdate :: BlockPostprocessor -> LVar -> SExp -> CodeGeneration [BlockStmt]
mkUpdate pp var exp =
  mkExp
    ( pp
      { ppInnerBlock =
           (\ blk rhs -> do
               pos <- setVariable var
               vExp <- Nothing <>@! var
               ppInnerBlock pp (blk ++ [pos @:= rhs]) vExp
           )
      }
    ) exp

mkLet :: BlockPostprocessor -> LVar -> SExp -> SExp -> CodeGeneration [BlockStmt]
mkLet pp var@(Loc pos) newExp inExp =
  mkUpdate (pp { ppInnerBlock =
                    (\ blk _ -> do
                        inBlk <- mkExp pp inExp
                        return (blk ++ inBlk)
                    )
               }
           ) var newExp
mkLet _ (Glob _) _ _ = T.lift $ Left "Cannot let bind to global variable"

-----------------------------------------------------------------------
-- Object creation

mkIdrisObject :: Int -> [LVar] -> CodeGeneration Exp
mkIdrisObject conId args =
  (\ args ->
    InstanceCreation [] (toClassType idrisObjectType) ((jInt conId):args) Nothing
  )
  <$> mapM (Nothing <>@!) args

-----------------------------------------------------------------------
-- Case expressions

mkCase :: BlockPostprocessor -> Bool -> LVar -> [SAlt] -> CodeGeneration [BlockStmt]
mkCase pp checked var cases
  | isDefaultOnlyCase cases = mkDefaultMatch pp cases
  | isConstCase cases = do
    ifte <- mkConstMatch (ignoreOuter pp) (\ pp -> mkDefaultMatch pp cases) var cases
    ppOuterBlock pp [BlockStmt ifte]
  | otherwise = do
    switchExp <- mkGetConstructorId checked var
    matchBlocks <- mkConsMatch (ignoreOuter pp) (\ pp -> mkDefaultMatch pp cases) var cases
    ppOuterBlock pp [BlockStmt $ Switch switchExp matchBlocks]

isConstCase :: [SAlt] -> Bool
isConstCase ((SConstCase _ _):_) = True
isConstCase ((SDefaultCase _):cases) = isConstCase cases
isConstCase _ = False

isDefaultOnlyCase :: [SAlt] -> Bool
isDefaultOnlyCase [SDefaultCase _] = True
isDefaultOnlyCase [] = True
isDefaultOnlyCase _ = False

mkDefaultMatch :: BlockPostprocessor -> [SAlt] -> CodeGeneration [BlockStmt]
mkDefaultMatch pp (x@(SDefaultCase branchExpression):_) =
  do pushScope
     stmt <- mkExp pp branchExpression
     popScope
     return stmt
mkDefaultMatch pp (x:xs) = mkDefaultMatch pp xs
mkDefaultMatch pp [] =
  ppExp (throwRuntimeException pp) (jString "Non-exhaustive pattern")

mkMatchConstExp :: LVar -> Const -> CodeGeneration Exp
mkMatchConstExp var c
  | isPrimitive cty =
    (\ var -> (primFnType ~> opName (LEq undefined)) [var, jc] ~==~ jInt 1)
    <$> (Just cty <>@! var)
  | isArray cty =
    (\ var -> (arraysType ~> "equals") [var, jc])
    <$> (Just cty <>@! var)
  | isString cty =
    (\ var -> ((primFnType ~> opName (LStrEq)) [var, jc] ~==~ jInt 1))
    <$> (Just cty <>@! var)
  | otherwise =
    (\ var -> (var ~> "equals") [jc])
    <$> (Just cty <>@! var)
  where
    cty = constType c
    jc = mkConstant c

mkConstMatch :: BlockPostprocessor ->
                (BlockPostprocessor -> CodeGeneration [BlockStmt]) ->
                LVar ->
                [SAlt] ->
                CodeGeneration Stmt
mkConstMatch pp getDefaultStmts var ((SConstCase constant branchExpression):cases) = do
  matchExp <- mkMatchConstExp var constant
  pushScope
  branchBlock <- mkExp pp branchExpression
  popScope
  otherBranches <- mkConstMatch pp getDefaultStmts var cases
  return
    $ IfThenElse matchExp (StmtBlock $ Block branchBlock) otherBranches
mkConstMatch pp getDefaultStmts var (c:cases) = mkConstMatch pp getDefaultStmts var cases
mkConstMatch pp getDefaultStmts _ [] = do
  defaultBlock <- getDefaultStmts pp
  return $ StmtBlock (Block defaultBlock)

mkGetConstructorId :: Bool -> LVar -> CodeGeneration Exp
mkGetConstructorId True var =
  (\ var -> ((idrisObjectType <> var) ~> "getConstructorId") [])
  <$> (Nothing <>@! var)
mkGetConstructorId False var =
  (\ var match ->
    Cond (InstanceOf var (toRefType idrisObjectType)) match (jInt (-1))
  )
  <$> (Nothing <>@! var)
  <*> mkGetConstructorId True var

mkConsMatch :: BlockPostprocessor ->
               (BlockPostprocessor -> CodeGeneration [BlockStmt]) ->
               LVar ->
               [SAlt] ->
               CodeGeneration [SwitchBlock]
mkConsMatch pp getDefaultStmts var ((SConCase parentStackPos consIndex _ params branchExpression):cases) = do
  pushScope
  caseBranch <- mkCaseBinding pp var parentStackPos params branchExpression
  popScope
  otherBranches <- mkConsMatch pp getDefaultStmts var cases
  return $
    (SwitchBlock (SwitchCase $ jInt consIndex) caseBranch):otherBranches
mkConsMatch pp getDefaultStmts var (c:cases)  = mkConsMatch pp getDefaultStmts var cases
mkConsMatch pp getDefaultStmts _ [] = do
  defaultBlock <- getDefaultStmts pp
  return $
    [SwitchBlock Default defaultBlock]

mkCaseBinding :: BlockPostprocessor -> LVar -> Int -> [Name] -> SExp -> CodeGeneration [BlockStmt]
mkCaseBinding pp var stackStart params branchExpression =
  mkExp pp (toLetIn var stackStart params branchExpression)
  where
    toLetIn :: LVar -> Int -> [Name] -> SExp -> SExp
    toLetIn var stackStart members start =
      foldr
        (\ pos inExp -> SLet (Loc (stackStart + pos)) (SProj var pos) inExp)
        start
        [0.. (length members - 1)]

-----------------------------------------------------------------------
-- Projection (retrieve the n-th field of an object)

mkProjection :: LVar -> Int -> CodeGeneration Exp
mkProjection var memberNr =
  (\ var -> ArrayAccess $ ((var ~> "getData") []) @! memberNr)
  <$> (Just idrisObjectType <>@! var)

-----------------------------------------------------------------------
-- Constants

mkConstantArray :: (V.Unbox a) => J.Type -> (a -> Const) -> V.Vector a -> Exp
mkConstantArray cty elemToConst elems =
  ArrayCreateInit
    cty
    0
    (ArrayInit . map (InitExp . mkConstant . elemToConst) $ V.toList elems)

mkConstant :: Const -> Exp
mkConstant c@(I        x) = constType c <> (Lit . Word $ toInteger x)
mkConstant c@(BI       x) = bigInteger (show x)
mkConstant c@(Fl       x) = constType c <> (Lit . Float $ x)
mkConstant c@(Ch       x) = constType c <> (Lit . Char $ x)
mkConstant c@(Str      x) = constType c <> (Lit . String $ x)
mkConstant c@(B8       x) = constType c <> (Lit . Word $ toInteger x)
mkConstant c@(B16      x) = constType c <> (Lit . Word $ toInteger x)
mkConstant c@(B32      x) = constType c <> (Lit . Word $ toInteger x)
mkConstant c@(B64      x) = (bigInteger (show c) ~> "longValue") []
mkConstant c@(AType    x) = ClassLit (Just $ box (constType c))
mkConstant c@(StrType   ) = ClassLit (Just $ stringType)
mkConstant c@(VoidType  ) = ClassLit (Just $ voidType)
mkConstant c@(WorldType ) = ClassLit (Just $ worldType)
mkConstant c@(TheWorld  ) = worldType <> (Lit . Word $ toInteger 0)
mkConstant c@(Forgot    ) = ClassLit (Just $ objectType)

-----------------------------------------------------------------------
-- Foreign function calls

-- TODO: @bgaster add support for calling C (via Java's JNI)
mkForeign :: BlockPostprocessor -> FDesc -> FDesc -> [(FDesc, LVar)] -> CodeGeneration [BlockStmt]
mkForeign pp resTy@(FCon t) fname params
  | isCType t = error ("Java backend does not (currently) support calling C")
  | isJavaType t = mkForeignJava pp resTy fname params
  | otherwise = error (show t ++ " " ++ show fname ++ " " ++ show params)

mkForeign pp resTy@(FApp t args) fname params
  | isCType t = error ("Java backend does not (currently) support calling C")
  | isJavaType t = mkForeignJava pp resTy fname params
  | otherwise =
      error ("mkForeign" ++ show t ++ " " ++ show fname ++ " " ++ show params)

mkForeign pp t fname args =
  error ("mkForeign fall " ++ show t ++ " " ++ show fname ++ " " ++ show args)

mkForeignJava :: BlockPostprocessor ->
                 FDesc ->
                 FDesc ->
                 [(FDesc, LVar)] ->
                 CodeGeneration [BlockStmt]

mkForeignJava pp resTy (FApp (UN (T.unpack -> "JavaInvoke")) [FStr fname]) params = do
  method <- liftParsed (parser name fname)
  args <- foreignVarAccess params
  wrapReturn pp resTy (call method args)

mkForeignJava pp resTy (FCon (UN (T.unpack -> "JavaNew"))) params = do
  clsTy <- liftParsed (parser classType (nameFromReturnType resTy))
  args <- foreignVarAccess params
  wrapReturn pp resTy (InstanceCreation [] clsTy args Nothing)

mkForeignJava pp
              resTy
              (FApp (UN (T.unpack -> "JavaInvokeDyn")) [FStr mname])
              params = do
  method <- liftParsed (parser ident mname)
  (tgt:args) <- foreignVarAccess params
  wrapReturn pp resTy ((tgt ~> (show $ pretty method)) args)

-- The next clause implements the allocation of a class with an
-- overridden method, i.e. using anonymous classes.

-- FIXME: @bgaster ---currently only support a single overloaded
-- method, but it should be straightforward to allow the user to pass
-- an array of overridden methods.

mkForeignJava pp
              resTy
              (FApp (UN (T.unpack -> "JavaNewAnonymous")) [FStr mname])
              ((f,l):params) = do
  clsTy <- liftParsed (parser classType (nameFromReturnType resTy))
  args <- foreignVarAccess params

  -- create method for inner class
  methodResTy <- liftParsed (parser resultType (nameFromReturnType f))

  -- can't be an array location!
  Right var <- varPos l

  let (names, mparams) = unzip $ foldr mkVars [] $ zip [0..] $ paramTypes f
      -- the following is slightly complicated by handling the case with arguments
      -- and without, as one case
      (argName, names') = if null names
                          then ("null", [])
                          else (head names, tail names)
      calls = ((idrisClosureType ~> "unwrapTailCall")
              [call (mangle' (sMN 0 "APPLY"))
                              [((idrisClosureType ~> "unwrapTailCall")
                                [foldl mkCall
                                       (mkCall (ExpName $ J.Name [var]) argName)
                                       (tail names)]), Lit Null]])

      (methodResTy', returnExp) = mkMethodType methodResTy

  callEx <- wrapReturn returnExp methodResTy' calls
  let mbody   = simpleMethod [Public] methodResTy mname mparams (Block callEx)

  wrapReturn pp resTy (InstanceCreation [] clsTy args (Just $ ClassBody [mbody]))
  where
    mkMethodType Nothing = (FCon $ sUN "Java_Unit", ignoreResult)
    mkMethodType _       = (FCon $ sUN "Any", addReturn)

    mkVars (i,t) params = let n = mname ++ show i
                          in (n, FormalParam [Final] t False (VarId $ Ident n)) : params

    mkCall e n = call (mangle' (sMN 0 "APPLY")) [ e, jConst n ]

mkForeignJava pp
              resTy
              (FApp (UN (T.unpack -> "JavaInstanceOf"))
               [fdesc])
              params
  = do
     let mty = foreignType fdesc
     case mty of
       Nothing -> error ("mkFJava " ++ show resTy ++ " " ++ show fdesc ++ " " ++ show params)
       Just ty ->  do
            (tgt:args) <- foreignVarAccess params
            return [BlockStmt (ExpStmt (InstanceOf tgt (toRefType ty)))]

mkForeignJava pp resTy fdesc params =
  error ("mkFJava " ++ show resTy ++ " " ++ show fdesc ++ " " ++ show params)

-- Some helpers
foreignVarAccess = mapM (\(fty, var) -> (foreignType fty <>@! var))

wrapReturn pp (FCon t) exp
  | sUN "Java_Unit" == t = ((ppInnerBlock pp') [BlockStmt $ ExpStmt exp] (Lit Null)) >>= ppOuterBlock pp'
  | otherwise            = ((ppInnerBlock pp') [] exp) >>= ppOuterBlock pp'
  where
    pp' = rethrowAsRuntimeException pp

wrapReturn pp (FApp t args) exp = ((ppInnerBlock pp') [] exp) >>= ppOuterBlock pp'
  where
    pp' = rethrowAsRuntimeException pp

-----------------------------------------------------------------------
-- Primitive functions

mkPrimitiveFunction :: PrimFn -> [LVar] -> CodeGeneration Exp
mkPrimitiveFunction op args =
  (\ args -> (primFnType ~> opName op) (endiannessArguments op ++ args))
  <$> sequence (zipWith (\ a t -> (Just t) <>@! a) args (sourceTypes op))

mkThread :: LVar -> CodeGeneration Exp
mkThread arg =
  (\ closure -> (closure ~> "fork") []) <$> mkMethodCallClosure (sMN 0 "EVAL") [arg]
