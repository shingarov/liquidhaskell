-- | This module has the code that uses the GHC definitions to:
--   1. MAKE a name-resolution environment,
--   2. USE the environment to translate plain symbols into Var, TyCon, etc. 

module Language.Haskell.Liquid.Bare.Types 
  ( -- * Name resolution environment 
    Env (..)
  , TyThingMap 
  , ModSpecs
  , LocalVars 

    -- * Tycon and Datacon processing environment
  , TycEnv (..) 
  , DataConMap
  , TyConMap

    -- * Signature processing environment 
  , SigEnv (..)

    -- * Measure related environment 
  , MeasEnv (..)

    -- * Misc 
  , PlugTV (..)
  , varRSort 
  , varSortedReft
  ) where 

import qualified Text.PrettyPrint.HughesPJ             as PJ 
import qualified Data.HashSet                          as S
import qualified Data.HashMap.Strict                   as M
import qualified Language.Fixpoint.Types               as F 
import qualified Language.Haskell.Liquid.Measure       as Ms
import qualified Language.Haskell.Liquid.Types.RefType as RT 
import           Language.Haskell.Liquid.Types.Types   
import           Language.Haskell.Liquid.Types.Specs 
import           Language.Haskell.Liquid.GHC.API       as Ghc hiding (Located) 


type ModSpecs = M.HashMap ModName Ms.BareSpec

-------------------------------------------------------------------------------
-- | See [NOTE: Plug-Holes-TyVars] for a rationale for @PlugTV@ 
-------------------------------------------------------------------------------

data PlugTV = HsTV -- ^ Use tyvars from GHC specification 
            | LqTV -- ^ Use tyvars from Liquid specification
            deriving (Show)

instance F.PPrint PlugTV where 
  pprintTidy _ = PJ.text . show 


-------------------------------------------------------------------------------
-- | Name resolution environment 
-------------------------------------------------------------------------------
data Env = RE 
  { reLMap      :: !LogicMap
  , reSyms      :: ![(F.Symbol, Ghc.Var)]    -- ^ see "syms" in old makeGhcSpec'
  , _reSubst    :: !F.Subst                  -- ^ see "su"   in old makeGhcSpec'
  , _reTyThings :: !TyThingMap 
  , reCfg       :: !Config
  , reQualImps  :: !QImports                 -- ^ qualified imports
  , reAllImps   :: !(S.HashSet F.Symbol)     -- ^ all imported modules
  , reLocalVars :: !LocalVars                -- ^ lines at which local variables are defined.
  , reGlobSyms  :: !(S.HashSet F.Symbol)     -- ^ global symbols, typically unlifted measures like 'len', 'fromJust'
  -- , reCbs       :: ![CoreBind]               -- ^ needed to resolve local vars in signatures e.g. tests-names-pos-local02.hs
  , reSrc       :: !GhcSrc                   -- ^ all source info
  }

instance HasConfig Env where 
  getConfig = reCfg 

-- | @LocalVars@ is a map from names to lists of pairs of @Ghc.Var@ and 
--   the lines at which they were defined. 
type LocalVars = M.HashMap F.Symbol [(Int, Ghc.Var)]

-------------------------------------------------------------------------------
-- | A @TyThingMap@ is used to resolve symbols into GHC @TyThing@ and, 
--   from there into Var, TyCon, DataCon, etc.
-------------------------------------------------------------------------------
type TyThingMap = M.HashMap F.Symbol [(F.Symbol, Ghc.TyThing)]

-------------------------------------------------------------------------------
-- | A @SigEnv@ contains the needed to process type signatures 
-------------------------------------------------------------------------------
data SigEnv = SigEnv 
  { sigEmbs       :: !(F.TCEmb Ghc.TyCon) 
  , sigTyRTyMap   :: !(M.HashMap Ghc.TyCon RTyCon)
  , sigExports    :: !Ghc.NameSet
  , sigRTEnv      :: !BareRTEnv
  }

-------------------------------------------------------------------------------
-- | A @TycEnv@ contains the information needed to process Type- and Data- Constructors 
-------------------------------------------------------------------------------
data TycEnv = TycEnv 
  { tcTyCons      :: ![TyConP]
  , tcDataCons    :: ![DataConP]
  , tcSelMeasures :: ![Measure SpecType Ghc.DataCon]
  , tcSelVars     :: ![(Ghc.Var, LocSpecType)]
  , tcTyConMap    :: !TyConMap 
  , tcAdts        :: ![F.DataDecl]
  , tcDataConMap  :: !DataConMap 
  , tcEmbs        :: !(F.TCEmb Ghc.TyCon)
  , tcName        :: !ModName
  }

type TyConMap   = M.HashMap Ghc.TyCon RTyCon
type DataConMap = M.HashMap (F.Symbol, Int) F.Symbol

-------------------------------------------------------------------------------
-- | Intermediate representation for Measure information 
-------------------------------------------------------------------------------
-- REBARE: used to be output of makeGhcSpecCHOP2
data MeasEnv = MeasEnv 
  { meMeasureSpec :: !(MSpec SpecType Ghc.DataCon)          -- measures
  , meClassSyms   :: ![(F.Symbol, Located (RRType F.Reft))] -- cms' 
  , meSyms        :: ![(F.Symbol, Located (RRType F.Reft))] -- ms' 
  , meDataCons    :: ![(Ghc.Var,  LocSpecType)]             -- cs'
                                                            -- xs' :: [Symbol] = fst <$> meSyms
  , meClasses     :: ![DataConP]                            -- cls 
  , meMethods     :: ![(ModName, Ghc.Var, LocSpecType)]     -- mts 
  }

-------------------------------------------------------------------------------
-- | Converting @Var@ to @Sort@
-------------------------------------------------------------------------------
varSortedReft :: F.TCEmb Ghc.TyCon -> Ghc.Var -> F.SortedReft 
varSortedReft emb = RT.rTypeSortedReft emb . varRSort 

varRSort  :: Ghc.Var -> RSort
varRSort  = RT.ofType . Ghc.varType