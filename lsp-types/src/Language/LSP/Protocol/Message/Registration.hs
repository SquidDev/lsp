{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.LSP.Protocol.Message.Registration where

import Language.LSP.Protocol.Internal.Method
import Data.Kind
import Language.LSP.Protocol.Message.Meta
import Language.LSP.Protocol.Message.Method
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Utils.Misc

import Data.Aeson
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import Prettyprinter

-- | Typed dynamic registration type, with correct options.
data TRegistration (m :: Method ClientToServer t) = TRegistration
  { _id :: Text
  -- ^ The id used to register the request. The id can be used to deregister
  -- the request again.
  , _method :: SClientMethod m
  -- ^ The method / capability to register for.
  , _registerOptions :: !(Maybe (RegistrationOptions m))
  -- ^ Options necessary for the registration.
  -- Make this strict to aid the pattern matching exhaustiveness checker
  }
  deriving stock (Generic)

deriving stock instance Eq (RegistrationOptions m) => Eq (TRegistration m)
deriving stock instance Show (RegistrationOptions m) => Show (TRegistration m)

-- TODO: can we do this generically somehow?
-- This generates the function
-- regHelper :: SMethod m
--           -> (( Show (RegistrationOptions m)
--               , ToJSON (RegistrationOptions m)
--               , FromJSON ($regOptTcon m)
--              => x)
--           -> x
regHelper ::
  forall m_aTrKP x_aTrKQ. SMethod m_aTrKP
                          -> (Show (RegistrationOptions m_aTrKP) =>
                              ToJSON (RegistrationOptions m_aTrKP) =>
                              FromJSON (RegistrationOptions m_aTrKP) => x_aTrKQ)
                             -> x_aTrKQ
regHelper SMethod_TextDocumentImplementation x_aTrJH = x_aTrJH
regHelper SMethod_TextDocumentTypeDefinition x_aTrJI = x_aTrJI
regHelper SMethod_TextDocumentDocumentColor x_aTrJJ = x_aTrJJ
regHelper SMethod_TextDocumentColorPresentation x_aTrJK = x_aTrJK
regHelper SMethod_TextDocumentFoldingRange x_aTrJL = x_aTrJL
regHelper SMethod_TextDocumentDeclaration x_aTrJM = x_aTrJM
regHelper SMethod_TextDocumentSelectionRange x_aTrJN = x_aTrJN
regHelper SMethod_TextDocumentPrepareCallHierarchy x_aTrJO
  = x_aTrJO
regHelper SMethod_CallHierarchyIncomingCalls x_aTrJP = x_aTrJP
regHelper SMethod_CallHierarchyOutgoingCalls x_aTrJQ = x_aTrJQ
regHelper SMethod_TextDocumentSemanticTokensFull x_aTrJR = x_aTrJR
regHelper SMethod_TextDocumentSemanticTokensFullDelta x_aTrJS
  = x_aTrJS
regHelper SMethod_TextDocumentSemanticTokensRange x_aTrJT = x_aTrJT
regHelper SMethod_TextDocumentLinkedEditingRange x_aTrJU = x_aTrJU
regHelper SMethod_WorkspaceWillCreateFiles x_aTrJV = x_aTrJV
regHelper SMethod_WorkspaceWillRenameFiles x_aTrJW = x_aTrJW
regHelper SMethod_WorkspaceWillDeleteFiles x_aTrJX = x_aTrJX
regHelper SMethod_TextDocumentMoniker x_aTrJY = x_aTrJY
regHelper SMethod_TextDocumentPrepareTypeHierarchy x_aTrJZ
  = x_aTrJZ
regHelper SMethod_TypeHierarchySupertypes x_aTrK0 = x_aTrK0
regHelper SMethod_TypeHierarchySubtypes x_aTrK1 = x_aTrK1
regHelper SMethod_TextDocumentInlineValue x_aTrK2 = x_aTrK2
regHelper SMethod_TextDocumentInlayHint x_aTrK3 = x_aTrK3
regHelper SMethod_InlayHintResolve x_aTrK4 = x_aTrK4
regHelper SMethod_TextDocumentDiagnostic x_aTrK5 = x_aTrK5
regHelper SMethod_WorkspaceDiagnostic x_aTrK6 = x_aTrK6
regHelper SMethod_Initialize x_aTrK7 = x_aTrK7
regHelper SMethod_Shutdown x_aTrK8 = x_aTrK8
regHelper SMethod_TextDocumentWillSaveWaitUntil x_aTrK9 = x_aTrK9
regHelper SMethod_TextDocumentCompletion x_aTrKa = x_aTrKa
regHelper SMethod_CompletionItemResolve x_aTrKb = x_aTrKb
regHelper SMethod_TextDocumentHover x_aTrKc = x_aTrKc
regHelper SMethod_TextDocumentSignatureHelp x_aTrKd = x_aTrKd
regHelper SMethod_TextDocumentDefinition x_aTrKe = x_aTrKe
regHelper SMethod_TextDocumentReferences x_aTrKf = x_aTrKf
regHelper SMethod_TextDocumentDocumentHighlight x_aTrKg = x_aTrKg
regHelper SMethod_TextDocumentDocumentSymbol x_aTrKh = x_aTrKh
regHelper SMethod_TextDocumentCodeAction x_aTrKi = x_aTrKi
regHelper SMethod_CodeActionResolve x_aTrKj = x_aTrKj
regHelper SMethod_WorkspaceSymbol x_aTrKk = x_aTrKk
regHelper SMethod_WorkspaceSymbolResolve x_aTrKl = x_aTrKl
regHelper SMethod_TextDocumentCodeLens x_aTrKm = x_aTrKm
regHelper SMethod_CodeLensResolve x_aTrKn = x_aTrKn
regHelper SMethod_TextDocumentDocumentLink x_aTrKo = x_aTrKo
regHelper SMethod_DocumentLinkResolve x_aTrKp = x_aTrKp
regHelper SMethod_TextDocumentFormatting x_aTrKq = x_aTrKq
regHelper SMethod_TextDocumentRangeFormatting x_aTrKr = x_aTrKr
regHelper SMethod_TextDocumentOnTypeFormatting x_aTrKs = x_aTrKs
regHelper SMethod_TextDocumentRename x_aTrKt = x_aTrKt
regHelper SMethod_TextDocumentPrepareRename x_aTrKu = x_aTrKu
regHelper SMethod_WorkspaceExecuteCommand x_aTrKv = x_aTrKv
regHelper SMethod_WorkspaceDidChangeWorkspaceFolders x_aTrKw
  = x_aTrKw
regHelper SMethod_WindowWorkDoneProgressCancel x_aTrKx = x_aTrKx
regHelper SMethod_WorkspaceDidCreateFiles x_aTrKy = x_aTrKy
regHelper SMethod_WorkspaceDidRenameFiles x_aTrKz = x_aTrKz
regHelper SMethod_WorkspaceDidDeleteFiles x_aTrKA = x_aTrKA
regHelper SMethod_NotebookDocumentDidOpen x_aTrKB = x_aTrKB
regHelper SMethod_NotebookDocumentDidChange x_aTrKC = x_aTrKC
regHelper SMethod_NotebookDocumentDidSave x_aTrKD = x_aTrKD
regHelper SMethod_NotebookDocumentDidClose x_aTrKE = x_aTrKE
regHelper SMethod_Initialized x_aTrKF = x_aTrKF
regHelper SMethod_Exit x_aTrKG = x_aTrKG
regHelper SMethod_WorkspaceDidChangeConfiguration x_aTrKH = x_aTrKH
regHelper SMethod_TextDocumentDidOpen x_aTrKI = x_aTrKI
regHelper SMethod_TextDocumentDidChange x_aTrKJ = x_aTrKJ
regHelper SMethod_TextDocumentDidClose x_aTrKK = x_aTrKK
regHelper SMethod_TextDocumentDidSave x_aTrKL = x_aTrKL
regHelper SMethod_TextDocumentWillSave x_aTrKM = x_aTrKM
regHelper SMethod_WorkspaceDidChangeWatchedFiles x_aTrKN = x_aTrKN
regHelper SMethod_SetTrace x_aTrKO = x_aTrKO


instance ToJSON (TRegistration m) where
  toJSON x@(TRegistration _ m _) = regHelper m (genericToJSON lspOptions x)

deriving via ViaJSON (TRegistration m) instance Pretty (TRegistration m)

data SomeRegistration = forall t (m :: Method ClientToServer t). SomeRegistration (TRegistration m)

instance ToJSON SomeRegistration where
  toJSON (SomeRegistration r) = toJSON r

instance FromJSON SomeRegistration where
  parseJSON = withObject "Registration" $ \o -> do
    SomeClientMethod m <- o .: "method"
    r <- TRegistration <$> o .: "id" <*> pure m <*> regHelper m (o .: "registerOptions")
    pure (SomeRegistration r)

instance Show SomeRegistration where
  show (SomeRegistration r@(TRegistration _ m _)) = regHelper m (show r)

deriving via ViaJSON SomeRegistration instance Pretty SomeRegistration

toUntypedRegistration :: TRegistration m -> Registration
toUntypedRegistration (TRegistration i meth opts) = Registration i (T.pack $ someMethodToMethodString $ SomeMethod meth) (Just $ regHelper meth (toJSON opts))

toSomeRegistration :: Registration -> Maybe SomeRegistration
toSomeRegistration r =
  let v = toJSON r
   in case fromJSON v of
        Success r' -> Just r'
        _ -> Nothing

-- ---------------------------------------------------------------------

-- | Typed dynamic unregistration type.
data TUnregistration (m :: Method ClientToServer t) = TUnregistration
  { _id :: Text
  -- ^ The id used to unregister the request or notification. Usually an id
  -- provided during the register request.
  , _method :: SMethod m
  -- ^ The method / capability to unregister for.
  }
  deriving stock (Generic)

deriving stock instance Eq (TUnregistration m)
deriving stock instance Show (TUnregistration m)

instance ToJSON (TUnregistration m) where
  toJSON x@(TUnregistration _ m) = regHelper m (genericToJSON lspOptions x)

deriving via ViaJSON (TUnregistration m) instance Pretty (TUnregistration m)

data SomeUnregistration = forall t (m :: Method ClientToServer t). SomeUnregistration (TUnregistration m)

instance ToJSON SomeUnregistration where
  toJSON (SomeUnregistration r) = toJSON r

instance FromJSON SomeUnregistration where
  parseJSON = withObject "Unregistration" $ \o -> do
    SomeClientMethod m <- o .: "method"
    r <- TUnregistration <$> o .: "id" <*> pure m
    pure (SomeUnregistration r)

deriving via ViaJSON SomeUnregistration instance Pretty SomeUnregistration

toUntypedUnregistration :: TUnregistration m -> Unregistration
toUntypedUnregistration (TUnregistration i meth) = Unregistration i (T.pack $ someMethodToMethodString $ SomeMethod meth)

toSomeUnregistration :: Unregistration -> Maybe SomeUnregistration
toSomeUnregistration r =
  let v = toJSON r
   in case fromJSON v of
        Success r' -> Just r'
        _ -> Nothing
