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
  forall m_aHl6f x_aHl6g. SMethod m_aHl6f
                          -> (Show (RegistrationOptions m_aHl6f) =>
                              ToJSON (RegistrationOptions m_aHl6f) =>
                              FromJSON (RegistrationOptions m_aHl6f) => x_aHl6g)
                             -> x_aHl6g
regHelper SMethod_TextDocumentImplementation x_aHl57 = x_aHl57
regHelper SMethod_TextDocumentTypeDefinition x_aHl58 = x_aHl58
regHelper SMethod_TextDocumentDocumentColor x_aHl59 = x_aHl59
regHelper SMethod_TextDocumentColorPresentation x_aHl5a = x_aHl5a
regHelper SMethod_TextDocumentFoldingRange x_aHl5b = x_aHl5b
regHelper SMethod_TextDocumentDeclaration x_aHl5c = x_aHl5c
regHelper SMethod_TextDocumentSelectionRange x_aHl5d = x_aHl5d
regHelper SMethod_TextDocumentPrepareCallHierarchy x_aHl5e
  = x_aHl5e
regHelper SMethod_CallHierarchyIncomingCalls x_aHl5f = x_aHl5f
regHelper SMethod_CallHierarchyOutgoingCalls x_aHl5g = x_aHl5g
regHelper SMethod_TextDocumentSemanticTokensFull x_aHl5h = x_aHl5h
regHelper SMethod_TextDocumentSemanticTokensFullDelta x_aHl5i
  = x_aHl5i
regHelper SMethod_TextDocumentSemanticTokensRange x_aHl5j = x_aHl5j
regHelper SMethod_TextDocumentLinkedEditingRange x_aHl5k = x_aHl5k
regHelper SMethod_WorkspaceWillCreateFiles x_aHl5l = x_aHl5l
regHelper SMethod_WorkspaceWillRenameFiles x_aHl5m = x_aHl5m
regHelper SMethod_WorkspaceWillDeleteFiles x_aHl5n = x_aHl5n
regHelper SMethod_TextDocumentMoniker x_aHl5o = x_aHl5o
regHelper SMethod_TextDocumentPrepareTypeHierarchy x_aHl5p
  = x_aHl5p
regHelper SMethod_TypeHierarchySupertypes x_aHl5q = x_aHl5q
regHelper SMethod_TypeHierarchySubtypes x_aHl5r = x_aHl5r
regHelper SMethod_TextDocumentInlineValue x_aHl5s = x_aHl5s
regHelper SMethod_TextDocumentInlayHint x_aHl5t = x_aHl5t
regHelper SMethod_InlayHintResolve x_aHl5u = x_aHl5u
regHelper SMethod_TextDocumentDiagnostic x_aHl5v = x_aHl5v
regHelper SMethod_WorkspaceDiagnostic x_aHl5w = x_aHl5w
regHelper SMethod_Initialize x_aHl5x = x_aHl5x
regHelper SMethod_Shutdown x_aHl5y = x_aHl5y
regHelper SMethod_TextDocumentWillSaveWaitUntil x_aHl5z = x_aHl5z
regHelper SMethod_TextDocumentCompletion x_aHl5A = x_aHl5A
regHelper SMethod_CompletionItemResolve x_aHl5B = x_aHl5B
regHelper SMethod_TextDocumentHover x_aHl5C = x_aHl5C
regHelper SMethod_TextDocumentSignatureHelp x_aHl5D = x_aHl5D
regHelper SMethod_TextDocumentDefinition x_aHl5E = x_aHl5E
regHelper SMethod_TextDocumentReferences x_aHl5F = x_aHl5F
regHelper SMethod_TextDocumentDocumentHighlight x_aHl5G = x_aHl5G
regHelper SMethod_TextDocumentDocumentSymbol x_aHl5H = x_aHl5H
regHelper SMethod_TextDocumentCodeAction x_aHl5I = x_aHl5I
regHelper SMethod_CodeActionResolve x_aHl5J = x_aHl5J
regHelper SMethod_WorkspaceSymbol x_aHl5K = x_aHl5K
regHelper SMethod_WorkspaceSymbolResolve x_aHl5L = x_aHl5L
regHelper SMethod_TextDocumentCodeLens x_aHl5M = x_aHl5M
regHelper SMethod_CodeLensResolve x_aHl5N = x_aHl5N
regHelper SMethod_TextDocumentDocumentLink x_aHl5O = x_aHl5O
regHelper SMethod_DocumentLinkResolve x_aHl5P = x_aHl5P
regHelper SMethod_TextDocumentFormatting x_aHl5Q = x_aHl5Q
regHelper SMethod_TextDocumentRangeFormatting x_aHl5R = x_aHl5R
regHelper SMethod_TextDocumentOnTypeFormatting x_aHl5S = x_aHl5S
regHelper SMethod_TextDocumentRename x_aHl5T = x_aHl5T
regHelper SMethod_TextDocumentPrepareRename x_aHl5U = x_aHl5U
regHelper SMethod_WorkspaceExecuteCommand x_aHl5V = x_aHl5V
regHelper SMethod_WorkspaceDidChangeWorkspaceFolders x_aHl5W
  = x_aHl5W
regHelper SMethod_WindowWorkDoneProgressCancel x_aHl5X = x_aHl5X
regHelper SMethod_WorkspaceDidCreateFiles x_aHl5Y = x_aHl5Y
regHelper SMethod_WorkspaceDidRenameFiles x_aHl5Z = x_aHl5Z
regHelper SMethod_WorkspaceDidDeleteFiles x_aHl60 = x_aHl60
regHelper SMethod_NotebookDocumentDidOpen x_aHl61 = x_aHl61
regHelper SMethod_NotebookDocumentDidChange x_aHl62 = x_aHl62
regHelper SMethod_NotebookDocumentDidSave x_aHl63 = x_aHl63
regHelper SMethod_NotebookDocumentDidClose x_aHl64 = x_aHl64
regHelper SMethod_Initialized x_aHl65 = x_aHl65
regHelper SMethod_Exit x_aHl66 = x_aHl66
regHelper SMethod_WorkspaceDidChangeConfiguration x_aHl67 = x_aHl67
regHelper SMethod_TextDocumentDidOpen x_aHl68 = x_aHl68
regHelper SMethod_TextDocumentDidChange x_aHl69 = x_aHl69
regHelper SMethod_TextDocumentDidClose x_aHl6a = x_aHl6a
regHelper SMethod_TextDocumentDidSave x_aHl6b = x_aHl6b
regHelper SMethod_TextDocumentWillSave x_aHl6c = x_aHl6c
regHelper SMethod_WorkspaceDidChangeWatchedFiles x_aHl6d = x_aHl6d
regHelper SMethod_SetTrace x_aHl6e = x_aHl6e


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
