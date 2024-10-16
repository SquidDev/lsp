{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.LSP.Protocol.Message.Lens where

import Control.Lens.TH
import qualified Language.LSP.Protocol.Internal.Types.LSPErrorCodes
import qualified Language.LSP.Protocol.Internal.Types.ErrorCodes
import qualified Language.LSP.Protocol.Message.LspId
import qualified Language.LSP.Protocol.Message.Method
import qualified Data.Text.Internal
import qualified Language.LSP.Protocol.Internal.Method
import qualified Language.LSP.Protocol.Types.Common
import qualified Data.Aeson.Types
import qualified Language.LSP.Protocol.Message.Meta
import qualified GHC.Int
import qualified Control.Lens.Type
import Data.Kind
import Language.LSP.Protocol.Message.Registration
import Language.LSP.Protocol.Message.Types
import Language.LSP.Protocol.Types.Lens

instance HasId (TRegistration (m_iTxyL :: Language.LSP.Protocol.Internal.Method.Method 'Language.LSP.Protocol.Message.Meta.ClientToServer t_iTxyK)) Data.Text.Internal.Text where
  {-# INLINE id #-}
  id f_aUISe (TRegistration x1_aUISf x2_aUISg x3_aUISh)
    = fmap
        (\ y1_aUISi -> TRegistration y1_aUISi x2_aUISg x3_aUISh)
        (f_aUISe x1_aUISf)
instance HasMethod (TRegistration (m_iTxyL :: Language.LSP.Protocol.Internal.Method.Method 'Language.LSP.Protocol.Message.Meta.ClientToServer t_iTxyK)) (Language.LSP.Protocol.Message.Method.SClientMethod m_iTxyL) where
  {-# INLINE method #-}
  method f_aUISr (TRegistration x1_aUISs x2_aUISt x3_aUISu)
    = fmap
        (\ y1_aUISv -> TRegistration x1_aUISs y1_aUISv x3_aUISu)
        (f_aUISr x2_aUISt)
instance (a_aUISA
          ~
          Maybe (Language.LSP.Protocol.Internal.Method.RegistrationOptions m_iTxyL)) =>
         HasRegisterOptions (TRegistration (m_iTxyL :: Language.LSP.Protocol.Internal.Method.Method 'Language.LSP.Protocol.Message.Meta.ClientToServer t_iTxyK)) a_aUISA where
  {-# INLINE registerOptions #-}
  registerOptions f_aUISB (TRegistration x1_aUISC x2_aUISD x3_aUISE)
    = fmap
        (\ y1_aUISF -> TRegistration x1_aUISC x2_aUISD y1_aUISF)
        (f_aUISB x3_aUISE)

instance HasId (TUnregistration (m_iTxyH :: Language.LSP.Protocol.Internal.Method.Method 'Language.LSP.Protocol.Message.Meta.ClientToServer t_iTxyG)) Data.Text.Internal.Text where
  {-# INLINE id #-}
  id f_aUIW3 (TUnregistration x1_aUIW4 x2_aUIW5)
    = fmap
        (\ y1_aUIW6 -> TUnregistration y1_aUIW6 x2_aUIW5)
        (f_aUIW3 x1_aUIW4)
instance HasMethod (TUnregistration (m_iTxyH :: Language.LSP.Protocol.Internal.Method.Method 'Language.LSP.Protocol.Message.Meta.ClientToServer t_iTxyG)) (Language.LSP.Protocol.Internal.Method.SMethod m_iTxyH) where
  {-# INLINE method #-}
  method f_aUIWf (TUnregistration x1_aUIWg x2_aUIWh)
    = fmap
        (\ y1_aUIWi -> TUnregistration x1_aUIWg y1_aUIWi)
        (f_aUIWf x2_aUIWh)

instance HasId RequestMessage ((Language.LSP.Protocol.Types.Common.|?) GHC.Int.Int32 Data.Text.Internal.Text) where
  {-# INLINE id #-}
  id f_aUIYk (RequestMessage x1_aUIYl x2_aUIYm x3_aUIYn x4_aUIYo)
    = fmap
        (\ y1_aUIYp -> RequestMessage x1_aUIYl y1_aUIYp x3_aUIYn x4_aUIYo)
        (f_aUIYk x2_aUIYm)
class HasJsonrpc s a | s -> a where
  jsonrpc :: Control.Lens.Type.Lens' s a
instance HasJsonrpc RequestMessage Data.Text.Internal.Text where
  {-# INLINE jsonrpc #-}
  jsonrpc
    f_aUIYq
    (RequestMessage x1_aUIYr x2_aUIYs x3_aUIYt x4_aUIYu)
    = fmap
        (\ y1_aUIYv -> RequestMessage y1_aUIYv x2_aUIYs x3_aUIYt x4_aUIYu)
        (f_aUIYq x1_aUIYr)
instance HasMethod RequestMessage Data.Text.Internal.Text where
  {-# INLINE method #-}
  method f_aUIYw (RequestMessage x1_aUIYx x2_aUIYy x3_aUIYz x4_aUIYA)
    = fmap
        (\ y1_aUIYB -> RequestMessage x1_aUIYx x2_aUIYy y1_aUIYB x4_aUIYA)
        (f_aUIYw x3_aUIYz)
class HasParams s a | s -> a where
  params :: Control.Lens.Type.Lens' s a
instance HasParams RequestMessage (Maybe Data.Aeson.Types.Value) where
  {-# INLINE params #-}
  params f_aUIYG (RequestMessage x1_aUIYH x2_aUIYI x3_aUIYJ x4_aUIYK)
    = fmap
        (\ y1_aUIYL -> RequestMessage x1_aUIYH x2_aUIYI x3_aUIYJ y1_aUIYL)
        (f_aUIYG x4_aUIYK)

class HasError s a | s -> a where
  error :: Control.Lens.Type.Lens' s a
instance HasError ResponseMessage (Maybe ResponseError) where
  {-# INLINE error #-}
  error f_aUJ2j (ResponseMessage x1_aUJ2k x2_aUJ2l x3_aUJ2m x4_aUJ2n)
    = fmap
        (\ y1_aUJ2o -> ResponseMessage x1_aUJ2k x2_aUJ2l x3_aUJ2m y1_aUJ2o)
        (f_aUJ2j x4_aUJ2n)
instance HasId ResponseMessage ((Language.LSP.Protocol.Types.Common.|?) GHC.Int.Int32 ((Language.LSP.Protocol.Types.Common.|?) Data.Text.Internal.Text Language.LSP.Protocol.Types.Common.Null)) where
  {-# INLINE id #-}
  id f_aUJ2F (ResponseMessage x1_aUJ2G x2_aUJ2H x3_aUJ2I x4_aUJ2J)
    = fmap
        (\ y1_aUJ2K -> ResponseMessage x1_aUJ2G y1_aUJ2K x3_aUJ2I x4_aUJ2J)
        (f_aUJ2F x2_aUJ2H)
instance HasJsonrpc ResponseMessage Data.Text.Internal.Text where
  {-# INLINE jsonrpc #-}
  jsonrpc
    f_aUJ2L
    (ResponseMessage x1_aUJ2M x2_aUJ2N x3_aUJ2O x4_aUJ2P)
    = fmap
        (\ y1_aUJ2Q -> ResponseMessage y1_aUJ2Q x2_aUJ2N x3_aUJ2O x4_aUJ2P)
        (f_aUJ2L x1_aUJ2M)
class HasResult s a | s -> a where
  result :: Control.Lens.Type.Lens' s a
instance HasResult ResponseMessage (Maybe Data.Aeson.Types.Value) where
  {-# INLINE result #-}
  result
    f_aUJ2V
    (ResponseMessage x1_aUJ2W x2_aUJ2X x3_aUJ2Y x4_aUJ2Z)
    = fmap
        (\ y1_aUJ30 -> ResponseMessage x1_aUJ2W x2_aUJ2X y1_aUJ30 x4_aUJ2Z)
        (f_aUJ2V x3_aUJ2Y)

instance HasJsonrpc NotificationMessage Data.Text.Internal.Text where
  {-# INLINE jsonrpc #-}
  jsonrpc f_aUJ6u (NotificationMessage x1_aUJ6v x2_aUJ6w x3_aUJ6x)
    = fmap
        (\ y1_aUJ6y -> NotificationMessage y1_aUJ6y x2_aUJ6w x3_aUJ6x)
        (f_aUJ6u x1_aUJ6v)
instance HasMethod NotificationMessage Data.Text.Internal.Text where
  {-# INLINE method #-}
  method f_aUJ6z (NotificationMessage x1_aUJ6A x2_aUJ6B x3_aUJ6C)
    = fmap
        (\ y1_aUJ6D -> NotificationMessage x1_aUJ6A y1_aUJ6D x3_aUJ6C)
        (f_aUJ6z x2_aUJ6B)
instance HasParams NotificationMessage (Maybe Data.Aeson.Types.Value) where
  {-# INLINE params #-}
  params f_aUJ6I (NotificationMessage x1_aUJ6J x2_aUJ6K x3_aUJ6L)
    = fmap
        (\ y1_aUJ6M -> NotificationMessage x1_aUJ6J x2_aUJ6K y1_aUJ6M)
        (f_aUJ6I x3_aUJ6L)

instance HasCode ResponseError ((Language.LSP.Protocol.Types.Common.|?) Language.LSP.Protocol.Internal.Types.LSPErrorCodes.LSPErrorCodes Language.LSP.Protocol.Internal.Types.ErrorCodes.ErrorCodes) where
  {-# INLINE code #-}
  code f_aUJ8L (ResponseError x1_aUJ8M x2_aUJ8N x3_aUJ8O)
    = fmap
        (\ y1_aUJ8P -> ResponseError y1_aUJ8P x2_aUJ8N x3_aUJ8O)
        (f_aUJ8L x1_aUJ8M)
instance HasMessage ResponseError Data.Text.Internal.Text where
  {-# INLINE message #-}
  message f_aUJ8Q (ResponseError x1_aUJ8R x2_aUJ8S x3_aUJ8T)
    = fmap
        (\ y1_aUJ8U -> ResponseError x1_aUJ8R y1_aUJ8U x3_aUJ8T)
        (f_aUJ8Q x2_aUJ8S)
class HasXdata s a | s -> a where
  xdata :: Control.Lens.Type.Lens' s a
instance HasXdata ResponseError (Maybe Data.Aeson.Types.Value) where
  {-# INLINE xdata #-}
  xdata f_aUJ8Z (ResponseError x1_aUJ90 x2_aUJ91 x3_aUJ92)
    = fmap
        (\ y1_aUJ93 -> ResponseError x1_aUJ90 x2_aUJ91 y1_aUJ93)
        (f_aUJ8Z x3_aUJ92)

instance HasId (TRequestMessage (m_iSZlY :: Language.LSP.Protocol.Internal.Method.Method f_iSZlX 'Language.LSP.Protocol.Message.Meta.Request)) (Language.LSP.Protocol.Message.LspId.LspId m_iSZlY) where
  {-# INLINE id #-}
  id f_aUJbG (TRequestMessage x1_aUJbH x2_aUJbI x3_aUJbJ x4_aUJbK)
    = fmap
        (\ y1_aUJbL -> TRequestMessage x1_aUJbH y1_aUJbL x3_aUJbJ x4_aUJbK)
        (f_aUJbG x2_aUJbI)
instance HasJsonrpc (TRequestMessage (m_iSZlY :: Language.LSP.Protocol.Internal.Method.Method f_iSZlX 'Language.LSP.Protocol.Message.Meta.Request)) Data.Text.Internal.Text where
  {-# INLINE jsonrpc #-}
  jsonrpc
    f_aUJbM
    (TRequestMessage x1_aUJbN x2_aUJbO x3_aUJbP x4_aUJbQ)
    = fmap
        (\ y1_aUJbR -> TRequestMessage y1_aUJbR x2_aUJbO x3_aUJbP x4_aUJbQ)
        (f_aUJbM x1_aUJbN)
instance HasMethod (TRequestMessage (m_iSZlY :: Language.LSP.Protocol.Internal.Method.Method f_iSZlX 'Language.LSP.Protocol.Message.Meta.Request)) (Language.LSP.Protocol.Internal.Method.SMethod m_iSZlY) where
  {-# INLINE method #-}
  method
    f_aUJc0
    (TRequestMessage x1_aUJc1 x2_aUJc2 x3_aUJc3 x4_aUJc4)
    = fmap
        (\ y1_aUJc5 -> TRequestMessage x1_aUJc1 x2_aUJc2 y1_aUJc5 x4_aUJc4)
        (f_aUJc0 x3_aUJc3)
instance (a_aUJc6
          ~ Language.LSP.Protocol.Internal.Method.MessageParams m_iSZlY) =>
         HasParams (TRequestMessage (m_iSZlY :: Language.LSP.Protocol.Internal.Method.Method f_iSZlX 'Language.LSP.Protocol.Message.Meta.Request)) a_aUJc6 where
  {-# INLINE params #-}
  params
    f_aUJc7
    (TRequestMessage x1_aUJc8 x2_aUJc9 x3_aUJca x4_aUJcb)
    = fmap
        (\ y1_aUJcc -> TRequestMessage x1_aUJc8 x2_aUJc9 x3_aUJca y1_aUJcc)
        (f_aUJc7 x4_aUJcb)

instance HasId (TResponseMessage (m_iSZnz :: Language.LSP.Protocol.Internal.Method.Method f_iSZny 'Language.LSP.Protocol.Message.Meta.Request)) (Maybe (Language.LSP.Protocol.Message.LspId.LspId m_iSZnz)) where
  {-# INLINE id #-}
  id f_aUJgD (TResponseMessage x1_aUJgE x2_aUJgF x3_aUJgG)
    = fmap
        (\ y1_aUJgH -> TResponseMessage x1_aUJgE y1_aUJgH x3_aUJgG)
        (f_aUJgD x2_aUJgF)
instance HasJsonrpc (TResponseMessage (m_iSZnz :: Language.LSP.Protocol.Internal.Method.Method f_iSZny 'Language.LSP.Protocol.Message.Meta.Request)) Data.Text.Internal.Text where
  {-# INLINE jsonrpc #-}
  jsonrpc f_aUJgI (TResponseMessage x1_aUJgJ x2_aUJgK x3_aUJgL)
    = fmap
        (\ y1_aUJgM -> TResponseMessage y1_aUJgM x2_aUJgK x3_aUJgL)
        (f_aUJgI x1_aUJgJ)
instance (a_aUJgZ
          ~
          Either (TResponseError m_iSZnz) (Language.LSP.Protocol.Internal.Method.MessageResult m_iSZnz)) =>
         HasResult (TResponseMessage (m_iSZnz :: Language.LSP.Protocol.Internal.Method.Method f_iSZny 'Language.LSP.Protocol.Message.Meta.Request)) a_aUJgZ where
  {-# INLINE result #-}
  result f_aUJh0 (TResponseMessage x1_aUJh1 x2_aUJh2 x3_aUJh3)
    = fmap
        (\ y1_aUJh4 -> TResponseMessage x1_aUJh1 x2_aUJh2 y1_aUJh4)
        (f_aUJh0 x3_aUJh3)

instance HasJsonrpc (TNotificationMessage (m_iSZlP :: Language.LSP.Protocol.Internal.Method.Method f_iSZlO 'Language.LSP.Protocol.Message.Meta.Notification)) Data.Text.Internal.Text where
  {-# INLINE jsonrpc #-}
  jsonrpc f_aUJkj (TNotificationMessage x1_aUJkk x2_aUJkl x3_aUJkm)
    = fmap
        (\ y1_aUJkn -> TNotificationMessage y1_aUJkn x2_aUJkl x3_aUJkm)
        (f_aUJkj x1_aUJkk)
instance HasMethod (TNotificationMessage (m_iSZlP :: Language.LSP.Protocol.Internal.Method.Method f_iSZlO 'Language.LSP.Protocol.Message.Meta.Notification)) (Language.LSP.Protocol.Internal.Method.SMethod m_iSZlP) where
  {-# INLINE method #-}
  method f_aUJkw (TNotificationMessage x1_aUJkx x2_aUJky x3_aUJkz)
    = fmap
        (\ y1_aUJkA -> TNotificationMessage x1_aUJkx y1_aUJkA x3_aUJkz)
        (f_aUJkw x2_aUJky)
instance (a_aUJkB
          ~ Language.LSP.Protocol.Internal.Method.MessageParams m_iSZlP) =>
         HasParams (TNotificationMessage (m_iSZlP :: Language.LSP.Protocol.Internal.Method.Method f_iSZlO 'Language.LSP.Protocol.Message.Meta.Notification)) a_aUJkB where
  {-# INLINE params #-}
  params f_aUJkC (TNotificationMessage x1_aUJkD x2_aUJkE x3_aUJkF)
    = fmap
        (\ y1_aUJkG -> TNotificationMessage x1_aUJkD x2_aUJkE y1_aUJkG)
        (f_aUJkC x3_aUJkF)

instance HasCode (TResponseError (m_iSZoD :: Language.LSP.Protocol.Internal.Method.Method f_iSZoC 'Language.LSP.Protocol.Message.Meta.Request)) ((Language.LSP.Protocol.Types.Common.|?) Language.LSP.Protocol.Internal.Types.LSPErrorCodes.LSPErrorCodes Language.LSP.Protocol.Internal.Types.ErrorCodes.ErrorCodes) where
  {-# INLINE code #-}
  code f_aUJo6 (TResponseError x1_aUJo7 x2_aUJo8 x3_aUJo9)
    = fmap
        (\ y1_aUJoa -> TResponseError y1_aUJoa x2_aUJo8 x3_aUJo9)
        (f_aUJo6 x1_aUJo7)
instance HasMessage (TResponseError (m_iSZoD :: Language.LSP.Protocol.Internal.Method.Method f_iSZoC 'Language.LSP.Protocol.Message.Meta.Request)) Data.Text.Internal.Text where
  {-# INLINE message #-}
  message f_aUJob (TResponseError x1_aUJoc x2_aUJod x3_aUJoe)
    = fmap
        (\ y1_aUJof -> TResponseError x1_aUJoc y1_aUJof x3_aUJoe)
        (f_aUJob x2_aUJod)
instance (a_aUJok
          ~
          Maybe (Language.LSP.Protocol.Internal.Method.ErrorData m_iSZoD)) =>
         HasXdata (TResponseError (m_iSZoD :: Language.LSP.Protocol.Internal.Method.Method f_iSZoC 'Language.LSP.Protocol.Message.Meta.Request)) a_aUJok where
  {-# INLINE xdata #-}
  xdata f_aUJol (TResponseError x1_aUJom x2_aUJon x3_aUJoo)
    = fmap
        (\ y1_aUJop -> TResponseError x1_aUJom x2_aUJon y1_aUJop)
        (f_aUJol x3_aUJoo)

