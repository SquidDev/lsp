{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.LSP.Protocol.Message.Lens where

import Control.Lens.TH
import qualified Language.LSP.Protocol.Message.LspId
import qualified Control.Lens.Type
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Internal.Method
import qualified GHC.Int
import qualified Language.LSP.Protocol.Internal.Types.LSPErrorCodes
import qualified Language.LSP.Protocol.Internal.Types.ErrorCodes
import qualified Language.LSP.Protocol.Message.Method
import qualified Data.Aeson.Types
import qualified Language.LSP.Protocol.Message.Meta
import qualified Data.Text.Internal
import Data.Kind
import Language.LSP.Protocol.Message.Registration
import Language.LSP.Protocol.Message.Types
import Language.LSP.Protocol.Types.Lens

instance HasId (TRegistration (m_iHqXJ :: Language.LSP.Protocol.Internal.Method.Method 'Language.LSP.Protocol.Message.Meta.ClientToServer t_iHqXI)) Data.Text.Internal.Text where
  {-# INLINE id #-}
  id f_aIE6p (TRegistration x1_aIE6q x2_aIE6r x3_aIE6s)
    = (fmap
         (\ y1_aIE6t -> ((TRegistration y1_aIE6t) x2_aIE6r) x3_aIE6s))
        (f_aIE6p x1_aIE6q)
instance HasMethod (TRegistration (m_iHqXJ :: Language.LSP.Protocol.Internal.Method.Method 'Language.LSP.Protocol.Message.Meta.ClientToServer t_iHqXI)) (Language.LSP.Protocol.Message.Method.SClientMethod m_iHqXJ) where
  {-# INLINE method #-}
  method f_aIE6C (TRegistration x1_aIE6D x2_aIE6E x3_aIE6F)
    = (fmap
         (\ y1_aIE6G -> ((TRegistration x1_aIE6D) y1_aIE6G) x3_aIE6F))
        (f_aIE6C x2_aIE6E)
instance (a_aIE6L
          ~
          Maybe (Language.LSP.Protocol.Internal.Method.RegistrationOptions m_iHqXJ)) =>
         HasRegisterOptions (TRegistration (m_iHqXJ :: Language.LSP.Protocol.Internal.Method.Method 'Language.LSP.Protocol.Message.Meta.ClientToServer t_iHqXI)) a_aIE6L where
  {-# INLINE registerOptions #-}
  registerOptions f_aIE6M (TRegistration x1_aIE6N x2_aIE6O x3_aIE6P)
    = (fmap
         (\ y1_aIE6Q -> ((TRegistration x1_aIE6N) x2_aIE6O) y1_aIE6Q))
        (f_aIE6M x3_aIE6P)

instance HasId (TUnregistration (m_iHqXF :: Language.LSP.Protocol.Internal.Method.Method 'Language.LSP.Protocol.Message.Meta.ClientToServer t_iHqXE)) Data.Text.Internal.Text where
  {-# INLINE id #-}
  id f_aIE9I (TUnregistration x1_aIE9J x2_aIE9K)
    = (fmap (\ y1_aIE9L -> (TUnregistration y1_aIE9L) x2_aIE9K))
        (f_aIE9I x1_aIE9J)
instance HasMethod (TUnregistration (m_iHqXF :: Language.LSP.Protocol.Internal.Method.Method 'Language.LSP.Protocol.Message.Meta.ClientToServer t_iHqXE)) (Language.LSP.Protocol.Internal.Method.SMethod m_iHqXF) where
  {-# INLINE method #-}
  method f_aIE9U (TUnregistration x1_aIE9V x2_aIE9W)
    = (fmap (\ y1_aIE9X -> (TUnregistration x1_aIE9V) y1_aIE9X))
        (f_aIE9U x2_aIE9W)

instance HasId RequestMessage ((Language.LSP.Protocol.Types.Common.|?) GHC.Int.Int32 Data.Text.Internal.Text) where
  {-# INLINE id #-}
  id f_aIEbH (RequestMessage x1_aIEbI x2_aIEbJ x3_aIEbK x4_aIEbL)
    = (fmap
         (\ y1_aIEbM
            -> (((RequestMessage x1_aIEbI) y1_aIEbM) x3_aIEbK) x4_aIEbL))
        (f_aIEbH x2_aIEbJ)
class HasJsonrpc s a | s -> a where
  jsonrpc :: Control.Lens.Type.Lens' s a
instance HasJsonrpc RequestMessage Data.Text.Internal.Text where
  {-# INLINE jsonrpc #-}
  jsonrpc
    f_aIEbN
    (RequestMessage x1_aIEbO x2_aIEbP x3_aIEbQ x4_aIEbR)
    = (fmap
         (\ y1_aIEbS
            -> (((RequestMessage y1_aIEbS) x2_aIEbP) x3_aIEbQ) x4_aIEbR))
        (f_aIEbN x1_aIEbO)
instance HasMethod RequestMessage Data.Text.Internal.Text where
  {-# INLINE method #-}
  method f_aIEbT (RequestMessage x1_aIEbU x2_aIEbV x3_aIEbW x4_aIEbX)
    = (fmap
         (\ y1_aIEbY
            -> (((RequestMessage x1_aIEbU) x2_aIEbV) y1_aIEbY) x4_aIEbX))
        (f_aIEbT x3_aIEbW)
class HasParams s a | s -> a where
  params :: Control.Lens.Type.Lens' s a
instance HasParams RequestMessage (Maybe Data.Aeson.Types.Value) where
  {-# INLINE params #-}
  params f_aIEc3 (RequestMessage x1_aIEc4 x2_aIEc5 x3_aIEc6 x4_aIEc7)
    = (fmap
         (\ y1_aIEc8
            -> (((RequestMessage x1_aIEc4) x2_aIEc5) x3_aIEc6) y1_aIEc8))
        (f_aIEc3 x4_aIEc7)

class HasError s a | s -> a where
  error :: Control.Lens.Type.Lens' s a
instance HasError ResponseMessage (Maybe ResponseError) where
  {-# INLINE error #-}
  error f_aIEf2 (ResponseMessage x1_aIEf3 x2_aIEf4 x3_aIEf5 x4_aIEf6)
    = (fmap
         (\ y1_aIEf7
            -> (((ResponseMessage x1_aIEf3) x2_aIEf4) x3_aIEf5) y1_aIEf7))
        (f_aIEf2 x4_aIEf6)
instance HasId ResponseMessage ((Language.LSP.Protocol.Types.Common.|?) GHC.Int.Int32 ((Language.LSP.Protocol.Types.Common.|?) Data.Text.Internal.Text Language.LSP.Protocol.Types.Common.Null)) where
  {-# INLINE id #-}
  id f_aIEfo (ResponseMessage x1_aIEfp x2_aIEfq x3_aIEfr x4_aIEfs)
    = (fmap
         (\ y1_aIEft
            -> (((ResponseMessage x1_aIEfp) y1_aIEft) x3_aIEfr) x4_aIEfs))
        (f_aIEfo x2_aIEfq)
instance HasJsonrpc ResponseMessage Data.Text.Internal.Text where
  {-# INLINE jsonrpc #-}
  jsonrpc
    f_aIEfu
    (ResponseMessage x1_aIEfv x2_aIEfw x3_aIEfx x4_aIEfy)
    = (fmap
         (\ y1_aIEfz
            -> (((ResponseMessage y1_aIEfz) x2_aIEfw) x3_aIEfx) x4_aIEfy))
        (f_aIEfu x1_aIEfv)
class HasResult s a | s -> a where
  result :: Control.Lens.Type.Lens' s a
instance HasResult ResponseMessage (Maybe Data.Aeson.Types.Value) where
  {-# INLINE result #-}
  result
    f_aIEfE
    (ResponseMessage x1_aIEfF x2_aIEfG x3_aIEfH x4_aIEfI)
    = (fmap
         (\ y1_aIEfJ
            -> (((ResponseMessage x1_aIEfF) x2_aIEfG) y1_aIEfJ) x4_aIEfI))
        (f_aIEfE x3_aIEfH)

instance HasJsonrpc NotificationMessage Data.Text.Internal.Text where
  {-# INLINE jsonrpc #-}
  jsonrpc f_aIEiz (NotificationMessage x1_aIEiA x2_aIEiB x3_aIEiC)
    = (fmap
         (\ y1_aIEiD -> ((NotificationMessage y1_aIEiD) x2_aIEiB) x3_aIEiC))
        (f_aIEiz x1_aIEiA)
instance HasMethod NotificationMessage Data.Text.Internal.Text where
  {-# INLINE method #-}
  method f_aIEiE (NotificationMessage x1_aIEiF x2_aIEiG x3_aIEiH)
    = (fmap
         (\ y1_aIEiI -> ((NotificationMessage x1_aIEiF) y1_aIEiI) x3_aIEiH))
        (f_aIEiE x2_aIEiG)
instance HasParams NotificationMessage (Maybe Data.Aeson.Types.Value) where
  {-# INLINE params #-}
  params f_aIEiN (NotificationMessage x1_aIEiO x2_aIEiP x3_aIEiQ)
    = (fmap
         (\ y1_aIEiR -> ((NotificationMessage x1_aIEiO) x2_aIEiP) y1_aIEiR))
        (f_aIEiN x3_aIEiQ)

instance HasCode ResponseError ((Language.LSP.Protocol.Types.Common.|?) Language.LSP.Protocol.Internal.Types.LSPErrorCodes.LSPErrorCodes Language.LSP.Protocol.Internal.Types.ErrorCodes.ErrorCodes) where
  {-# INLINE code #-}
  code f_aIEko (ResponseError x1_aIEkp x2_aIEkq x3_aIEkr)
    = (fmap
         (\ y1_aIEks -> ((ResponseError y1_aIEks) x2_aIEkq) x3_aIEkr))
        (f_aIEko x1_aIEkp)
instance HasMessage ResponseError Data.Text.Internal.Text where
  {-# INLINE message #-}
  message f_aIEkt (ResponseError x1_aIEku x2_aIEkv x3_aIEkw)
    = (fmap
         (\ y1_aIEkx -> ((ResponseError x1_aIEku) y1_aIEkx) x3_aIEkw))
        (f_aIEkt x2_aIEkv)
class HasXdata s a | s -> a where
  xdata :: Control.Lens.Type.Lens' s a
instance HasXdata ResponseError (Maybe Data.Aeson.Types.Value) where
  {-# INLINE xdata #-}
  xdata f_aIEkC (ResponseError x1_aIEkD x2_aIEkE x3_aIEkF)
    = (fmap
         (\ y1_aIEkG -> ((ResponseError x1_aIEkD) x2_aIEkE) y1_aIEkG))
        (f_aIEkC x3_aIEkF)

instance HasId (TRequestMessage (m_iGRPd :: Language.LSP.Protocol.Internal.Method.Method f_iGRPc 'Language.LSP.Protocol.Message.Meta.Request)) (Language.LSP.Protocol.Message.LspId.LspId m_iGRPd) where
  {-# INLINE id #-}
  id f_aIEmK (TRequestMessage x1_aIEmL x2_aIEmM x3_aIEmN x4_aIEmO)
    = (fmap
         (\ y1_aIEmP
            -> (((TRequestMessage x1_aIEmL) y1_aIEmP) x3_aIEmN) x4_aIEmO))
        (f_aIEmK x2_aIEmM)
instance HasJsonrpc (TRequestMessage (m_iGRPd :: Language.LSP.Protocol.Internal.Method.Method f_iGRPc 'Language.LSP.Protocol.Message.Meta.Request)) Data.Text.Internal.Text where
  {-# INLINE jsonrpc #-}
  jsonrpc
    f_aIEmQ
    (TRequestMessage x1_aIEmR x2_aIEmS x3_aIEmT x4_aIEmU)
    = (fmap
         (\ y1_aIEmV
            -> (((TRequestMessage y1_aIEmV) x2_aIEmS) x3_aIEmT) x4_aIEmU))
        (f_aIEmQ x1_aIEmR)
instance HasMethod (TRequestMessage (m_iGRPd :: Language.LSP.Protocol.Internal.Method.Method f_iGRPc 'Language.LSP.Protocol.Message.Meta.Request)) (Language.LSP.Protocol.Internal.Method.SMethod m_iGRPd) where
  {-# INLINE method #-}
  method
    f_aIEn4
    (TRequestMessage x1_aIEn5 x2_aIEn6 x3_aIEn7 x4_aIEn8)
    = (fmap
         (\ y1_aIEn9
            -> (((TRequestMessage x1_aIEn5) x2_aIEn6) y1_aIEn9) x4_aIEn8))
        (f_aIEn4 x3_aIEn7)
instance (a_aIEna
          ~ Language.LSP.Protocol.Internal.Method.MessageParams m_iGRPd) =>
         HasParams (TRequestMessage (m_iGRPd :: Language.LSP.Protocol.Internal.Method.Method f_iGRPc 'Language.LSP.Protocol.Message.Meta.Request)) a_aIEna where
  {-# INLINE params #-}
  params
    f_aIEnb
    (TRequestMessage x1_aIEnc x2_aIEnd x3_aIEne x4_aIEnf)
    = (fmap
         (\ y1_aIEng
            -> (((TRequestMessage x1_aIEnc) x2_aIEnd) x3_aIEne) y1_aIEng))
        (f_aIEnb x4_aIEnf)

instance HasId (TResponseMessage (m_iGRQI :: Language.LSP.Protocol.Internal.Method.Method f_iGRQH 'Language.LSP.Protocol.Message.Meta.Request)) (Maybe (Language.LSP.Protocol.Message.LspId.LspId m_iGRQI)) where
  {-# INLINE id #-}
  id f_aIEqZ (TResponseMessage x1_aIEr0 x2_aIEr1 x3_aIEr2)
    = (fmap
         (\ y1_aIEr3 -> ((TResponseMessage x1_aIEr0) y1_aIEr3) x3_aIEr2))
        (f_aIEqZ x2_aIEr1)
instance HasJsonrpc (TResponseMessage (m_iGRQI :: Language.LSP.Protocol.Internal.Method.Method f_iGRQH 'Language.LSP.Protocol.Message.Meta.Request)) Data.Text.Internal.Text where
  {-# INLINE jsonrpc #-}
  jsonrpc f_aIEr4 (TResponseMessage x1_aIEr5 x2_aIEr6 x3_aIEr7)
    = (fmap
         (\ y1_aIEr8 -> ((TResponseMessage y1_aIEr8) x2_aIEr6) x3_aIEr7))
        (f_aIEr4 x1_aIEr5)
instance (a_aIErh
          ~
          Either ResponseError (Language.LSP.Protocol.Internal.Method.MessageResult m_iGRQI)) =>
         HasResult (TResponseMessage (m_iGRQI :: Language.LSP.Protocol.Internal.Method.Method f_iGRQH 'Language.LSP.Protocol.Message.Meta.Request)) a_aIErh where
  {-# INLINE result #-}
  result f_aIEri (TResponseMessage x1_aIErj x2_aIErk x3_aIErl)
    = (fmap
         (\ y1_aIErm -> ((TResponseMessage x1_aIErj) x2_aIErk) y1_aIErm))
        (f_aIEri x3_aIErl)

instance HasJsonrpc (TNotificationMessage (m_iGRP6 :: Language.LSP.Protocol.Internal.Method.Method f_iGRP5 'Language.LSP.Protocol.Message.Meta.Notification)) Data.Text.Internal.Text where
  {-# INLINE jsonrpc #-}
  jsonrpc f_aIEu3 (TNotificationMessage x1_aIEu4 x2_aIEu5 x3_aIEu6)
    = (fmap
         (\ y1_aIEu7
            -> ((TNotificationMessage y1_aIEu7) x2_aIEu5) x3_aIEu6))
        (f_aIEu3 x1_aIEu4)
instance HasMethod (TNotificationMessage (m_iGRP6 :: Language.LSP.Protocol.Internal.Method.Method f_iGRP5 'Language.LSP.Protocol.Message.Meta.Notification)) (Language.LSP.Protocol.Internal.Method.SMethod m_iGRP6) where
  {-# INLINE method #-}
  method f_aIEug (TNotificationMessage x1_aIEuh x2_aIEui x3_aIEuj)
    = (fmap
         (\ y1_aIEuk
            -> ((TNotificationMessage x1_aIEuh) y1_aIEuk) x3_aIEuj))
        (f_aIEug x2_aIEui)
instance (a_aIEul
          ~ Language.LSP.Protocol.Internal.Method.MessageParams m_iGRP6) =>
         HasParams (TNotificationMessage (m_iGRP6 :: Language.LSP.Protocol.Internal.Method.Method f_iGRP5 'Language.LSP.Protocol.Message.Meta.Notification)) a_aIEul where
  {-# INLINE params #-}
  params f_aIEum (TNotificationMessage x1_aIEun x2_aIEuo x3_aIEup)
    = (fmap
         (\ y1_aIEuq
            -> ((TNotificationMessage x1_aIEun) x2_aIEuo) y1_aIEuq))
        (f_aIEum x3_aIEup)

instance HasCode (TResponseError (m_iIEx9 :: Language.LSP.Protocol.Internal.Method.Method f_iIEx8 'Language.LSP.Protocol.Message.Meta.Request)) ((Language.LSP.Protocol.Types.Common.|?) Language.LSP.Protocol.Internal.Types.LSPErrorCodes.LSPErrorCodes Language.LSP.Protocol.Internal.Types.ErrorCodes.ErrorCodes) where
  {-# INLINE code #-}
  code f_aIExm (TResponseError x1_aIExn x2_aIExo x3_aIExp)
    = (fmap
         (\ y1_aIExq -> ((TResponseError y1_aIExq) x2_aIExo) x3_aIExp))
        (f_aIExm x1_aIExn)
instance HasMessage (TResponseError (m_iIEx9 :: Language.LSP.Protocol.Internal.Method.Method f_iIEx8 'Language.LSP.Protocol.Message.Meta.Request)) Data.Text.Internal.Text where
  {-# INLINE message #-}
  message f_aIExr (TResponseError x1_aIExs x2_aIExt x3_aIExu)
    = (fmap
         (\ y1_aIExv -> ((TResponseError x1_aIExs) y1_aIExv) x3_aIExu))
        (f_aIExr x2_aIExt)
instance (a_aIExA
          ~
          Maybe (Language.LSP.Protocol.Internal.Method.ErrorData m_iIEx9)) =>
         HasXdata (TResponseError (m_iIEx9 :: Language.LSP.Protocol.Internal.Method.Method f_iIEx8 'Language.LSP.Protocol.Message.Meta.Request)) a_aIExA where
  {-# INLINE xdata #-}
  xdata f_aIExB (TResponseError x1_aIExC x2_aIExD x3_aIExE)
    = (fmap
         (\ y1_aIExF -> ((TResponseError x1_aIExC) x2_aIExD) y1_aIExF))
        (f_aIExB x3_aIExE)

