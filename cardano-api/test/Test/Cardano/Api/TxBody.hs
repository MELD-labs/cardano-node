{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.TxBody (tests) where

import           Cardano.Prelude

import           Hedgehog (forAll, property, tripping)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (testGroupGenerator)

import           Cardano.Api

import           Gen.Cardano.Api.Typed (genTxBody, genTxBodyContent)


test_roundtrip_TxBody_make_get :: [TestTree]
test_roundtrip_TxBody_make_get =
  [ testProperty (show era) $
    property $ do
      content <- forAll $ genTxBodyContent era
      tripping
        (normalizeOriginal $ review content)
        (\_ -> makeTransactionBody content)
        (<&> \(TxBody content') -> normalizeRoundtrip content')
  | AnyCardanoEra era <- [minBound..]
  ]


-- * normalize = strip unnecessary details
--
-- After roundtrip, @Just mempty@ may become @Nothing@ or vice versa.
-- Input data also may be generated as either @Just 0@ or @Nothing@.

normalizeOriginal :: TxBodyContent ViewTx era -> TxBodyContent ViewTx era
normalizeOriginal content =
  content
    { txAuxScripts    = normalizeAuxScripts    $ txAuxScripts    content
    , txCertificates  = normalizeCertificates  $ txCertificates  content
    , txIns           = sortOn fst             $ txIns           content
    , txInsCollateral = normalizeInsCollateral $ txInsCollateral content
    , txMetadata      = normalizeMetadata      $ txMetadata      content
    , txMintValue     = normalizeMintValue     $ txMintValue     content
    , txWithdrawals   = normalizeWithdrawals   $ txWithdrawals   content
    }

normalizeRoundtrip :: TxBodyContent ViewTx era -> TxBodyContent ViewTx era
normalizeRoundtrip content@TxBodyContent{txAuxScripts, txIns, txInsCollateral} =
  content
    { txAuxScripts    = normalizeAuxScripts    txAuxScripts
    , txIns           = sortOn fst             txIns
    , txInsCollateral = normalizeInsCollateral txInsCollateral
    }

normalizeInsCollateral :: TxInsCollateral era -> TxInsCollateral era
normalizeInsCollateral = \case
  TxInsCollateralNone -> TxInsCollateralNone
  -- for original
  TxInsCollateral _ ins | null ins -> TxInsCollateralNone
  -- for roundtrip
  TxInsCollateral support ins -> TxInsCollateral support $ sort ins

normalizeMetadata :: TxMetadataInEra era -> TxMetadataInEra era
normalizeMetadata = \case
  TxMetadataInEra _ (TxMetadata m) | null m -> TxMetadataNone
  other                                     -> other

normalizeAuxScripts :: TxAuxScripts era -> TxAuxScripts era
normalizeAuxScripts = \case
  TxAuxScripts _       []      -> TxAuxScriptsNone
  TxAuxScripts support scripts ->
    -- sorting uses script versions, hence sort after upgrade
    TxAuxScripts support $
    sortOn languageOfScriptInEra $ map upgradeScriptInEra scripts
  other -> other

languageOfScriptInEra :: ScriptInEra era -> AnyScriptLanguage
languageOfScriptInEra (ScriptInEra lang _) =
  AnyScriptLanguage $ languageOfScriptLanguageInEra lang

normalizeWithdrawals :: TxWithdrawals ViewTx era -> TxWithdrawals ViewTx era
normalizeWithdrawals = \case
  TxWithdrawals _ [] -> TxWithdrawalsNone
  other              -> other

normalizeCertificates :: TxCertificates ViewTx era -> TxCertificates ViewTx era
normalizeCertificates = \case
  TxCertificates _ [] _ -> TxCertificatesNone
  other                 -> other

normalizeMintValue :: TxMintValue ViewTx era -> TxMintValue ViewTx era
normalizeMintValue = \case
  TxMintValue _ v _ | v == mempty -> TxMintNone
  other                           -> other


-- * Ugrading scripts
--
-- The instruction set from V1 may be used as V2,
-- and we can't determine the language version from the transaction.
-- When the user uses V1 instructions,
-- we don't know if they used the V1 tag or the V2 tag.
-- So it's safe to drop this information.

upgradeScriptInEra :: ScriptInEra era -> ScriptInEra era
upgradeScriptInEra = \case
  ScriptInEra SimpleScriptV1InAllegra script ->
    ScriptInEra SimpleScriptV2InAllegra $ upgradeScript script
  ScriptInEra SimpleScriptV1InMary script ->
    ScriptInEra SimpleScriptV2InMary $ upgradeScript script
  ScriptInEra SimpleScriptV1InAlonzo script ->
    ScriptInEra SimpleScriptV2InAlonzo $ upgradeScript script
  other -> other

upgradeScript :: Script SimpleScriptV1 -> Script SimpleScriptV2
upgradeScript (SimpleScript SimpleScriptV1 script) =
  SimpleScript SimpleScriptV2 $ upgradeSimpleScript script

upgradeSimpleScript ::
  SimpleScript SimpleScriptV1 -> SimpleScript SimpleScriptV2
upgradeSimpleScript = \case
  RequireSignature hash -> RequireSignature hash
  RequireAllOf scripts  -> RequireAllOf $ map upgradeSimpleScript scripts
  RequireAnyOf scripts  -> RequireAnyOf $ map upgradeSimpleScript scripts
  RequireMOf n scripts  -> RequireMOf n $ map upgradeSimpleScript scripts


review :: TxBodyContent BuildTx era -> TxBodyContent ViewTx era
review body =
  TxBodyContent
    { txAuxScripts      =                      txAuxScripts     body
    , txCertificates    = reviewCertificates $ txCertificates   body
    , txExtraKeyWits    =                      txExtraKeyWits   body
    , txExtraScriptData = ViewTx
    , txFee             =                      txFee            body
    , txIns             = map reviewTxIn     $ txIns            body
    , txInsCollateral   =                      txInsCollateral  body
    , txMetadata        =                      txMetadata       body
    , txMintValue       = reviewMintValue    $ txMintValue      body
    , txOuts            =                      txOuts           body
    , txProtocolParams  = ViewTx
    , txUpdateProposal  =                      txUpdateProposal body
    , txValidityRange   =                      txValidityRange  body
    , txWithdrawals     = reviewWithdrawals  $ txWithdrawals    body
    }

reviewTxIn ::
  (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era)) ->
  (TxIn, BuildTxWith ViewTx  (Witness WitCtxTxIn era))
reviewTxIn = second $ const ViewTx

reviewWithdrawals :: TxWithdrawals BuildTx era -> TxWithdrawals ViewTx era
reviewWithdrawals = \case
  TxWithdrawalsNone                 -> TxWithdrawalsNone
  TxWithdrawals support withdrawals ->
    TxWithdrawals
      support
      [(address, amount, ViewTx) | (address, amount, _) <- withdrawals]

reviewCertificates :: TxCertificates BuildTx era -> TxCertificates ViewTx era
reviewCertificates = \case
  TxCertificatesNone                    -> TxCertificatesNone
  TxCertificates support certificates _ ->
    TxCertificates support certificates ViewTx

reviewMintValue :: TxMintValue BuildTx era -> TxMintValue ViewTx era
reviewMintValue = \case
  TxMintNone                  -> TxMintNone
  TxMintValue support value _ -> TxMintValue support value ViewTx


test_roundtrip_TxBody_get_make :: [TestTree]
test_roundtrip_TxBody_get_make =
  [ testProperty (show era) $
    property $ do
      txbody <- forAll $ genTxBody era
      tripping
        txbody
        (\(TxBody content) -> content)
        (makeTransactionBody . rebuildBodyContent)
  | AnyCardanoEra era <- [minBound..]
  ]


rebuildBodyContent :: TxBodyContent ViewTx era -> TxBodyContent BuildTx era
rebuildBodyContent body =
  TxBodyContent
    { txAuxScripts      =                       txAuxScripts     body
    , txCertificates    = rebuildCertificates $ txCertificates   body
    , txExtraKeyWits    =                       txExtraKeyWits   body
    , txExtraScriptData = BuildTxWith TxExtraScriptDataNone
    , txFee             =                       txFee            body
    , txIns             = map rebuildTxIn     $ txIns            body
    , txInsCollateral   =                       txInsCollateral  body
    , txMetadata        =                       txMetadata       body
    , txMintValue       = rebuildMintValue    $ txMintValue      body
    , txOuts            =                       txOuts           body
    , txProtocolParams  = BuildTxWith Nothing
    , txUpdateProposal  =                       txUpdateProposal body
    , txValidityRange   =                       txValidityRange  body
    , txWithdrawals     = rebuildWithdrawals  $ txWithdrawals    body
    }

rebuildTxIn ::
  (TxIn, BuildTxWith ViewTx  (Witness WitCtxTxIn era)) ->
  (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))
rebuildTxIn = second $ const $ BuildTxWith $ KeyWitness KeyWitnessForSpending

rebuildWithdrawals :: TxWithdrawals ViewTx era -> TxWithdrawals BuildTx era
rebuildWithdrawals = \case
  TxWithdrawalsNone                 -> TxWithdrawalsNone
  TxWithdrawals support withdrawals ->
    TxWithdrawals
      support
      [ ( address
        , amount
        , panic "rebuildWithdrawals: build field should not be checked"
        )
      | (address, amount, _) <- withdrawals
      ]

rebuildCertificates :: TxCertificates ViewTx era -> TxCertificates BuildTx era
rebuildCertificates = \case
  TxCertificatesNone                    -> TxCertificatesNone
  TxCertificates support certificates _ ->
    TxCertificates
      support
      certificates
      (panic "rebuildCertificates: build field should not be checked")

rebuildMintValue :: TxMintValue ViewTx era -> TxMintValue BuildTx era
rebuildMintValue = \case
  TxMintNone                  -> TxMintNone
  TxMintValue support value _ -> TxMintValue support value $ BuildTxWith mempty


tests :: TestTree
tests = $testGroupGenerator
