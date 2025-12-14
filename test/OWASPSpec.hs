{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : OWASPSpec
-- Description : Tests for OWASP Top 10 security rules
-- Copyright   : (c) 2024
-- License     : MIT
--
-- Comprehensive tests for the Argus.Rules.Builtin.OWASP module
-- covering all OWASP Top 10 categories.
module OWASPSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec

import Argus.Rules.Builtin.OWASP
import Argus.Rules.Types
import Argus.Types (Severity(..))

spec :: Spec
spec = do
  describe "OWASP Module Structure" $ do
    moduleStructureSpec

  describe "A01 - Broken Access Control" $ do
    accessControlSpec

  describe "A02 - Cryptographic Failures" $ do
    cryptoFailuresSpec

  describe "A03 - Injection" $ do
    injectionSpec

  describe "A04 - Insecure Design" $ do
    insecureDesignSpec

  describe "A05 - Security Misconfiguration" $ do
    misconfigurationSpec

  describe "A06 - Vulnerable Components" $ do
    vulnerableComponentsSpec

  describe "A07 - Authentication Failures" $ do
    authFailuresSpec

  describe "A08 - Integrity Failures" $ do
    integrityFailuresSpec

  describe "A09 - Logging Failures" $ do
    loggingFailuresSpec

  describe "A10 - SSRF" $ do
    ssrfSpec

  describe "Rule Properties" $ do
    rulePropertiesSpec

--------------------------------------------------------------------------------
-- Module Structure Tests
--------------------------------------------------------------------------------

moduleStructureSpec :: Spec
moduleStructureSpec = do
  it "exports allOWASPRules list" $ do
    length allOWASPRules `shouldSatisfy` (> 0)

  it "rules alias equals allOWASPRules" $ do
    length rules `shouldBe` length allOWASPRules

  it "has rules for all 10 OWASP categories" $ do
    let ids = map ruleId allOWASPRules
    -- Should have rules starting with A01 through A10
    any (T.isInfixOf "a01") ids `shouldBe` True
    any (T.isInfixOf "a02") ids `shouldBe` True
    any (T.isInfixOf "a03") ids `shouldBe` True
    any (T.isInfixOf "a04") ids `shouldBe` True
    any (T.isInfixOf "a05") ids `shouldBe` True
    any (T.isInfixOf "a06") ids `shouldBe` True
    any (T.isInfixOf "a07") ids `shouldBe` True
    any (T.isInfixOf "a08") ids `shouldBe` True
    any (T.isInfixOf "a09") ids `shouldBe` True
    any (T.isInfixOf "a10") ids `shouldBe` True

  it "all rules have OWASP tag" $ do
    all (hasTag "OWASP") allOWASPRules `shouldBe` True

  it "all rules have Security tag" $ do
    all (hasTag "Security") allOWASPRules `shouldBe` True

  it "all rules have CWE references" $ do
    all hasCweReference allOWASPRules `shouldBe` True

--------------------------------------------------------------------------------
-- A01 - Broken Access Control Tests
--------------------------------------------------------------------------------

accessControlSpec :: Spec
accessControlSpec = do
  it "a01BrokenAccessControl exists and has multiple rules" $ do
    length a01BrokenAccessControl `shouldSatisfy` (>= 3)

  it "includes IDOR detection rule" $ do
    any (ruleIdContains "idor") a01BrokenAccessControl `shouldBe` True

  it "includes missing authorization check rule" $ do
    any (ruleIdContains "auth") a01BrokenAccessControl `shouldBe` True

  it "includes path manipulation rule" $ do
    any (ruleIdContains "path") a01BrokenAccessControl `shouldBe` True

  it "all A01 rules have A01 tag" $ do
    all (hasTag "A01") a01BrokenAccessControl `shouldBe` True

--------------------------------------------------------------------------------
-- A02 - Cryptographic Failures Tests
--------------------------------------------------------------------------------

cryptoFailuresSpec :: Spec
cryptoFailuresSpec = do
  it "a02CryptographicFailures exists and has multiple rules" $ do
    length a02CryptographicFailures `shouldSatisfy` (>= 3)

  it "includes weak cipher detection" $ do
    any (ruleIdContains "weak") a02CryptographicFailures `shouldBe` True

  it "includes hardcoded key detection" $ do
    any (ruleIdContains "hardcoded") a02CryptographicFailures `shouldBe` True

  it "includes ECB mode detection" $ do
    any (ruleIdContains "ecb") a02CryptographicFailures `shouldBe` True

  it "all A02 rules have A02 tag" $ do
    all (hasTag "A02") a02CryptographicFailures `shouldBe` True

  it "all A02 rules reference cryptographic CWEs" $ do
    all hasCweReference a02CryptographicFailures `shouldBe` True

--------------------------------------------------------------------------------
-- A03 - Injection Tests
--------------------------------------------------------------------------------

injectionSpec :: Spec
injectionSpec = do
  it "a03Injection exists and has multiple rules" $ do
    length a03Injection `shouldSatisfy` (>= 5)

  it "includes SQL injection detection" $ do
    any (ruleIdContains "sql") a03Injection `shouldBe` True

  it "includes command injection detection" $ do
    any (ruleIdContains "command") a03Injection `shouldBe` True

  it "includes LDAP injection detection" $ do
    any (ruleIdContains "ldap") a03Injection `shouldBe` True

  it "includes XPath injection detection" $ do
    any (ruleIdContains "xpath") a03Injection `shouldBe` True

  it "includes log injection detection" $ do
    any (ruleIdContains "log") a03Injection `shouldBe` True

  it "all A03 rules have A03 tag" $ do
    all (hasTag "A03") a03Injection `shouldBe` True

  it "all injection rules have high priority severity" $ do
    all (\r -> ruleSeverity r `elem` [Error, Warning]) a03Injection `shouldBe` True

--------------------------------------------------------------------------------
-- A04 - Insecure Design Tests
--------------------------------------------------------------------------------

insecureDesignSpec :: Spec
insecureDesignSpec = do
  it "a04InsecureDesign exists and has multiple rules" $ do
    length a04InsecureDesign `shouldSatisfy` (>= 2)

  it "includes rate limiting check" $ do
    any (ruleIdContains "rate") a04InsecureDesign `shouldBe` True

  it "includes trust boundary check" $ do
    any (ruleIdContains "trust") a04InsecureDesign `shouldBe` True

  it "all A04 rules have A04 tag" $ do
    all (hasTag "A04") a04InsecureDesign `shouldBe` True

--------------------------------------------------------------------------------
-- A05 - Security Misconfiguration Tests
--------------------------------------------------------------------------------

misconfigurationSpec :: Spec
misconfigurationSpec = do
  it "a05SecurityMisconfiguration exists and has multiple rules" $ do
    length a05SecurityMisconfiguration `shouldSatisfy` (>= 2)

  it "includes debug mode detection" $ do
    any (ruleIdContains "debug") a05SecurityMisconfiguration `shouldBe` True

  it "includes verbose error detection" $ do
    any (ruleIdContains "verbose") a05SecurityMisconfiguration `shouldBe` True

  it "all A05 rules have A05 tag" $ do
    all (hasTag "A05") a05SecurityMisconfiguration `shouldBe` True

--------------------------------------------------------------------------------
-- A06 - Vulnerable Components Tests
--------------------------------------------------------------------------------

vulnerableComponentsSpec :: Spec
vulnerableComponentsSpec = do
  it "a06VulnerableComponents exists" $ do
    length a06VulnerableComponents `shouldSatisfy` (>= 2)

  it "includes known vulnerability detection" $ do
    any (ruleIdContains "vuln") a06VulnerableComponents `shouldBe` True

  it "includes outdated dependency detection" $ do
    any (ruleIdContains "outdated") a06VulnerableComponents `shouldBe` True

  it "all A06 rules have A06 tag" $ do
    all (hasTag "A06") a06VulnerableComponents `shouldBe` True

--------------------------------------------------------------------------------
-- A07 - Authentication Failures Tests
--------------------------------------------------------------------------------

authFailuresSpec :: Spec
authFailuresSpec = do
  it "a07AuthenticationFailures exists and has multiple rules" $ do
    length a07AuthenticationFailures `shouldSatisfy` (>= 3)

  it "includes weak password detection" $ do
    any (ruleIdContains "password") a07AuthenticationFailures `shouldBe` True

  it "includes session handling detection" $ do
    any (ruleIdContains "session") a07AuthenticationFailures `shouldBe` True

  it "includes plaintext password detection" $ do
    any (ruleIdContains "plaintext") a07AuthenticationFailures `shouldBe` True

  it "all A07 rules have A07 tag" $ do
    all (hasTag "A07") a07AuthenticationFailures `shouldBe` True

--------------------------------------------------------------------------------
-- A08 - Integrity Failures Tests
--------------------------------------------------------------------------------

integrityFailuresSpec :: Spec
integrityFailuresSpec = do
  it "a08IntegrityFailures exists and has multiple rules" $ do
    length a08IntegrityFailures `shouldSatisfy` (>= 2)

  it "includes insecure deserialization detection" $ do
    any (ruleIdContains "deserial") a08IntegrityFailures `shouldBe` True

  it "includes code injection detection" $ do
    any (ruleIdContains "code") a08IntegrityFailures `shouldBe` True

  it "all A08 rules have A08 tag" $ do
    all (hasTag "A08") a08IntegrityFailures `shouldBe` True

--------------------------------------------------------------------------------
-- A09 - Logging Failures Tests
--------------------------------------------------------------------------------

loggingFailuresSpec :: Spec
loggingFailuresSpec = do
  it "a09LoggingFailures exists and has multiple rules" $ do
    length a09LoggingFailures `shouldSatisfy` (>= 2)

  it "includes sensitive data logging detection" $ do
    any (ruleIdContains "sensitive") a09LoggingFailures `shouldBe` True

  it "includes audit logging check" $ do
    any (ruleIdContains "audit") a09LoggingFailures `shouldBe` True

  it "all A09 rules have A09 tag" $ do
    all (hasTag "A09") a09LoggingFailures `shouldBe` True

--------------------------------------------------------------------------------
-- A10 - SSRF Tests
--------------------------------------------------------------------------------

ssrfSpec :: Spec
ssrfSpec = do
  it "a10SSRF exists and has multiple rules" $ do
    length a10SSRF `shouldSatisfy` (>= 2)

  it "includes SSRF detection" $ do
    any (ruleIdContains "ssrf") a10SSRF `shouldBe` True

  it "includes open redirect detection" $ do
    any (ruleIdContains "redirect") a10SSRF `shouldBe` True

  it "all A10 rules have A10 tag" $ do
    all (hasTag "A10") a10SSRF `shouldBe` True

--------------------------------------------------------------------------------
-- Rule Properties Tests
--------------------------------------------------------------------------------

rulePropertiesSpec :: Spec
rulePropertiesSpec = do
  it "all rules have unique IDs" $ do
    let ids = map ruleId allOWASPRules
    length ids `shouldBe` length (nubOrd ids)

  it "all rules have non-empty messages" $ do
    all (not . T.null . ruleMessage) allOWASPRules `shouldBe` True

  it "all rules require human review (NeedsReview or Unsafe)" $ do
    all (\r -> ruleSafety r `elem` [NeedsReview, Unsafe]) allOWASPRules `shouldBe` True

  it "all rules are enabled by default" $ do
    all ruleEnabled allOWASPRules `shouldBe` True

  it "total rule count is substantial" $ do
    length allOWASPRules `shouldSatisfy` (>= 25)

  it "all rules have Security category" $ do
    all ((== Security) . ruleCategory) allOWASPRules `shouldBe` True

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Check if rule has a specific tag
hasTag :: Text -> Rule -> Bool
hasTag tag rule = tag `elem` ruleTags rule

-- | Check if rule has CWE reference in references or tags
hasCweReference :: Rule -> Bool
hasCweReference rule =
  any ("CWE" `T.isInfixOf`) (ruleReferences rule)
  || any ("CWE" `T.isInfixOf`) (ruleTags rule)

-- | Check if rule ID contains a substring (case insensitive)
ruleIdContains :: Text -> Rule -> Bool
ruleIdContains sub rule = T.toLower sub `T.isInfixOf` T.toLower (ruleId rule)

-- | Remove duplicates while preserving order
nubOrd :: Ord a => [a] -> [a]
nubOrd = go mempty
  where
    go _ [] = []
    go seen (x:xs)
      | x `elem` seen = go seen xs
      | otherwise = x : go (x : seen) xs
