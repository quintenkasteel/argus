{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.OWASP
-- Description : OWASP Top 10 security rules for Haskell
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides lint rules specifically designed to detect vulnerabilities
-- corresponding to the OWASP Top 10 Web Application Security Risks (2021).
--
-- == OWASP Top 10 Coverage
--
-- [@A01:2021 - Broken Access Control@]
-- Rules detecting missing authorization checks, direct object references
--
-- [@A02:2021 - Cryptographic Failures@]
-- Rules for weak crypto, insecure key handling, plaintext transmission
--
-- [@A03:2021 - Injection@]
-- SQL, NoSQL, OS command, LDAP injection patterns
--
-- [@A04:2021 - Insecure Design@]
-- Architectural security anti-patterns
--
-- [@A05:2021 - Security Misconfiguration@]
-- Debug mode, verbose errors, insecure defaults
--
-- [@A06:2021 - Vulnerable Components@]
-- Outdated or known-vulnerable dependencies
--
-- [@A07:2021 - Identification and Authentication Failures@]
-- Weak session handling, credential exposure
--
-- [@A08:2021 - Software and Data Integrity Failures@]
-- Insecure deserialization, unsafe updates
--
-- [@A09:2021 - Security Logging and Monitoring Failures@]
-- Missing audit logging, sensitive data in logs
--
-- [@A10:2021 - Server-Side Request Forgery (SSRF)@]
-- Unvalidated URL handling, request proxying
module Argus.Rules.Builtin.OWASP
  ( -- * All OWASP Rules
    rules
  , allOWASPRules

    -- * By OWASP Category
  , a01BrokenAccessControl
  , a02CryptographicFailures
  , a03Injection
  , a04InsecureDesign
  , a05SecurityMisconfiguration
  , a06VulnerableComponents
  , a07AuthenticationFailures
  , a08IntegrityFailures
  , a09LoggingFailures
  , a10SSRF
  ) where

import Data.Text (Text)
import Argus.Rules.Types

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Create an OWASP rule with standard tags
-- Takes the OWASP category (e.g., "A01"), CWE references, and the base rule
owaspRule :: Text -> [Text] -> Rule -> Rule
owaspRule category cwes baseRule = baseRule
  { ruleTags = ["OWASP", "Security", category] <> cwes
  , ruleReferences = cwes
  }

--------------------------------------------------------------------------------
-- All Rules
--------------------------------------------------------------------------------

-- | All OWASP rules
rules :: [Rule]
rules = allOWASPRules

-- | Complete list of OWASP Top 10 rules
allOWASPRules :: [Rule]
allOWASPRules = concat
  [ a01BrokenAccessControl
  , a02CryptographicFailures
  , a03Injection
  , a04InsecureDesign
  , a05SecurityMisconfiguration
  , a06VulnerableComponents
  , a07AuthenticationFailures
  , a08IntegrityFailures
  , a09LoggingFailures
  , a10SSRF
  ]

--------------------------------------------------------------------------------
-- A01:2021 - Broken Access Control
--------------------------------------------------------------------------------

-- | Rules for broken access control (CWE-200, CWE-284, CWE-285, CWE-639)
a01BrokenAccessControl :: [Rule]
a01BrokenAccessControl = map (owaspRule "A01" ["CWE-284", "CWE-285"])
  [ ruleDirectObjectReference
  , ruleMissingAuthCheck
  , rulePathManipulation
  , rulePrivilegeEscalation
  ]

ruleDirectObjectReference :: Rule
ruleDirectObjectReference = defaultRule
  { ruleId = "owasp/a01-idor"
  , ruleSeverity = Warning
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "getBy.*Id.*\\$.*param|lookupBy.*Id.*\\$.*input"
  , ruleMessage = "A01:2021 - Potential Insecure Direct Object Reference (IDOR). User-controlled IDs used directly for database lookups may allow unauthorized access. [CWE-639]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = NeedsReview
  }

ruleMissingAuthCheck :: Rule
ruleMissingAuthCheck = defaultRule
  { ruleId = "owasp/a01-missing-auth"
  , ruleSeverity = Warning
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "delete.*\\$|update.*\\$|modify.*\\$"
  , ruleMessage = "A01:2021 - Destructive operation without visible authorization check. Ensure proper access control is in place. [CWE-285]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = NeedsReview
  }

rulePathManipulation :: Rule
rulePathManipulation = defaultRule
  { ruleId = "owasp/a01-path-manipulation"
  , ruleSeverity = Error
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "\\.\\./|\\.\\.\\\\\\\\|\\/\\.\\.\\/|\\.\\.%2[fF]"
  , ruleMessage = "A01:2021 - Path traversal pattern detected in string literal. Never include '../' patterns in paths. [CWE-22]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment]
  , ruleSafety = Unsafe
  }

rulePrivilegeEscalation :: Rule
rulePrivilegeEscalation = defaultRule
  { ruleId = "owasp/a01-privilege-escalation"
  , ruleSeverity = Warning
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "setAdmin|makeAdmin|promoteUser|grantRole|addPermission"
  , ruleMessage = "A01:2021 - Privilege escalation function. Ensure proper authorization checks before granting elevated privileges. [CWE-269]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = NeedsReview
  }

--------------------------------------------------------------------------------
-- A02:2021 - Cryptographic Failures
--------------------------------------------------------------------------------

-- | Rules for cryptographic failures (CWE-259, CWE-327, CWE-328, CWE-330)
a02CryptographicFailures :: [Rule]
a02CryptographicFailures = map (owaspRule "A02" ["CWE-327", "CWE-328"])
  [ ruleWeakCipher
  , ruleWeakHash
  , ruleWeakRandom
  , ruleHardcodedKey
  , ruleInsecureKeySize
  , ruleECBMode
  , ruleNoPadding
  , rulePlaintextProtocol
  ]

ruleWeakCipher :: Rule
ruleWeakCipher = defaultRule
  { ruleId = "owasp/a02-weak-cipher"
  , ruleSeverity = Error
  , ruleCategory = Security
  -- Note: Using alternation instead of negative lookahead (not supported by regex-tdfa)
  , rulePattern = RegexPatternSpec "\\bRC4\\b|\\bRC2\\b|\\bBlowfish\\b|\\bIDEA\\b|\\b3DES\\b|\\bTripleDES\\b|\\bDES_"
  , ruleMessage = "A02:2021 - Weak/deprecated cipher algorithm. Use AES-256-GCM or ChaCha20-Poly1305 instead. [CWE-327]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = Unsafe
  }

ruleWeakHash :: Rule
ruleWeakHash = defaultRule
  { ruleId = "owasp/a02-weak-hash"
  , ruleSeverity = Error
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "\\bMD5\\b|\\bmd5\\b|\\bSHA1\\b|\\bsha1\\b|hashMD5|hashSHA1|Crypto\\.Hash\\.MD5|Crypto\\.Hash\\.SHA1"
  , ruleMessage = "A02:2021 - Weak/deprecated hash algorithm. MD5 and SHA1 are cryptographically broken. Use SHA-256, SHA-3, or BLAKE2. [CWE-328]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = Unsafe
  }

ruleWeakRandom :: Rule
ruleWeakRandom = defaultRule
  { ruleId = "owasp/a02-weak-random"
  , ruleSeverity = Warning
  , ruleCategory = Security
  -- Note: Matches common insecure random functions; verify crypto context
  , rulePattern = RegexPatternSpec "\\brandomIO\\b|\\brandomRIO\\b|\\bnewStdGen\\b|\\bmkStdGen\\b|import\\s+System\\.Random\\b"
  , ruleMessage = "A02:2021 - Potentially weak random number generation. System.Random is not cryptographically secure. Use Crypto.Random for security-sensitive operations. [CWE-330]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = NeedsReview
  }

ruleHardcodedKey :: Rule
ruleHardcodedKey = defaultRule
  { ruleId = "owasp/a02-hardcoded-key"
  , ruleSeverity = Error
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "(aes|secret|encrypt|cipher|crypto).*Key.*=.*\"[A-Za-z0-9+/=]{16,}\""
  , ruleMessage = "A02:2021 - Hardcoded cryptographic key detected. Store keys securely using environment variables or a key management system. [CWE-321]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment]
  , ruleSafety = Unsafe
  }

ruleInsecureKeySize :: Rule
ruleInsecureKeySize = defaultRule
  { ruleId = "owasp/a02-weak-key"
  , ruleSeverity = Warning
  , ruleCategory = Security
  -- Note: Simplified pattern without negative lookahead (not supported by regex-tdfa)
  , rulePattern = RegexPatternSpec "keySize.*=.*(64|128)\\b|RSA.*(512|1024)\\b|\\bAES64\\b|\\bAES128\\b"
  , ruleMessage = "A02:2021 - Potentially weak key size. Use at least 256-bit for symmetric and 2048-bit for asymmetric encryption. [CWE-326]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = NeedsReview
  }

ruleECBMode :: Rule
ruleECBMode = defaultRule
  { ruleId = "owasp/a02-ecb-mode"
  , ruleSeverity = Error
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "ECB|ecbEncrypt|ecbDecrypt|modeECB"
  , ruleMessage = "A02:2021 - ECB mode encryption is insecure. ECB mode reveals patterns in encrypted data. Use GCM, CTR, or CBC with HMAC. [CWE-327]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = Unsafe
  }

ruleNoPadding :: Rule
ruleNoPadding = defaultRule
  { ruleId = "owasp/a02-no-padding"
  , ruleSeverity = Warning
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "NoPadding|zeroPadding|withoutPadding"
  , ruleMessage = "A02:2021 - Encryption without proper padding. Use PKCS7 padding or authenticated encryption modes like GCM. [CWE-327]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = NeedsReview
  }

rulePlaintextProtocol :: Rule
rulePlaintextProtocol = defaultRule
  { ruleId = "owasp/a02-plaintext"
  , ruleSeverity = Warning
  , ruleCategory = Security
  -- Note: Simplified pattern - matches plaintext protocols (may have false positives for localhost)
  , rulePattern = RegexPatternSpec "\"http://[a-zA-Z]|\"ftp://|\"telnet://|\"smtp://"
  , ruleMessage = "A02:2021 - Plaintext protocol URL. Use encrypted protocols (HTTPS, FTPS, SMTPS) to protect data in transit. [CWE-319]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment]
  , ruleSafety = NeedsReview
  }

--------------------------------------------------------------------------------
-- A03:2021 - Injection
--------------------------------------------------------------------------------

-- | Rules for injection attacks (CWE-77, CWE-78, CWE-79, CWE-89, CWE-90, CWE-917)
a03Injection :: [Rule]
a03Injection = map (owaspRule "A03" ["CWE-89", "CWE-78"])
  [ ruleSQLInjection
  , ruleNoSQLInjection
  , ruleCommandInjection
  , ruleLDAPInjection
  , ruleXPathInjection
  , ruleTemplateInjection
  , ruleLogInjection
  , ruleXSS
  , ruleHTMLInjection
  ]

ruleSQLInjection :: Rule
ruleSQLInjection = defaultRule
  { ruleId = "owasp/a03-sql-injection"
  , ruleSeverity = Error
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "(rawSql|rawQuery|rawExecute|execute_|query_).*\\+\\+|\"SELECT.*\".*<>"
  , ruleMessage = "A03:2021 - SQL injection risk. String concatenation in SQL queries allows attackers to manipulate queries. Use parameterized queries. [CWE-89]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = Unsafe
  }

ruleNoSQLInjection :: Rule
ruleNoSQLInjection = defaultRule
  { ruleId = "owasp/a03-nosql-injection"
  , ruleSeverity = Error
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "(mongoDB|redis|cassandra).*\\$.*user|\\$where.*\\+\\+|\\$regex.*input"
  , ruleMessage = "A03:2021 - NoSQL injection risk. User input in NoSQL queries can lead to data exposure or manipulation. Validate and sanitize inputs. [CWE-943]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = Unsafe
  }

ruleCommandInjection :: Rule
ruleCommandInjection = defaultRule
  { ruleId = "owasp/a03-command-injection"
  , ruleSeverity = Error
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "(callCommand|system|rawSystem|shell|readProcess|createProcess).*\\$.*input|\\+\\+.*\\$.*command"
  , ruleMessage = "A03:2021 - OS command injection risk. User input in shell commands can execute arbitrary code. Use typed process APIs with explicit arguments. [CWE-78]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = Unsafe
  }

ruleLDAPInjection :: Rule
ruleLDAPInjection = defaultRule
  { ruleId = "owasp/a03-ldap-injection"
  , ruleSeverity = Error
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "ldapSearch.*\\+\\+|ldapFilter.*<>.*input|\"\\(.*=.*\".*\\+\\+"
  , ruleMessage = "A03:2021 - LDAP injection risk. User input in LDAP queries can bypass authentication or extract data. Escape special characters. [CWE-90]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = Unsafe
  }

ruleXPathInjection :: Rule
ruleXPathInjection = defaultRule
  { ruleId = "owasp/a03-xpath-injection"
  , ruleSeverity = Error
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "xpathQuery.*\\+\\+|\"//.*\\[.*=.*\".*<>|evalXPath.*input"
  , ruleMessage = "A03:2021 - XPath injection risk. User input in XPath queries can access unauthorized data. Use parameterized XPath or validate inputs. [CWE-643]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = Unsafe
  }

ruleTemplateInjection :: Rule
ruleTemplateInjection = defaultRule
  { ruleId = "owasp/a03-template-injection"
  , ruleSeverity = Error
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "renderTemplate.*\\$.*user|compileTemplate.*input|evalTemplate.*request"
  , ruleMessage = "A03:2021 - Server-side template injection risk. User input in templates can execute arbitrary code. Use sandboxed template engines. [CWE-94]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = Unsafe
  }

ruleLogInjection :: Rule
ruleLogInjection = defaultRule
  { ruleId = "owasp/a03-log-injection"
  , ruleSeverity = Warning
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "(log|trace|debug|info|warn|error).*\\$.*user.*input|logMsg.*<>.*request"
  , ruleMessage = "A03:2021 - Log injection risk. Unsanitized user input in logs can forge entries or inject control characters. Encode log messages. [CWE-117]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = NeedsReview
  }

ruleXSS :: Rule
ruleXSS = defaultRule
  { ruleId = "owasp/a03-xss"
  , ruleSeverity = Error
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "toHtml.*\\$.*user|rawHtml.*request|preEscapedText.*input|unsafeByteString.*param"
  , ruleMessage = "A03:2021 - Cross-Site Scripting (XSS) risk. User input rendered as HTML without escaping can execute malicious scripts. Use proper HTML escaping. [CWE-79]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = Unsafe
  }

ruleHTMLInjection :: Rule
ruleHTMLInjection = defaultRule
  { ruleId = "owasp/a03-html-injection"
  , ruleSeverity = Warning
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "\"<script|\"<iframe|\"<object|\"<embed|\"<form.*action|javascript:"
  , ruleMessage = "A03:2021 - HTML/JavaScript injection pattern in string literal. Ensure this is not user-controlled content. [CWE-79]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment]
  , ruleSafety = NeedsReview
  }

--------------------------------------------------------------------------------
-- A04:2021 - Insecure Design
--------------------------------------------------------------------------------

-- | Rules for insecure design patterns
a04InsecureDesign :: [Rule]
a04InsecureDesign = map (owaspRule "A04" ["CWE-799", "CWE-1173"])
  [ ruleNoRateLimit
  , ruleMissingValidation
  , ruleInsecureDefault
  , ruleTrustBoundary
  ]

ruleNoRateLimit :: Rule
ruleNoRateLimit = defaultRule
  { ruleId = "owasp/a04-no-rate-limit"
  , ruleSeverity = Suggestion
  , ruleCategory = Security
  -- Note: Pattern matches sensitive operations - manually verify rate limiting exists
  , rulePattern = RegexPatternSpec "\\b(login|authenticate|sendEmail|resetPassword|register)\\s*::"
  , ruleMessage = "A04:2021 - Sensitive operation detected. Ensure rate limiting is in place to prevent brute force attacks. [CWE-307]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = NeedsReview
  }

ruleMissingValidation :: Rule
ruleMissingValidation = defaultRule
  { ruleId = "owasp/a04-missing-validation"
  , ruleSeverity = Warning
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "fromJust.*request|head.*params|unsafeFromJust.*input"
  , ruleMessage = "A04:2021 - Missing input validation. Partial functions on user input can cause crashes. Validate inputs at system boundaries. [CWE-20]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = Unsafe
  }

ruleInsecureDefault :: Rule
ruleInsecureDefault = defaultRule
  { ruleId = "owasp/a04-insecure-default"
  , ruleSeverity = Warning
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "allowAll|permitAll|disableSecurity|bypassAuth|skipValidation"
  , ruleMessage = "A04:2021 - Insecure default setting. Security features should be enabled by default; require explicit opt-out. [CWE-276]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = NeedsReview
  }

ruleTrustBoundary :: Rule
ruleTrustBoundary = defaultRule
  { ruleId = "owasp/a04-trust-boundary"
  , ruleSeverity = Suggestion
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "trusted.*=.*True|setTrusted|markSafe|htmlSafe"
  , ruleMessage = "A04:2021 - Trust boundary violation. Be careful when marking data as trusted; ensure proper validation occurred. [CWE-501]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = NeedsReview
  }

--------------------------------------------------------------------------------
-- A05:2021 - Security Misconfiguration
--------------------------------------------------------------------------------

-- | Rules for security misconfiguration
a05SecurityMisconfiguration :: [Rule]
a05SecurityMisconfiguration = map (owaspRule "A05" ["CWE-16", "CWE-209"])
  [ ruleDebugEnabled
  , ruleVerboseErrors
  , ruleDefaultCredentials
  , ruleOpenCORS
  , ruleMissingSecurityHeaders
  ]

ruleDebugEnabled :: Rule
ruleDebugEnabled = defaultRule
  { ruleId = "owasp/a05-debug-enabled"
  , ruleSeverity = Error
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "debug.*=.*True|setDebug True|enableDebug|debugMode = True"
  , ruleMessage = "A05:2021 - Debug mode enabled. Debug mode can expose sensitive information and should be disabled in production. [CWE-489]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = Unsafe
  }

ruleVerboseErrors :: Rule
ruleVerboseErrors = defaultRule
  { ruleId = "owasp/a05-verbose-errors"
  , ruleSeverity = Warning
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "showDetailedErrors|displayException|show.*SomeException|displayError.*stack"
  , ruleMessage = "A05:2021 - Verbose error messages may leak sensitive information. Return generic errors to users; log details server-side. [CWE-209]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = NeedsReview
  }

ruleDefaultCredentials :: Rule
ruleDefaultCredentials = defaultRule
  { ruleId = "owasp/a05-default-creds"
  , ruleSeverity = Error
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "password.*=.*\"(admin|password|123456|default|test)\"|user.*=.*\"admin\""
  , ruleMessage = "A05:2021 - Default or weak credentials detected. Remove default credentials and enforce strong password policies. [CWE-798]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment]
  , ruleSafety = Unsafe
  }

ruleOpenCORS :: Rule
ruleOpenCORS = defaultRule
  { ruleId = "owasp/a05-open-cors"
  , ruleSeverity = Warning
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "Access-Control-Allow-Origin.*\\*|allowOrigin.*\\*|corsPolicy.*anyOrigin"
  , ruleMessage = "A05:2021 - Overly permissive CORS policy. Wildcard origins allow any site to make requests. Specify allowed origins explicitly. [CWE-942]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment]
  , ruleSafety = NeedsReview
  }

ruleMissingSecurityHeaders :: Rule
ruleMissingSecurityHeaders = defaultRule
  { ruleId = "owasp/a05-missing-headers"
  , ruleSeverity = Suggestion
  , ruleCategory = Security
  -- Note: Simplified pattern - flags HTTP responses for manual security header review
  , rulePattern = RegexPatternSpec "respondWith\\s+200|responseLBS\\s+status200"
  , ruleMessage = "A05:2021 - HTTP response detected. Ensure security headers are set: X-Content-Type-Options, X-Frame-Options, Content-Security-Policy. [CWE-693]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = NeedsReview
  }

--------------------------------------------------------------------------------
-- A06:2021 - Vulnerable Components
--------------------------------------------------------------------------------

-- | Rules for vulnerable components
a06VulnerableComponents :: [Rule]
a06VulnerableComponents = map (owaspRule "A06" ["CWE-937", "CWE-1035"])
  [ ruleKnownVulnerable
  , ruleOutdatedDep
  ]

ruleKnownVulnerable :: Rule
ruleKnownVulnerable = defaultRule
  { ruleId = "owasp/a06-vulnerable-pkg"
  , ruleSeverity = Error
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "cryptonite.*<.*0\\.30|aeson.*<.*2\\.0|warp.*<.*3\\.3\\.17"
  , ruleMessage = "A06:2021 - Known vulnerable package version. Update to a patched version to address known security vulnerabilities. [CWE-1104]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment]
  , ruleSafety = Unsafe
  }

ruleOutdatedDep :: Rule
ruleOutdatedDep = defaultRule
  { ruleId = "owasp/a06-outdated-dep"
  , ruleSeverity = Suggestion
  , ruleCategory = Security
  -- Note: Matches old-style crypto imports - recommend cryptonite instead
  , rulePattern = RegexPatternSpec "import.*Crypto\\.Cipher|import.*Crypto\\.Pubkey|import.*Data\\.Binary\\.Strict"
  , ruleMessage = "A06:2021 - Potentially outdated cryptographic library. Prefer cryptonite for modern cryptographic operations. [CWE-1104]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = NeedsReview
  }

--------------------------------------------------------------------------------
-- A07:2021 - Identification and Authentication Failures
--------------------------------------------------------------------------------

-- | Rules for authentication failures
a07AuthenticationFailures :: [Rule]
a07AuthenticationFailures = map (owaspRule "A07" ["CWE-287", "CWE-384"])
  [ ruleWeakPassword
  , rulePlaintextPassword
  , ruleSessionFixation
  , ruleMissingMFA
  , ruleInsecureSession
  , ruleJWTNoneAlg
  , ruleJWTWeakSecret
  , ruleHardcodedToken
  ]

ruleWeakPassword :: Rule
ruleWeakPassword = defaultRule
  { ruleId = "owasp/a07-weak-password"
  , ruleSeverity = Warning
  , ruleCategory = Security
  -- Note: Matches short password length requirements
  , rulePattern = RegexPatternSpec "minLength.*=.*[1-7]\\b|passwordLength.*<.*8|minPasswordLength.*=.*[1-7]\\b"
  , ruleMessage = "A07:2021 - Weak password requirements. Require at least 8 characters; NIST recommends supporting up to 64. [CWE-521]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = NeedsReview
  }

rulePlaintextPassword :: Rule
rulePlaintextPassword = defaultRule
  { ruleId = "owasp/a07-plaintext-password"
  , ruleSeverity = Error
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "storePassword.*Text|savePassword.*String|password.*==|verifyPassword.*=="
  , ruleMessage = "A07:2021 - Password may be stored/compared in plaintext. Always hash passwords with bcrypt, scrypt, or Argon2. [CWE-256]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = Unsafe
  }

ruleSessionFixation :: Rule
ruleSessionFixation = defaultRule
  { ruleId = "owasp/a07-session-fixation"
  , ruleSeverity = Warning
  , ruleCategory = Security
  -- Note: Simplified pattern - flags authentication functions for session handling review
  , rulePattern = RegexPatternSpec "\\bloginSuccess\\b|\\bauthenticateUser\\b|\\bvalidateCredentials\\b"
  , ruleMessage = "A07:2021 - Authentication detected. Ensure session IDs are regenerated after login to prevent fixation attacks. [CWE-384]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = NeedsReview
  }

ruleMissingMFA :: Rule
ruleMissingMFA = defaultRule
  { ruleId = "owasp/a07-missing-mfa"
  , ruleSeverity = Suggestion
  , ruleCategory = Security
  -- Note: Flags privileged operations for MFA review
  , rulePattern = RegexPatternSpec "\\badminLogin\\b|\\bprivilegedAction\\b|\\bsudoOperation\\b|\\belevatedAccess\\b"
  , ruleMessage = "A07:2021 - Privileged operation detected. Consider requiring multi-factor authentication for sensitive actions. [CWE-308]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = NeedsReview
  }

ruleInsecureSession :: Rule
ruleInsecureSession = defaultRule
  { ruleId = "owasp/a07-insecure-session"
  , ruleSeverity = Warning
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "sessionTimeout.*=.*(0|3600000)|maxAge.*=.*(-1|0)|httpOnly.*=.*False|secure.*=.*False"
  , ruleMessage = "A07:2021 - Insecure session configuration. Set appropriate timeouts and enable HttpOnly, Secure, and SameSite flags. [CWE-613]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = Unsafe
  }

ruleJWTNoneAlg :: Rule
ruleJWTNoneAlg = defaultRule
  { ruleId = "owasp/a07-jwt-none-alg"
  , ruleSeverity = Error
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "alg.*=.*\"none\"|algorithm.*None|JWTAlgNone|unsafeVerify|skipVerification"
  , ruleMessage = "A07:2021 - JWT with 'none' algorithm or verification bypass. Always verify JWT signatures with a strong algorithm. [CWE-347]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = Unsafe
  }

ruleJWTWeakSecret :: Rule
ruleJWTWeakSecret = defaultRule
  { ruleId = "owasp/a07-jwt-weak-secret"
  , ruleSeverity = Error
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "jwtSecret.*=.*\".{1,15}\"|hmacSecret.*=.*\".{1,15}\"|signingKey.*=.*\"(secret|password|key)\""
  , ruleMessage = "A07:2021 - Weak or short JWT secret. Use at least 256-bit (32+ character) cryptographically random secret for HMAC signing. [CWE-326]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment]
  , ruleSafety = Unsafe
  }

ruleHardcodedToken :: Rule
ruleHardcodedToken = defaultRule
  { ruleId = "owasp/a07-hardcoded-token"
  , ruleSeverity = Error
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "apiToken.*=.*\"[A-Za-z0-9]{20,}\"|bearerToken.*=.*\"|authToken.*=.*\"[^\"]{10,}\""
  , ruleMessage = "A07:2021 - Hardcoded authentication token. Store tokens in environment variables or secure configuration. [CWE-798]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment]
  , ruleSafety = Unsafe
  }

--------------------------------------------------------------------------------
-- A08:2021 - Software and Data Integrity Failures
--------------------------------------------------------------------------------

-- | Rules for integrity failures
a08IntegrityFailures :: [Rule]
a08IntegrityFailures = map (owaspRule "A08" ["CWE-502", "CWE-494"])
  [ ruleInsecureDeserialize
  , ruleMissingSignature
  , ruleCodeInjection
  ]

ruleInsecureDeserialize :: Rule
ruleInsecureDeserialize = defaultRule
  { ruleId = "owasp/a08-insecure-deserialize"
  , ruleSeverity = Error
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "decode.*ByteString|deserialize.*input|readBinary.*request|cereal.*untrusted"
  , ruleMessage = "A08:2021 - Insecure deserialization. Deserializing untrusted data can lead to RCE. Validate input format and use safe parsers. [CWE-502]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = Unsafe
  }

ruleMissingSignature :: Rule
ruleMissingSignature = defaultRule
  { ruleId = "owasp/a08-missing-signature"
  , ruleSeverity = Warning
  , ruleCategory = Security
  -- Note: Flags download operations for signature verification review
  , rulePattern = RegexPatternSpec "\\bdownloadFile\\b|\\bfetchUpdate\\b|\\bloadRemotePlugin\\b|\\bfetchPackage\\b"
  , ruleMessage = "A08:2021 - Download operation detected. Ensure signatures or checksums are verified for downloaded content. [CWE-494]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = NeedsReview
  }

ruleCodeInjection :: Rule
ruleCodeInjection = defaultRule
  { ruleId = "owasp/a08-code-injection"
  , ruleSeverity = Error
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "runQ.*\\$|evalHaskell|interpretHaskell.*input|hint.*runInterpreter"
  , ruleMessage = "A08:2021 - Dynamic code execution with user input. Executing dynamically generated code can lead to arbitrary code execution. [CWE-94]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = Unsafe
  }

--------------------------------------------------------------------------------
-- A09:2021 - Security Logging and Monitoring Failures
--------------------------------------------------------------------------------

-- | Rules for logging failures
a09LoggingFailures :: [Rule]
a09LoggingFailures = map (owaspRule "A09" ["CWE-778", "CWE-532"])
  [ ruleSensitiveDataLogged
  , ruleMissingAuditLog
  , ruleLogForging
  ]

ruleSensitiveDataLogged :: Rule
ruleSensitiveDataLogged = defaultRule
  { ruleId = "owasp/a09-sensitive-logged"
  , ruleSeverity = Error
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "log.*(password|secret|token|apiKey|creditCard|ssn)|print.*(credential|auth)"
  , ruleMessage = "A09:2021 - Sensitive data in logs. Never log passwords, tokens, or PII. Mask sensitive values before logging. [CWE-532]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = Unsafe
  }

ruleMissingAuditLog :: Rule
ruleMissingAuditLog = defaultRule
  { ruleId = "owasp/a09-missing-audit"
  , ruleSeverity = Suggestion
  , ruleCategory = Security
  -- Note: Flags security-critical operations for audit logging review
  , rulePattern = RegexPatternSpec "\\bdeleteUser\\b|\\bchangePermission\\b|\\bmodifySecurityConfig\\b|\\brevokeAccess\\b"
  , ruleMessage = "A09:2021 - Security-critical action detected. Ensure audit logging is in place for authentication attempts and access control changes. [CWE-778]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = NeedsReview
  }

ruleLogForging :: Rule
ruleLogForging = defaultRule
  { ruleId = "owasp/a09-log-forging"
  , ruleSeverity = Warning
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "logInfo.*\\+\\+.*userInput|logMsg.*<>.*request\\.body"
  , ruleMessage = "A09:2021 - Log forging risk. User input in log messages can inject fake entries. Sanitize and encode log data. [CWE-117]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = NeedsReview
  }

--------------------------------------------------------------------------------
-- A10:2021 - Server-Side Request Forgery (SSRF)
--------------------------------------------------------------------------------

-- | Rules for SSRF
a10SSRF :: [Rule]
a10SSRF = map (owaspRule "A10" ["CWE-918", "CWE-601"])
  [ ruleSSRF
  , ruleOpenRedirect
  , ruleURLFromUser
  ]

ruleSSRF :: Rule
ruleSSRF = defaultRule
  { ruleId = "owasp/a10-ssrf"
  , ruleSeverity = Error
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "(httpGet|httpPost|fetch|request|curl).*\\$.*userUrl|http.*Manager.*\\$.*param"
  , ruleMessage = "A10:2021 - Server-Side Request Forgery (SSRF). User-controlled URLs can access internal services. Validate and whitelist allowed URLs. [CWE-918]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = Unsafe
  }

ruleOpenRedirect :: Rule
ruleOpenRedirect = defaultRule
  { ruleId = "owasp/a10-open-redirect"
  , ruleSeverity = Warning
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "redirect.*\\$.*param|redirectTo.*request|Location.*\\$.*userUrl"
  , ruleMessage = "A10:2021 - Open redirect vulnerability. User-controlled redirect URLs can be used for phishing. Validate redirect targets. [CWE-601]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = NeedsReview
  }

ruleURLFromUser :: Rule
ruleURLFromUser = defaultRule
  { ruleId = "owasp/a10-url-from-user"
  , ruleSeverity = Warning
  , ruleCategory = Security
  , rulePattern = RegexPatternSpec "parseURI.*input|parseRequest.*param|mkRequest.*userUrl"
  , ruleMessage = "A10:2021 - URL parsed from user input. Validate URL scheme (http/https only), host (against whitelist), and path. [CWE-918]"
  , ruleEnabled = True
  , ruleConditions = [NotInComment, NotInString]
  , ruleSafety = NeedsReview
  }
