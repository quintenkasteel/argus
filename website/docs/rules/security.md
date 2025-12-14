---
sidebar_position: 3
title: Security Rules
description: Detect security vulnerabilities in Haskell code
---

# Security Rules

Argus detects common security vulnerabilities including injection attacks, unsafe operations, cryptography issues, and hardcoded secrets.

## Injection Vulnerabilities

### SQL Injection

Detects SQL queries built with string concatenation:

```haskell
-- Unsafe: SQL injection vulnerability
query = "SELECT * FROM users WHERE id = '" ++ userId ++ "'"
runQuery query

-- Safe: parameterized queries
runQuery "SELECT * FROM users WHERE id = ?" [userId]
```

**Rule:** `security/sql-injection`

**Detection Patterns:**
- String concatenation in SQL contexts
- `++` operator with SQL keywords
- `mconcat` with SQL-like strings
- `printf`/`format` with SQL templates

### Command Injection

Detects shell command injection:

```haskell
-- Unsafe: command injection
system $ "rm -rf " ++ userInput
readProcess "sh" ["-c", "echo " ++ userInput] ""
callCommand $ "cat " ++ filename

-- Safe: use typed arguments
callProcess "rm" ["-rf", userInput]
readProcess "echo" [userInput] ""
```

**Rule:** `security/command-injection`

**Detection Patterns:**
- `system` with concatenated strings
- `readProcess` with shell commands
- `callCommand` with user input
- `rawSystem` with constructed commands

### Path Traversal

Detects potential path traversal attacks:

```haskell
-- Unsafe: path traversal
readFile $ basePath ++ "/" ++ userInput

-- Safe: validate and normalize
readFile =<< canonicalizePath (basePath </> userInput)
```

**Rule:** `security/path-traversal`

## Unsafe Operations

### `unsafePerformIO`

```haskell
-- Unsafe: breaks referential transparency
globalConfig = unsafePerformIO $ readFile "config.json"

-- Safe: use proper IO
getConfig :: IO Config
getConfig = readFile "config.json" >>= parseConfig
```

**Rule:** `security/unsafe-perform-io`

**Severity:** `error`

### `unsafeCoerce`

```haskell
-- Unsafe: type coercion can cause memory corruption
value = unsafeCoerce ptr :: SomeType

-- Safe: use proper type conversion
value = castPtr ptr
```

**Rule:** `security/unsafe-coerce`

**Severity:** `error`

### `unsafeDupablePerformIO`

```haskell
-- Unsafe: even more dangerous than unsafePerformIO
result = unsafeDupablePerformIO action
```

**Rule:** `security/unsafe-dupable-perform-io`

**Severity:** `error`

## Cryptography Issues

### Weak Hash Algorithms

```haskell
-- Weak: MD5 is cryptographically broken
import Crypto.Hash.MD5
hash = MD5.hash input

-- Weak: SHA1 is deprecated
import Crypto.Hash.SHA1
hash = SHA1.hash input

-- Strong: use SHA256 or better
import Crypto.Hash.SHA256
hash = SHA256.hash input
```

**Rules:** `security/weak-hash-md5`, `security/weak-hash-sha1`

### Weak Encryption

```haskell
-- Weak: DES is insecure
cipher = DES.encrypt key plaintext

-- Weak: RC4 is broken
cipher = RC4.encrypt key plaintext

-- Strong: use AES-256-GCM
cipher = AES.encryptGCM key nonce plaintext
```

**Rules:** `security/weak-cipher-des`, `security/weak-cipher-rc4`

### Hardcoded Keys

```haskell
-- Unsafe: hardcoded encryption key
encryptionKey = "my-secret-key-12345"
encrypted = AES.encrypt encryptionKey plaintext

-- Safe: load from secure storage
encryptionKey <- getEnv "ENCRYPTION_KEY"
```

**Rule:** `security/hardcoded-key`

### Insecure Random

```haskell
-- Unsafe: not cryptographically secure
import System.Random
randomBytes = take 32 $ randoms (mkStdGen 42)

-- Safe: use cryptographic RNG
import Crypto.Random
randomBytes <- getRandomBytes 32
```

**Rule:** `security/insecure-random`

## Secrets Detection

### Hardcoded Credentials

```haskell
-- Unsafe: hardcoded password
password = "admin123"
connection = connect host port username password

-- Safe: use environment variables
password <- getEnv "DB_PASSWORD"
```

**Rule:** `security/hardcoded-password`

**Detection Patterns:**
- Variables named `password`, `passwd`, `secret`, `apiKey`
- String literals matching password patterns
- Connection strings with embedded credentials

### API Keys

```haskell
-- Unsafe: hardcoded API key
apiKey = "sk_live_1234567890abcdef"
client = newClient apiKey

-- Safe: environment or secrets manager
apiKey <- getEnv "STRIPE_API_KEY"
```

**Rule:** `security/hardcoded-api-key`

**Detection Patterns:**
- Stripe keys: `sk_live_*`, `pk_live_*`
- AWS keys: `AKIA*`
- Generic API key patterns

### Connection Strings

```haskell
-- Unsafe: credentials in connection string
connStr = "postgresql://user:password@localhost/db"

-- Safe: use environment variable
connStr <- getEnv "DATABASE_URL"
```

**Rule:** `security/connection-string-password`

## Debug Code

### Trace Functions

```haskell
-- Unsafe in production: debug output
result = trace "processing item" $ processItem item
value = traceShow item $ transform item
x = traceShowId $ complexCalculation

-- Safe: remove or guard with flag
result = processItem item
-- Or use structured logging
result <- withLogging $ processItem item
```

**Rules:** `security/trace`, `security/trace-show`, `security/trace-show-id`

**Severity:** `error`

### Debug.Trace Import

```haskell
-- Warning: Debug.Trace imported
import Debug.Trace
```

**Rule:** `security/debug-trace-import`

## FFI Safety

### Unchecked Foreign Calls

```haskell
-- Unsafe: no null check
foreign import ccall "get_string" c_getString :: IO CString
getString = peekCString =<< c_getString

-- Safe: check for null
getString = do
  ptr <- c_getString
  if ptr == nullPtr
    then return Nothing
    else Just <$> peekCString ptr
```

**Rule:** `security/unchecked-ffi`

### Unsafe Foreign Import

```haskell
-- Caution: marked unsafe
foreign import ccall unsafe "quick_func" quickFunc :: Int -> Int

-- Safer: use safe import
foreign import ccall safe "slow_func" slowFunc :: Int -> Int
```

**Rule:** `security/unsafe-ffi`

## OWASP Rules

Argus includes rules based on OWASP guidelines:

| OWASP Category | Argus Rules |
|----------------|-------------|
| A03:2021 Injection | `sql-injection`, `command-injection` |
| A02:2021 Crypto Failures | `weak-hash-*`, `weak-cipher-*`, `hardcoded-key` |
| A07:2021 XSS | `xss-*` (in web contexts) |
| A09:2021 Logging | `trace`, `debug-*` |

## Configuration

### Enable All Security Rules

```toml
[security]
enabled = true
check-injection = true
check-crypto = true
check-secrets = true
check-unsafe = true
check-ffi = true
check-debug = true
```

### Make Security Rules Errors

```toml
[categories]
security = "error"
```

### Allow Exceptions

```toml
[[scopes]]
modules = ["*.Internal", "*.Unsafe"]
ignore = ["security/unsafe-perform-io"]
```

### Custom Patterns

```toml
[[patterns.rules]]
name = "custom-secret-pattern"
match = "\\bsecret_[a-z]+\\b"
severity = "error"
message = "Potential hardcoded secret"
category = "security"
```

## Suppression

For intentional security-sensitive code:

```haskell
-- Necessary for FFI binding
value = unsafePerformIO $ peek ptr  -- argus:ignore security/unsafe-perform-io: FFI requirement

-- Reviewed cryptographic code
hash = MD5.hash legacy  -- argus:ignore security/weak-hash-md5: compatibility requirement
```

## CI Integration

### SARIF for GitHub Security

```bash
argus check --format sarif src/ > security-results.sarif
```

GitHub will display security issues in the Security tab.

### Fail on Security Issues

```bash
argus check --categories security --strict src/
```

## Best Practices

1. **Never hardcode secrets** - Use environment variables or secret managers
2. **Validate all user input** - Before using in queries or commands
3. **Use parameterized queries** - Never concatenate SQL
4. **Use strong cryptography** - SHA-256+ for hashing, AES-256-GCM for encryption
5. **Remove debug code** - No `trace` in production
6. **Review FFI carefully** - Check for null pointers and unsafe imports

## Next Steps

- **[Performance Rules](./performance)**: Performance anti-patterns
- **[Space Leak Rules](./space-leaks)**: Memory issues
- **[CI Integration](../usage-guide/ci-integration)**: Automated security scanning
