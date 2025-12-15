---
paths: src/**/*.hs
---
# GHC API Reference (GHC 9.10.3)

This project uses GHC 9.10.3 via LTS 24.21. Key constructor signatures:

| Constructor | Signature                          |
| ----------- | ---------------------------------- |
| `HsPar`     | `HsPar ext expr`                   |
| `HsLam`     | `HsLam ext lamCase matchGroup`     |
| `HsLet`     | `HsLet ext binds body`             |
| `ParPat`    | `ParPat ext pat`                   |
| `AsPat`     | `AsPat ext name pat`               |
| `HsAppType` | `HsAppType ext expr tyArg`         |

When pattern matching on GHC AST nodes, always verify constructor arity against these signatures.
