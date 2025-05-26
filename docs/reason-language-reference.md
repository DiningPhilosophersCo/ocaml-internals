# Reason: Language Reference

This document is supposed to serve as a language reference for Reason (aka reasonml). It is not meant to be a beginner friendly introduction to the language. On the other hand, good working knowledge of the language is assumed.

# Lexical conventions
(similar to https://ocaml.org/manual/5.3/lex.html)

# Values
(similar to https://ocaml.org/manual/5.3/values.html#start-section)

## Constants

# Type expressions

# Modules

# Module/Structure Items

## Type declarations

```
type-declarations := item-attributes? TYPE non-rec? type-declaration-details
| item-attributes? TYPE item-extension-sugar? non-rec? type-declaration-details
```

## Externals (Foreign Function Interface)

```
external := item-attributes? EXTERNAL item-extension-sugar? identifier COLON core-type EQUAL primitive-declaration
| item-attributes EXTERNAL item-extension-sugar? identifier COLON core-type SEMI
```


