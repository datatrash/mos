# Formatter
The formatter can be configured by placing a `mos.toml` file in your project folder, or any parent folder.

The following `mos.toml` represents the default formatting options:

```toml
[formatting.mnemonics]
one-per-line = true
casing = 'lowercase'
register-casing = 'lowercase'

[formatting.braces]
double-newline-after-closing-brace = true
position = 'same-line'

[formatting.whitespace]
trim = true
indent = 4
space-between-kvp = true
space-between-expression-factors = true
space-before-end-of-line-comments = true
collapse-multiple-empty-lines = true
newline-before-if = true
newline-before-variables = true
newline-before-pc = true
newline-before-label = true
```