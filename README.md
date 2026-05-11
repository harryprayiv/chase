# chase

Structural compression of Haskell and PureScript source for LLM context.

## What problem this solves

When you ask an LLM for help with a real codebase, you face a tradeoff: paste
all the source and burn most of the context window before you've asked your
question, or paste a curated subset and watch the LLM hallucinate the parts
you left out. Both fail in different ways.

`chase` is a third option. It reads your Haskell and PureScript source files
and emits a compressed representation that preserves the things an LLM
cannot reconstruct from a type signature (architectural decisions,
behavioral invariants, state machine topologies, magic constants, downstream
dependents, known problems) while dropping the things it can (function
bodies, import name lists, boilerplate). The result is a single text file
you paste into a fresh conversation that gives the model substantially more
useful context per token than either source or hand-written summaries.

The name comes from "cut to the chase": skip the preamble, get to what
actually matters.

## What it does, mechanically

For each source file, `chase` parses it with the appropriate language
backend and extracts:

**Haskell (`.hs`)** via `ghc-lib-parser` (GHC's own frontend exposed as a
library):

- module name and language pragmas
- import declarations (collapsed to one line each, no name lists)
- fixity declarations (verbatim, including precedence and associativity)
- type signatures, both top-level and class methods (verbatim from source
  span)
- pattern synonym declarations (signature and definition merged when both
  exist, otherwise whichever is present)
- data, newtype, type alias, type family, data family, and closed type
  family declarations (verbatim from source span)
- class declarations (head only; method signatures are extracted
  separately as type signatures)
- instance declarations (head only; bodies are method definitions, which
  chase deliberately drops)

**PureScript (`.purs`)** via a line-based heuristic scanner:

- module name and import declarations
- fixity declarations (`infixl`, `infixr`, `infix`)
- type signatures, both top-level and class methods
- foreign import declarations (kept as a separate section in the output,
  see below)
- `data`, `newtype`, and `type` declarations (verbatim)
- `class` declarations (head only)
- `instance`, `else instance`, and `derive` declarations (head only)
- value bindings without a preceding signature are dropped (consistent
  with Haskell mode dropping function bodies)

Function bodies are dropped entirely in both modes. The module skeleton
that remains is typically 3x to 10x smaller than the original source.

Foreign imports get their own `%foreign` section in PureScript output
rather than being mixed into the regular signatures section. They have a
distinct review surface (changing one usually means changing JavaScript
or another host-language file), so flagging them separately is useful
context for the LLM consumer. The Haskell side does not currently extract
`foreign import` declarations, but the schema has the field, so adding it
later is non-breaking.

For modules with annotations, `chase` also attaches:

- `%const` declarations for hoisted magic constants with notes
- `!` invariant lines after each function signature describing behavior
  that the type does not capture
- `>` consumed-by lines pointing at downstream code that depends on the
  function or its observable side effects
- `%decision` blocks recording first-class architectural decisions
- `%open_issue` blocks recording known problems with `blocking` and
  `affects` targets, distinct from settled decisions
- `%topology` blocks for state machines (intended for crem state machines
  but works for anything with a fixed transition graph)

Annotations live in a single JSON file alongside your `.cabal` (or
`spago.dhall`/`spago.yaml`) file. They are not stored in source comments.
This is deliberate: it keeps source clean, lets the chase library validate
that invariants, decision `affects` lists, and open-issue `affects` lists
reference real functions, and lets you version annotations independently
of the code they describe.

## Output formats

Two modes:

- **Bundle**: one concatenated `.chase` file with all module skeletons
  separated by `=== BEGIN <path> ===` markers. This is the format you
  paste into a conversation. Selected by giving an output path that ends
  in `.chase`.
- **Per-file**: a directory tree mirroring the source layout, one
  `.chase` file per source file. Useful for diffing across runs or
  grepping for specific symbols. Selected by giving any other output
  path.

Per-file output drops the source extension (`.hs` or `.purs`) and appends
`.chase`, so `src/Foo/Bar.purs` becomes `<out>/Foo/Bar.chase` and
`src/Foo/Bar.hs` becomes `<out>/Foo/Bar.chase`. If you have both
languages with colliding stems, current behavior is last-write-wins;
adding a language suffix is a candidate change.

A bundle preamble counts both successful and failed parses across both
languages:

    %bundle chase v1
    %files 115 ok, 0 failed

The per-file `%lang` line distinguishes the language in the rendered
output so a downstream consumer (or you, eyeballing it) can tell at a
glance which backend produced which block.

## Usage

The bare runner produces structure-only output if no annotations are
found. The source root can contain `.hs`, `.purs`, or both, in any
directory layout:

    cabal run chase -- backend/src cheeblr.chase
    cabal run chase -- backend/src chase-output/
    cabal run chase -- frontend/src cheeblr-frontend.chase

You can point it at a project root containing both, and the bundle will
include all of them in alphabetical order, dispatched per-file by
extension:

    cabal run chase -- . cheeblr-full.chase

If a file named `chase-annotations.json` exists in the current working
directory, it is loaded automatically and merged into the output. To use
a file at a different path, pass it as a third positional argument:

    cabal run chase -- backend/src cheeblr.chase ./annotations/cheeblr.json

The default-path being missing is silent (annotations are simply empty).
An explicit path being missing is a hard error.

## Annotation file format

A JSON file keyed by module name. Module names are language-neutral: the
same key works for a Haskell module or a PureScript module of the same
name. If you have a Haskell `DB.Auth` and a PureScript `DB.Auth`, they
share the annotation entry (this is almost never what you want; rename
one).

```json
{
  "version": 1,
  "modules": {
    "DB.Auth": {
      "decisions": [
        {
          "name": "OpaqueRotatingSessionTokens",
          "what": "PostgreSQL-backed opaque tokens with per-request rotation",
          "why": [
            "JWTs cannot be revoked without a denylist.",
            "PostgreSQL is already in the stack, so no new infrastructure."
          ],
          "affects": ["createSession", "lookupSession", "rotateSessionToken"]
        }
      ],
      "openIssues": [
        {
          "name": "RotationRaceWindow",
          "what": "Two concurrent requests can both see the pre-rotation token between issuance and DB commit",
          "why": [
            "rotateSessionToken issues the new token before the prior row is invalidated.",
            "Window is bounded by network latency between client and Postgres but is not zero.",
            "Acceptable today because all callers of lookupSession run inside the same transaction; not acceptable if a future caller starts a separate Read Committed read."
          ],
          "blocking": ["multi-region replica reads"],
          "affects": ["rotateSessionToken", "lookupSession"]
        }
      ],
      "invariants": {
        "hashPassword": [
          "16-byte salt from getEntropy",
          "returns PHC string with argonOpts baked in"
        ],
        "lookupSession": {
          "intent": "Resolve a token to (Session, User) and bump last_seen_at",
          "effects": ["DBPool", "IO"],
          "notes": [
            "SIDE EFFECT: updates last_seen_at on every successful lookup",
            "called on every authenticated request"
          ],
          "spec": [
            { "input": "lookupSession pool validToken",   "expected": "Just (sess, user)" },
            { "input": "lookupSession pool revokedToken", "expected": "Nothing" }
          ],
          "consumes": [
            "App.cookieAuthMiddleware (every authenticated request, hot path)",
            "Server.Admin.buildSessionInfos via SessionInfo.siLastSeen",
            "the last_seen_at write specifically: any worker reading idle sessions"
          ]
        }
      }
    }
  }
}
```

Invariants accept either a flat array of strings (each becomes a `!`
line verbatim) or an object with structured fields:

- `intent` renders as a leading `intent: ...` line
- `effects` renders as one comma-joined `effects: A, B, C` line
- `notes` are appended as plain `!` lines
- `spec` entries each render as `spec: <input> => <expected>`
- `consumes` entries render with the `>` sigil and a `consumed by:`
  prefix
- `hint` becomes the optional escape-hatch line like `body: ~30 lines`

Decision `why` accepts either a string or an array of strings; arrays
are joined with spaces and rendered on a single line. The `affects`
array is drift-checked against the module's signatures and pattern
synonyms.

Open issues use the same `name`, `what`, `why`, `affects` shape as
decisions plus an additional `blocking` array. `affects` is
drift-checked against same-file signatures and patterns. `blocking` is
free text and is deliberately not drift-checked: it commonly names
downstream services, features, or executables rather than functions in
the same module. Both `blocking` and `affects` are optional and are
omitted from the rendered output if empty.

## What goes in a good annotation

The bar to add a `!` invariant line is "an LLM, looking only at the type
signature, would get this wrong." Examples that meet the bar:

- side effects not visible in the return type (DB writes, log emits,
  IORef mutations, time reads)
- specific encoding choices (URL-safe base64, hex, big-endian)
- hardcoded values that look configurable (timeouts, retry counts, list
  lengths)
- ordering constraints (X must be called before Y)
- silent failure modes (returns Nothing on validation error vs. throws)
- consequences of the implementation that affect callers (one extra DB
  round-trip, allocates a new connection, holds a lock)
- for PureScript: which foreign imports a function ultimately routes
  through, when the FFI runtime contract isn't obvious from the
  PureScript-side type

Examples that don't meet the bar:

- restating what the type already says
- documenting parameters by name when names are already in the signature
- general-purpose explanations of what a function does

If you find yourself writing an invariant that explains the type, delete
it. The LLM can read the type.

The bar for a `>` consumed-by entry is different: "an LLM reasoning
about a change to this function would benefit from knowing about this
downstream dependency." Examples:

- side effects read by code in another module (admin UI fields populated
  by a write-on-read; the LLM cannot find this from callgraph because
  the consumer reads a column, not a function)
- late-binding consumers (subscribers to a broadcaster, SSE/WS handlers,
  Katip scribes that fan out)
- middleware-injected dependencies (auth headers consumed by every
  authenticated Servant route)
- the specific column or output a consumer reads, when the dependency is
  finer-grained than the function itself
- cross-language consumers: a Haskell endpoint consumed by a specific
  PureScript service module, or a PureScript foreign import implemented
  by a known JS file

`consumes` entries are free text and typically use module-qualified
function names. Parenthetical context is encouraged when it sharpens the
relationship: `"App.cookieAuthMiddleware (every authenticated request)"`
is more useful than `"App.cookieAuthMiddleware"` alone.

The bar for an `openIssue` is "this currently doesn't work right, or
works with a caveat the reader needs to know about, and is not yet
resolved." The distinguishing feature is that an open issue is a problem
you have not fixed. Decisions describe settled tradeoffs you would
defend. Invariants describe how the code currently behaves, factually.
Open issues describe broken or incomplete behavior that callers need to
plan around. Examples:

- a known correctness gap that hasn't been prioritized yet (current
  lineup applied retroactively to historical data,
  idempotency-by-checksum not wired even though the schema supports it)
- a partial-function bug that hasn't bitten yet because of a
  precondition upstream (T.head on what is currently always a
  single-character Text)
- an N+M query pattern that's acceptable today but will need to be
  joined when traffic grows
- a string-encoding contract that crosses module boundaries with no type
  to enforce it (the kind of thing that breaks silently when one side
  changes)

The `blocking` field captures what an open issue is blocking downstream:
features, executables, services, deployment scenarios. If the blocking
list is empty, the issue exists but isn't currently in anyone's way;
it's documentation of a known limitation rather than a roadmap item. The
`affects` field stays drift-checked against same-file signatures and
patterns, same as a decision's affects.

If a thing is fixed, delete the open issue. The annotation file is not
an archive.

## Recommended workflow

1. Run the bare extractor first. Read the `.chase` output as if you'd
   never seen the codebase. Identify modules where the structural
   skeleton alone would not be enough to make a small correct change.
2. Annotate one of those modules. Start with the smallest one with the
   most behavioral subtlety.
3. Test in a fresh LLM conversation: paste only the annotated `.chase`
   bundle, ask for a small change, see what comes back. The mistakes the
   model makes tell you what's missing from your annotations.
4. Iterate. If the LLM's answer leans on speculation rather than
   retrieved facts, that's a sign you need a sharper invariant or a
   `consumes` pointer. If the LLM cites your invariants by name, the
   annotation is doing its job.
5. When you find a problem mid-development that you can't fix
   immediately, add an `openIssue` instead of a TODO comment in source.
   The next LLM session will see it; future you will too.
6. Repeat for the next priority module.

Do not annotate everything before testing. The format and the kinds of
invariants worth recording will both shift after the first real test.

## Parser notes

`chase` uses two different parser backends, dispatched by file
extension. The dispatch logic lives in `Chase.Parse.parseSourceFile`;
the per-language work happens in `Chase.Parse.Haskell` and
`Chase.Parse.PureScript`.

### Haskell

The Haskell parser is `ghc-lib-parser`, a snapshot of GHC's own frontend
exposed as a library. This means chase parses any syntax GHC parses:
GADT-style data declarations with inline kind signatures,
`effectful`-style `data X :: Effect where`, modern type and data
families, standalone kind signatures, TemplateHaskell splices and
quasi-quoted decl blocks, and forward-compatible language extensions.
No fixity table or extension allowlist is maintained; GHC's parser
handles those internally.

The version of Haskell that chase parses is determined by which version
of `ghc-lib-parser` cabal resolves at build time. The cabal-version
constraints leave the upper bound generous so a fresh `cabal build`
picks up whatever current `ghc-lib-parser` is published.
`ghc-lib-parser` tracks GHC by roughly one month, which is the closest
a parsing-only tool gets to "always works on whatever GHC syntax you
use." The other side of that coin: `ghc-lib-parser` minor releases
occasionally rename or drop fields on internal types like
`GHC.Settings.Settings`. These breaks are small and surface as compile
errors in the parser-setup code; treat each one as a single-PR fix
rather than a reason to vendor or pin.

Source-level `LANGUAGE` pragmas are read out of the file header and
applied to the parser's `DynFlags` before parsing. Without this step,
extensions like `TemplateHaskell` and `QuasiQuotes` would be ignored
even when the file declares them, since the parser's lexer rules for
syntactic extensions are gated on flags rather than always active.

There is one extension implication that chase applies by hand:
`TemplateHaskell` enables `TemplateHaskellQuotes` as well. GHC's normal
driver walks the `impliedXFlags` table to expand these implications,
but `xopt_set` in `ghc-lib-parser` only flips the named bit and does
not walk implications. The `$` lexer rule for splices is gated on
`TemplateHaskellQuotesBit`, not `TemplateHaskellBit`, so without this
explicit implication walk, files using `$( ... )` splices would fail
with "parse error on input \`$\`" despite their `LANGUAGE
TemplateHaskell` pragma. If you ever hit a similar parse failure on a
file whose `LANGUAGE` pragmas look correct, the same kind of fix
applies: find which extension bit the lexer rule actually checks, and
add it as a side effect of enabling the extension that should imply
it.

If a Haskell file does fail to parse, chase reports the exact
diagnostic from GHC's parser and continues with the rest of the
bundle. Failures are counted in the bundle preamble.

### PureScript

The PureScript parser is a line-based heuristic scanner. There is no
maintained PureScript parser library in Haskell-land that doesn't drag
the entire purs compiler in as a dependency, so an AST approach would
couple chase to whichever version of the purs compiler is buildable on
your system. The line-based approach exploits PureScript's strict
indentation rule: top-level declarations always start in column 0. An
"anchor" is a column-0 line that matches a declaration prefix (`data`,
`newtype`, `type`, `class`, `instance`, `else instance`, `derive`,
`foreign`, `infixl`/`infixr`/`infix`, or a lowercase identifier
followed by `::`). A "block" is the run of lines from one anchor up to
the line before the next anchor.

Multi-line `{- -}` block comments are blanked out (replaced with
spaces, including the markers themselves) before line scanning runs,
so commented-out declarations don't fragment real blocks. Both line
count and column positions are preserved across the blanking, which
keeps any subsequent line or column references against the cleaned
source consistent with the original file. Nested block comments are
not supported: the first `-}` closes regardless of depth. This matches
the most common PureScript practice and keeps the stripper a
single-pass scan. One known edge case: a `{-` or `-}` marker split
across a line boundary (line ending with `{`, next line starting with
`-`) will not be detected. Idiomatic PureScript does not place these
mid-marker breaks at line edges.

The tradeoff of a line-based scanner is brittleness to anything that
breaks the column-0 assumption: CPP-style preprocessing, unusual
formatting tools that re-indent top-level decls, or sources that mix
literate-style narrative with code. On normal idiomatic PureScript
(spago-style modules, code formatted by `purs-tidy`), the scanner
handles all the constructs in the production cheeblr frontend
including foreign imports, `derive` clauses, `else instance` chains,
and operator fixity declarations.

PureScript "parse failures" almost always mean IO failures (missing
file, permission, decode error), not syntax problems: the scanner is
deliberately tolerant and will produce a partial structural skeleton
even when the source has malformed individual declarations, rather
than blocking the entire bundle.

## Limitations to know about

The output is not source. You cannot regenerate the codebase from a
`.chase` file. It is a description, not a definition. If anyone treats
it as the source of truth, the project is in trouble.

Annotations are free-form text. They are not type-checked against
behavior; they are checked only for the existence of the function names
they reference. Wrong annotations produce wrong-but-confident LLM
output. The drift checker is a sanity net, not a verifier.

`consumes` is not drift-checked. Validating it would require gathering
signatures from every parsed file before checking any single file's
annotations; today the drift checker runs per-file. Cross-module
consumer references rot silently. Open-issue `blocking` entries are
intentionally not drift-checked either, for the same reason but with a
different rationale: they commonly name downstream services or features
that aren't functions at all.

Module names are the annotation key. Two source files declaring
`module Main where` (typical for cabal projects with multiple
executables, e.g. an app entry point and a one-shot bootstrap) collide
on the same key. A Haskell module and a PureScript module with the
same module name also collide. The current workaround is to leave the
secondary one unannotated. The fix is to key annotations by file path
instead, which would be a backwards-compatible addition.

Class and instance declarations render only their head line in the
`data decls` section. Class method signatures are extracted separately
as top-level signatures and appear in the signatures section.
Instance method bodies are dropped, consistent with how chase treats
function bodies elsewhere. This applies to both Haskell and PureScript
output.

Standalone deriving declarations (`deriving instance Show Foo` in
Haskell, `derive instance` in PureScript) are extracted on the
PureScript side as separate blocks. The Haskell side does not
currently emit standalone deriving declarations as their own entries;
if a Haskell module's public surface depends on standalone deriving,
that surface will not appear in chase output.

PureScript `.js` and `.purs.js` foreign runtime files are NOT
inspected. The chase output records that a function is a foreign
import and its PureScript-side type, but says nothing about what the
JS implementation does. Annotate it if the FFI contract matters.

The PureScript line-based scanner does not handle CPP-style
preprocessing in `.purs` files. PureScript doesn't ship CPP and almost
no PureScript code uses it, but if your codebase does, the scanner
will read pre-processed text as source and may produce nonsense
boundaries.

## Why this and not Haddock?

Haddock documents APIs for human readers of a public interface. Its
audience is a developer who has already decided to use your library and
wants to know what each function takes and returns. It assumes the
reader will fill in behavioral context from documentation paragraphs,
examples, and source.

`chase` is for an LLM helping you change your own private code. The
audience already has the type signatures (those are emitted verbatim).
What it doesn't have is the room to read paragraphs, the patience to
chase examples, or the ability to fill in unwritten conventions. The
`!` invariant format and the `>` consumed-by format are optimized for
that constraint: one line per fact, attached to the function it
describes, sized for an LLM's working memory.

You can use both. They serve different readers.

## Build

Standard cabal:

    cabal build
    cabal run chase -- <source-root> <output> [<annotations.json>]

The flake provides a GHC 9.10 dev shell:

    nix develop

## Status

Working tool. Tested on four real codebases:

- chase itself (Haskell, 8/8 modules ok, self-annotated)
- cheeblr backend (Haskell, ~66 modules, ~9,000 lines, includes crem
  state machines with TemplateHaskell-driven singletons and aeson-TH
  JSON derivation)
- cheeblr frontend (PureScript, includes Deku FRP components, yoga-json
  serialization, foreign imports for SSE/WebSocket plumbing and audio
  playback)
- pelotero-engine (Haskell, ~49 modules, heavy `effectful` usage with
  GADT-style effect declarations)

Format is a draft and will change. The annotation API has already grown
several times. The `consumes` field was added after the first round of
real LLM smoke tests showed which kinds of cross-module reasoning the
`!`-only format wasn't pulling weight on. The Haskell parser was
switched from `haskell-src-exts` to `ghc-lib-parser` after the former
failed on 33% of pelotero-engine files due to GADT-style
`data X :: Effect where` declarations and on cheeblr's
TemplateHaskell-using state machine modules. The PureScript path was
added when annotating cheeblr's frontend made it clear that
single-language coverage was leaving half the system invisible to the
LLM. The `openIssues` annotation type was added after annotating
pelotero-engine surfaced enough known-bad behaviors that mixing them
into `decisions` as TODO-flavored entries was making the decision list
incoherent: decisions are settled tradeoffs you would defend, open
issues are unresolved problems, and the `blocking` field on open
issues captures something decisions don't have a slot for.

Honest open questions:

- Whether the `!` invariant format generalizes beyond the author's
  coding style and conventions.
- Whether the manual annotation labor is sustainable on codebases
  larger than a few thousand lines, especially the `consumes`
  curation, which is exactly the kind of cross-cutting metadata that
  rots fastest on a moving codebase.
- Whether the PureScript line-based scanner is robust enough for
  PureScript code that doesn't follow the cheeblr style conventions.
  The cheeblr frontend is the only large PureScript codebase it's been
  tested against; other styles (e.g. heavy use of operator sections at
  the top level, unusual import indentation) could expose scanner
  gaps.
- Whether the next move is to extend annotations further (cross-module
  drift checking for `consumes`, file-path keying to handle `Main`
  collisions and Haskell/PureScript module-name collisions, structured
  rejected-alternatives in decisions, decision dates) or to extract
  more from the AST automatically (standalone deriving declarations on
  the Haskell side, class/instance method bodies for callers who
  actually want them, full foreign import bodies on the PureScript
  side).
- Type-level and class-level decisions are real and need a way to be
  referenced honestly. A separate `affectsTypes` / `affectsClasses`
  field, or relaxing the parser to also recognize types and class
  names, would let these decisions stay specific instead of falling
  back to `[]`.

Open to issues and PRs from anyone using this on their own code.