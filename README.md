# chase

Structural compression of Haskell source for LLM context.

## What problem this solves

When you ask an LLM for help with a real codebase, you face a tradeoff: paste
all the source and burn most of the context window before you've asked your
question, or paste a curated subset and watch the LLM hallucinate the parts
you left out. Both fail in different ways.

`chase` is a third option. It reads your Haskell source files and emits a
compressed representation that preserves the things an LLM cannot reconstruct
from a type signature (architectural decisions, behavioral invariants, state
machine topologies, magic constants, downstream dependents, known problems)
while dropping the things it can (function bodies, import name lists,
boilerplate). The result is a single text file you paste into a fresh
conversation that gives the model substantially more useful context per
token than either source or hand-written summaries.

The name comes from "cut to the chase": skip the preamble, get to what
actually matters.

## What it does, mechanically

For each Haskell source file, `chase` parses with `ghc-lib-parser` (GHC's
own parser, exposed as a library) and extracts:

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

Function bodies are dropped entirely. The module skeleton that remains is
typically 3x to 10x smaller than the original source.

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

Annotations live in a single JSON file alongside your `.cabal` file. They
are not stored in source comments. This is deliberate: it keeps source
clean, lets the chase library validate that invariants, decision
`affects` lists, and open-issue `affects` lists reference real
functions, and lets you version annotations independently of the code
they describe.

## Output formats

Two modes:

- **Bundle**: one concatenated `.chase` file with all module skeletons
  separated by `=== BEGIN <path> ===` markers. This is the format you paste
  into a conversation. Selected by giving an output path that ends in
  `.chase`.
- **Per-file**: a directory tree mirroring the source layout, one `.chase`
  file per source file. Useful for diffing across runs or grepping for
  specific symbols. Selected by giving any other output path.

## Usage

The bare runner produces structure-only output if no annotations are found:

    cabal run chase -- backend/src cheeblr.chase
    cabal run chase -- backend/src chase-output/

If a file named `chase-annotations.json` exists in the current working
directory, it is loaded automatically and merged into the output. To use a
file at a different path, pass it as a third positional argument:

    cabal run chase -- backend/src cheeblr.chase ./annotations/cheeblr.json

The default-path being missing is silent (annotations are simply empty).
An explicit path being missing is a hard error.

## Annotation file format

A JSON file keyed by module name:

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

Invariants accept either a flat array of strings (each becomes a `!` line
verbatim) or an object with structured fields:

- `intent` renders as a leading `intent: ...` line
- `effects` renders as one comma-joined `effects: A, B, C` line
- `notes` are appended as plain `!` lines
- `spec` entries each render as `spec: <input> => <expected>`
- `consumes` entries render with the `>` sigil and a `consumed by:` prefix
- `hint` becomes the optional escape-hatch line like `body: ~30 lines`

Decision `why` accepts either a string or an array of strings; arrays are
joined with spaces and rendered on a single line. The `affects` array is
drift-checked against the module's signatures and pattern synonyms.

Open issues use the same `name`, `what`, `why`, `affects` shape as
decisions plus an additional `blocking` array. `affects` is drift-checked
against same-file signatures and patterns. `blocking` is free text and is
deliberately not drift-checked: it commonly names downstream services,
features, or executables rather than functions in the same module.
Both `blocking` and `affects` are optional and are omitted from the
rendered output if empty.

## What goes in a good annotation

The bar to add a `!` invariant line is "an LLM, looking only at the type
signature, would get this wrong." Examples that meet the bar:

- side effects not visible in the return type (DB writes, log emits, IORef
  mutations, time reads)
- specific encoding choices (URL-safe base64, hex, big-endian)
- hardcoded values that look configurable (timeouts, retry counts, list
  lengths)
- ordering constraints (X must be called before Y)
- silent failure modes (returns Nothing on validation error vs. throws)
- consequences of the implementation that affect callers (one extra DB
  round-trip, allocates a new connection, holds a lock)

Examples that don't meet the bar:

- restating what the type already says
- documenting parameters by name when names are already in the signature
- general-purpose explanations of what a function does

If you find yourself writing an invariant that explains the type, delete
it. The LLM can read the type.

The bar for a `>` consumed-by entry is different: "an LLM reasoning about
a change to this function would benefit from knowing about this downstream
dependency." Examples:

- side effects read by code in another module (admin UI fields populated
  by a write-on-read; the LLM cannot find this from callgraph because the
  consumer reads a column, not a function)
- late-binding consumers (subscribers to a broadcaster, SSE/WS handlers,
  Katip scribes that fan out)
- middleware-injected dependencies (auth headers consumed by every
  authenticated Servant route)
- the specific column or output a consumer reads, when the dependency is
  finer-grained than the function itself

`consumes` entries are free text and typically use module-qualified
function names. Parenthetical context is encouraged when it sharpens the
relationship: `"App.cookieAuthMiddleware (every authenticated request)"`
is more useful than `"App.cookieAuthMiddleware"` alone.

The bar for an `openIssue` is "this currently doesn't work right, or works
with a caveat the reader needs to know about, and is not yet resolved."
The distinguishing feature is that an open issue is a problem you have not
fixed. Decisions describe settled tradeoffs you would defend. Invariants
describe how the code currently behaves, factually. Open issues describe
broken or incomplete behavior that callers need to plan around. Examples:

- a known correctness gap that hasn't been prioritized yet (current
  lineup applied retroactively to historical data, idempotency-by-checksum
  not wired even though the schema supports it)
- a partial-function bug that hasn't bitten yet because of a precondition
  upstream (T.head on what is currently always a single-character Text)
- an N+M query pattern that's acceptable today but will need to be
  joined when traffic grows
- a string-encoding contract that crosses module boundaries with no type
  to enforce it (the kind of thing that breaks silently when one side
  changes)

The `blocking` field captures what an open issue is blocking downstream:
features, executables, services, deployment scenarios. If the blocking
list is empty, the issue exists but isn't currently in anyone's way; it's
documentation of a known limitation rather than a roadmap item. The
`affects` field stays drift-checked against same-file signatures and
patterns, same as a decision's affects.

If a thing is fixed, delete the open issue. The annotation file is not an
archive.

## Recommended workflow

1. Run the bare extractor first. Read the `.chase` output as if you'd never
   seen the codebase. Identify modules where the structural skeleton alone
   would not be enough to make a small correct change.
2. Annotate one of those modules. Start with the smallest one with the
   most behavioral subtlety.
3. Test in a fresh LLM conversation: paste only the annotated `.chase`
   bundle, ask for a small change, see what comes back. The mistakes the
   model makes tell you what's missing from your annotations.
4. Iterate. If the LLM's answer leans on speculation rather than retrieved
   facts, that's a sign you need a sharper invariant or a `consumes`
   pointer. If the LLM cites your invariants by name, the annotation is
   doing its job.
5. When you find a problem mid-development that you can't fix immediately,
   add an `openIssue` instead of a TODO comment in source. The next LLM
   session will see it; future you will too.
6. Repeat for the next priority module.

Do not annotate everything before testing. The format and the kinds of
invariants worth recording will both shift after the first real test.

## Parser notes

The parser is `ghc-lib-parser`, a snapshot of GHC's own frontend exposed
as a library. This means chase parses any syntax GHC parses: GADT-style
data declarations with inline kind signatures, `effectful`-style
`data X :: Effect where`, modern type and data families, standalone kind
signatures, TemplateHaskell splices and quasi-quoted decl blocks, and
forward-compatible language extensions. No fixity table or extension
allowlist is maintained; GHC's parser handles those internally.

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

If a file does fail to parse, chase reports the exact diagnostic from
GHC's parser and continues with the rest of the bundle. Failures are
counted in the bundle preamble (`%files N ok, M failed`).

## Limitations to know about

The output is not source. You cannot regenerate the codebase from a
`.chase` file. It is a description, not a definition. If anyone treats it
as the source of truth, the project is in trouble.

Annotations are free-form text. They are not type-checked against
behavior; they are checked only for the existence of the function names
they reference. Wrong annotations produce wrong-but-confident LLM output.
The drift checker is a sanity net, not a verifier.

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
on the same key. The current workaround is to leave the secondary main
unannotated. The fix is to key annotations by file path instead, which
would be a backwards-compatible addition.

Class and instance declarations render only their head line in the
`data decls` section. Class method signatures are extracted separately
as top-level signatures and appear in the signatures section. Instance
method bodies are dropped, consistent with how chase treats function
bodies elsewhere.

Standalone deriving declarations (`deriving instance Show Foo`) are not
currently extracted. If a module's public surface depends on standalone
deriving, that surface will not appear in chase output.

## Why this and not Haddock?

Haddock documents APIs for human readers of a public interface. Its
audience is a developer who has already decided to use your library and
wants to know what each function takes and returns. It assumes the reader
will fill in behavioral context from documentation paragraphs, examples,
and source.

`chase` is for an LLM helping you change your own private code. The
audience already has the type signatures (those are emitted verbatim).
What it doesn't have is the room to read paragraphs, the patience to
chase examples, or the ability to fill in unwritten conventions. The `!`
invariant format and the `>` consumed-by format are optimized for that
constraint: one line per fact, attached to the function it describes,
sized for an LLM's working memory.

You can use both. They serve different readers.

## Build

Standard cabal:

    cabal build
    cabal run chase -- <source-root> <output> [<annotations.json>]

The flake provides a GHC 9.10 dev shell:

    nix develop

## Status

Working tool. Tested on three real codebases:

- chase itself (6/6 modules ok, fully self-annotated)
- cheeblr (66/66 modules ok, ~9,000 lines, includes crem state
  machines with TemplateHaskell-driven singletons and aeson-TH JSON
  derivation)
- pelotero-engine (49/49 modules ok, heavy `effectful` usage with
  GADT-style effect declarations)

Format is a draft and will change. The annotation API has already grown
three times. The `consumes` field was added after the first round of real
LLM smoke tests showed which kinds of cross-module reasoning the
`!`-only format wasn't pulling weight on. The parser was switched from
`haskell-src-exts` to `ghc-lib-parser` after the former failed on 33%
of pelotero-engine files due to GADT-style `data X :: Effect where`
declarations and on cheeblr's TemplateHaskell-using state machine
modules. The `openIssues` annotation type was added after annotating
pelotero-engine surfaced enough known-bad behaviors that mixing them
into `decisions` as TODO-flavored entries was making the decision list
incoherent: decisions are settled tradeoffs you would defend, open
issues are unresolved problems, and the `blocking` field on open issues
captures something decisions don't have a slot for.

Honest open questions:

- Whether the `!` invariant format generalizes beyond the author's coding
  style and conventions.
- Whether the manual annotation labor is sustainable on codebases larger
  than a few thousand lines, especially the `consumes` curation, which is
  exactly the kind of cross-cutting metadata that rots fastest on a
  moving codebase.
- Whether the next move is to extend annotations further (cross-module
  drift checking for `consumes`, file-path keying to handle `Main`
  collisions, structured rejected-alternatives in decisions, decision
  dates) or to extract more from the AST automatically (standalone
  deriving declarations, class/instance method bodies for callers who
  actually want them).

Open to issues and PRs from anyone using this on their own code.