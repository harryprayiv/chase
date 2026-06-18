# chase

Structural compression of Haskell and PureScript source for LLM context.

## What problem this solves

When you ask an LLM for help with a real codebase, you face a tradeoff:
paste all the source and burn most of the context window before you've
asked your question, or paste a curated subset and watch the LLM
hallucinate the parts you left out. Both fail in different ways.

`chase` is a third option. It reads your Haskell and PureScript source
files and emits a compressed representation that preserves the things
an LLM cannot reconstruct from a type signature (architectural
decisions, behavioral invariants, state machine topologies, magic
constants, downstream dependents, known problems, test coverage gaps)
while dropping the things it can (function bodies, import name lists,
boilerplate). The result is a single text file you paste into a fresh
conversation that gives the model substantially more useful context
per token than either source or hand-written summaries.

Writing the annotations is itself a problem. Hand-authoring them is
the highest-quality path but does not scale past a few thousand lines,
and on an unfamiliar module you do not yet know what an invariant for
*this* function should say. Chase ships a second mode, `chase-annotate`,
that hands the structural skeleton to an LLM through Grace and validates
the result against the same drift checker the main extractor uses. The
two passes form a closed loop: the parser bounds what the LLM can claim
about names, the typed Grace schema bounds what shapes it can return,
and the LLM proposes the behavioral content neither side can infer.
Wrong function names cannot escape the loop; wrong invariant text
still can, and that part is on the human reviewer.

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

**PureScript (`.purs`)** via the actual PureScript CST parser, vendored
from the upstream `purescript` compiler and built against GHC 9.10:

- module name and import declarations
- fixity declarations (`infixl`, `infixr`, `infix`)
- type signatures, both top-level and class methods (with class methods
  flattened into the top-level signature stream so invariants attach by
  name without needing to know whether the binding is free-standing or
  inside a class body)
- foreign import declarations (kept as a separate `%foreign` section in
  the output, see below)
- `data`, `newtype`, and `type` declarations (verbatim)
- `class` declarations (head only; method signatures are extracted
  separately, same convention as Haskell)
- `instance`, `else instance`, and `derive` declarations (head only,
  consistent with Haskell)
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

When test roots are configured (or auto-detected), `chase` also attaches:

- `?` test-coverage lines under each signature, foreign import, and
  pattern synonym, identifying which test functions reference the name
  (or stating "no test references" when nothing does)

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

The CLI takes one positional argument (the source path) and a small set
of flags. Everything else has a sensible default.

```
chase SOURCE [-o PATH] [-a FILE | --no-annotations]
             [-t DIR[,DIR..] | --no-tests] [-q]
```

The bare invocation auto-detects everything:

    cabal run chase -- backend/src

This scans `backend/src` for `.hs` and `.purs` files, writes a bundle to
`./backend.chase` in the current directory, picks up a
`chase-annotation.json` or `chase-annotations.json` if one exists in the
CWD or alongside the source, and auto-detects test directories named
`test/`, `tests/`, or `spec/` adjacent to the source root. If
auto-detection finds nothing, those features stay off silently.

Explicit forms:

    cabal run chase -- backend/src -o cheeblr.chase
    cabal run chase -- backend/src -o chase-output/
    cabal run chase -- backend/src -o cheeblr.chase -a ./annotations/cheeblr.json
    cabal run chase -- backend/src -o cheeblr.chase -t test,integration-test
    cabal run chase -- backend/src --no-tests --no-annotations
    cabal run chase -- backend/src -q

The `-o` value's suffix selects the mode: `.chase` produces a bundle
file; anything else is treated as a per-file output directory. Default
is `<source-basename>.chase` in the current directory.

The `-t` flag takes a comma-separated list of directories, not multiple
`-t` flags. Test root paths are validated; missing directories trigger a
warning to stderr but do not abort the run.

The `-a` flag points at one annotations file. If the explicit path is
missing, the run aborts with a hard error (distinct from auto-detect,
which is silent when no file is found). `--no-annotations` skips
auto-detection entirely.

`-q` / `--quiet` suppresses the per-file `parse`, `write`, `scan`, and
`index` progress lines. Drift warnings and parse failures are still
printed regardless.

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

## Generating annotations with grace

The structural skeleton tells the LLM what your code is shaped like.
The annotations tell it how the code actually behaves. Two ways to
produce them:

- **By hand**, when you have a feel for what an invariant for *this*
  function should say. Highest quality per annotation, does not scale
  past a few thousand lines.
- **By `chase-annotate`**, which hands the skeleton to an LLM through
  Grace, validates the response against chase's drift checker, and
  writes the result in the JSON format chase already consumes. A draft
  on every signature in one shot.

The interesting part is how those two halves talk to each other, and
that is where chase's design departs from how LLM tooling usually gets
built.

### The closed loop

A normal LLM-in-the-loop tool calls the model, parses the response, and
hopes the response was right. Failure modes that survive include
references to functions that do not exist, JSON that does not match the
shape the caller wanted, and confident assertions about code the model
cannot have seen. The downstream code copes by treating the LLM's
output as untrusted, which means a human has to read every line.

Chase's drift checker already validates names: an invariant attached to
a function the parser never saw, or a decision whose `affects` list
mentions an unknown name, becomes a warning when chase renders. That
validator is the same code path whether the annotations were
hand-written or generated. Plumbing the same checker into the LLM loop
turns it into a feedback signal.

The mechanics:

1. `chase-annotate` parses the source file via `Chase.Parse`, renders
   the chase skeleton, and hands the skeleton to the typed Grace
   expression at `grace/genAnnotations.ffg` along with the OpenAI key.
2. Grace compiles the expression's return type to a JSON Schema and
   submits both the prompt and the schema to OpenAI. The response is
   rejected by OpenAI itself if the shape does not match. The Haskell
   side gets back a typed `GenAnnotations` value, not a string to
   parse.
3. The typed value is converted to a chase `ModuleAnnotations`, merged
   onto the parsed `ChaseFile`, and run through
   `Chase.Pipeline.checkAnnotationDrift`. Any drift warnings (an
   invariant attached to `fooBar` when the file has no `fooBar`) come
   back as a `[Text]`.
4. If the warning list is empty, write the file. If it is not empty
   and the retry budget is not exhausted, call the generator again
   with `driftWarnings` set to the warnings. The Grace template
   surfaces those warnings inline in the prompt text so the model
   sees its own previous failure mode.
5. The loop converges in one or two passes on every module tested.
   When it does not converge, write the file anyway and print the
   residual warnings to stderr; the human reviewer fixes those by
   hand.

What this closes off: hallucinated function names. The drift checker
rejects them, the warnings fold back, the model corrects. Names in the
final annotation file are real names from the source.

What this does not close off: hallucinated behavior. The model can
write a plausible-sounding `why` block that describes something the
function does not do. The static analyzer has no way to know. The
human reviewer is the only check, the same way the human reviewer is
the only check on any other LLM output that survives schema
validation.

The point of the architecture is not to eliminate hallucination. It is
to eliminate the most embarrassing class of hallucination (the model
fabricating function names) by making the same parser that grounds the
LLM's input also validate the LLM's output. The cost is one extra round
trip in roughly half of generation passes; the budget is bounded by
`--max-retries`.

### Architecture

Three deliberate separations make this work without entangling the
deterministic core of chase with the LLM-facing surface.

**The Grace expression is a typed Haskell function from the outside.**
`grace/genAnnotations.ffg` is a Grace file declaring a function of
type `GenArgs -> IO GenAnnotations`. The bridge loads it via Grace's
`load` function, gets back something with that Haskell type via the
`FromGrace (a -> IO b)` instance, and calls it. The prompt is not a
string embedded in Haskell that requires a recompile to edit; it is a
checked-in, version-controlled file that the Haskell side knows by
type. Editing the prompt is a diff, not a build.

**The bridge is a separate cabal sublibrary.** `chase-grace-bridge`
lives under `src-bridge/` with its own dependency closure (grace,
which transitively pulls in megaparsec, http-client, openai-servant,
and the rest). The main `chase` library and the deterministic `chase`
executable do not know it exists. Anyone who only wants the
structural extractor builds chase without ever resolving grace.
Replacing grace with a different prompt library or backend (a
self-hosted inference server with a JSON-Schema-driven structured
output API, a different vendor's API) is a two-file change:
`grace/genAnnotations.ffg` and `src-bridge/Chase/GraceBridge.hs`.
Nothing in `src/` moves.

**The schema lives at the type, not in two places.** The grace
schema's field names come from the Haskell record's
`Generic`-derived `selName`. Renaming a Haskell field renames the
grace schema field. There is no manual translation layer between the
two, which means there is no manual translation layer to forget to
update.

### Usage

    chase-annotate src/MyModule.hs --key $OPENAI_API_KEY

The generator parses the file via the same `Chase.Parse` entry point
the main extractor uses, renders the chase skeleton, and feeds it to
the grace template along with the API key. The template prompts the
model for typed structured output matching the schema. The result is
serialized to `chase-annotations.json` and validated against the same
drift checker the main extractor runs.

Flags:

- `--template FILE`: path to the grace prompt template. Defaults to
  `grace/genAnnotations.ffg`. Edit the template to change the prompt
  shape; no Haskell rebuild required.
- `-o FILE` / `--output FILE`: where to write the annotations JSON.
  Defaults to `chase-annotations.json`.
- `--max-retries N`: how many drift-feedback iterations to allow.
  Defaults to 2.

### What the model gets wrong

Predictable failure modes from the first uses:

- Restates the type as an invariant ("takes a list and returns a
  sorted list" under `sort :: Ord a => [a] -> [a]`). The template
  explicitly tells the model not to do this; it does anyway
  sometimes. Delete those lines on review.
- Hallucinates side effects that match common patterns but are not
  present (claims a function "logs to stderr on failure" when the
  function does not log at all). The structural skeleton does not
  contain the function body, so the model is guessing from the name
  and signature.
- Misses real side effects that the type does not advertise. The
  `last_seen_at` write inside `lookupSession` is the canonical
  example; the model cannot see it because the function body is not
  in the bundle. This is the kind of invariant only the human author
  can supply.
- Confuses `decisions` and `openIssues`. The grace template tries to
  distinguish them; the model sometimes still puts a known-bad
  behavior in `decisions` (defending it) when it belongs in
  `openIssues` (flagging it).

The intended workflow is: run `chase-annotate`, read the output,
keep what is right, fix what is close, delete what is wrong. The
generator's value is in giving you a starting point on every
signature in one shot, not in producing something you commit
unreviewed.

## Test coverage analysis

When invoked with test roots (passed explicitly via `-t` or
auto-detected from `test/`, `tests/`, `spec/` adjacent to the source),
`chase` produces an additional `?` line under every signature, foreign
import, and pattern synonym in the output. The line says one of two
things:

    foo :: Int -> Int
      ? tested by: ModuleA.testFoo, ModuleB.regressionFoo

    bar :: Int -> Int
      ? no test references

The purpose is to surface obvious coverage gaps cheaply. A function with
no test references is almost certainly not tested. A function with a
long list of references is at least exercised by a lot of test code.
What it doesn't tell you, deliberately, is whether the tests that
reference a function actually verify its behavior in any rigorous sense.
See the limitations below.

### How it works

Coverage is computed by parsing every `.hs` and `.purs` file under your
test roots, finding each top-level value binding (Haskell `FunBind`,
PureScript `DeclValue`), and collecting every unqualified identifier
that appears in its body. The result is an inverted index mapping
referenced name to the list of test functions that mention it. That
index is then attached to every source-side `ChaseFile` during the
pipeline pass: for each signature, foreign import, and pattern synonym,
the renderer looks up the name and emits the matching test references.

The walk is static. No tests are executed. No HPC instrumentation. No
build system involvement beyond having `cabal build` succeed once so the
test source compiles.

The reference-collection mechanism differs by language but produces the
same `[Text]` shape: Haskell test bodies are walked via SYB's
`everything` over the GHC AST, picking up every `HsVar` in expression
position; PureScript test bodies are walked by a hand-rolled traversal
over the CST (`collectFromExpr`, `collectFromDoStatement`,
`collectFromWhere`, etc.) with explicit cases for every `CST.Expr`
constructor. SYB would have been the cheaper choice on the PureScript
side too, but the vendored PureScript CST types do not all derive
`Data`, so an explicit walker was the path of least resistance. The
trade-off is concrete: any new `CST.Expr` constructor introduced
upstream and pulled in via a vendor refresh has to be added to the
walker manually, or refs inside it will be silently missed. SYB on the
Haskell side picks up new constructors automatically.

### What gets recorded as a test

Only top-level value bindings in test files are recorded as test
bindings. PatBindings like `(setup, teardown) = ...` and nested
let-bindings inside test bodies are not recorded as separate "tests."
This is deliberate: a test is overwhelmingly written as `testFooHandlesX
= ...` (Haskell) or `testFoo = ...` (PureScript), both of which are
top-level value bindings. PatBindings produce values, and tying coverage
attribution to one of `setup` or `teardown` is arbitrary. Their RHS
references are still picked up indirectly: if any FunBind body
transitively touches a PatBind-bound value's RHS via the collector's AST
walk, those names contribute. The PatBind itself just doesn't show up
as a `> tested by:` source.

### What gets recorded as a reference

Anything that ends up as an identifier in expression position inside a
test function's body. That covers ordinary function applications,
higher-order arguments to combinators, references inside `do` blocks,
references inside `let` and `where`, references inside record
construction or update, and references inside list, tuple, or array
literals.

It does NOT cover:

- function references introduced by Template Haskell splices (chase
  does not run splices; whatever the splice would have produced is not
  walked)
- references hidden behind generic dispatch (e.g. a `Generic`-derived
  call whose actual target is decided by an instance the parser cannot
  resolve)
- references that only appear at the type level (rare for tests but
  real)
- references accessed exclusively through a record selector or a type
  class method where the underlying implementation name is never
  lexically present in the test source

These are inherent limits of static analysis without name resolution.
The output is a smell test ("zero references = almost certainly
untested"), not a guarantee.

### Self-references are filtered

A recursive helper inside a test file like `go n = if n <= 0 then []
else n : go (n - 1)` would otherwise appear in its own coverage list:
`go tested by: TestModule.go`. The indexer drops self-references during
attribution. A real production function with the same name as a test
binding still appears in the coverage list if any OTHER test binding
references it; only the self-loop is filtered.

### When to use coverage analysis

- Before a refactor of a module you're not sure is well-covered. Run
  coverage and look for the `? no test references` lines on the
  functions you're about to touch. Those are the ones where you'll find
  out the hard way.
- As a sanity check after adding new code to make sure you actually
  wrote tests for it.
- When triaging an unfamiliar codebase, as a fast way to identify
  abandoned-looking parts of the API.

What it is not: a replacement for HPC, mutation testing, or any actual
test-quality tool. A function appearing in five test files might still
be exercising only one branch. The `?` line tells you something is
referencing it. That's the entire contract.

## Recommended workflow

1. Run the bare extractor first. Read the `.chase` output as if you'd
   never seen the codebase. Identify modules where the structural
   skeleton alone would not be enough to make a small correct change.
2. Annotate one of those modules. Start with the smallest one with the
   most behavioral subtlety. Either write the annotations by hand, or
   run `chase-annotate src/Foo.hs --key $OPENAI_API_KEY` to get a draft
   and edit it. The drift checker catches hallucinated names regardless
   of which path you took.
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
6. Once annotations on a module are stable, run the extractor with test
   coverage enabled (auto-detected if your tests live under `test/`,
   `tests/`, or `spec/` next to the source; or pass `-t` explicitly).
   The `? no test references` lines are your "obviously untested" list.
   Either write a test or add an `openIssue` explaining why it's
   intentional.
7. Repeat for the next priority module.

Do not annotate everything before testing. The format and the kinds of
invariants worth recording will both shift after the first real test.
Do not turn on coverage analysis before you have annotations either:
coverage tells you what's untested, but on a fresh codebase the bigger
problem is that the LLM doesn't yet have enough behavioral context to
read the existing tests usefully. The same principle applies to
`chase-annotate`: a draft annotation file from the generator is more
useful as input to step 3 than running it across the whole codebase
before you have a feel for what the drafts look like.

## Parser notes

`chase` uses two different parser backends, dispatched by file
extension. The dispatch logic lives in `Chase.Parse.parseSourceFile`;
the per-language work happens in `Chase.Parse.Haskell` and
`Chase.Parse.PureScript`.

### Haskell

The Haskell parser is `ghc-lib-parser`, a snapshot of GHC's own parser
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

The PureScript parser is the actual PureScript CST parser, vendored
from the upstream `purescript` compiler (currently pinned at v0.15.16
in the flake). At build time, the `vendor-purescript-cst` flake app
copies the relevant subset of `purescript/src/Language/PureScript/CST`
(plus its few non-CST dependencies: `Names`, `PSString`, `Roles`,
`Comments`, `AST.SourcePos`, `Control.Monad.Supply`,
`Data.Text.PureScript`) into `vendor/purescript-cst/` under a renamed
module prefix `Chase.Vendor.PureScript.*`. The chase cabal file
includes that vendor directory in `hs-source-dirs`, and the parser is
then a regular dependency-free in-tree module under GHC 9.10.

This is a deliberate departure from the previous strategy. The earlier
version used a line-based column-0 anchor scanner over raw `.purs`
text, because no standalone PureScript parser was published on
Hackage and pulling the whole purs compiler in as a build dependency
was a non-starter. That scanner worked on idiomatic, `purs-tidy`
formatted PureScript and broke on anything that violated the column-0
assumption (CPP preprocessing, unusual formatters, literate-style
sources). The vendored CST parser does not have any of those
constraints: it's the same parser the official compiler uses, so it
accepts exactly what `purs build` accepts.

Mechanically:

- `parseCstFile` reads the file as `Text`, lexes via
  `Chase.Vendor.PureScript.CST.Lexer`, runs the parser via
  `Chase.Vendor.PureScript.CST.Parser`, and returns either a
  `CST.Module ()` or a `NonEmpty CSTErr.ParserError`.
- `extractStructure` consumes the parsed `CST.Module ()` and walks
  `CST.Declaration ()` constructors directly: `DeclSignature`,
  `DeclValue`, `DeclData`, `DeclNewtype`, `DeclType`, `DeclClass`,
  `DeclInstanceChain`, `DeclDerive`, `DeclFixity`, `DeclForeign`. There
  is no string scanning of the source body; the verbatim text rendered
  in the output is sliced from the raw source by the start and end
  source spans computed from token positions on each declaration.
- Class method signatures are extracted from `CST.Class` bodies as
  `CST.Labeled (Name Ident) (Type ())` entries, then flattened into
  the top-level signature stream so annotations attach by name without
  needing to distinguish where the signature came from. This matches
  the Haskell side's convention.
- Foreign imports are extracted into a separate `chaseForeignImports`
  field on `ChaseFile` and rendered in their own `%foreign` section.
  `ForeignValue`, `ForeignData`, and `ForeignKind` forms are all
  handled.
- `DeclInstanceChain` and `DeclDerive` render only the head line of
  their source span, never the body, consistent with the Haskell
  convention for instances.
- Parse errors come back as structured `CSTErr.ParserError` values
  with source positions, formatted by `renderParserErrors` into the
  same `ParseFailure` shape used by the Haskell side. A `.purs` file
  that fails to parse is reported with the actual CST error message
  and source location, not a "we couldn't figure out what this line
  is" guess.

The vendor step is run separately from `cabal build`:

    nix run .#vendor-purescript-cst

This is a one-time operation that populates `vendor/purescript-cst/`.
If the directory already exists, the app removes and recreates it (the
nix store path is content-addressed, so any change to the upstream
purescript pin produces a fresh output directory). After running it,
`cabal build` works normally.

The vendoring strategy was chosen over a Hackage-style published parser
package because the PureScript compiler's CST module is not currently
packaged for standalone use, and waiting for upstream to ship it as one
would have left chase blocked indefinitely. Renaming the modules under
`Chase.Vendor.*` keeps them namespaced and prevents any collision if a
downstream consumer of chase has its own dependency on something from
the PureScript ecosystem.

If a `.purs` file fails to parse, chase reports the CST parser's
diagnostics with source position and continues with the rest of the
bundle. Failures are counted in the bundle preamble, alongside Haskell
failures.

## Limitations to know about

The output is not source. You cannot regenerate the codebase from a
`.chase` file. It is a description, not a definition. If anyone treats
it as the source of truth, the project is in trouble.

Annotations are free-form text. They are not type-checked against
behavior; they are checked only for the existence of the function names
they reference. Wrong annotations produce wrong-but-confident LLM
output. The drift checker is a sanity net, not a verifier. This applies
to `chase-annotate` output too: the names are guaranteed real, the
sentences attached to them are not.

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

Test coverage is matched by unqualified name only, across both
languages. A test that calls `Chase.Parse.parseSourceFile` and a test
that calls `Chase.Parse.Haskell.parseSourceFile` both produce a
`TestRef` keyed by `parseSourceFile`, so when chase renders, both
functions get the same `? tested by:` list. One is accurate, the
other is a false positive. The same collision happens across the
language boundary: a Haskell `runChase` and a PureScript `runChase`
share the index. The fix is to extract qualifiers from `Qual`
`RdrName`s on the Haskell side and from `CST.QualifiedName` on the
PureScript side, then match the qualifier prefix against the source
module name during attachment. That requires plumbing the source
module name through `attachCoverage`. Until then, coverage on
projects with naming collisions is a smell test only, not
authoritative.

Test coverage is also blind to higher-order indirection it cannot see
lexically: refs going through Template Haskell splice output, refs
hidden behind generic dispatch, refs accessed exclusively through a
record selector or class method where the underlying function name
never lexically appears. These are inherent static analysis limits;
chase does not run name resolution.

The PureScript reference-collector is a hand-rolled walker over
`CST.Expr` constructors. SYB would have picked up new upstream
constructors automatically, but the vendored CST does not derive
`Data` on all of its types. If the vendored PureScript version is
bumped and upstream adds new `CST.Expr` constructors, the walker
needs an explicit case for each one; refs inside missing constructors
will be silently dropped from the index. The Haskell walker via SYB
does not have this exposure.

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
`!` invariant format, the `>` consumed-by format, and the `?`
coverage-gap format are optimized for that constraint: one line per
fact, attached to the function it describes, sized for an LLM's working
memory.

You can use both. They serve different readers.

## Why this and not just paste the source?

Source has function bodies in it. Function bodies are most of the
tokens and very little of the information an LLM needs to reason about
your code at the architectural level. Pasting the source either burns
the context window before you've asked your question (for any real
codebase) or forces you to curate a subset, which puts the LLM in the
position of inferring everything you left out. Chase output is the
curation done once and reused.

The harder question is: if function bodies are the information you
actually need for some specific task, would chase get in the way?
Answer: yes, and that is the right time to fall back to pasting the
relevant function bodies in. Chase is for the architectural and
behavioral context that does not vary per question. The function body
of the specific thing you are debugging is per-question context, and
the bundle is small enough that you can fit both.

## Build

The flake provides a GHC 9.10 dev shell. The PureScript CST parser
must be vendored into the source tree before the first build:

    nix run .#vendor-purescript-cst
    nix develop
    cabal build
    cabal run chase -- <source-path> [-o output] [-a annotations.json] [-t test-dirs]

The vendor step only needs to be repeated when the upstream PureScript
version is bumped in `flake.nix` (or when you want to refresh from a
clean state). The resulting `vendor/purescript-cst/` directory is
expected to be present at build time but should not be committed
verbatim; gitignore it and treat it as generated.

The `chase-annotate` executable is built by the same `cabal build`
invocation but requires resolving grace (and grace's transitive
dependencies) at build time. If you do not want to build the LLM-facing
surface, build only the main `chase` executable: `cabal build chase`.
The library and the deterministic executable have no grace dependency
on the build graph.

## Status

Working tool. Tested on four real codebases:

- chase itself (Haskell, 9/9 modules ok, self-annotated, self-test-covered)
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
LLM. The PureScript parser itself was then switched from a line-based
column-0 anchor scanner to the vendored upstream CST parser once it
became clear that the heuristic scanner was both fragile (any deviation
from idiomatic formatting broke it) and lossy (no source positions on
sub-declaration spans, no structured error reporting, no real handle on
`else instance` chains or operator sections). The `openIssues`
annotation type was added after annotating pelotero-engine surfaced
enough known-bad behaviors that mixing them into `decisions` as
TODO-flavored entries was making the decision list incoherent.

Test coverage analysis was added as a separate orthogonal pass on top
of the existing pipeline, covering both Haskell and PureScript test
files. The deliberate choice to use static reference scanning (HsVar
via SYB on the Haskell side, an explicit CST expression walker on the
PureScript side) rather than HPC instrumentation keeps it offline,
fast, and decoupled from any test runner. It also means the `?` line
is a smell test, not a coverage guarantee. The honest framing is: a
function with no test references is almost certainly untested; a
function with many test references is at least exercised, but the
quality of that exercise is something static analysis cannot speak to.

The Grace integration is the most consequential addition. The shape of
the change is that chase now contains both halves of a closed-loop
LLM-in-tooling architecture: the parser bounds what the LLM can claim
about the names in the source, the typed Grace schema bounds what
shapes the LLM can return, and the drift checker that already validated
hand-written annotations now also validates LLM-generated ones, folding
its warnings back into the next prompt. Hallucinated function names do
not escape the loop. The deterministic chase library and its executable
do not learn about grace: the LLM-facing surface lives in a separate
cabal sublibrary plus a single executable plus the version-controlled
Grace prompt file, three artifacts you can replace independently of the
core. The win is architectural, not headline-feature: chase did not
gain a chatbot, it gained a typed feedback path between the LLM and the
existing static analysis. Content hallucination (wrong invariant text,
invented side effects, plausible-sounding but false `why` blocks) is
still on the human reviewer; this is not a hallucination-elimination
claim, it is a hallucination-of-names elimination claim.

The CLI was also rebuilt around `optparse-applicative` with
auto-detection for both annotations and test roots, so the common case
(`chase src`) needs no flags.

Honest open questions:

- Whether the `!` invariant format generalizes beyond the author's
  coding style and conventions.
- Whether the manual annotation labor is sustainable on codebases
  larger than a few thousand lines, especially the `consumes`
  curation, which is exactly the kind of cross-cutting metadata that
  rots fastest on a moving codebase.
- Whether the static-reference coverage pass is useful in practice or
  whether the false-positive rate from unqualified name collisions
  (now cross-language) makes it noisy enough to discount entirely.
  The fix (qualifier tracking) is known; whether it's worth doing
  depends on whether anyone hits the false-positive case enough to
  care.
- Whether the PureScript ref walker needs to be regenerated or
  retired the next time upstream CST adds new constructors. The cost
  of a missed constructor is silent: refs inside it just don't
  contribute to the index.
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
- Whether the Grace generator scales to the kinds of modules where
  annotations matter most. The generator does well on modules with
  strong type signatures and obvious behavioral conventions; it
  struggles on modules whose key invariants are about side effects the
  type does not advertise (the `last_seen_at`-on-read pattern is the
  canonical example). Whether this is fixable by extending the grace
  template, by including more of the source than the structural
  skeleton, or by giving up and treating the generator as a draft-only
  tool, is an open question.
- Whether the bridge architecture (chase deterministic, grace-driven
  annotation separate) holds up when more LLM-facing passes are added
  (coverage triage, refactor plans, cross-module `consumes`
  inference), or whether the bridge sublibrary grows into a catch-all
  that should itself be split.

Open to issues and PRs from anyone using this on their own code.

# Some Background

I vibe coded Chase over a couple of days because I was tired of pasting source into chat windows and watching the LLM hallucinate the parts I didn't include. The idea was straightforward. Function bodies are most of the tokens and very little of the architecturally interesting content, so I discard them, keep the type signatures, decorate them with hand-written invariants, and add a drift checker that complains whenever an annotation mentions a function the parser has never seen. Cut to the chase, as the name suggests.

I had never heard of Grace. I had not read its documentation. I had not seen its source. I worked from the problem in front of me, made the choices that felt right at each step, and arrived at a tool that does what I needed.

Last week I found Gabriella Gonzalez's work, and the experience was a strange one.

Grace is a typed configuration language with first-class support for LLM prompting. You write a Haskell function declaring the type you want back. Grace compiles that type into a JSON Schema, sends the schema and the prompt to the model, and the model cannot return a malformed shape. What arrives in your program is a typed Haskell value, carrying the same confidence the type system gives you for anything else. It is elegant.

The strange part was that Gabriella had built, in complete ignorance of my existence, the other half of the loop I had been constructing.

I think the framing is what makes this interesting. I did not labor over an architectural design for a year. I built a thing I needed, quickly, and it landed on a shape that Gabriella had arrived at independently. When two people, working independently, with no communication between them, arrive at structures that fit together this cleanly, the usual reading is that the underlying pattern is real. We did not invent the shape. The shape was already there. Two of us noticed it. Wadler and Simon Peyton Jones speak of monads and functors as discoveries rather than inventions, structures that exist in mathematics whether or not anyone names them. The closed-loop architecture, where a parser bounds the names an LLM can use and a typed schema bounds the shapes it can return, may be something of the same kind. It was sitting there waiting for someone to notice it. Two of us did.

Compose the two and the picture is clear enough. Chase already has a drift checker that validates annotation references against the parsed source. Plug its output into Grace's structured response cycle, and the same checker becomes a feedback signal. The model proposes annotations. The parser rejects any that reference functions it has never seen. The rejections fold back into the next prompt as a `driftWarnings` field. The model observes its own previous mistake. The loop converges in a retry or two.

Three constraints, independent of one another, that an agentic workflow has no path around:

- The schema constraint, from Grace: the structured output API refuses malformed shapes.
- The parser constraint, from Chase: every name in the response must exist in the actual source.
- The feedback constraint: drift warnings re-enter the next prompt automatically.

What remains is the model's freedom to invent things about behavior. It can describe side effects the function does not have, or compose a plausible reason for something the function does not do. That territory still belongs to the human reviewer, and I suspect it always will. Type systems constrain shape, not meaning.

The coincidence of the names is a small pleasure, and I admit I enjoy it. Cut to the chase, then fall from grace. Skip the preamble, then accept the constraint. The larger fact is that two functional programs, written independently, neither aware of the other, fit together as if they had been designed together. They were not designed together. The fit was already there.

Grace is the more ambitious project by a wide margin. It is a real typed language with parsing, type inference, a normalizer, and a careful treatment of structured output. Chase is two days of vibe coding with a drift checker attached. The asymmetry does not matter much. Both halves are doing the same kind of work: holding language models accountable to a structure outside themselves.

Type systems have always been the discipline that keeps programs honest. It is good to see them keeping language models honest as well.