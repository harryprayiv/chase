# chase

Structural compression of Haskell and PureScript source for LLM context, with the facts a compiler knows attached.

## What problem this solves

When you ask an LLM for help with a real codebase, you face a tradeoff: paste all the source and burn most of the context window before you've asked your question, or paste a curated subset and watch the LLM hallucinate the parts you left out. Both fail in different ways.

`chase` is a third option. It reads your Haskell and PureScript source files and emits a compressed representation that preserves the things an LLM cannot reconstruct from a type signature (architectural decisions, behavioral invariants, state machine topologies, magic constants, downstream dependents, known problems, test coverage gaps) while dropping the things it can (function bodies, import name lists, boilerplate). The result is a single text file you paste into a fresh conversation that gives the model substantially more useful context per token than either source or hand-written summaries.

Since 2026-10-25 chase also reads the code itself for the facts that used to need a human: which clauses and guards a function has, which error constructors it raises, which effectful calls outside the project it makes, who calls it, which constructors of a sum type are built from which, which constructors nothing builds, and what the compiler's own pattern checker warns about. These come from three deterministic sources: the parse tree, the compiler's HIE files, and the build log. None of them involve a model. The hand-written annotations are then checked against them, so an invariant that says a state machine has an edge the code does not witness, or a `consumes` entry naming a function that does not call this one, is reported as drift.

The design owes a debt to [Allium](https://github.com/juxt/allium), a behavioural specification language for LLM-driven engineering. Allium's rule shape (when, requires, ensures), its transition graphs with declared terminal states, its distinction between an open question and a known defect, and its checker's obligations (every declared edge witnessed, every non-terminal state with an exit, every requirement with a producer) are the structure chase's annotations and derived facts now follow. Allium's checker is deterministic and chase takes it as the model; Allium's skills are LLM-driven and chase does not use them, since the point of chase is to get the same properties from the compiler.

The name comes from "cut to the chase": skip the preamble, get to what actually matters.

## What it does, mechanically

For each source file, `chase` parses it with the appropriate language backend and extracts the structural skeleton.

**Haskell (`.hs`)** via `ghc-lib-parser` (GHC's own frontend exposed as a library):

- module name and language pragmas
- import declarations (collapsed to one line each, no name lists)
- fixity declarations (verbatim, including precedence and associativity)
- type signatures, both top-level and class methods (verbatim from source span)
- pattern synonym declarations (signature and definition merged when both exist, otherwise whichever is present)
- data, newtype, type alias, type family, data family, and closed type family declarations (verbatim from source span)
- class declarations (head only; method signatures are extracted separately as type signatures)
- instance declarations (head only; bodies are method definitions, which chase deliberately drops)

**PureScript (`.purs`)** via the actual PureScript CST parser, vendored from the upstream `purescript` compiler:

- module name and import declarations
- fixity declarations (`infixl`, `infixr`, `infix`)
- type signatures, both top-level and class methods (with class methods flattened into the top-level signature stream so invariants attach by name)
- foreign import declarations (kept as a separate `%foreign` section in the output)
- `data`, `newtype`, and `type` declarations (verbatim)
- `class` declarations (head only)
- `instance`, `else instance`, and `derive` declarations (head only)
- value bindings without a preceding signature are dropped (consistent with Haskell mode dropping function bodies)

Function bodies are dropped entirely in both modes. The module skeleton that remains is typically 3x to 10x smaller than the original source.

### The derive pass

Before the Haskell bodies are dropped, chase reads them. These facts are rendered under each signature with a `~` sigil, so a reader can tell them from the hand-written `!` invariants, and the bundle preamble says what each one means. They are may-facts: a clause may be unreachable, a failure may be on a dead path, a literal may be harmless. They are still deterministic and still facts a reader of the signature alone cannot know.

- `~ clause:` one line per clause of the function that has a constructor or literal pattern or a guard, as `pats | guard, guard`. This is Allium's `requires`, read from the code.
- `~ fails:` the error constructors and error strings raised in the body under `throwError`, `throwIO`, `throwM`, `throw`, `throwE`, `Left`, `fail`, `error` or `exitWith`, applied directly or through `$`.
- `~ literals:` the numeric literals, characters and short token-like strings in the body (no space, at most 22 characters), capped at six. `0`, `1` and `""` are dropped.
- `%const_candidate name = literal` for a zero-argument top-level binding whose body is a literal and that has no `%const` annotation.
- `%abstract T  hidden: ...  made by: ...` for a data type the export list exports without all its constructors, with the exported functions whose result type names it. This is the smart-constructor boundary, and it is the fact a model gets wrong most often.
- `~ called by:` the functions that reference this one, from the HIE files when present and otherwise from an import-resolved scan of the run's own roots; capped at eight with a count of the rest. `~ no callers in scanned roots` when there are none.
- A `=== DERIVED TRANSITIONS ===` block at the end of the bundle (or `DERIVED.chase` in per-file mode): for every data type with two or more constructors anywhere in the run, the edges from the constructors a clause or case alternative matches to the constructors it builds, each with the witnessing function. A clause that builds a constructor without matching one of the type gives an edge from `*`. Sum types are collected across all parsed files first, so a type declared in one module and transitioned in another is covered.

### The HIE backend

GHC writes a `.hie` file per module when built with `-fwrite-ide-info`. It is the compiler's own record of the module after renaming and type checking: every identifier resolved to its defining module and package, its type at every occurrence, and the context (binding, use, pattern) it occurs in. This is what `haskell-language-server`, `hiedb`, `weeder` and `calligraphy` are built on.

When a HIE directory is present, chase adds:

- callers resolved by name, which removes the qualifier-collision limitation of the lexical index and covers Template Haskell output, record selectors and class methods
- `~ uses:` the functions defined outside the project's packages that the body references and whose type at the use site mentions `IO`, `MonadIO`, `MonadUnliftIO`, `IOE` or `Eff`. This is the effects line, derived: `lookupSession` shows `Database.PostgreSQL.Simple.execute` whether or not anyone wrote it down.
- `~ unreferenced:` a binding that is not exported and that nothing in the HIE set references.
- A `=== CONSTRUCTOR USAGE ===` block: every declared constructor nothing builds (`%never_built`), and every constructor of a multi-constructor type nothing matches (`%never_matched`), each with the functions on the other side. These are Allium's missing-producer and unreachable-state findings.

The HIE format is tied to the GHC version that wrote it, so chase's `chase-hie` sublibrary is built with the real `ghc` package and reads files from that GHC only; the flake's `compiler` setting is where the version is chosen. A file from another GHC is reported with its path and skipped. The sublibrary is the only component that depends on `ghc`, and it does not depend on `ghc-lib-parser`, so the two sets of `GHC.*` modules never meet in one component.

To make a project write its HIE files where chase looks by default, put this in its `cabal.project.local`:

    program-options
      ghc-options: -fwrite-ide-info -hiedir .inference/hie

`program-options` applies to the local packages only. The path is relative to each package's directory, so a multi-package project writes one `.inference/hie` per package and chase reads the root one unless given `--hie DIR`.

### The compiler's warnings

GHC's pattern-match checker decides, with the real types in hand, which constructors a function does not handle and which of its alternatives can never be reached. That is the check an Allium transition graph calls dead transitions and deadlocks, done by the compiler the project already runs. GHC 9.6 writes these as text, so chase reads the build log: every block starting `file:line:col: warning: [GHC-nnnnn] [-Wflag]` becomes a `~ ghc [-Wflag]: message` line under the nearest signature at or above its line, or a `%ghc` line at the top of the block when it precedes every signature. Errors are skipped.

Warnings appear only for modules the compiler actually compiled, so the log has to come from a full compile:

    cabal build all --ghc-options=-fforce-recomp 2>&1 | tee .inference/build.log

The flags worth having on for this are the ones already in a `-Wall` project: `-Wincomplete-patterns`, `-Wincomplete-uni-patterns`, `-Wincomplete-record-updates`, `-Wpartial-fields`.

### Checks against the annotations

The derived facts are also what the hand-written annotations are checked against, printed as `drift:` lines by the run:

- a `%topology` edge no clause witnesses (no clause matches its source constructor and builds its target)
- a derived edge the topology does not declare, naming the witness
- a topology vertex outside its `terminal` list with no outbound edge (skipped when no `terminal` list is given, since absence of edges does not mean a state is terminal)
- a `consumes` entry whose leading `Module.function` token is not among the function's callers (entries that do not start with a qualified name are not checked, since `consumes` is wider than the call graph)

Plus the checks chase had before: an invariant, decision or open issue naming a function the file does not define; an invariant that restates its signature; an open issue claiming something is absent that the file defines; annotations for a module no parsed file defines.

### Annotations

For modules with annotations, `chase` also attaches:

- `%const` declarations for hoisted magic constants with notes
- `!` invariant lines after each function signature describing behavior that the type does not capture; the `intent:`, `requires:`, `ensures:`, `fails:` and `effects:` prefixes are the author's structured claims
- `>` consumed-by lines pointing at downstream code that depends on the function or its observable side effects
- `%decision` blocks recording first-class architectural decisions
- `%open_issue` blocks recording known problems with `blocking` and `affects` targets
- `%open_question` lines recording undecided design questions, which are not defects
- `%topology` blocks for state machines, with their terminal states

When test roots are configured (or auto-detected), `chase` also attaches `? tested by:` lines under the signatures that tests reference, and one `%untested` block per module listing the names no test references.

Annotations live in a single JSON file in the project's `.inference` directory. They are not stored in source comments. This is deliberate: it keeps source clean, lets chase check them against the structure and the derived facts, and lets you version annotations independently of the code they describe.

## Output formats

Two modes:

- **Bundle**: one concatenated `.chase` file with all module skeletons separated by `=== BEGIN <path> ===` markers, followed by the run-wide `=== DERIVED TRANSITIONS ===` and `=== CONSTRUCTOR USAGE ===` blocks. This is the format you paste into a conversation. Selected by giving an output path that ends in `.chase`.
- **Per-file**: a directory tree mirroring the source layout, one `.chase` file per source file, plus `DERIVED.chase` for the run-wide blocks. Useful for diffing across runs or grepping for specific symbols. Selected by giving any other output path.

Per-file output drops the source extension and appends `.chase`, so `src/Foo/Bar.purs` becomes `<out>/Foo/Bar.chase`. If you have both languages with colliding stems, current behavior is last-write-wins.

Nothing in a bundle depends on when chase ran, so the same sources, annotations, test roots, HIE files and build log give the same bytes. open-slop's ingest relies on that to skip bundles it has already embedded.

## Usage

    chase SOURCE [-o PATH] [-a FILE | --no-annotations]
                 [-t DIR[,DIR..] | --no-tests]
                 [--hie DIR | --no-hie] [--ghc-log FILE | --no-ghc-log]
                 [--skeleton FILE] [--no-derive] [-q]

The bare invocation auto-detects everything:

    cabal run chase -- backend/src

This scans `backend/src` for `.hs` and `.purs` files, writes a bundle to `./src.chase` in the current directory, and looks for the project the source belongs to: the nearest directory above `SOURCE` that has a `.inference` directory, within its repository. From that directory it picks up `chase-annotation.json` or `chase-annotations.json`, `hie/` for the HIE files, and `build.log` for the compiler's warnings, each when it exists. Test directories named `test/`, `tests/` or `spec/` adjacent to the source root are auto-detected. If auto-detection finds nothing, those features stay off silently.

Explicit forms:

    cabal run chase -- backend/src -o cheeblr.chase
    cabal run chase -- backend/src -o chase-output/
    cabal run chase -- backend/src -a ./annotations/cheeblr.json
    cabal run chase -- backend/src -t test,integration-test
    cabal run chase -- backend/src --hie dist-newstyle --ghc-log /tmp/build.log
    cabal run chase -- backend/src --skeleton .inference/chase-annotations.json
    cabal run chase -- backend/src --no-derive --no-tests --no-annotations
    cabal run chase -- backend/src -q

`--no-derive` turns off the whole derive pass and gives the output chase produced before it existed. `--skeleton FILE` is described below. `-q` suppresses the per-file progress lines; drift warnings, parse failures and the one-line summaries of what was read are still printed.

## Annotation file format

A JSON file keyed by module name. Module names are language-neutral: the same key works for a Haskell module or a PureScript module of the same name.

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
          "why": "rotateSessionToken issues the new token before the prior row is invalidated.",
          "blocking": ["multi-region replica reads"],
          "affects": ["rotateSessionToken", "lookupSession"]
        }
      ],
      "openQuestions": [
        "Should a session survive a password change, or should every session of the user be revoked?"
      ],
      "invariants": {
        "hashPassword": [
          "16-byte salt from getEntropy",
          "returns PHC string with argonOpts baked in"
        ],
        "lookupSession": {
          "intent": "Resolve a token to (Session, User) and bump last_seen_at",
          "requires": ["the token is one createSession issued, not a derived or truncated one"],
          "ensures": ["last_seen_at on the session row is set to now"],
          "fails": ["Nothing when the token is unknown or revoked; the row is not touched"],
          "effects": ["DBPool", "IO"],
          "notes": ["called on every authenticated request"],
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
      },
      "constants": [
        { "name": "tokenBytes", "value": "32", "notes": ["BUG: not configurable; the admin UI assumes 43 base64url characters"] }
      ],
      "topologies": [
        {
          "name": "SessionState",
          "vertices": ["Fresh", "Active", "Revoked"],
          "transitions": { "Fresh": ["Active"], "Active": ["Active", "Revoked"] },
          "terminal": ["Revoked"]
        }
      ]
    }
  }
}
```

Invariants accept either a flat array of strings (each becomes a `!` line verbatim) or an object with structured fields:

- `intent` renders as a leading `intent: ...` line
- `requires`, `ensures` and `fails` each render one `!` line per entry with that prefix; they are what must hold before the call, what holds after, and how it fails, the three parts of an Allium rule
- `effects` renders as one comma-joined `effects: A, B, C` line
- `notes` are appended as plain `!` lines
- `spec` entries each render as `spec: <input> => <expected>`
- `consumes` entries render with the `>` sigil and a `consumed by:` prefix, and are checked against the derived callers when they start with a qualified function name
- `hint` becomes the optional escape-hatch line like `body: ~30 lines`

Decision `why` accepts either a string or an array of strings. The `affects` array is drift-checked against the module's signatures and pattern synonyms.

Open issues use the same `name`, `what`, `why`, `affects` shape as decisions plus a `blocking` array. `blocking` is free text and is not drift-checked: it commonly names downstream services, features or executables rather than functions in the same module.

`openQuestions` is a list of strings. An open question is a design decision not yet made; an open issue is a defect in code that exists. Keeping them apart is one of the things Allium gets right, and the earlier format conflated them.

Topologies carry an optional `terminal` list. With it, a vertex outside the list that has no outbound edge is reported. Without it, that check is skipped rather than guessed.

## Generating a skeleton

`chase SOURCE --skeleton FILE` writes, for every parsed module that FILE does not have yet, an annotations entry with everything mechanical filled in from the derived facts:

- an invariant for every signature, with `requires` from the clause lines, `fails` from the throw sites, `effects` from the HIE uses, `consumes` from the callers, and an empty `intent` for the author
- `constants` from the constant candidates, with empty notes
- `topologies` from the derived edges, with every constructor as a vertex and an empty `terminal` list to fill
- empty `decisions`, `openIssues` and `openQuestions`

Modules already in FILE are left exactly as they are, so the command is safe to re-run as a project grows. This is the deterministic half of Allium's `distill`: the structure of every rule comes from the code, and only the meaning is written by hand or by a model.

The intended workflow is: run it once into the project's annotations file, then fill in `intent`, `ensures`, the terminal states, and delete the entries for functions that have nothing to say beyond their type. A skeleton entry with an empty intent renders nothing for that field, so an unfilled skeleton is not noise in the bundle, but it is not information either.

## What goes in a good annotation

The bar to add a `!` invariant line is "an LLM, looking only at the type signature and the derived `~` lines, would get this wrong." The derived lines already carry which constructors a function matches, which errors it raises, which effectful calls it makes and who calls it, so an invariant that restates any of those is redundant. Examples that meet the bar:

- what a side effect means: the `~ uses` line says `Data.Time.getCurrentTime`; the invariant says the timestamp is written to `last_seen_at` on every successful lookup
- when a failure happens: the `~ fails` line says `NotFound`; the invariant says NotFound is returned for a revoked token as well as an unknown one, so the caller cannot tell them apart
- specific encoding choices (URL-safe base64, hex, big-endian)
- ordering constraints (X must be called before Y)
- consequences of the implementation that affect callers (one extra DB round-trip, allocates a new connection, holds a lock)
- for PureScript: which foreign imports a function ultimately routes through

Examples that don't meet the bar:

- restating what the type already says
- restating a `~` line
- documenting parameters by name when names are already in the signature

The bar for a `>` consumed-by entry is "an LLM reasoning about a change to this function would benefit from knowing about this downstream dependency," and specifically a dependency the call graph does not show: a column read by another module, a late-binding subscriber, a middleware-injected header. The direct callers are already on the `~ called by` line.

The bar for an `openIssue` is "this currently doesn't work right, or works with a caveat the reader needs to know about, and is not yet resolved." The bar for an `openQuestion` is "this is a design choice nobody has made yet." If a thing is fixed or decided, delete it. The annotation file is not an archive.

## Generating annotations with grace

The structural skeleton and the derived facts tell the LLM what your code is shaped like and what it touches. The annotations tell it what that means. Three ways to produce them:

- **By hand**, when you have a feel for what an invariant for this function should say. Highest quality per annotation, does not scale past a few thousand lines.
- **By `--skeleton`**, which fills in everything mechanical and leaves the prose. The right first step on any new project.
- **By `chase-annotate`**, which hands the skeleton, the source and the callers to an LLM through Grace, validates the response against chase's drift checker, and writes the result in the JSON format chase already consumes.

### The closed loop

Chase's drift checker validates names: an invariant attached to a function the parser never saw, or a decision whose `affects` list mentions an unknown name, becomes a warning when chase renders. That validator is the same code path whether the annotations were hand-written or generated. Plumbing the same checker into the LLM loop turns it into a feedback signal.

The mechanics:

1. `chase-annotate` parses the source file, renders the chase skeleton with its derived lines, and hands it to the typed Grace expression at `grace/genAnnotations.ffg` along with the source and the callers.
2. Grace compiles the expression's return type to a JSON Schema and submits both the prompt and the schema to the model. The response is rejected by the API itself if the shape does not match. The Haskell side gets back a typed `GenAnnotations` value, where each invariant has `requires`, `ensures`, `fails` and free `body` lines.
3. Every line is checked against the function's own definition: a line naming something absent from that function's signature and body is removed and reported.
4. The typed value is converted to chase annotations, merged onto the parsed file, and run through the drift checker. Warnings come back as a list.
5. If the warning list is empty, write the file. If not, and the retry budget is not exhausted, call the generator again with the warnings inline in the prompt. The attempt kept is the best by score (good invariants, then fewer warnings), not the last.

What this closes off: hallucinated function names, and claims about the wrong function. What it does not close off: a plausible-sounding `why` that describes something the function does not do. The human reviewer is the only check on that.

### Usage

    chase-annotate src/MyModule.hs --key-file ~/.config/openai/key

The API key is never a command-line argument; it comes from `--key-file`, then `$OPENAI_API_KEY_FILE`, then `$OPENAI_API_KEY`. The endpoint is Grace's: set `OPENAI_BASE_URL` to point at a local gateway or any OpenAI-compatible server.

Flags: `--template FILE` (the Grace prompt, defaulting to the one installed with chase), `-o FILE` (defaulting to the project's annotations file in `.inference`), `--max-retries N` (default 2), `--hie DIR` and `--no-hie` (callers from the HIE files, auto-detected as for `chase`), `--roots DIR` (directories to scan for callers when no HIE files are read; defaults to the `hs-source-dirs` of the nearest `.cabal`), `-v`.

### What the model gets wrong

- Restates the type as an invariant. The template and the restatement check both push against it; delete those lines on review.
- Restates a `~` line. Same treatment.
- Misses real side effects. Less common now that the `~ uses` line is in the prompt, since the model is told what the body calls and asked what it means.
- Confuses `decisions` and `openIssues`, and both with open questions. The model is not asked for open questions at all; those are yours.

## Test coverage analysis

When invoked with test roots, `chase` records, for every signature, foreign import and pattern synonym, which test functions reference the name. A name with references gets a `? tested by:` line; the names with none are listed in one `%untested` block per module.

Coverage is computed by parsing every `.hs` and `.purs` file under the test roots, finding each top-level value binding, collecting every identifier that appears in its body, and resolving each one through the file's imports to the module that defines it. The walk is static. No tests are executed. A function with no test references is almost certainly not tested; a function with many is at least exercised, and the quality of that exercise is something static analysis cannot speak to.

Not covered: references introduced by Template Haskell splices, references hidden behind generic dispatch, references accessed only through a record selector or class method where the function name never lexically appears. The caller index has the same blind spots when it comes from the lexical scan and none of them when it comes from the HIE files; coverage still uses the lexical scan.

## Recommended workflow

1. Add the two `cabal.project.local` lines so the project writes its HIE files, and build once with the log captured. Run the bare extractor. Read the `.chase` output as if you had never seen the codebase.
2. Run `--skeleton` into the project's annotations file. Read the `%never_built`, `%abstract` and `~ ghc` lines first; they are the cheapest findings and usually the first real surprises.
3. Fill in the prose for the modules where the skeleton alone would not be enough to make a small correct change. Start with the smallest one with the most behavioral subtlety. Write the `terminal` lists for the topologies, and read the `drift:` lines the next run prints.
4. Test in a fresh LLM conversation: paste only the annotated bundle, ask for a small change, see what comes back. The mistakes the model makes tell you what is missing from your annotations.
5. When you find a problem mid-development that you cannot fix immediately, add an `openIssue`; when you find a choice you are not ready to make, add an `openQuestion`. The next LLM session will see both; future you will too.
6. Repeat for the next priority module.

## Parser notes

### Haskell

The Haskell parser is `ghc-lib-parser`, a snapshot of GHC's own parser exposed as a library. Source-level `LANGUAGE` pragmas are read out of the file header and applied to the parser's flags before parsing; `TemplateHaskell` also enables `TemplateHaskellQuotes` by hand, since the `$` lexer rule for splices is gated on the latter.

The derive pass walks the parsed tree with SYB. Constructor shapes that changed between ghc-lib-parser releases are matched by record field or by constructor name rather than by position, so a release bump either compiles or fails loudly. The one hand-maintained piece is the `Settings` record in `Chase.Parse.Haskell`, whose fields track the ghc-lib-parser release.

If a Haskell file fails to parse, chase reports the exact diagnostic from GHC's parser and continues with the rest of the bundle.

### PureScript

The PureScript parser is the actual PureScript CST parser, vendored from the upstream `purescript` compiler and renamed under `Chase.Vendor.PureScript.*`. It accepts exactly what `purs build` accepts. The vendor step is run separately from `cabal build`:

    nix run .#vendor-purescript-cst

PureScript has no derive pass yet: its files take part in a run without contributing clauses, edges or callers. The CST has the same clause, failure and transition shapes as the Haskell tree, and the PureScript compiler's CoreFn output is the resolved-name equivalent of HIE; both are the next step on that side.

## Limitations to know about

The output is not source. You cannot regenerate the codebase from a `.chase` file.

Every `~` line from the parse is a may-fact. A clause can be unreachable, a constructor built in a branch may never be returned, a literal may be a list index. The HIE-derived lines are exact for what they say but partial in one way: instance method bodies have no module-scope binding and are attributed to nothing, so a constructor used only inside an instance appears as `%never_built` or `%never_matched`, and a function called only from an instance has no callers.

The HIE backend reads files from the GHC that built chase and no other. A project on a different GHC gets a warning per file and the lexical caller index.

The build log carries warnings only for modules the compiler compiled, and a module compiled for several components appears once per component in the log; chase deduplicates by file, line and flag.

Annotations are free-form text where they are not structured. The structured fields (`requires`, `fails`, `consumes`, topology edges and terminals) are checked against the derived facts; `intent`, `ensures` and `notes` are not, because nothing deterministic knows what a function is for.

Module names are the annotation key. Two files declaring `module Main where` collide.

Class and instance declarations render only their head line. Standalone deriving declarations are not extracted on the Haskell side.

PureScript `.js` foreign runtime files are not inspected.

## Why this and not Haddock?

Haddock documents APIs for human readers of a public interface. `chase` is for an LLM helping you change your own private code. The audience already has the type signatures; what it does not have is the room to read paragraphs or the ability to fill in unwritten conventions. The `!`, `~`, `>` and `?` lines are one fact each, attached to the function they describe, sized for a model's working memory. You can use both.

## Why this and not just paste the source?

Source has function bodies in it. Function bodies are most of the tokens and very little of the information an LLM needs to reason about your code at the architectural level. Chase output is the curation done once and reused, and since the derive pass it carries the facts from the bodies that matter (what each function matches, raises, touches and is called by) without the bodies.

If function bodies are the information you need for some specific task, paste the relevant bodies in alongside the bundle. The bundle is small enough that you can fit both.

## Build

The flake provides a GHC 9.6 dev shell. The PureScript CST parser must be vendored into the source tree before the first build:

    nix run .#vendor-purescript-cst
    nix develop
    cabal build all
    cabal test chase-probes

The `chase-hie` sublibrary depends on the `ghc` package of that compiler; changing `compiler` in `flake.nix` changes which GHC's HIE files chase can read. The `chase-annotate` executable additionally requires grace; `cabal build chase` builds the deterministic executable alone.

The probes in `test/Probe.hs` are named after the rules in `.inference/chase.allium` and `.inference/chase-derive.allium`: U1 to U5 for the Unison renderer, the annotation loader, bundle reproducibility, source discovery and coverage resolution; D1 to D5 for the three topology checks, derived edges across modules, and the consumes check.

## Status

Working tool. Tested on chase itself (23 modules, self-annotated, HIE and build log in the loop, byte-identical across runs), cheeblr backend and frontend, and pelotero-engine.

The derive pass, the HIE backend, the build-log reader, the skeleton generator and the structured annotation fields were added on 2026-10-25 after reading Allium. The specification of what they do is `.inference/chase-derive.allium`; `.inference/chase.allium` covers everything else.

Honest open questions:

- A clause that matches constructor A and calls `show B` produces the edge A to B. Limiting built constructors to return positions is tractable syntactically but misses constructors passed to a state-setting function.
- Instance method bodies should become bindings named `Class.Type.method` so their references count.
- A multi-package project writes one `.inference/hie` per package directory; auto-detection should probably walk the `hs-source-dirs` of every `.cabal` under the root.
- The caller index parses every source file a second time when no HIE files are present.
- The PureScript derive pass: whether to port the syntactic pass before CoreFn exists, or whether CoreFn makes it redundant.
- Whether the `%untested` block and the capped `~ called by` lines are the right density, or whether a bundle needs a budget flag.

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
