# Revision history for chase

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.


It works, and the resolution is real, not hash-mush. `uniDork.processOne` came back with all 24 `PreparedCommand` arguments typed, `Db.upsertMovieSql` rendered its full effect-row signature, record `fields:` are folded, and aliases like `Rename.priorProposedWith > aka: Rename.findPriorTargetForCrcWith` deduped correctly. The one timeout (`exercises.ex4_oauth.doc.pt1`) degraded to hash-only and shouted about it, which is exactly the loud-failure behavior we wanted. The pipeline is sound.

Now the blunt part: roughly a third of this bundle is not uniDork. The `exercises.*` and `examples.*` namespaces are the Unison Cloud tutorial (microblog, oauth, websockets, todoApp, the whole `resultsService` admin tree, `validator`, all the `submit.*` and `solutions.*`). That's easily 300 of the 963 resolved definitions. For a tool whose entire reason to exist is signal density for an LLM, shipping the Unison Cloud onboarding course inside your media-pipeline context is dead weight that buries the actual code and burns tokens. It is also what caused your single timeout: `exercises.ex4_oauth.doc.pt1` is a fat `Doc`, slow to render, and it hit the 30s default.

Two ways to fix it, and they are not mutually exclusive:

The hygiene fix is the real one. That tutorial code almost certainly does not belong in `uniDork/main`. If you `fork` or `move` it into its own project (or just delete it from this branch), the bundle cleans itself and you stop paying to resolve 300 irrelevant definitions every run. Worth checking why it landed top-level in your media project's main branch in the first place, since it suggests a stray `pull` into the wrong namespace.

The tool fix is a namespace exclude, which you want regardless because you will hit vendored or tutorial noise again. Add a repeatable `--exclude` that drops names by prefix at walk time, before fetching, so `--exclude exercises --exclude examples` cuts the noise and ~300 fetches in one move. While in there, the companion trim from last time pays off now: drop `Doc`-typed terms using the same listing-tag mechanism as constructors. That removes `README`, `ReleaseNotes`, and every `.doc` value, which are prose rather than code skeleton and are the timeout-prone ones anyway.

To fold both in as whole pasteable files I need to see the current `Chase.Unison.Extract.hs`, since the filter belongs in the walk and plan alongside `isDependencyNamespace` and the constructor-drop logic, and I am not going to guess at that plumbing and hand you a snippet that does not paste clean. Paste it and I will return the full module with `--exclude` and `Doc`-dropping wired in, plus the one-line `Main.hs` change to parse the flag.

The hash-drift ledger stays next in line, but it should wait. Drift detection on a bundle that is a third tutorial noise is premature, and `--exclude` is what makes the annotation pass worth doing.