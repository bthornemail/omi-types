# OMI Types

`omi-types` is a fork of `omi-canvas` (upstream: `git@github.com:bthornemail/omi-canvas.git`) that keeps the whole canvas type engine and adds the type surface of *The OMI Protocol in Pure Haskell - Corrected* (`boxedPINE/_archive/`), rewritten to this package's rules: no host `Int`, `String` or `Maybe` in the core, only `Bit`, `Nibble`, `Byte`, `Word16`, `Word32`, `Relation` and types built from them.

## Added Types

| Module | Types | From the Corrected document |
|--------|-------|-----------------------------|
| `OMI.Ruler` | `Ruler` | Part III: two relation halves that trade places each step |
| `OMI.Orbit` | `Block`, `BlockReading` | Part V: the orbit of 60 and its four blocks, read from bit patterns |
| `OMI.BQF` | `QuadraticForm` | Part IV: the forms as declared coefficients; evaluation is a value read, left to adapters |
| `OMI.Handler` | `Position` (all ten), `Radix`, `Kind`, `Separator`, `Deviation`, `Operation` | Part VI: `Char` fields become small enums |
| `OMI.BIOS` | `Endpoint`, `Interior`, `GatePath`, `BIOS`, `BootKernel` | Part VIII: `Path` → `GatePath`, `Kernel` → `BootKernel` to avoid clashes |
| `OMI.Torus` | `Scope`, `Shape`, `Cell`, `Distance`, `Weight` | Part IX: distances and weights are enums, not integers |
| `OMI.Protocol` | `Protocol` | Part X: declarations are `SExpr`, decided by the canvas pipeline |
| `OMI.Types` | every type exported in `src/OMI` (99): the three distinct `Bool`s and the second `DecisionTable` as `FanoBool`, `LispBool`, `RelationBool`, `LispDecisionTable` | Part XI: one import |

Also carried from the earlier merge of the `omi` reference: `OMI.Bits`, `OMI.Delta`.

Not ported: the document's string `Grammar` record. Here the `Position` constructors are the grammar; the string patterns live in `boxedPINE/rosetta/src/grammar/grammar.ts`.

`OMI.Try` makes try / catch / finally total: an `Attempt` is either a tried value or a caught `Deviation` that carries the structured coordinate (position, expected, actual, difference). The difference is the repair, so a caught attempt is open for another try. BIOS paths start from the relation they are given and try the frame first.

`OMI.WSN` is Wirth syntax notation typed over `Byte` (`Syntax`, its constructors hidden): `parse` reads WSN text by Wirth's own seven productions, `render` writes it back one production a line, and `accepts` asks whether a sentence belongs to a production, as unbound/PLAN9's `zlazy` does, under Henderson and Morris's two rules: a step is taken only when the question reaches it, and never twice (a table keeps each production's ends at each place; `expanded` lists them, and the tests check their number against `zlazy`'s for every stack sentence); an identifier with no production matches nothing, since WSN leaves `letter` undefined. `cast` settles it by a zeroth through boxedPINE's `terms.mjs` feature pipeline (consonants upper, vowels lowered 0x20, digits kept): YHWH casts no vowel, YAHWEH casts `a` and `e`. The tests run the stack element (`wsn/stack.wsn`) under both casts, with the answers Plan 9 gave, and the logic grammar's adder. Its list operations are folds: right folds where the search may stop at the first whole match, a left fold where the zeroth's order is kept.

`OMI.Polytope` types the polyhedra of unbound's TSX showcase, each type once: `PolyStructure` (V, E, F, the Schläfli {p, q}, the Betti numbers), which Polyhedral Spin Explorer and SCGNN VM both declare field for field, and `Vertex` as the union of the two templates' Vertex types (`Vertex2` in the plane, `Vertex3` in space); names, ids, labels and colours are host strings and stay outside the core. Its laws run on the kernel's bits (a ripple-carry adder and shift-and-add on `Word16`): `euler` (V + F = E + 2), `eulerPoincare` (V + F + b1 = E + b0 + b2, so the templates' `universalSpin`, which multiplies by the sign of the difference, is 0 for every polyhedron) and `regular` (p F = 2 E = q V). The tests run Polyhedral Spin Explorer's nine rows as written: all keep Euler and Euler-Poincaré; the five Platonic rows and its custom cube are regular; its truncated tetrahedron, cuboctahedron and truncated cube carry a Schläfli symbol their counts do not keep, and the first two carry another solid's counts. And the Betti numbers are computed, not declared: `betti` gives b_k = dim C_k - rank d_k - rank d_(k+1) over Z/2, `rank` by elimination on bits, the method of `/home/main/devops/waveform-core` (columns added by symmetric difference). The tests derive each complex from bits (the tetrahedron, every triple of four vertices; the cube, the 3-bit words, edges one bit apart, faces one bit fixed; the octahedron, its dual) and get (1, 0, 1) for each, as the templates declare; a torus, a 3 x 3 grid glued at its edges, gets (1, 2, 1), so the computation is not a constant.

`PLiteral` and `PStruct` are coordinate nibbles: indices 0..15 into the first 16 indices of the buffer. Sixteen of them make the first 8 byte indices (`coordinateFrame`).

---

## Inherited: OMI Canvas Type Engine v0

`omi-canvas` is the Haskell type engine for discovering OMI declarations, reducing them through the canonical pipeline, witnessing memory reconciliation, and projecting accepted relations onto canvas surfaces.

This repository does not make Canvas authoritative. Canvas is a projection layer over the canonical construction pipeline:

```text
Declaration
  -> Citation
  -> Gauge
  -> WittgensteinOperator
  -> TruthGate
  -> DecisionTable
  -> KarnaughMap
  -> Combinator
  -> Delta
  -> Blackboard
  -> ProjectionFace
  -> Attestation
```

The milestone anchor is:

```text
a97d66f Add Haskell OMI canvas type engine
```

## Public API Boundary

The exported module chain is:

```text
OMI.Gauge
  -> OMI.Wittgenstein
  -> OMI.TruthGate
  -> OMI.DecisionTable
  -> OMI.Karnaugh
  -> OMI.Pipeline
  -> OMI.Canvas
```

Adapter and witness layers sit around that chain:

```text
OMI.Markdown -> OMI.Lisp declarations
OMI.Memory -> OMI.Reconcile -> OMI.VCS
OMI.Carrier -> OMI.Stream / OMI.Net.Frame / OMI.Gossip.Types / OMI.Runtime
OMI.Canvas.JSON <- OMI.Canvas / OMI.Pipeline output
```

The API enforces the doctrine:

- constructors are hidden at authority boundaries
- callers cannot forge `Attestation` or `ProjectionFace`
- `Canvas` projects relation structure but does not accept it
- `TruthGate`, `DecisionTable`, and `KarnaughMap` classify, declare, and reduce
- `OMI.Pipeline` is the public path to the attestation boundary

## Core Modules

- `OMI.Kernel` defines the minimal OMI kernel values: `Null`, `Bit`, `Pair`, `Nibble`, `Byte`, `Word16`, `Word32`, and `Relation`.
- `OMI.Core` defines canonical null values and relation wrappers.
- `OMI.Lisp` parses OMI-Lisp declarations and preserves the existing decision-table S-expression shape.
- `OMI.Gauge` validates the F* gauge family and canonical pre-header.
- `OMI.Wittgenstein` maps gauge low nibbles onto the 16 local truth operators.
- `OMI.TruthGate` classifies declarations against gate families.
- `OMI.DecisionTable` exposes typed decision-table extraction.
- `OMI.Karnaugh` reduces decision tables into typed Karnaugh regions.
- `OMI.Pipeline` composes the canonical stage functions.
- `OMI.Canvas` exposes typed projection nodes, edges, and faces.
- `OMI.Carrier` models the minimal unary carrier fragment: carrier prefix, CAR addressed frame, CDR addressed frame, and active unary register.
- `OMI.Markdown` extracts front matter, OMI-Lisp fences, decision-table fences, canvas fences, evidence spans, and FS/GS/RS/US scope metadata. It does not accept.
- `OMI.Canvas.JSON` encodes accepted projection faces into JSON Canvas-shaped output. It does not validate or receipt.
- `OMI.Scope` models FS/GS/RS/US scope as an OMI relation.
- `OMI.Memory` models bitboards, bit-blips, blackboard faces, carrier faces, reconcile state, and version witnesses.
- `OMI.Reconcile` builds the reconciliation path from bitboard/bit-blip witnesses to attestation.
- `OMI.VCS` records only accepted reconciliation values that already have a receipt boundary.
- `OMI.Stream`, `OMI.Net.Frame`, `OMI.Gossip.Types`, and `OMI.Runtime` are pure adapter shells for stream/runtime concepts (historical archive at `/home/main/omi/omi-types/archive/`). They do not import runtime IO, snapshot hash authority, WAL authority, or storage authority.

## Verification

Run the package path first:

```sh
cabal build all
cabal test all
```

Keep the direct GHC regression check green:

```sh
ghc -isrc -fforce-recomp -fno-code app/Main.hs
```

## Repository Boundaries

- Runtime C, firmware, and ISA behavior remain in `/home/main/omi/omi-isa`.
- Coq proofs remain in `/home/main/omi/omi-axioms`.
- This repo mirrors the proven concepts as Haskell types and total stage functions.
- JSON Canvas is adapter output only; it is not an authority surface.
- Hashes/checksums used by adapters are evidence metadata only. OMI identity remains the addressed place-value relation, not a digest.
- There is no heavy `OmiFragment` schema. Carrier exchange is prefix + CAR frame + CDR frame + unary register; composition carries meaning and receipt accepts.

## Polytope Toolbox

The polytope registry (moved to `/home/main/omi/omi-types/archive/polytope/`) provides 160 polytope templates and 4 OMI configuration witnesses as projection data. The registry is not authority — rendering is projection only.

- Haskell types and JSON loaders: `omi-types/archive/polytope/OmiCanvasPolytopeRegistry.hs`
- Obsidian/Canvas toolbox: `omi-types/archive/polytope/omi-polytope-toolbox.canvas`
- Normalized template JSON: `omi-types/archive/polytope/omi-polytope-toolbox.normalized.json`

Usage flow:

```text
registry JSON
  → Haskell Registry type (omi-types/archive/polytope/)
  → filter/group by dimension/category
  → render as toolbox cards
  → clone/drag into canvas as snap-to-grid templates
  → attach accepted OMI relation metadata only after validation
```


