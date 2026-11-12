# Local execution environment

**Figure version:** 1.0.6  
**Status:** independently closed publication asset · non-normative local implementation view  
**Independent closure:** 16/16 PASS · 0 material findings  
**NCS alignment:** PASS with declared font-family limitation  
**Generative-image AI:** not used

> **Authority boundary.** Figure 2 explains how selected MM execution, filesystem, identity, least-privilege, toolchain and evidence-boundary requirements are materialized in the qualified local adapter. It is subordinate to Figure 1 and the certified MM authorities and grants no scientific, GEE, production or journal-submission authority.

![Diagram of the MM qualified local execution adapter. Windows host and WSL2 Ubuntu boundaries lead to separated mmadmin, mm-a1 and mm-review roles. mm-a1 and mm-review are connected to their sandboxes by non-directional identity associations. Directed access arrows show A1 reading input and review, writing output and the primary Gate workspace bounded within scratch, and reviewer evidence access originating from review and reaching the Reviewer sandbox through an external right-edge port. The figure separates immutable Codex toolchain, operational credentials and child network capability; shows one-shot fail-closed harness controls; and preserves M4 HOLD_INFRASTRUCTURE_PREMODEL and M4R1 HOLD F03. Exported visible connector geometry is mechanically checked after target-panel overpaint and stroke. No scientific, GEE or production runtime is authorized.](assets/figures/mm-local-execution-adapter-v1.0.6.svg)

**Figure 2 | Qualified local execution adapter and controlled development environment for the Module Minimum (MM) system. Non-directional lines bind execution and review identities to their systemd sandboxes, whereas directed arrows represent typed read, write and evidence-access flows. Input/review are read-only authorities; output and the primary Gate workspace bounded within scratch/ are the only displayed mutable targets. The evidence-read route uses an external right-edge Reviewer port and preserves physical separation from A1. Exported PDF/SVG visible-ink geometry, not nominal source geometry alone, controls connector acceptance. The local adapter remains qualified without authorizing scientific, GEE or production runtime; M4 remains NOT ESTABLISHED and M4R1 remains HOLD F03.**

## What the figure establishes

- `mmadmin` is the administrative/bootstrap actor; it is not the scientific mutable actor.
- `mm-a1` is the sole mutable non-admin execution identity within the qualified boundary.
- `mm-review` is the independent read-only reviewer identity and has no mutable output/scratch access.
- `input/` and `review/` are authority/read surfaces; `output/` and the primary Gate workspace are the displayed mutable targets.
- the primary Gate workspace is explicitly bounded within `scratch/`; global scratch is not presented as generally writable;
- identity bindings are non-directional associations, while read/write/evidence accesses are typed directed flows;
- toolchain, credentials/configuration and command-child network capability are separated;
- M4 remains `NOT_ESTABLISHED / HOLD_INFRASTRUCTURE_PREMODEL`; M4R1 remains `HOLD F03`;
- a qualified local adapter does **not** authorize primary scientific runtime.

## Independent publication readback

The final targeted independent closure reconstructed the PDF and SVG vector geometry without executing or rerendering the generator.

| Publication predicate | Independent result |
|---|---:|
| Required checks | **16/16 PASS** |
| Material findings | **0** |
| PDF/SVG minimum visible terminal | **3.944167 mm** |
| Frozen minimum | **3.000000 mm** |
| Producer robustness target | **3.250000 mm** |
| Evidence-route clearance from A1 | **1.347155 mm** |
| Minimum port corner clearance | **3.449994 mm** |
| Five-port incidence | **perpendicular** |
| Negative fixtures | **12 genuine predicates** |
| Publication status | **PUBLICATION_ASSET_ELIGIBLE** |

## Editorial and technical profile

- final canvas: **180 × 165 mm**;
- visible text: **5.0–6.8 pt**;
- visible strokes: **0.45–0.80 pt**;
- SVG: **93 editable text elements**, **0 raster image elements**;
- PDF: single-page vector asset with embedded CID TrueType fonts and no raster images;
- PNG fallback: 2125 × 1948 px at approximately 300 dpi;
- colour: accessible RGB with no red–green semantic dependence and no rainbow scale;
- connector labels: black with white backing;
- font-family limitation: embedded **Liberation Sans**; no Arial/Helvetica claim.

## Reproducibility and evidence

- [Vector SVG](assets/figures/mm-local-execution-adapter-v1.0.6.svg)
- [Independent vector readback](assets/figures/evidence/MM_LOCAL_EXECUTION_ADAPTER_FIGURE_V1_0_6_INDEPENDENT_VECTOR_READBACK_v1.0.0.json)
- [Independent closure report](assets/figures/evidence/MM_LOCAL_EXECUTION_ADAPTER_FIGURE_V1_0_6_FINAL_TARGETED_INDEPENDENT_CLOSURE_PASS_REPORT_v1.0.0.md)
- [Independent closure result](assets/figures/evidence/MM_LOCAL_EXECUTION_ADAPTER_FIGURE_V1_0_6_FINAL_TARGETED_INDEPENDENT_CLOSURE_PASS_RESULT_v1.0.0.json)
- [Final Control Plane adjudication](assets/figures/evidence/MM_LOCAL_EXECUTION_ADAPTER_FIGURE_V1_0_6_FINAL_CONTROL_PLANE_ADJUDICATION_v1.0.0.md)

## Publication boundary

This page is a documentation projection only. It does not authorize scientific, DSSAT, GEE, Sobol, production runtime or journal submission. Historical HOLDs remain preserved in project provenance.