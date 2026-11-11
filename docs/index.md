---
title: MM Scientific Software Development and Assurance Methodology
subtitle: Controlled methodological derivative of the certified MM scientific execution architecture and assurance manual
version: 1.0.1
status: CONTROLLED_DERIVATIVE_CANDIDATE
language: en-US
project: Artigo 2 - MM
issue_date: 2026-11-11
normative_authority: false
runtime_authority: false
publication_authority: false
---

# MM Scientific Software Development and Assurance Methodology

**Version:** 1.0.1  
**Project:** Artigo 2 - MM  
**Document class:** controlled human-readable methodological derivative  
**Normative status:** non-normative; certified project authorities control  
**Runtime consequence:** none  
**Publication consequence:** none

> **Control rule.** This Markdown explains and operationalizes the development methodology supported by the certified MM authorities. It does **not** supersede the certified Architecture, Manual, Project Evidence Base, exact Git commits/trees, certified packages, or run-specific Gate authorities. Any discrepancy is a `HOLD`, not an editorial choice.

## 1. Purpose

This document defines the **scientific software development and assurance methodology** used to develop, verify, validate, integrate, release, and govern the Module Minimum (MM/MHSMM) scientific software system. Its purpose is to make the development method independently inspectable, reproducible, traceable, and suitable for rigorous scientific peer review.

The methodology is derived from the project's certified **Scientific Execution Architecture v1.0.2**, certified **Scientific Software Development & Assurance Manual v1.0.1 RC3**, certified **Project Evidence Base v1.0.0**, qualified local execution-adapter record, Harness Reliability Profile, and controlled Technical Reference. It is also informed by NASA software engineering, software assurance, IV&V, and model/simulation credibility guidance, without claiming NASA compliance, endorsement, affiliation, or certification.

The method is designed for a multidisciplinary scientific system in which geospatial processing, mechanistic plant modeling, animal response, stochastic economics, uncertainty quantification, data engineering, provenance, and scientific inference interact. In this context, software correctness is necessary but not sufficient: the project must also establish that the **scientific problem, requirements, architecture, interfaces, tests, controls, and acceptance criteria are the right ones for the intended scientific use**.

## 2. The governing assurance question

The central assurance question is deliberately two-dimensional:

1. **Was the system built correctly?**  
   Does the implementation conform to the specified requirements, architecture, interfaces, contracts, numerical rules, security constraints, and release criteria?
2. **Was the correct system built?**  
   Do the requirements, architecture, controls, tests, and acceptance criteria represent the intended scientific use and the actual scientific question?

NASA software engineering guidance distinguishes these dimensions as **verification** and **validation**. Verification evaluates whether software products satisfy specified requirements and are produced correctly; validation evaluates whether the as-built product fulfills its intended use in its intended environment. NASA IV&V guidance further requires independent examination of artifacts for properties including **correctness, completeness, consistency, traceability, and testability** across requirements, architecture/design, code, interfaces, and testing [EXT-NASA-NPR7150], [EXT-NASA-SWE067], [EXT-NASA-IVV], [EXT-NASA-SA].

For MM, this distinction is a methodological invariant:

```text
IMPLEMENTATION_CONFORMS_TO_REQUIREMENTS
        does not imply
REQUIREMENTS_REPRESENT_THE_RIGHT_SCIENTIFIC_SYSTEM
```

and:

```text
REPRODUCIBLE_EXECUTION
        does not imply
SCIENTIFIC_VALIDITY
```

A Gate cannot receive a scientific PASS merely because code ran successfully, a build was reproducible, or outputs were produced. The controlling evidence must support both the implementation claim and the intended-use/scientific-validity claim that applies to that Gate.

## 3. Authority hierarchy and evidence discipline

### 3.1 Certified controlling authorities

The current certified baseline represented by this methodology is:

| Authority | Certified state | Immutable control |
|---|---|---|
| Scientific Execution Architecture | `MM_SCIENTIFIC_EXECUTION_ARCHITECTURE_v1.0.2_RC2` | Certification Commit B `75da6a6a1006c445eff4d589396813d6429fc51e` |
| Scientific Software Development & Assurance Manual | `MM_SCIENTIFIC_SOFTWARE_DEVELOPMENT_AND_ASSURANCE_MANUAL_v1.0.1_RC3` | Certification Commit B `01e9a94a7909af9436443ad016e5df06302576b4` |
| Project Evidence Base | `v1.0.0` | Certification Commit B `650dd2cc8a8b04e993309485fe4f72b07858dfa6` |
| Production-reference `main` | reference only | `af64a215ce8c67548802df1a3997015db37ec0f6` |

The Architecture is the superior constitutional authority. The Manual is a subordinate normative refinement: it may operationalize, restrict, or add evidence requirements, but it cannot make an architecturally forbidden behavior admissible.

Formally, the certified relationship is:

```text
B(Architecture ∧ Manual) ⊆ B(Architecture)
Override(Manual, Architecture) = false
```

### 3.2 Evidence classification

Every material claim should be labeled internally by its evidentiary status:

- **DIRECT** — measured or read directly from admitted bytes, a runtime observation, or an authoritative source;
- **BOUND** — established through an exact cryptographic or referential binding to a DIRECT authority;
- **DERIVED** — logically derived from admitted predicates or source relationships;
- **HISTORICAL** — valid as a preserved prior observation but not freshly re-measured;
- **NOT_ESTABLISHED** — not proven by the available evidence.

No `DERIVED` statement may be presented as a fresh physical observation. No `HISTORICAL` result may be silently promoted into current runtime authorization. No gap may be filled by conversational plausibility.

### 3.3 Project Source discipline

Project Sources form a canonical reusable working knowledge base, not the complete event log. Intermediate attempts, commands, transient failures, and superseded states remain in conversation/Gate provenance; durable authorities and terminal states are represented in canonical sources. This prevents event history from being mistaken for the active scientific baseline.

## 4. Architecture of the MM scientific system

MM separates **scientific modules** from the **execution-control infrastructure** that governs them.

### 4.1 Scientific modules

```text
MM SCIENTIFIC SYSTEM
│
├── MM-ENVIRONMENT
│   └── climate, soil, terrain, land cover, geospatial support, GEE
│
├── MM-PLANT
│   └── DSSAT / forage / plant response
│
├── MM-ANIMAL
│   └── animal production and biological constraints
│
├── MM-ECONOMIC
│   └── economics, risk, Monte Carlo, decision outputs
│
├── MM-INTEGRATION-CORE
│   └── contract-bound cross-module orchestration
│
└── MM-DATA-CATALOG
    └── data contracts, metadata, quality, provenance, catalog interfaces
```

Modules are independently evolvable but may interact only through explicit, versioned contracts. An internal state in one module is not an implicit API for another module.

```text
MODULE OUTPUT
      ↓
VERSIONED DATA CONTRACT
      ↓
CATALOG RECORD
      ↓
PROVENANCE / EVIDENCE BINDING
      ↓
INTEGRATION ADAPTER
      ↓
CONSUMER MODULE
```

### 4.2 Minimum inter-module contract

Each material interface must specify, as applicable:

- schema and semantic version;
- variable definition and scientific meaning;
- units and dimensional conventions;
- valid domain/ranges and biological or physical constraints;
- missingness, null semantics, and forbidden implicit defaults;
- spatial reference, including CRS, coordinate operation where material, axis semantics, grid/affine transform, resolution, extent, support, mask, and resampling rules;
- temporal reference, aggregation window, calendar, and time-zone semantics;
- provenance, acquisition/version identity, transformations, and evidence links;
- compatibility/deprecation rules;
- positive, negative, boundary, and off-nominal contract tests.

## 5. Five-plane execution architecture

The scientific system is governed through five coupled planes.

| Plane | Function | Core invariant |
|---|---|---|
| **Control Plane** | defines Gates, scope, authorities, permissions, terminal predicates, and adjudication | no scientific side effect |
| **Execution Plane** | performs explicitly authorized mutable work | exactly one mutable actor per Gate |
| **Authority Plane** | stores immutable controlling identities | no implicit “latest” |
| **Data / Catalog Plane** | governs computational data contracts and discovery metadata | catalog metadata does not substitute for computational semantics |
| **Evidence / Provenance Plane** | stores claims, evidence, provenance, receipts, readbacks, conflicts, and gaps | evidence must be explicit, attributable, and decision-linked |

The local Linux/Codex environment is an **Execution Adapter**, not the architecture itself.

## 6. Gate-governed scientific development

A **Gate** is a versioned stateful control boundary. It binds:

- exact input authorities;
- scientific question and intended decision;
- actors and roles;
- capabilities;
- preconditions and predicates;
- permitted side effects and budgets;
- objective evidence requirements;
- verification and validation methods;
- terminal classes;
- independent readback/review;
- next allowed state;
- `STOP=true`.

A successful tool invocation is never sufficient evidence of a scientifically valid state.

### 6.1 Minimum Gate contract

```text
GATE_ID
MODE / FAIL_CLOSED_POLICY
CERTIFIED_INPUT_AUTHORITIES
SCIENTIFIC_QUESTION / INTENDED_USE
CONTEXT / MATERIALITY / MODULES / INTERFACES
ACTIVE_ACTORS / SOLE_MUTABLE_ACTOR
CAPABILITY_MANIFEST
REQUIREMENT_AND_CONDITION_PLAN
VERIFICATION_PLAN
VALIDATION_PLAN
OBJECTIVE_EVIDENCE_PLAN
TERMINAL_STATE_PARTITION
SIDE_EFFECT_BUDGET
READBACK_METHOD
PASS / HOLD FIELD SET
NEXT_ALLOWED
STOP=true
```

### 6.2 Total and exclusive terminal partition

Before consequential execution, the admissible state space must map to a **total and exclusive** terminal partition:

```text
for every admissible state s:
    exactly one terminal class C_i(s) is true
```

The partition must explicitly cover missing values, invalid values, null states, empty support, partial support, control failure, transport failure, off-nominal behavior, and unrelated failure modes. A natural-language statement that cases are “complete” is insufficient when mechanical enumeration or executable validation is feasible.

### 6.3 Fail-closed rule

Missing, ambiguous, contradictory, stale, unclassified, or insufficient evidence yields `HOLD`, never inferred PASS or authorization.

## 7. Scientific software development lifecycle

The MM lifecycle is an evidence-producing sequence, not a coding-first workflow.

### Phase 0 — Scientific need and intended use

Define:

- scientific question;
- intended use and decision context;
- estimand or target quantity;
- domain of validity;
- stakeholders/users;
- consequences of false positive, false negative, and unsupported outputs;
- materiality and risk.

**Validation question:** Is the proposed software problem the problem that actually needs to be solved?

### Phase 1 — Requirements engineering

Requirements must be:

- correct relative to the scientific need;
- complete for the declared scope;
- internally consistent;
- unambiguous;
- traceable upward to scientific/system needs;
- traceable downward to architecture/design/code/tests;
- individually verifiable or otherwise explicitly justified;
- testable where testability is required.

Requirements must not be approved merely because they are implementable.

### Phase 2 — Architecture and interface design

Architecture allocates requirements to modules, components, interfaces, authorities, data contracts, and execution boundaries. The review asks both:

- **verification:** does the architecture satisfy the baselined requirements?;
- **validation:** is the architecture suitable for the intended scientific use and credible under realistic operating/scientific conditions?

All material cross-module relations must be explicit and versioned.

### Phase 3 — Detailed design and algorithm specification

The design record must identify:

- algorithm and mathematical formulation;
- assumptions and regularity conditions;
- numerical method and tolerances;
- failure modes;
- data structures and schemas;
- interfaces and invariants;
- precision/convergence requirements;
- performance/resource assumptions where material;
- negative and boundary behavior;
- evidence that the design remains traceable to requirements.

### Phase 4 — Implementation

Implementation must preserve the approved architecture and design. Required controls include:

- version-controlled exact source;
- deterministic or explicitly characterized build process;
- dependency/environment identity;
- code review/diff review;
- static analysis where applicable;
- unit tests and negative fixtures;
- no undocumented shared mutable state;
- no silent fallback that changes scientific semantics;
- explicit treatment of errors and unknown states.

### Phase 5 — Verification

Verification establishes that implementation products conform to specified requirements. Suitable methods include, depending on the object:

- static analysis;
- peer inspection;
- unit and integration testing;
- contract tests;
- property/invariant checks;
- mutation testing;
- numerical regression tests;
- exact-byte identity/readback;
- independent oracle comparison;
- off-nominal and boundary tests.

Each requirement must have an explicit verification method and minimum objective evidence.

### Phase 6 — Validation

Validation establishes that the as-built scientific software is suitable for its intended use. Depending on the module, this can require:

- comparison with analytical solutions;
- comparison with trusted benchmark/reference implementations;
- reproduction of accepted baselines;
- comparison against independent empirical observations;
- external or withheld validation datasets;
- sensitivity and robustness analysis;
- domain-expert review;
- operationally representative scenarios;
- evaluation of model-form limitations and boundary conditions.

Validation evidence must be scientifically relevant to the declared use; passing unit tests alone does not establish model validity.

### Phase 7 — Integration verification and validation

The integrated MM must be evaluated separately from its components. Integration assurance includes:

- contract compatibility;
- unit consistency;
- temporal/spatial support consistency;
- provenance continuity;
- cross-module state consistency;
- propagation of missingness and uncertainty;
- interface error handling;
- scenario reproducibility;
- integrated scientific behavior against independent expectations.

### Phase 8 — Uncertainty, sensitivity, and robustness

The methodology distinguishes:

- parameter uncertainty;
- stochastic variability;
- measurement uncertainty;
- structural/model-form uncertainty;
- numerical error;
- spatial support/resolution uncertainty;
- scenario uncertainty.

Sensitivity/uncertainty designs must be defined prospectively, with estimators, sampling schemes, dependence assumptions, random streams/seeds, convergence/precision criteria, and decision-relevant outputs recorded before execution.

### Phase 9 — Controlled release

A release candidate must satisfy its frozen requirement, verification, validation, evidence, and compatibility criteria. Release is denied if required evidence is absent even when the implementation appears correct.

### Phase 10 — Independent readback and assurance

A separate read-only reviewer reconstructs the decisive identities and predicates. The producer cannot self-certify its own mutable output.

### Phase 11 — Freeze / certification

For durable certified authorities, the project uses an A→readback→B pattern:

1. **Content Commit A** establishes recoverable content.
2. Complete readback/reconstruction verifies exact persisted bytes.
3. **Certification Commit B** records the zero-mismatch result.

Certification does not authorize runtime.

## 8. Verification, validation, and IV&V-inspired completeness checks

The following questions are mandatory review prompts for material MM changes.

### 8.1 Requirements

- Are all in-scope scientific/system requirements represented?
- Are requirements correct, complete, consistent, traceable, readable, and testable?
- Are constraints, interfaces, error states, and boundary conditions explicit?
- Can each requirement be verified without reinterpretation after observing results?
- Are requirements that cannot be directly tested still objectively assessable by analysis/review?

### 8.2 Architecture

- Does every in-scope requirement map to one or more architectural elements?
- Are architecture relationships and interfaces correct, consistent, complete, and traceable?
- Can the architecture support the intended scientific use?
- Are undesired interactions and failure propagation considered?
- Is each authoritative boundary explicit?

### 8.3 Detailed design

- Is design traceable to requirements and architecture?
- Are interfaces internally consistent and externally complete?
- Are numerical and statistical assumptions explicit?
- Are all material negative/off-nominal states represented?

### 8.4 Implementation

- Does code preserve architecture, contracts, and requirements?
- Are all invariants enforced by typed/executable predicates where feasible?
- Does the implementation reject invalid states rather than normalize them silently?
- Are exact versions/dependencies/build inputs recorded?

### 8.5 Testing

- Does every material requirement have adequate test or analysis coverage?
- Are tests correct and complete relative to the requirement set?
- Do negative fixtures target known and plausible defect classes?
- Are tests independent enough to detect common-mode errors?
- Are acceptance criteria defined before observing the candidate?
- Are nominal, boundary, off-nominal, robustness, and failure-recovery cases included as applicable?

### 8.6 Acceptance

A candidate may pass only if the **evidence package is complete enough to support the claim**. Evidence completeness is therefore part of product assurance, not an administrative afterthought.

## 9. Multi-agent assurance and separation of duties

The certified architecture registers eleven core assurance roles but activates only the subset required by a Gate.

| ID | Role | Primary function | Permission |
|---|---|---|---|
| A0 | coordinator / final adjudicator | scope, authority, sequencing, final decision | read-only |
| A1 | sole mutable actor | edits, executes, external side effects | bounded write/execute |
| A2 | scientific method / DOE | intended use, estimand, controls, causal/scientific validity | read-only |
| A3 | requirements / formal methods | completeness, consistency, testability, decision partitions | read-only |
| A4 | geodesy / CRS / GEE | projection, grids, masks, spatial algorithms | read-only |
| A5 | provenance / Evidence Base | custody, hashes, chain of evidence | read-only |
| A6 | mathematical / numerical | well-posedness, stability, convergence, error bounds | read-only |
| A7 | statistics / geostatistics / UQ | dependence, validation, uncertainty, sensitivity | read-only |
| A8 | scientific software / systems / AI | SDLC, environments, tools, cloud orchestration | read-only |
| A9 | adversarial independent auditor | counterexamples, mutation, terminal challenge | read-only |
| A10 | data engineering / data quality | schemas, contracts, quality, catalog/lineage | read-only |

Conditional domain SMEs (`D-ENV`, `D-PLANT`, `D-ANIMAL`, `D-ECON`) are activated for materially affected modules.

The single-writer invariant is:

```text
MUTABLE_AGENT_COUNT = 1
SOLE_MUTABLE_AGENT = A1
ALL_REVIEWERS = read-only
```

This structure reduces concurrent-write ambiguity and common-mode assurance failure. It does not make internal review equivalent to formal external IV&V.

## 10. Execution classes and authorization boundary

The Architecture distinguishes execution classes, including:

```text
ASSURANCE_TOOL_EXECUTION
LOCAL_VERIFICATION_EXECUTION
CONTROL_PREQUALIFICATION_RUNTIME
PRIMARY_SCIENTIFIC_RUNTIME
ORACLE_RUNTIME
PRODUCTION_RUNTIME
```

Authorization is separate from certification:

```text
FROZEN_CERTIFIED ≠ RUNTIME_AUTHORIZED
PASS_PREVIOUS_GATE ≠ PERMISSION_TO_RUN
```

Each consequential runtime requires a separate Gate with its own inputs, capabilities, evidence plan, terminal partition, and side-effect budget.

## 11. Local execution adapter and least privilege

The qualified local adapter is currently represented as:

```text
Windows host
    ↓
WSL2 / Ubuntu 24.04.4 LTS
    ↓
/opt/mm
    ├── input/      root-owned read-only
    ├── review/     root-owned read-only
    ├── output/     authorized A1 mutable surface
    ├── scratch/    controlled execution state
    └── toolchain/  root-owned immutable toolchain
```

Actors are separated:

- `mmadmin` UID 1000 — administrative/bootstrap actor, not the scientific mutable actor;
- `mm-a1` UID 1001 — sole mutable scientific actor, non-admin, no sudo/adm, zero effective/permitted/ambient capabilities in the qualified sandbox, `NoNewPrivs=1`;
- `mm-review` UID 1003 — read-only independent reviewer role with restricted mutable surfaces.

The qualified adapter record demonstrates OS/toolchain boundary properties but explicitly does **not** authorize scientific, GEE, publication, GitHub-write, or production runtime.

## 12. Harness reliability profile

Prospective mutable execution is constrained by the Harness Reliability Profile. Core controls include:

1. one-shot execution (`MAX_ATTEMPTS=1`);
2. no automatic retry;
3. no implicit agent recursion or self-relaunch;
4. typed process results;
5. exact materialized-byte identity;
6. observation before adjudication;
7. total/exclusive terminal partition;
8. resource bounds and descendant cleanup;
9. filesystem least privilege;
10. network least privilege;
11. explicit UID/GID/capability boundary;
12. numeric side-effect budgets;
13. atomic evidence lifecycle;
14. independent readback;
15. explicit `STOP=true`;
16. regression closure for each discovered harness defect.

The Harness constrains execution but is not itself execution authority.

## 13. Data engineering, metadata, provenance, and evidence

### 13.1 Computational contract versus catalog metadata

The project separates:

- computational data contracts — exact interface semantics needed for execution;
- DCAT — dataset/service discovery;
- PROV-O — provenance/derivation relations;
- SHACL — graph constraint validation;
- DQV — data-quality metadata;
- Project Evidence Base — certified scientific/technical claims, decisions, conflicts, gaps, and evidence relations.

These layers are complementary and not interchangeable.

### 13.2 Data-quality evidence

A material dataset/transformation should have a quality profile that records:

- schema validity;
- units and ranges;
- missingness/null semantics;
- duplicate/key checks where relevant;
- spatial/temporal coverage;
- CRS/grid identity where relevant;
- provenance and source version;
- transformation lineage;
- freshness where material;
- required-versus-executed check equality.

A declared `CERTIFIED` state without objective evidence is invalid.

### 13.3 Evidence Base evolution

The certified Project Evidence Base v1.0.0 is immutable and append-only in evolution. Its certified baseline retains one open conflict and nine open gaps; this methodology does not silently close them. New material evidence must be admitted prospectively through audited linkage.

## 14. Geospatial and GEE assurance

Spatial correctness requires more than a syntactically valid CRS identifier. Material geospatial operations must control, as applicable:

- CRS definition;
- coordinate operation/datum transformation;
- axis semantics;
- affine/grid transform;
- pixel origin/alignment;
- nominal resolution/scale;
- spatial extent and target support;
- observable support/mask;
- resampling/aggregation semantics;
- geodesic versus planar area conventions;
- task/export identity and receipt.

A missing/non-observable value is not equivalent to a scientific zero. Empty support, partial support, and complete support are distinct states and should map to explicit terminal predicates.

## 15. Mathematical and numerical assurance

For mathematical/numerical components, the development record should establish:

- well-posed problem and domain of validity;
- assumptions/regularity conditions;
- conditioning and sensitivity;
- algorithmic stability;
- finite-precision behavior;
- convergence criterion and stopping rule;
- justified tolerance/error bound linked to the scientific decision;
- comparison against analytical/trusted reference results where feasible;
- negative/boundary behavior;
- exact artifact identity.

## 16. Statistical, geostatistical, Monte Carlo, and UQ assurance

Material statistical work should specify before execution:

- sampling design;
- probability distributions and parameterization;
- truncation/bounds;
- dependence/correlation structure;
- random seed and independent stream strategy;
- convergence/precision target;
- estimator and sample-size design;
- validation design;
- spatial/temporal cross-validation that avoids leakage;
- uncertainty category being propagated or excluded;
- decision outputs and interpretation.

Iteration count alone is not evidence of Monte Carlo adequacy; precision must be related to the scientific decision.

## 17. Scientific model credibility

NASA-HDBK-7009B is used by the project as a **methodological evidence source**, not as a claim of NASA compliance [EXT-NASA-HDBK7009B]. The project evidence overlay links it to:

- modular model/system decomposition;
- explicit interfaces and interactions among submodels;
- credibility of component and integrated models;
- input pedigree/provenance and data quality;
- uncertainty characterization;
- robustness of results;
- credibility considerations for AI-assisted modeling/simulation.

For MM, model credibility is therefore evaluated at both component and integrated-system levels.

## 18. Configuration management, deterministic packaging, and exact-byte authority

### 18.1 Configuration identity

A scientific result must bind to exact identities for material code, data, configuration, model version, contracts, and external dependencies. File name or modification time alone is insufficient.

### 18.2 Deterministic packaging

Where a package identity is itself evidence, the build process should define determinism rules such as path ordering, timestamp normalization, compression method, platform metadata, file modes, directory-entry behavior, and checksum coverage. When deterministic double-build is a requirement, both artifacts and their comparison must be independently inspectable.

### 18.3 Readback

A release/freeze process does not trust local success alone. Persisted artifacts are re-read from the authority surface, reconstructed where necessary, and compared to frozen expected bytes/hashes. Mismatch means HOLD.

## 19. Independent review, mutation testing, and adversarial assurance

Independent assurance is intended to challenge both the implementation and the specification/test system.

### 19.1 Mutation testing

Mutation suites should target:

- canonical requirements/source;
- executable resolver or decision logic;
- referential/traceability bindings;
- schema/type invariants;
- compatibility constraints;
- authority hashes;
- negative paths and failure classifications.

A candidate cannot pass if a material mutation that should be rejected remains undetected.

### 19.2 Common-mode protection

Where feasible, an independent oracle should be separately implemented rather than calling the production resolver. Independent review should recompute decisive facts from admitted bytes instead of accepting preparer receipts as dispositive evidence.

### 19.3 Evidence-completeness lesson from Attempt004

The terminal Attempt004 lineage provides a methodological lesson without becoming a normative authority. Its final independent review reconstructed 45 checks: checks 1–44 passed in documentary/static scope, while check 45 held because the **sealed negative-test matrix did not provide complete inspectable evidence for all mandatory negative classes**. The finding explicitly did not establish a new runtime implementation failure or corrupted candidate bytes.

The lesson is general:

```text
IMPLEMENTATION_GUARDS_APPEAR_CORRECT
        does not imply
MANDATORY_ASSURANCE_EVIDENCE_IS_COMPLETE
```

Therefore, when a Gate requires a negative matrix, the actual fixtures/test sources and their coverage are part of the deliverable—not optional supporting prose.

## 20. Agent Control / Execution Plane implementation roadmap

The M0→M8 roadmap is a **reference implementation and qualification track** for the Agent Control / Execution Plane. It is not the normative Architecture itself and does not block production of this methodological derivative.

```text
M0   Architecture Review
 ↓
M1   Always-on Core Extraction
 ↓
M2   Skill Package Materialization
 ↓
M3   Static Validation
 ↓
M4   Read-only Routing Evaluation
 ↓
M4A  Prospective Behavioral DOE
 ↓
M5   Supply-chain Discovery Qualification
 ↓
M6   Harness Integration
 ↓
M7   Controlled Mutable Pilot
 ↓
M8   Freeze / Certification Candidate
```

Current durable sources used here establish `M0=PASS_FINAL_ADJUDICATED` and `M1=PASS_M1_CORE_EXTRACTION`. Separate historical M4 evidence records a pre-model infrastructure HOLD and `M4_BEHAVIORAL_RESULT=NOT_ESTABLISHED`; a later M4R1 design review records an F03 causal-cycle HOLD. **No status for an intermediate milestone is inferred merely because a later milestone was attempted.** Each milestone's own controlling record remains authoritative for its state.

This roadmap may continue independently from the Methodology publication workstream.

## 21. Development-change acceptance matrix

Every material change should answer the following matrix before release.

| Dimension | Required question | Typical objective evidence |
|---|---|---|
| Scientific need | Is the intended problem/use correct? | problem statement, estimand, domain review |
| Requirements | Are requirements correct, complete, consistent, traceable, testable? | requirement register, gap analysis, traceability |
| Architecture | Does architecture satisfy requirements and intended use? | architecture mapping, interface review, failure analysis |
| Design | Is detailed design complete and analyzable? | design spec, invariants, algorithm/numerical rationale |
| Implementation | Does code implement the approved design? | source diff, static analysis, unit tests |
| Interfaces | Are contracts correct and version-compatible? | schema/contract tests, negative fixtures |
| Verification | Was the product built correctly? | requirement-linked tests/analyses |
| Validation | Was the right product built for intended use? | benchmarks, empirical/reference comparisons, expert review |
| Numerical/statistical | Are error, convergence, dependence, and uncertainty controlled? | diagnostics, precision/error evidence |
| Data quality | Are inputs and transformations fit for purpose? | quality profile, provenance, validation |
| Security/privilege | Are capabilities and mutable surfaces bounded? | sandbox preflight, actor/capability evidence |
| Reproducibility | Can exact state be reconstructed? | versions, hashes, manifests, deterministic build/readback |
| Assurance completeness | Are required positive/negative/off-nominal tests present? | coverage matrix, mutation/adversarial results |
| Adjudication | Does evidence map to exactly one terminal? | mechanical partition/readback |

## 22. Claim discipline for scientific publications

For manuscript-facing claims, the project should maintain a claim-to-evidence map. Every material claim should specify:

- claim text;
- scientific object and scope;
- supporting artifact(s);
- source/authority identity;
- verification evidence;
- validation evidence;
- uncertainty/limitations;
- independent review status;
- manuscript destination (main text, Methods, Supplementary, repository).

Claims unsupported by an admitted source are omitted or explicitly labeled `NOT_ESTABLISHED`.

## 23. Publication-facing reproducibility profile

This section is **non-normative editorial guidance**. It does not alter the certified MM authorities.

### 23.1 Nature Computational Science alignment

Nature Computational Science states that it publishes fundamental and applied work on computational techniques and mathematical models, including tools/frameworks that advance scientific research and methodologies that use computing in novel ways across scientific disciplines [EXT-NCS-AIMS]. This creates a plausible editorial scope for an MM paper centered on a reusable computational/scientific modeling framework, provided the manuscript demonstrates substantial conceptual or methodological significance rather than software engineering alone.

The journal's policies state that research manuscripts with newly developed code central to the main claims may require a software submission checklist and code availability for peer-review evaluation [EXT-NCS-POLICY]. Nature Portfolio guidance also expects central custom code to be made available to editors/referees on request and encourages release that allows readers to repeat published results [EXT-NATURE-CODE].

### 23.2 Computational reproducibility

Nature-family computational reporting guidance emphasizes, for custom scientific code:

- evidence supporting result validity, such as convergence tests or comparison with analytical/trusted standard approaches;
- code availability;
- installation/running documentation;
- tests and use examples;
- explicit software versions and input files where relevant [EXT-NATURE-COMP-REPRO].

MM therefore treats documentation, version identity, testability, and result validation as publication-facing scientific evidence—not repository polish.

### 23.3 Editorial non-claim

A high journal metric or apparent scope match does not establish publishability. Editorial acceptance depends on novelty, significance, evidence, clarity, reproducibility, and the final scientific story. The methodology is designed to maximize defensibility and reviewer auditability, not to guarantee a specific journal outcome.

## 24. Two-article role of this methodology

The current MM editorial strategy separates:

- **RAFE-1 / Data- or Resource-oriented paper:** the MM/MHSMM scientific instrument, reusable computational representation, FAIR/provenance layer, reproducibility, certified Architecture/Manual, Gate model, assurance, readback, and release/reuse;
- **RAFE-2 / Research paper:** substantive agricultural, economic, environmental, spatial, uncertainty, and land-use knowledge obtained from qualified scientific execution.

This methodology is principally an RAFE-1 asset. RAFE-2 should describe only the assurance material necessary to establish credibility of its scientific results, avoiding duplication of the full infrastructure narrative.

## 25. Resumption of scientific development

The Methodology workstream and the Agent Control qualification workstream are distinct. Completion of this Markdown does not authorize primary scientific runtime, but it can serve as the controlled methodological baseline for renewed scientific development.

A new scientific development Gate may authorize, as appropriate:

- scientific design and model specification;
- data-contract development;
- code development;
- static/local verification;
- baseline reproduction preparation;
- validation-design preparation;
- UQ/DOE design;
- DSSAT/MM integration development;
- documentation and test development.

A consequential **primary scientific runtime** remains separately authorized under the Architecture and Manual, with Gate-specific requirements and execution-plane qualification.

## 26. Known limitations and unresolved states

This document intentionally preserves the following non-claims:

1. The certified Architecture and Manual do not automatically validate every future model, dataset, scenario, or runtime.
2. The Project Evidence Base v1.0.0 retains one open conflict and nine open gaps at its certified baseline; this Markdown does not close them.
3. The local execution adapter is qualified for defined boundary properties but does not itself authorize scientific runtime.
4. The M0→M8 Agent Control implementation track is not fully certified; M4/M4R1 records include unresolved infrastructure/design HOLD states.
5. Attempt004 is terminated/archived and is not reopened by this methodology.
6. Internal multi-agent assurance is not represented as formal external IV&V.
7. Use of NASA, ISO/IEC/IEEE, NIST, W3C, FAIR4RS, or other external guidance does not imply external compliance or endorsement unless separately demonstrated.
8. This Markdown is a controlled derivative candidate until its own source-lock/readback process is accepted.

## 27. Methodology document acceptance criteria

This Markdown may be promoted from candidate to controlled methodological baseline only if all of the following pass:

- all controlling internal source identities are named;
- no certified authority is silently overridden;
- no historical HOLD is rewritten as PASS;
- no runtime authorization is inferred from certification;
- all quantitative project claims are source-backed;
- NASA verification/validation/IV&V distinctions are represented accurately;
- external guidance is explicitly non-normative for MM;
- current implementation-roadmap states are not overclaimed;
- Nature-facing statements are sourced to official Nature Portfolio material;
- all links are syntactically valid;
- heading hierarchy is valid;
- duplicate anchors/headings that would impair navigation are absent or explicitly handled;
- source register contains every external and internal source used materially;
- exact output byte length and SHA-256 are recorded;
- an independent/read-only content readback is performed before publication;
- `PUBLICATION_AUTHORIZED=false` remains true unless a separate publication Gate changes it;
- `SCIENTIFIC_RUNTIME_AUTHORIZED=false` remains true unless a separate run Gate changes it.

## 28. MkDocs implementation profile

The Markdown is authored to remain readable as plain text and render cleanly in MkDocs. The accompanying starter configuration follows the official MkDocs custom-theme model: a custom theme uses a `main.html` Jinja2 template outside `docs_dir`, referenced by `theme.custom_dir` in `mkdocs.yml` [EXT-MKDOCS-THEMES].

The documentation theme is a presentation layer only:

```text
CERTIFIED AUTHORITY
      ↓
CONTROLLED METHODOLOGICAL SOURCE
      ↓
INTEGRAL VERSIONED MARKDOWN
      ↓
MKDOCS RENDERING
      ↓
HUMAN-READABLE SITE
```

```text
HUMAN-READABLE SITE ≠ SCIENTIFIC AUTHORITY
```

No GitHub Pages deployment is authorized by this artifact.

## 29. Operating checklist for future MM development Gates

Before any material development Gate:

1. Name the Gate and exact parent authority.
2. State the scientific question and intended use.
3. Identify affected modules and interfaces.
4. Classify materiality and risks.
5. Select A0, A1, and only necessary read-only reviewers/SMEs.
6. Lock exact source/data/configuration authorities.
7. Build the requirement and traceability plan.
8. Define verification **and** validation plans.
9. Define controls, negative fixtures, off-nominal cases, and acceptance criteria before candidate observation.
10. Prove terminal-state totality and exclusivity.
11. Prequalify capabilities and side-effect boundaries.
12. Execute only within the authorized budget.
13. Capture raw evidence before adjudication.
14. Perform independent readback/oracle/adversarial checks.
15. Adjudicate PASS/HOLD without partial-language ambiguity.
16. Freeze only after zero-mismatch readback.
17. Preserve historical failures; remediate prospectively.
18. End with an explicit `NEXT_ALLOWED` and `STOP=true`.

## 30. References and source authority

### 30.1 Internal controlling and explanatory sources

- **[MM-CR08]** `MM_CR08_ARCHITECTURE_AUTHORITY_AND_DECISION_LEDGER_v1.0.0` — certified Architecture authority and decision ledger.
- **[MM-CR07]** `MM_CR07_MANUAL_AUTHORITY_AND_ASSURANCE_v1.0.0` — certified Manual authority and assurance state.
- **[MM-CR12]** `MM_CR12_PROJECT_EVIDENCE_BASE_AUTHORITY_v1.0.0` — certified Evidence Base authority.
- **[MM-CR13]** `MM_CR13_ARCHITECTURE_EVIDENCE_OVERLAY_PROVENANCE_v1.0.0` — NASA-HDBK-7009B evidence overlay and provenance.
- **[MM-CR04]** `MM_CR04_MM_LOCAL_EXECUTION_ADAPTER_QUALIFICATION_v1.0.0` — qualified local adapter boundary; no runtime authorization.
- **[MM-HRP]** `MM_HARNESS_RELIABILITY_PROFILE_v1.0.0` — prospective harness reliability profile.
- **[MM-CR06]** `MM_CR06_TECHNICAL_REFERENCE_PUBLICATION_STATE_v1.0.0` — controlled publication state and Markdown/MkDocs architecture.
- **[MM-TDR]** `MM_CERTIFIED_INFRASTRUCTURE_ARCHITECTURE_AND_MANUAL_TECHNICAL_REFERENCE_v1.0.0.docx` — controlled explanatory derivative, document `MM-TDR-INFRA-ARCH-MANUAL-001`; not normative.
- **[MM-CR17]** `MM_CR17_AGENT_CONTROL_M1_ALWAYS_ON_CORE_EXTRACTION_AND_M2_HANDOFF_v1.0.0` — M0/M1 durable Agent Control state.
- **[MM-M4]** `M4_INDEPENDENT_READBACK_REPORT` — historical M4 pre-model infrastructure HOLD/readback state.
- **[MM-M4R1]** `MM_AGENT_CONTROL_M4R1_V1_0_2_INDEPENDENT_F03_DESIGN_REVIEW_HOLD_v1.0.0` — M4R1 F03 design HOLD.
- **[MM-A004]** final Attempt004 v1.0.4 independent review and terminal archival state — historical assurance lesson only; not normative.
- **[MM-RAFE]** `MM_RAFE_TWO_ARTICLE_EDITORIAL_STRATEGY_v1.0.0` — RAFE-1/RAFE-2 allocation strategy.
- **[MM-PSP]** `MM_PROJECT_SOURCE_ADMISSION_POLICY_v1.0.0` — Project Source admission/retention policy.

### 30.2 External methodological and reporting sources

- **[EXT-NASA-NPR7150]** NASA. *NPR 7150.2D — NASA Software Engineering Requirements*. https://nodis3.gsfc.nasa.gov/displayDir.cfm?Internal_ID=N_PR_7150_002D_
- **[EXT-NASA-HDBK7009B]** NASA. *NASA-HDBK-7009, Version B — NASA Handbook for Models and Simulations: An Implementation Guide for NASA-STD-7009B*. NASA Technical Standards System, 2026-02-03. https://standards.nasa.gov/standard/NASA/NASA-HDBK-7009
- **[EXT-NASA-SA]** NASA. *NASA Software Assurance and Software Safety Standard / software assurance resources*. https://www.nasa.gov/intelligent-systems-division/software-management-office/nasa-software-engineering-procedural-requirements-standards-and-related-resources/
- **[EXT-NASA-SWE067]** NASA Software Engineering Handbook. *SWE-067 — Verify Implementation*. https://swehb.nasa.gov/spaces/7150/pages/16450546/SWE-067+-+Verify+Implementation
- **[EXT-NASA-IVV]** NASA Software Engineering Handbook / NASA-STD-8739.8. *IV&V Requirements*. https://swehb.nasa.gov/spaces/SITE/pages/122388487/8739.8B+4.4.2+IV+V+Requirements
- **[EXT-NASA-IVV-REQ]** NASA Software Assurance guidance. Requirements/design IV&V properties including correctness, consistency, completeness, traceability, and testability. https://swehb.nasa.gov/spaces/SITE/pages/129991998/8739.8A+Ch+4+SOFTWARE+ASSURANCE+AND+SOFTWARE+SAFETY+REQUIREMENTS
- **[EXT-MKDOCS-THEMES]** MkDocs. *Developing Themes / Creating a custom theme*. https://www.mkdocs.org/dev-guide/themes/
- **[EXT-NCS-AIMS]** Nature Computational Science. *Aims & Scope*. https://www.nature.com/natcomputsci/about/aims
- **[EXT-NCS-POLICY]** Nature Computational Science. *Editorial Policies*. https://www.nature.com/natcomputsci/editorial-policies
- **[EXT-NATURE-CODE]** Nature Portfolio. *Initial submission — Computer code*. https://www.nature.com/nature/for-authors/initial-submission
- **[EXT-NATURE-COMP-REPRO]** Nature Communications. *Guidance on reproducibility for papers using computational tools*. https://www.nature.com/documents/Computational_tools_reporting_guidelines.pdf
- **[EXT-NATURE-METRICS]** Nature Portfolio. *Journal Metrics*. https://www.nature.com/nature-portfolio/about-journals/journal-metrics

### 30.3 External-source use statement

External standards and journal guidance inform rationale and publication-facing controls. They do not replace the MM certified authorities. Where an external source changes over time, the access date/version must be recorded prospectively before a future normative change is proposed.

---

## Terminal document state

```text
DOCUMENT=MM_SCIENTIFIC_SOFTWARE_DEVELOPMENT_METHODOLOGY_v1.0.1.md
DOCUMENT_CLASS=CONTROLLED_HUMAN_READABLE_METHODOLOGICAL_DERIVATIVE
NORMATIVE_AUTHORITY=false
CERTIFIED_ARCHITECTURE_CHANGED=false
CERTIFIED_MANUAL_CHANGED=false
PROJECT_EVIDENCE_BASE_CHANGED=false
ATTEMPT004_REOPENED=false
M0_TO_M8_STATUS_REWRITTEN=false
SCIENTIFIC_RUNTIME_AUTHORIZED=false
GEE_RUNTIME_AUTHORIZED=false
PRODUCTION_RUNTIME_AUTHORIZED=false
PUBLICATION_AUTHORIZED=false
NEXT_ALLOWED=SEPARATELY_QUALIFIED_TARGETED_INDEPENDENT_READBACK_OF_MM_METHODOLOGY_V1_0_1_FINDINGS_CLOSURE
STOP=true
```