# MM Methodology Source Authority Lock v1.0.1

**Project:** Artigo 2 - MM  
**Date:** 2026-11-11  
**Result:** `PASS_SOURCE_AUTHORITY_LOCK_FOR_TARGETED_METHODOLOGY_REMEDIATION_V1_0_1`

## Parent and controlling remediation authority

- Parent source lock: `MM_METHODOLOGY_SOURCE_AUTHORITY_LOCK_v1.0.0(1).md` — `1545` bytes — SHA-256 `608fe28a467c6672aa7b05568fcb19ac0bcaa8d5dad19120e8290e7b39ab6ed8`
- Targeted Control Plane disposition: `MM_METHODOLOGY_READBACK_FINDINGS_TARGETED_CONTROL_PLANE_DISPOSITION_v1.0.0.md` — `8264` bytes — SHA-256 `83b026f28ffcbfac836f0a19992824070109593b2aac5021d67340346ab77694`
- Targeted remediation work contract: `MM_METHODOLOGY_V1_0_1_TARGETED_REMEDIATION_WORK_CONTRACT_v1.0.0.md` — `2282` bytes — SHA-256 `e221a822d476101badda3233ffe56e3c78315df10a08f11d770d6260b69e9f2f`

## Controlling baseline

- Architecture v1.0.2 Certification Commit B: `75da6a6a1006c445eff4d589396813d6429fc51e`
- Manual v1.0.1 RC3 Certification Commit B: `01e9a94a7909af9436443ad016e5df06302576b4`
- Project Evidence Base v1.0.0 Certification Commit B: `650dd2cc8a8b04e993309485fe4f72b07858dfa6`
- Harness Reliability Profile v1.0.0: SHA-256 `62abcac3d28506181250ed6ec5d5138f811b64ec42e3e9c14b9fc086deda708a`
- Controlled Technical Reference: `MM-TDR-INFRA-ARCH-MANUAL-001`, version `1.0.0`, non-normative explanatory derivative.

No certified or mandatory internal authority is changed by this targeted remediation.

## Authorized external-source delta

The v1.0.1 Source Register adds exactly one material external-source identity:

- ID: `EXT-NASA-HDBK7009B`
- Publisher: NASA
- Document: `NASA-HDBK-7009`
- Version: `B`
- Document date: `2026-02-03`
- Title: *NASA HANDBOOK FOR MODELS AND SIMULATIONS: AN IMPLEMENTATION GUIDE FOR NASA-STD-7009B*
- Official source: `https://standards.nasa.gov/standard/NASA/NASA-HDBK-7009`
- `normative_for_mm=false`
- Use boundary: model/simulation credibility and only the evidence relationships already preserved by `MM-CR13`.

Source Register v1.0.1 identity:

- file: `MM_METHODOLOGY_SOURCE_REGISTER_v1.0.1.json`
- bytes: `8333`
- SHA-256: `d5d70f9d6ef2d7a995a011cb44951c49600137abf572c7a4f83ff1308cd76391`

## Boundary

This lock authorizes only the documentary v1.0.1 targeted remediation defined by the controlling disposition and work contract. It does not authorize scientific runtime, GEE, production, publication, GitHub deployment, modification of certified authorities, or reopening of Attempt004. No new raw-byte authority for the Technical Reference is introduced by this targeted remediation.

NASA, MkDocs, Nature Portfolio and other external sources remain non-normative for MM unless a separately governed prospective authority explicitly changes that status.

## Terminal

```text
RESULT=PASS_SOURCE_AUTHORITY_LOCK_FOR_TARGETED_METHODOLOGY_REMEDIATION_V1_0_1
NORMATIVE_AUTHORITY_CHANGED=false
HARNESS_PROFILE_CHANGED=false
RUNTIME_AUTHORIZED=false
PUBLICATION_AUTHORIZED=false
ATTEMPT004_REOPENED=false
NEXT_ALLOWED=MM_METHODOLOGY_V1_0_1_LOCAL_TARGETED_READBACK
STOP=true
```
