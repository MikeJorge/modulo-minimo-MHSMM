# MHSMM — Historical Minimum Module implementation

## 1. Repository purpose

This repository preserves the historical R implementation developed during the 2024 doctoral work underlying the Minimum Module (MM) methodology for full-cycle pasture-based beef production in Brazil. The hybrid Minimum Module simulation model (MHSMM) links production assumptions to herd size, land requirements and private viability. Its original Portuguese identifiers and internal terminology are intentionally preserved for implementation identity and traceability. This English documentation changes no scientific expression.

**For a quick review:** the [doctoral thesis](https://doi.org/10.11606/T.11.2024.tde-09122024-155051) provides the methodological specification; [section 6](#6-for-nature-food-reviewers) identifies the exact historical function examined in the Nature Food Perspective. The separate [MHSMM EN-USA v1.0.3 demonstrator](https://mike-jorge.shinyapps.io/mhsmm-enusa/) provides an interactive, English-language presentation. This repository supplies inspectable implementation evidence, rather than a new application release.

## 2. Scientific scope

The biological–economic model represents a full-cycle cattle enterprise, including breeding, rearing and finishing, under specified production and financial assumptions. Its calculations connect herd composition and animal-unit demand to modeled grazing area, production outputs, costs and cash flows. Required annual producer remuneration is an input to the private-viability calculation. The historical implementation accepts the Brazilian biome identifiers `Amazônia`, `Cerrado` and `Mata Atlântica`.

These calculations concern a scenario-dependent production and economic boundary. They do not establish ecological admissibility, parcel-level legal compliance, support eligibility or food-system sustainability. The ecological evidence criterion, fixed-case additional-price problem and prospective Sobol’ analysis discussed in the Perspective are proposed extensions; this repository does not implement that complete decision architecture. Historical biome-based area conventions should be interpreted as model accounting, not a parcel-level Legal Reserve assessment.

## 3. Model architecture

The doctoral framework organizes MHSMM into four interacting components: **Environment, Plant, Animal and Economic**. Environment defines the production setting; Plant concerns pasture production and management; Animal concerns herd dynamics and production; Economic connects investment, costs, revenues and remuneration to private viability.

The archived files distribute these responsibilities across functions. Their names are a guide, not a one-to-one reconstruction of the thesis architecture. For example, `ambiente_de_producao.R` contains production-unit definitions, inventory and herd simulation; `producao_vegetal.R` contains soil-preparation, pasture-establishment and maintenance accounting. Those filenames alone do not demonstrate a complete mechanistic environment–forage-growth pipeline. `MMBC_core` assembles the production and economic objects, while `MMBC` adds break-even analysis and result tables. See [section 9](#9-code-organization) for the actual organization.

## 4. Minimum Module boundary

The MM identifies a herd-scale private-viability boundary for specified inputs. The [2024 thesis](https://doi.org/10.11606/T.11.2024.tde-09122024-155051) specifies a retained configuration with nonnegative net present value (NPV), such that removing one breeding cow gives negative NPV. In the inspected R implementation, `find_MMBC_seq` retains a configuration only when its NPV is **strictly positive** and the configuration with one fewer breeding cow has **negative** NPV.

This is an implementation-level distinction at exact zero: nonnegative and strictly positive acceptance are not numerically identical there. The function examines a finite sequence of herd sizes for a sign-changing bracket and then uses binary search with an adjacent-cow comparison. That procedure does not establish a unique global minimum under arbitrary nonmonotonic NPV responses. The immutable source locator below identifies precisely which implementation supports this description; it is not a claim of empirical validation or numerical equivalence with other artifacts.

## 5. Historical identifiers and EN-USA terminology

Use this crosswalk to read the original code. The identifiers themselves remain unchanged.

| Historical identifier | EN-USA scientific meaning |
|---|---|
| `num_matrizes` | Initial number of breeding cows |
| `remuneracao` | Required annual producer remuneration, in Brazilian real per year |
| `nivel_intensificacao` | Intensification level; the historical coding is 1 = high, 2 = medium, 3 = low |
| `bioma` | Biome, using the original Brazilian biome names |
| `cap_sup` | Scenario-specific stocking-rate input used in the grazing-area calculation, in AU ha⁻¹ |
| `VPL` | Net present value (NPV) |
| `pasture_disp` | Modeled grazing area, in hectares |

In the herd simulation, `pasture_disp` is calculated as `UA / cap_sup`, where `UA` is the modeled animal-unit demand used by that calculation. Here, `cap_sup` is not presented as an independent field observation, a dynamically optimized stocking rate, a universally validated carrying capacity or an ecological threshold. Interpret it with the scenario assumptions and the historical animal-unit conventions.

The documentation preserves Brazilian real (BRL; R$), hectares (ha), Brazilian biome names and the original price-unit labels, including arroba (`@`) where used. It introduces no conversion to U.S. currency, land units or animal-unit conventions. A translated label does not alter an input’s definition, period or accounting basis.

## 6. For Nature Food reviewers

**Historical implementation examined in the Perspective:**

- Repository: [MikeJorge/modulo-minimo-MHSMM](https://github.com/MikeJorge/modulo-minimo-MHSMM)
- Commit: [`f8792493441b3b157bb3f493e2f7ee14a0808b86`](https://github.com/MikeJorge/modulo-minimo-MHSMM/commit/f8792493441b3b157bb3f493e2f7ee14a0808b86)
- File: [`R/MMBC.R` at that commit](https://github.com/MikeJorge/modulo-minimo-MHSMM/blob/f8792493441b3b157bb3f493e2f7ee14a0808b86/R/MMBC.R#L721)
- Function: `find_MMBC_seq`

This immutable locator identifies the R implementation inspected for the NPV zero-boundary distinction discussed in the Perspective. It does not establish equivalence among the thesis specification, later applications, the historical Shiny deployment and the EN-USA demonstrator. The current documentation HEAD includes a later documentation-only change; it does not replace the manuscript-inspected historical commit above.

## 7. Relationship to the EN-USA demonstrator

The [interactive MHSMM EN-USA v1.0.3 demonstrator](https://mike-jorge.shinyapps.io/mhsmm-enusa/) is a separate derivative artifact for international readers and reviewers. Its repository is [MikeJorge/MHSMM-shinyapp](https://github.com/MikeJorge/MHSMM-shinyapp), with canonical release commit `d9dcd8eb69f44583544a9bd45d63f53c6a29a25e`. Source-repository access requires authorization. Use the demonstrator’s own documentation for application-specific interpretation and access requirements.

The historical repository here retains the original implementation lineage. The shared methodology does not imply source or binary equivalence with the demonstrator. The English documentation helps readers navigate between these artifacts while keeping their distinct purposes and implementation identities visible.

## 8. Scientific sources

- **Doctoral specification and methodological documentation:** Michael Allim Jorge (2024), *Aplicação e validação da metodologia do módulo mínimo na exploração da bovinocultura de corte em pasto*. PhD thesis, University of São Paulo. [DOI: 10.11606/T.11.2024.tde-09122024-155051](https://doi.org/10.11606/T.11.2024.tde-09122024-155051).
- **Later peer-reviewed production–economic/risk application:** Jorge, M. A. et al. (2026), *Economics, risk analysis, and sustainability of minimum module for pasture-based beef production: a case study in Campo Grande, MS, Brazil*. *Revista Brasileira de Zootecnia* **55**, e20240197. [DOI: 10.37496/rbz5520240197](https://doi.org/10.37496/rbz5520240197).

The thesis and the journal article play distinct evidentiary roles. The 2026 application does not establish implementation of the entire mechanistic Environment–Plant coupling described in the doctoral framework. The article’s title is retained as published; its use of “sustainability” should not turn this repository’s private-viability calculations into a general sustainability assessment.

## 9. Code organization

| File | Role in the archived source |
|---|---|
| [`R/main.R`](R/main.R) | Historical execution entry point: sources functions and calls `MMBC` and `find_MMBC_seq` with example inputs |
| [`R/MMBC.R`](R/MMBC.R) | Integration through `MMBC_core` and `MMBC`; herd-scale search routines including `find_MMBC_seq` |
| [`R/ambiente_de_producao.R`](R/ambiente_de_producao.R) | Production-unit definitions, inventory and herd simulation, including grazing-area calculation |
| [`R/producao_vegetal.R`](R/producao_vegetal.R) | Soil preparation, pasture establishment and maintenance accounting |
| [`R/analise_producao_vegetal.R`](R/analise_producao_vegetal.R) | Point-by-point analysis using stored stocking-rate scenarios; separate from the functions sourced by `main.R` |
| [`R/animal_biological.R`](R/animal_biological.R) | Animal-category biological and production definitions |
| [`R/producao_animal.R`](R/producao_animal.R) | Assembly of animal-production inputs and associated parameter objects |
| [`R/economico.R`](R/economico.R) | Economic inputs, including labor, prices, general costs and financial assumptions |
| [`R/costs.R`](R/costs.R) | Production-cost calculations |
| [`R/viability.R`](R/viability.R) | Cash flows and financial indicators, including NPV |
| [`R/analysis_pe.R`](R/analysis_pe.R) | Break-even analysis |
| [`R/results.R`](R/results.R) | Result-table assembly |

`R/main.R` retains the input path `./data/input/EntradaDeDadosLimpo.csv`, whereas the tracked CSV is at `data/EntradaDeDadosLimpo.csv`. This historical path difference is preserved. The source is inspectable, but these documentation changes do not supply a reconstructed execution environment or demonstrate successful execution on current systems. Reading the entry script should therefore precede any attempt to run the historical implementation.

## 10. Citation and license

Cite the thesis for the original methodological specification, the 2026 article for that application, and the exact repository commit, file and function when describing inspected implementation behavior. A later documentation HEAD should not silently replace the historical locator in a scientific citation.

The repository’s [LICENSE](LICENSE) identifies the software license as MIT, with copyright © 2024 Michael Allim Jorge. The software license does not automatically establish redistribution rights for every historical dataset or third-party input. Consult the applicable source terms before reusing such material. The original [Portuguese README](README.md) remains available alongside this English guide.
