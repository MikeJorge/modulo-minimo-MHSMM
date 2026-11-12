# MM Figure 2 v1.0.6 — Final Targeted Independent Closure Report

**Projeto:** Artigo 2 - MM  
**Data local:** 2026-11-11 (America/Sao_Paulo)  
**Operação exata:** `SEPARATELY_QUALIFIED_FINAL_TARGETED_CLOSURE_OF_MM_LOCAL_EXECUTION_ADAPTER_FIGURE_V1_0_6`  
**Modalidade:** revisão independente, documental/estática/vetorial; *read-only*, sem importar, compilar, executar, re-renderizar ou reparar o gerador.  
**Decisão:** `PASS_FINAL_TARGETED_INDEPENDENT_CLOSURE_MM_LOCAL_EXECUTION_ADAPTER_FIGURE_V1_0_6`

## 1. Admissão externa antes de abrir o ZIP

O arquivo detached `MM_LOCAL_EXECUTION_ADAPTER_FIGURE_V1_0_6_PREISSUED_EXTERNAL_IDENTITY(1).txt` foi lido **antes de inspecionar o conteúdo ZIP nesta revisão**. O nome original da identidade identifica o pacote sem o sufixo de cópia `(1)` adicionado no upload. A autoridade externa é o conteúdo entregue e não foi reconstruída do ZIP.

```text
EXPECTED_PACKAGE_BYTES=1291525
OBSERVED_PACKAGE_BYTES=1291525
EXPECTED_PACKAGE_SHA256=6356f2833dd0fc48e9909b190c78de8e95631dd2f63a8881743b5371ef27c609
OBSERVED_PACKAGE_SHA256=6356f2833dd0fc48e9909b190c78de8e95631dd2f63a8881743b5371ef27c609
EXTERNAL_IDENTITY=PASS
```

Pacote: `MM_LOCAL_EXECUTION_ADAPTER_FIGURE_V1_0_6_FINAL_TARGETED_INDEPENDENT_CLOSURE_HANDOFF(1).zip`. Inspeção posterior à admissão: 26 membros físicos; CRC PASS; 24/24 payloads verificados contra `PACKAGE_MANIFEST.json` e 24/24 contra `SHA256SUMS.txt`; paths seguros, únicos e regulares; nenhuma execução de membro.

## 2. Cadeia parent, autoridade e anti-loop

O v1.0.5 permanece `HOLD_TARGETED_INDEPENDENT_VISUAL_READBACK_MM_LOCAL_EXECUTION_ADAPTER_FIGURE_V1_0_5`, 23/28 PASS e dois findings materiais de admissão/tinta visível. Sua disposição Control Plane SHA-256 `964418d6068098b37cdb7554bf159ed29af1c5a6a232eacdd2a9daed1436f7fa` foi conferida com o original dentro do ZIP, bem como os hashes exatos do relatório e resultado parent. **O HOLD histórico não foi reclassificado**.

A nova versão v1.0.6 constitui sucessor prospectivo. A mudança de largura da Reviewer sandbox 27→26 mm deslocou o right-port de 176→175 mm, tornando o terminal nominal 4,5 mm e o terminal efetivamente visível 3,944167 mm em ambos os vetores. O diff adiciona medições pós-overpaint, gids de painel e versionamento sem alterar semânticas de atores, permissão, runtime, canvas, fonte ou paleta. O parent e seus 39 PASS de ancestralidade são preservados no escopo de *lightweight non-regression*, sem rediscussão geral.

## 3. Reconstrução vetorial física independente

Os valores abaixo resultam de nova leitura dos objetos gráficos do PDF (PyMuPDF) e caminhos/grupos do SVG (XML), considerando a ordem de desenho e a metade da largura do contorno do painel preenchido desenhado posteriormente. Não foram copiados dos JSONs de QA.

| Fluxo | PDF tinta visível (mm) | SVG tinta visível (mm) | Corner PDF (mm) |
|---|---:|---:|---:|
| `read input` | 6.944167 | 6.944167 | 3.449994 |
| `read review` | 6.944167 | 6.944167 | 8.449990 |
| `write output` | 4.952990 | 4.952986 | 8.949997 |
| `write primary Gate workspace` | 9.944167 | 9.944167 | 3.749997 |
| `review evidence read` | 3.944167 | 3.944167 | 8.449996 |


**Predicados decisivos**:

- Evidence-read em PDF: 3.944167086 mm de tinta visível ≥ 3,000000 mm; alvo após seta em *drawing 39*, terminal em *drawing 21*.
- Evidence-read em SVG: 3.944166681 mm ≥ 3,000000 mm; grupos `terminal-evidence-read` / `panel-reviewer-sandbox`.
- Meta producer mais restritiva: 3,25 mm. Os **cinco terminais em ambos os formatos** são ≥ 3,25 mm.
- Afastamento efetivo, stroke-to-stroke, entre o corredor de evidência e o painel A1: 1.347155060 mm ≥ 1,0 mm.
- Corner clearance mínimo: 3.449993981 mm ≥ 3,0 mm; incidência perpendicular em todos os cinco ports.

Formato conferido: PDF 179.999995×164.999995 mm, 1 página vetorial e nenhuma imagem, com fontes TrueType CID incorporadas; SVG com 93 text editáveis e nenhuma imagem; seis tamanhos de fonte entre 5,0 e 6,8 pt; strokes visíveis entre 0,45 e 0,8 pt; PNG 2125×1948 px e aproximadamente 300 dpi.

## 4. Os 16 checks obrigatórios

| # | Predicado | Resultado | Evidência independente |
|---:|---|---|---|
| 01 | Identidade externa destacada, anterior à abertura | **PASS** | O registro detached PREISSUED_EXTERNAL_IDENTITY foi lido antes do ZIP nesta nova instância; esperado/observado 1.291.525 bytes e SHA-256 exato 6356f2833dd0fc48e9909b190c78de8e95631dd2f63a8881743b5371ef27c609. O sufixo de upload `(1)` não altera os bytes. |
| 02 | Segurança e integridade ZIP | **PASS** | 26 membros, únicos e regulares; nenhum path inseguro, symlink ou membro criptografado; CRC íntegro; `PACKAGE_MANIFEST.json` 24/24 e `SHA256SUMS.txt` 24/24 coincidem byte a byte com o conteúdo. |
| 03 | Vínculo do HOLD e disposição v1.0.5 | **PASS** | Resultado parent 19.691 bytes/SHA-256 `1c50fa81…306c`; relatório 16.544 bytes/SHA-256 `61ea829a…8960`; disposição CP SHA-256 `964418d6068098b37cdb7554bf159ed29af1c5a6a232eacdd2a9daed1436f7fa`. HOLD parent com dois findings exatos `FIG2-V105-ADMISSION-01` e `FIG2-V105-RB-01`, preservado. |
| 04 | Sucessor prospectivo sem mutação parent | **PASS** | Fontes distintas dentro do ZIP: v1.0.5 66.900 bytes/SHA-256 `c58e6d3c…bb3f`; v1.0.6 70.342 bytes/SHA-256 `1bde6838…625c1`. Nenhuma escrita foi feita sobre v1.0.5; a correção é nova candidata. |
| 05 | Delta limitado à microgeometria/QA/versioning | **PASS** | Diff textual reconstruído: 216 linhas em saída unified, largura Reviewer sandbox 27,0→26,0 mm, deslocando o right port 176,0→175,0 mm e a associação dependente; SVG panel gids e métricas de tinta visível adicionados, mais nomes/identidade de versão. Sem mudança de atores, permissões, texto científico, palette, canvas, font sizing ou estado runtime. |
| 06 | A/B double render e identidade entregue | **PASS** | QA registra A=B para SVG, PDF, PNG e manifesto; hashes/tamanhos A e B foram recomparados com os bytes efetivamente entregues, 4/4 exatos. **Escopo:** os dois builds físicos separados não foram fornecidos; a igualdade entre A e B é evidência de produção verificada documentalmente, não um rerender independente. O PASS histórico deste predicado permanece em non-regression. |
| 07 | Formato físico e vetorial | **PASS** | PDF 179,999995×164,999995 mm e SVG 180×165 mm; fonte visível 5,0–6,8 pt; strokes 0,45–0,80 pt; 93 elementos SVG text editáveis e zero images; PDF vetorial sem images, 2 CID TrueType fonts incorporadas; PNG 2125×1948 px a ~300 dpi. |
| 08 | Semântica evidence read e right port | **PASS** | Trajeto `review/ → Reviewer sandbox` identificado no fonte e no vetor exportado. A linha percorre corredor inferior até x=179,5 mm, sobe externamente e ingressa perpendicularmente pelo lado direito da Reviewer sandbox. Não amplia acesso de `mm-review` a output/scratch, não concede capabilities. |
| 09 | Terminal PDF visível após overpaint | **PASS** | PDF drawing 21 (seta) ocorre antes do filled Reviewer panel drawing 39; após subtrair o contorno de 0,60 pt, há 3.944167 mm de tinta visível, ≥ 3,000000 mm. |
| 10 | Terminal SVG visível após overpaint | **PASS** | No SVG, grupo `terminal-evidence-read` e `panel-reviewer-sandbox` (contorno de 0,60 pt) fornecem 3.944167 mm visíveis, ≥ 3,000000 mm. |
| 11 | Robustez producer PDF+SVG para cinco fluxos | **PASS** | Reconstituição independente das cinco geometrias exportadas: mínimos PDF/SVG 3,944167/3,944167 mm. Todos os dez valores superam tanto 3,0 mm congelado como a meta producer 3,25 mm. QA producer coincide com os valores exportados. |
| 12 | Distância física evidence route → A1 | **PASS** | PDF path 20, A1 panel 38; distância perpendicular do corredor y=86,0 mm ao perímetro do painel A1 subtraídas ambas as semilarguras dos strokes: 1.347155 mm ≥ 1,000000 mm. |
| 13 | Corner clearance e incidência | **PASS** | Cinco ports conferidos nos vetores PDF/SVG; corner clearance PDF mínima 3,449994 mm ≥ 3,000000 mm; `review evidence read` corner 8,449996 mm; incidência horizontal ao right-edge e perpendicular em todos os cinco destinos. |
| 14 | Gramática e 12 mutações negativas | **PASS** | AST do fonte contém 12 chamadas calculadas de `results.append`, sem resultado booleano constante; `add_poly_arrow` e `add_poly_association` invocam a mesma gramática de produção. Fixture real chama `_validate_connector_grammar("identity_binding", True)` e exige ValueError `IDENTITY_BINDING_ARROWHEAD_FORBIDDEN`; QA originária reporta 12/12. SVG associações de identidade não contêm arrowheads. |
| 15 | 39 PASS parent e restrições preservados | **PASS** | O parent documenta 39/44 PASS na ancestral v1.0.4 e target v1.0.5 PASS exceto admissão/tinta visível. O delta v1.0.6 só altera direito Reviewer/QA/versionamento. Inspeção estática e vetorial demonstra non-regression, sem reexecutar 39 testes; CR-04, M4 HOLD premodel, M4R1 HOLD F03 e proibições permanecem. |
| 16 | Conjunção terminal | **PASS** | Checks 1–15 todos PASS; zero finding material remanescente; `FIG2-V105-ADMISSION-01` e `FIG2-V105-RB-01` fechados prospectivamente. `PUBLICATION_ASSET_ELIGIBLE` é elegibilidade do arquivo, não autorização para MkDocs, GitHub Pages, runtime ou submissão. |


## 5. Delimitação de evidência e decisão

O `double_render` (SVG/PDF/PNG/manifesto) é corroborado por registros de builds A e B com identidades idênticas aos arquivos físicos entregues. **Não há duas cópias de builds fisicamente separadas no ZIP**. Por isso, esta revisão não reivindica execução própria do gerador nem observação independente de um segundo processo de renderização; adota a mesma fronteira documental de non-regression já aceita no review parent.

A ausência de rerender está alinhada ao contrato: o escopo independente é readback estático/vetorial do candidato congelado. A revisão **não** garante conformidade editorial integral futura, validação científica do MM ou autoridade de publicação.

Nenhum finding material novo foi identificado. Os dois findings do v1.0.5 são encerrados **somente prospectivamente para v1.0.6**, sem apagar o histórico. Font family `Liberation Sans` (e não Arial/Helvetica) é limitação expressamente declarada na submissão editorial NCS.

## 6. Terminal exato

```text
RESULT=PASS_FINAL_TARGETED_INDEPENDENT_CLOSURE_MM_LOCAL_EXECUTION_ADAPTER_FIGURE_V1_0_6
REQUIRED_CHECKS=16_OF_16_PASS
MATERIAL_FINDING_COUNT=0
FIG2_V105_ADMISSION_01=CLOSED
FIG2_V105_RB_01=CLOSED
PRESERVED_PARENT_PASS_CHECKS_NONREGRESSED=true
FIGURE_STATUS=PUBLICATION_ASSET_ELIGIBLE
NCS_ALIGNMENT=PASS_WITH_DECLARED_FONT_FAMILY_LIMITATION
MKDOCS_INSERTION_AUTHORIZED=false
GITHUB_PAGES_ACTIVATION_AUTHORIZED=false
SCIENTIFIC_RUNTIME_AUTHORIZED=false
PUBLICATION_SUBMISSION_AUTHORIZED=false
NEXT_ALLOWED=CONTROL_PLANE_FINAL_ADJUDICATION_AND_MKDOCS_INSERTION_OF_MM_LOCAL_EXECUTION_ADAPTER_FIGURE_V1_0_6
STOP=true
```

O `NEXT_ALLOWED` define a próxima operação sujeita a **nova autorização do Control Plane**; este relatório não concede permissão para inserir o gráfico no MkDocs ou ativar o GitHub Pages.

## 7. Governança e preservação

```text
REVIEWER_INDEPENDENCE=ESTABLISHED_FROM_PRODUCER_FOR_THIS_REVIEW_INSTANCE
ORIGINAL_V105_HOLD_PRESERVED=true
SOURCE_ORIGINAL_MODIFIED=false
GENERATOR_IMPORTED=false
GENERATOR_EXECUTED=false
GENERATOR_COMPILED=false
CANDIDATE_RERENDERED=false
CANDIDATE_REPACKAGED=false
LIVE_OPT_MM_ACCESSED=false
SUBAGENTS_USED=false
GITHUB_WRITE_COUNT=0
SCIENTIFIC_RUNTIME_EXECUTED=false
GEE_RUNTIME_EXECUTED=false
PRODUCTION_RUNTIME_EXECUTED=false
STOP=true
```

**Evidência de readback vetorial:** `MM_LOCAL_EXECUTION_ADAPTER_FIGURE_V1_0_6_INDEPENDENT_VECTOR_READBACK_v1.0.0.json`, SHA-256 `3f5a5410d516cb39a43ecfdeecc44f2e536522bcb2ac96840ba64c34e7d91f5a`.

**Project Source:** `PROJECT_SOURCE_RECOMMENDATION=ADD`; título proposto `MM_LOCAL_EXECUTION_ADAPTER_FIGURE_V1_0_6_FINAL_TARGETED_INDEPENDENT_CLOSURE_PASS_v1.0.0`; classe `B — ACTIVE_STATE / INDEPENDENT_ASSURANCE`; materialidade: encerramento material de dois findings e definição de elegibilidade de asset; supersession: nenhum registro histórico removido automaticamente; retenção até posterior adjudicação Control Plane incorporando a identidade, a revisão e as restrições. A recomendação **não** representa admissão efetiva ao Project Sources.