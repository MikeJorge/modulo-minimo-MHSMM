#' @title Cria tabelas com resultados da análise
#'
#' @param propriedade Propriedade simulada
#' @param costs Custos
#' @param viab Análise de Viabilidade Econômica
#' @param ambiente_de_producao Ambiente de produção
#' @param bioma Bioma
#'
#' @return A list
#' @export
#'
#' @examples
results <- function(propriedade, costs, viab, ambiente_de_producao, bioma) {
  
  # Custos ---------------------------------------------------------------------
  df_custos <- data.frame(
    "Custos" = c(
      "A - Custo Fixo",
      "A.i. Custo de Pastagem (remuneração da terra)",
      "A.ii. Instalações e benfeitorias",
      "Depreciações",
      "A.iii. Maquinas e equipamentos",
      "Depreciações",
      "A.iv.Pró-labore do Produtor",
      
      "B - Custo Variável",
      "B.i. Pastagem",
      "Formação de Pastagem",
      "Manutenção de Pastagem",
      "B.ii.Manutenção de instalações e benfeitorias",
      "B.iii. Manutenção de máquinas e equipamentos",
      "B.iv.Insumos",
      "Suplemento mineral",
      "Suplemento-proteico/energético recria/terminação",
      "Suplemento cria (creep-feeding)",
      "Vacinas",
      "Vermífugos",
      "Outros medicamentos",
      "Combustível e lubrificantes",
      "B.v. Serviços e mão de obra",
      "Salários + encargos de empregados",
      "Serviços gerais e contador",
      "Assisência técnica",
      "B.vi. Outros custos",
      "Energia elétrica, telefone e transporte interno",
      
      "C - Custo Total (A+B)"
    ),
    
    "Valor (R$)" = c(
      sprintf("%.2f", costs$fixo),
      sprintf("%.2f", costs$all$fixo$pastagem),
      sprintf("%.2f", costs$all$fixo$instalacoes),
      sprintf("%.2f", costs$all$fixo$instalacoes_dep),
      sprintf("%.2f", costs$all$fixo$maquinas),
      sprintf("%.2f", costs$all$fixo$maquinas_dep),
      sprintf("%.2f", costs$all$fixo$prolabore),
      
      sprintf("%.2f", costs$variavel),
      sprintf("%.2f", costs$all$variavel$pasture$total),
      sprintf("%.2f", costs$all$variavel$pasture$pasto_formacao),
      sprintf("%.2f", costs$all$variavel$pasture$pasto_manutencao),
      sprintf("%.2f", costs$all$variavel$instalacao_manutencao),
      sprintf("%.2f", costs$all$variavel$maquina_manutencao),
      sprintf("%.2f", costs$all$variavel$supplies$total),
      sprintf("%.2f", costs$all$variavel$supplies$mineral),
      sprintf("%.2f", costs$all$variavel$supplies$protein),
      sprintf("%.2f", costs$all$variavel$supplies$creep),
      sprintf("%.2f", costs$all$variavel$supplies$vaccines),
      sprintf("%.2f", costs$all$variavel$supplies$vermifuge),
      sprintf("%.2f", costs$all$variavel$supplies$other_medication),
      sprintf("%.2f", costs$all$variavel$supplies$fuel_lubricant),
      sprintf("%.2f", costs$all$variavel$labor$total),
      sprintf("%.2f", costs$all$variavel$labor$employees),
      sprintf("%.2f", costs$all$variavel$labor$general_costs),
      sprintf("%.2f", costs$all$variavel$labor$assistance),
      sprintf("%.2f", costs$all$variavel$other$total),
      sprintf("%.2f", costs$all$variavel$other$other_costs),
      
      sprintf("%.2f", costs$total)
    ),
    
    "Participação no custo Total (%)" = c(
      sprintf("%.2f",  100 * costs$fixo / costs$total),
      sprintf("%.2f",  100 * costs$all$fixo$pastagem / costs$total),
      sprintf("%.2f",  100 * costs$all$fixo$instalacoes / costs$total),
      sprintf("%.2f",  100 * costs$all$fixo$instalacoes_dep / costs$total),
      sprintf("%.2f",  100 * costs$all$fixo$maquinas / costs$total),
      sprintf("%.2f",  100 * costs$all$fixo$maquinas_dep / costs$total),
      sprintf("%.2f",  100 * costs$all$fixo$prolabore / costs$total),
      
      sprintf("%.2f",  100 * costs$variavel / costs$total),
      sprintf("%.2f",  100 * costs$all$variavel$pasture$total / costs$total),
      sprintf("%.2f",  100 * costs$all$variavel$pasture$pasto_formacao / costs$total),
      sprintf("%.2f",  100 * costs$all$variavel$pasture$pasto_manutencao / costs$total),
      sprintf("%.2f",  100 * costs$all$variavel$instalacao_manutencao / costs$total),
      sprintf("%.2f",  100 * costs$all$variavel$maquina_manutencao / costs$total),
      sprintf("%.2f",  100 * costs$all$variavel$supplies$total / costs$total),
      sprintf("%.2f",  100 * costs$all$variavel$supplies$mineral / costs$total),
      sprintf("%.2f",  100 * costs$all$variavel$supplies$protein / costs$total),
      sprintf("%.2f",  100 * costs$all$variavel$supplies$creep / costs$total),
      sprintf("%.2f",  100 * costs$all$variavel$supplies$vaccines / costs$total),
      sprintf("%.2f",  100 * costs$all$variavel$supplies$vermifuge / costs$total),
      sprintf("%.2f",  100 * costs$all$variavel$supplies$other_medication / costs$total),
      sprintf("%.2f",  100 * costs$all$variavel$supplies$fuel_lubricant / costs$total),
      sprintf("%.2f",  100 * costs$all$variavel$labor$total / costs$total),
      sprintf("%.2f",  100 * costs$all$variavel$labor$general_costs / costs$total),
      sprintf("%.2f",  100 * costs$all$variavel$labor$assistance / costs$total),
      sprintf("%.2f",  100 * costs$all$variavel$labor$assistance / costs$total),
      sprintf("%.2f",  100 * costs$all$variavel$other$total / costs$total),
      sprintf("%.2f",  100 * costs$all$variavel$other$other_costs / costs$total),
      
      sprintf("%.2f",  100 * costs$total / costs$total)
    ),
    
    check.names = F
  )
  
  # Análise econômica ----------------------------------------------------------
  df_analise_economica <- data.frame(
    "Análise econômica" = c(
      "D - Receita total",
      "I - Impostos e taxas (Funrural)",
      "J - Receita líquida",
      "E - Custo operacional fixo",
      "F - Custo operacional variável",
      "G - Margem bruta - (D - F)",
      "H - Custo total",
      "Lucro operacional - margem líquida - (D - E)",
      "Lucro total - (J - H)",
      "Lucratividade (R$ ha-1 ano-1)"
    ),
    
    "R$" = c(
      sprintf("%.2f", propriedade$evolution$pVdT),
      sprintf("%.2f", as.numeric(viab$fluxos$impostos["Ano 1"])),
      sprintf("%.2f", propriedade$evolution$pVdT - as.numeric(viab$fluxos$impostos["Ano 1"])),
      sprintf("%.2f", costs$fixo),
      sprintf("%.2f", costs$variavel),
      sprintf("%.2f", propriedade$evolution$pVdT - costs$variavel),
      sprintf("%.2f", costs$total),
      sprintf("%.2f", propriedade$evolution$pVdT - costs$fixo),
      sprintf("%.2f", (propriedade$evolution$pVdT - as.numeric(viab$fluxos$impostos["Ano 1"])) - costs$total),
      sprintf("%.2f", ((propriedade$evolution$pVdT - as.numeric(viab$fluxos$impostos["Ano 1"])) - costs$total) / propriedade$productive_unit$pasture_disp)
    ),
    
    check.names = FALSE
  )
  
  # Indicadores - Econômicos ---------------------------------------------------
  df_indicadores_economicos <- data.frame(
    "Indicadores - Econômicos" = c(
      "Valor do rebanho - R$ total",
      "Despesas - R$",
      "Despesas - R$ ha -1 ano-1",
      "Receitas total - R$",
      "Receitas total - R$ ha -1 ano-1",
      "Custo operacional por cabeça - R$ cab. ano-1" ,
      "Custo operacional por hectare - R$ ha -1 ano-1", 
      "Custo total  R$ @-1",
      "Custo total R$ cab. an.",
      "Custo total  R$ ha -1 ano-1",
      "Custo total por kg vivo produzio - R$ kg peso vivo-1",
      "Custo operacional por kg vivo produzido - R$ kg p.v-1",
      "Custo da dieta por dia - R$ cab.an.",
      "Margem bruta - R$",
      "Margem bruta - R$ ha-1 ano-1 ",
      "Remuneração do empreendedor - R$ - mês",
      "Ponto de equilíbrio - cabeça animal"
    ),
    
    "R$" = c(
      sprintf("%.2f", propriedade$evolution$pTot),
      sprintf("%.2f", costs$total),
      sprintf("%.2f", costs$total / propriedade$productive_unit$pasture_disp),
      sprintf("%.2f", propriedade$evolution$pVdT),
      sprintf("%.2f", propriedade$evolution$pVdT / propriedade$productive_unit$pasture_disp),
      sprintf("%.2f", costs$variavel / propriedade$evolution$Qr),
      sprintf("%.2f", costs$variavel / propriedade$productive_unit$pasture_disp),
      sprintf("%.2f", costs$total / propriedade$evolution$prod_ha),
      sprintf("%.2f", costs$total / propriedade$evolution$ini),
      sprintf("%.2f", costs$total / propriedade$productive_unit$pasture_disp),
      sprintf("%.2f", costs$total / (propriedade$evolution$prod_ha * 30)),
      sprintf("%.2f", costs$variavel / (propriedade$evolution$prod_ha * 30)),
      sprintf("%.2f", costs$all$variavel$supplies$total / propriedade$evolution$ini),
      sprintf("%.2f", propriedade$evolution$pVdT - costs$variavel),
      sprintf("%.2f", (propriedade$evolution$pVdT - costs$variavel) / propriedade$productive_unit$pasture_disp),
      sprintf("%.2f", costs$all$fixo$prolabore / 12),
      800
    ),
    
    check.names = FALSE
  )
  
  # Indicadores técnicos/gerencias ---------------------------------------------
  df_indicadores_tecnicos <- data.frame(
    "Indicadores técnicos/gerencias" = c(
      "Quantidades de vacas - animais",
      "Taxa de lotação - UA ha-1",
      "Taxa de natalidade - %"
    ),
    
    "R$" = c(
      propriedade$productive_unit$num_matrizes,
      propriedade$productive_unit$cap_sup,
      propriedade$ref_zoo$Tn
    ),
    
    check.names = FALSE
  )
  
  # MMEBCCC --------------------------------------------------------------------
  # Módulo Mínimo da Exploração da Bovinocultura de Corte Ciclo Completo
  df_mmebccc <- data.frame(
    "Resultado" = c(
      "Remuneração mínima requerida pelo pecuarista R$ ano-1",
      "Área disponível (ha-1) [ADP]",
      "Unidade Animal (UA) Total - UAT",
      "Capacidade de suporte (Cs)",
      "Área total (ha) [Módulo Mínimo]",
      "Área de RL (20%)",
      "Área de APP (10%)",
      "Nível de Intensificação alta (1); média (2); baixo (3)",
      
      "Taxa de nascimento",
      "Número de matrizes",
      "Custo de Oportunidade (Arrend. R$ ha-1 ano-1)",
      "Taxa de Arrendamento",
      "Produtividade @ ha-1ano-1",
      "Custo Operacional Total  (COT R$ ha-1ano-1)",
      "Receita Total - Rt ano-1",
      "Preço do boi gordo R$ @-1",
      
      "Preço vaca gorda boiadeira R$ @-1",
      "Valor total do Rebanho R$ cab. an.",
      "Ponto de equilíbrio econômico (cabeça animal)",
      "Taxa real de desconto",
      "Lucro R$ ha-1ano-1",
      "Investimento total - Capex - R$",
      "Taxa interna de retorno modificada - MTIR",
      "PayBack - ano",
      
      "Valor Presente Líquido - VPL - R$"
    ),
    
    "Valor" = c(
      costs$all$fixo$prolabore,
      sprintf("%.0f", propriedade$productive_unit$pasture_disp),
      sprintf("%.0f", propriedade$productive_unit$UA),
      
      propriedade$productive_unit$cap_sup,
      
      # Área total (ha) [Módulo Mínimo]
      ifelse(bioma == "Mata Atlântica", 
             sprintf("%.0f", propriedade$productive_unit$pasture_disp / 0.8),
             ifelse(bioma == "Amazônia", 
                    sprintf("%.0f", propriedade$productive_unit$pasture_disp / 0.2),
                    ifelse(bioma == "Cerrado", 
                           sprintf("%.0f", propriedade$productive_unit$pasture_disp / 0.7),
                           ""))),
      # Área de RL
      ifelse(bioma == "Mata Atlântica", 
             sprintf("%.0f", (propriedade$productive_unit$pasture_disp / 0.8) * 0.15),
             ifelse(bioma == "Amazônia", 
                    sprintf("%.0f", (propriedade$productive_unit$pasture_disp / 0.2) * 0.7),
                    ifelse(bioma == "Cerrado", 
                           sprintf("%.0f", (propriedade$productive_unit$pasture_disp / 0.7) * 0.2),
                           ""))),
      # Área de APP
      ifelse(bioma == "Mata Atlântica", 
             sprintf("%.0f", (propriedade$productive_unit$pasture_disp / 0.8) * 0.05),
             ifelse(bioma == "Amazônia", 
                    sprintf("%.0f", (propriedade$productive_unit$pasture_disp / 0.2) * 0.1),
                    ifelse(bioma == "Cerrado", 
                           sprintf("%.0f", (propriedade$productive_unit$pasture_disp / 0.7) * 0.1),
                           ""))),
      
      propriedade$productive_unit$nivel_intensificacao,
      
      paste0(sprintf("%.0f", 100 * propriedade$ref_zoo$Tn), "%"),
      propriedade$productive_unit$num_matrizes,
      sprintf("%.2f", propriedade$productive_unit$arrend_total),
      paste0(sprintf("%.0f", 100 * propriedade$productive_unit$arrend_tax), "%"),
      sprintf("%.2f", propriedade$evolution$prod_ha / propriedade$productive_unit$pasture_disp),
      sprintf("%.2f", costs$total),
      sprintf("%.2f", as.numeric(viab$fluxos$receita["Ano 1"])),
      sprintf("%.2f", propriedade$boi_gordo_18_20$P11),
      
      sprintf("%.2f", propriedade$vaca$P1),
      sprintf("%.2f", propriedade$evolution$pTot),
      800,
      paste0(sprintf("%.2f", viab$taxa_desconto_real * 100), "%"),
      sprintf("%.2f", viab$lucro_hectare),
      sprintf("%.2f", viab$investimento_total),
      paste0(sprintf("%.2f", 100 * viab$MTIR), "%"),
      viab$payback,
      
      sprintf("%.0f", viab$VPL)
    ),
    
    check.names = FALSE
  )
  
  # Fluxo de caixa -------------------------------------------------------------
  df_fluxo <- as.data.frame(rbind(
    viab$fluxos$receita,
    viab$fluxos$sucata,
    viab$fluxos$impostos,
    viab$fluxos$receita_liquida,
    viab$fluxos$custo_total,
    viab$fluxos$administracao,
    viab$fluxos$custo_oportunidade,
    viab$fluxos$ebitda,
    viab$fluxos$depreciacao,
    viab$fluxos$ebit,
    viab$fluxos$itr,
    viab$fluxos$lair,
    viab$fluxos$imposto_renda,
    
    rep(NA, length(viab$fluxos$receita)),
    # 
    # round(100 * viab$fluxos$itr / viab$fluxos$ebit, 2),
    # round(100 * viab$fluxos$lair / viab$fluxos$ebit, 2),
    # round(100 * viab$fluxos$ebitda / viab$fluxos$receita_liquida, 2),
    # round(100 * viab$fluxos$custo_total / viab$fluxos$receita_liquida, 2),
    # 
    # rep(NA, length(viab$fluxos$receita)),
    
    viab$fluxos$investimentos_capex,
    viab$fluxos$maquinas_implementos,
    viab$fluxos$benfeitorias,
    viab$fluxos$reposicao_desgaste,
    viab$fluxos$rebanho,
    viab$fluxos$reposicao_animal,
    viab$fluxos$pagamento_dividas,
    
    # rep(NA, length(viab$fluxos$receita)),
    # 
    # round(100 * viab$fluxos$maquinas_implementos / viab$fluxos$investimentos_capex, 2),
    # round(100 * viab$fluxos$benfeitorias / viab$fluxos$investimentos_capex, 2),
    # round(100 * viab$fluxos$reposicao_desgaste / viab$fluxos$investimentos_capex, 2),
    # round(100 * viab$fluxos$rebanho / viab$fluxos$investimentos_capex, 2),
    # round(100 * viab$fluxos$reposicao_animal / viab$fluxos$investimentos_capex, 2),
    # 
    rep(NA, length(viab$fluxos$receita)),
    
    viab$fluxos$fluxo_caixa_socios,
    viab$fluxos$fluxo_caixa_livre_acumulado,
    viab$fluxos$fluxo_caixa_hectare
  ))
  rownames(df_fluxo) <- c(
    "1 Receita",
    "2 Outros (sucata)",
    "3 Impostos sobre a receita",
    "4 Receita Líquida",
    "5 Custo total - CT",
    "6 Administração (remuneração do Produtor)",
    "7 Custo de oportunidade (ex.arrendamento da terra)",
    "8 EBITDA",
    "9 Depreciação",
    "10 EBIT",
    "11 ITR",
    "12 LAIR",
    "13 Imposto de Renda",
    " ",
    # "ITR - Part. EBIT (%)",
    # "LAIR - Part. EBIT (%)",
    # "EBITDA - Part. Receita liquida (%)",
    # "Custo total - Part. Receita líquida (%)",
    # ".",
    "14 Investimentos - CAPEX",
    "15 Máquinas e Implementos",
    "16 Benfeitorias (Barracão)",
    "17 Reposição pelo desgaste",
    "18 Rebanho inicial",
    "19 Reposição de animais",
    "20 Pagamento de Dívidas + juros",
    # "..",
    # "Máquinas e Implementos - Part. CAPEX (%)",
    # "Benfeitorias - Part. CAPEX (%)",
    # "Reposição pelo desgaste - Part. CAPEX (%)",
    # "Rebanho inicial - Part. CAPEX (%)",
    # "Reposição animal - Part. CAPEX (%)",
    "...",
    "21 Fluxo de Caixa dos Sócios",
    "22 Fluxo de Caixa Livre Acumulado",
    "23 Resultado por hectare"
  )
  
  # Evolução -------------------------------------------------------------------
  
  df_evolucao <- data.frame(
    "Categoria animal" = c(
      "Vacas 37 a 48 meses",
      "Novilhas 29 a 36 meses",
      "Novilhas 21 a 28 meses",
      "Novilhas 18 a 20 meses",
      "Bezerras 12 meses",
      "Desmama fêmeas (7 a 8 meses)",
      "Animais nascidos",
      "Desmama macho (7 a 8 meses)",
      "Bezerros 12 meses",
      "Garrote de 18 meses",
      "Boi Magro (12 a 13 arrobas)",
      "Boi gordo 18 a 20 meses (dente de leite - DL)",
      "Boi gordo 21 a 28 meses (até 2 dentes permanentes)",
      "Boi gordo 29 a 36 meses (até 4 dentes permanentes)",
      "Boi gordo 37 a 48 meses (adulto)",
      "Boi gordo 'toruno' acima de 60 meses",
      "Touros"
    ),
    "UA (450kg/pv)" = c(
      propriedade$vaca$UAF6,
      propriedade$novilha_29_36$UAF5,
      propriedade$novilha_21_28$UAF4,
      propriedade$novilha_18_20$UAF3,
      propriedade$bezerro$UAF2,
      propriedade$desmama$UAF1,
      "",
      propriedade$desmama$UAM1,
      propriedade$bezerro$UAM2,
      propriedade$garrote$UAM3g,
      propriedade$boi_magro$UAM3bm,
      propriedade$boi_gordo_18_20$UAM3,
      propriedade$boi_gordo_21_28$UAM4,
      propriedade$boi_gordo_29_36$UAM5,
      propriedade$boi_gordo_37_48$UAM6,
      propriedade$boi_gordo_toruno$UAM6bgt,
      propriedade$touro$UAM6t
    ),
    "Cabeças" = c(
      propriedade$evolution$F6,
      propriedade$evolution$F5,
      propriedade$evolution$F4,
      propriedade$evolution$F3,
      propriedade$evolution$F2,
      propriedade$evolution$F1,
      NA,
      propriedade$evolution$M1,
      propriedade$evolution$M2,
      propriedade$evolution$M3g,
      propriedade$evolution$M3bm,
      propriedade$evolution$M3,
      propriedade$evolution$M4,
      propriedade$evolution$M5,
      propriedade$evolution$M6,
      propriedade$evolution$M6bgt,
      propriedade$evolution$M6t
    ),
    "Unidade animal (UA)" = c(
      propriedade$evolution$UAF6_i,
      propriedade$evolution$UAF5_i,
      propriedade$evolution$UAF4_i,
      propriedade$evolution$UAF3_i,
      propriedade$evolution$UAF2_i,
      propriedade$evolution$UAF1_i,
      NA,
      propriedade$evolution$UAM1_i,
      propriedade$evolution$UAM2_i,
      propriedade$evolution$UAM3g_i,
      propriedade$evolution$UAM3bm_i,
      propriedade$evolution$UAM3_i,
      propriedade$evolution$UAM4_i,
      propriedade$evolution$UAM5_i,
      propriedade$evolution$UAM6_i,
      propriedade$evolution$UAM6bgt_i,
      propriedade$evolution$UAM6t_i
    ),
    "Peso médio (kg)" = c(
      propriedade$vaca$wF6,
      propriedade$novilha_29_36$wF5,
      propriedade$novilha_21_28$wF4,
      propriedade$novilha_18_20$wF3,
      propriedade$bezerro$wF2,
      propriedade$desmama$wF1,
      "",
      propriedade$desmama$wM1,
      propriedade$bezerro$wM2,
      propriedade$garrote$wM3g,
      propriedade$boi_magro$wM3bm,
      propriedade$boi_gordo_18_20$wM3,
      propriedade$boi_gordo_21_28$wM4,
      propriedade$boi_gordo_29_36$wM5,
      propriedade$boi_gordo_37_48$wM6,
      propriedade$boi_gordo_toruno$wM6bgt,
      propriedade$touro$wM6t
    ),
    "Aquisição" = c(
      propriedade$vaca$AqF6 + propriedade$vaca$AqAcomp,
      propriedade$novilha_29_36$AqF5,
      propriedade$novilha_21_28$AqF4,
      propriedade$novilha_18_20$AqF3,
      propriedade$bezerro$AqF2,
      propriedade$desmama$AqF1,
      propriedade$vaca$AqAcomp,
      propriedade$desmama$AqM1,
      propriedade$bezerro$AqM2,
      propriedade$garrote$AqM3g,
      propriedade$boi_magro$AqM3bm,
      propriedade$boi_gordo_18_20$AqM3,
      propriedade$boi_gordo_21_28$AqM4,
      propriedade$boi_gordo_29_36$AqM5,
      propriedade$boi_gordo_37_48$AqM6,
      propriedade$boi_gordo_toruno$AqM6bgt,
      propriedade$touro$AqM6t
    ),
    "Nascimento" = c(
      rep(NA, 6),
      propriedade$evolution$nascimento,
      rep(NA, 10)
    ),
    "Mortalidade" = c(
      propriedade$evolution$MtF6,
      propriedade$evolution$MtF5,
      propriedade$evolution$MtF4,
      propriedade$evolution$MtF3,
      propriedade$evolution$MtF2,
      propriedade$evolution$MtF1,
      NA,
      propriedade$evolution$MtM1,
      propriedade$evolution$MtM2,
      propriedade$evolution$MtM3g,
      propriedade$evolution$MtM3bm,
      propriedade$evolution$MtM3,
      propriedade$evolution$MtM4,
      propriedade$evolution$MtM5,
      propriedade$evolution$MtM6,
      propriedade$evolution$MtM6bgt,
      propriedade$evolution$MtM6t
    ),
    "Vendas" = c(
      propriedade$evolution$VdF6,
      propriedade$evolution$VdF5,
      propriedade$evolution$VdF4,
      propriedade$evolution$VdF3,
      propriedade$evolution$VdF2,
      propriedade$evolution$VdF1,
      NA,
      propriedade$evolution$VdM1,
      propriedade$evolution$VdM2,
      propriedade$evolution$VdM3g,
      propriedade$evolution$VdM3bm,
      propriedade$evolution$VdM3,
      propriedade$evolution$VdM4,
      propriedade$evolution$VdM5,
      propriedade$evolution$VdM6,
      propriedade$evolution$VdM6bgt,
      propriedade$evolution$VdM6t
    ),
    "Número de cabeças" = c(
      propriedade$evolution$Ncfc6,
      propriedade$evolution$Ncfc5,
      propriedade$evolution$Ncfc4,
      propriedade$evolution$Ncfc3,
      propriedade$evolution$Ncfc2,
      propriedade$evolution$Ncfc1,
      propriedade$evolution$num_cabecas,
      propriedade$evolution$Ncmc1,
      propriedade$evolution$Ncmc2,
      propriedade$evolution$Ncmc3g,
      propriedade$evolution$Ncmc3bm,
      propriedade$evolution$Ncmc3,
      propriedade$evolution$Ncmc4,
      propriedade$evolution$Ncmc5,
      propriedade$evolution$Ncmc6,
      propriedade$evolution$Ncmc6bgt,
      propriedade$evolution$Ncmc6t
    ),
    "Mudança de era" = c(
      propriedade$evolution$Ncfc6 + propriedade$evolution$Ncfc5,
      propriedade$evolution$F5,
      propriedade$evolution$F4,
      propriedade$evolution$F3,
      propriedade$evolution$F2,
      propriedade$evolution$F1,
      NA,
      propriedade$evolution$M1,
      propriedade$evolution$M2,
      propriedade$evolution$M3g,
      propriedade$evolution$M3bm,
      propriedade$evolution$M3,
      propriedade$evolution$M4,
      propriedade$evolution$M5,
      propriedade$evolution$M6,
      propriedade$evolution$M6bgt,
      propriedade$evolution$Ncmc6t
    ),
    "Peso total (kg)" = c(
      propriedade$evolution$wtF6,
      propriedade$evolution$wtF5,
      propriedade$evolution$wtF4,
      propriedade$evolution$wtF3,
      propriedade$evolution$wtF2,
      propriedade$evolution$wtF1,
      NA,
      propriedade$evolution$wtM1,
      propriedade$evolution$wtM2,
      propriedade$evolution$wtM3g,
      propriedade$evolution$wtM3bm,
      propriedade$evolution$wtM3,
      propriedade$evolution$wtM4,
      propriedade$evolution$wtM5,
      propriedade$evolution$wtM6,
      NA,
      propriedade$evolution$wtM6t
    ),
    "Peso total (@)" = c(
      propriedade$evolution$wtArF6,
      propriedade$evolution$wtArF5,
      propriedade$evolution$wtArF4,
      propriedade$evolution$wtArF3,
      propriedade$evolution$wtArF2,
      propriedade$evolution$wtArF1,
      NA,
      propriedade$evolution$wtArM1,
      propriedade$evolution$wtArM2,
      propriedade$evolution$wtArM3g,
      propriedade$evolution$wtArM3bm,
      propriedade$evolution$wtArM3,
      propriedade$evolution$wtArM4,
      propriedade$evolution$wtArM5,
      propriedade$evolution$wtArM6,
      NA,
      propriedade$evolution$wtArM6t
    ),
    "Preço / kg" = c(
      propriedade$evolution$wkgF6,
      propriedade$evolution$wkgF5,
      propriedade$evolution$wkgF4,
      propriedade$evolution$wkgF3,
      propriedade$evolution$wkgF2,
      propriedade$evolution$wkgF1,
      NA,
      propriedade$evolution$wkgM1,
      propriedade$evolution$wkgM2,
      propriedade$evolution$wkgM3g,
      propriedade$evolution$wkgM3bm,
      propriedade$evolution$wkgM3,
      propriedade$evolution$wkgM4,
      propriedade$evolution$wkgM5,
      propriedade$evolution$wkgM6,
      NA,
      propriedade$evolution$wkgM6t
    ),
    "Valor total (R$)" = c(
      propriedade$evolution$pF6,
      propriedade$evolution$pF5,
      propriedade$evolution$pF4,
      propriedade$evolution$pF3,
      propriedade$evolution$pF2,
      propriedade$evolution$pF1,
      NA,
      propriedade$evolution$pM1,
      propriedade$evolution$pM2,
      propriedade$evolution$pM3g,
      propriedade$evolution$pM3bm,
      propriedade$evolution$pM3,
      propriedade$evolution$pM4,
      propriedade$evolution$pM5,
      NA, #propriedade$evolution$pM6,
      NA,
      propriedade$evolution$pM6t
    ),
    
  check.names = FALSE)
  
  # Inventário -----------------------------------------------------------------
  
  df_inventario <- data.frame(
    "Máquina/Implemente" = c(
      ifelse(propriedade$productive_unit$nivel_intensificacao <= 2, "Trator 75 cv", "Trator 65 cv"),
      ifelse(propriedade$productive_unit$nivel_intensificacao <= 2, "Trator 180 cv", "Trator 140 cv"),
      "Distribuidor de sementes/fertilizantes",
      "Distribuidor de Calcário", "Terraçador", "Pulverizador de Barras 6m Montado",
      "Grade Aradora", "Grade Niveladora", "Carreta (4 a 6 toneladas)", 
      "Matabroto Hidráulico EBT M", "Conjunto Hidráulico dianteiro", "Barracão"
    ),
    "Valor Inicial (R$)" = ambiente_de_producao$inventario$valor_inicial,
    "Valor Sucata" = ambiente_de_producao$inventario$valor_sucata_perc,
    "Valor Sucata (R$)" = ambiente_de_producao$inventario$valor_sucata,
    "Vida Útil (anos)" = ambiente_de_producao$inventario$vida_util,
    "Depreciação (R$/ano)" = ambiente_de_producao$inventario$depreciacao,
    check.names = FALSE
  )
  
  # Saída ----------------------------------------------------------------------
  
  out <- list("MMEBCCC" = df_mmebccc,
              "Custos" = df_custos,
              "Indicadores técnicos/gerencias" = df_indicadores_tecnicos,
              "Indicadores - Econômicos" = df_indicadores_economicos,
              "Análise econômica" = df_analise_economica,
              "Fluxos" = df_fluxo,
              "Evolução" = df_evolucao,
              "Inventário" = df_inventario)
  return(out)
}

#' Resultados em formato de tabela
#'
#' @param m MMBC core
#'
#' @return um array
#' @export
#'
#' @examples
results_clean <- function(m) {
  
  # Nomes das variáveis --------------------------------------------------------
  
  colunas <- c(
    "A - Custo Fixo",
    "A.i. Custo de Pastagem (remuneração da terra)",
    "A.ii. Instalações e benfeitorias",
    "Deprecições (Instalações e benfeitorias)",
    "A.iii. Maquinas e equipamentos",
    "Deprecições (Maquinas e equipamentos)",
    "A.iv.Pró-labore do Produtor",
    "B - Custo Variável",
    "B.i. Pastagem [somatório]",
    "Formação de Pastagem",
    "Manutenção de Pastagem",
    "B.ii.Manutenção de instalações e benfeitorias",
    "B.iii. Manutenção de máquinas e equipamentos",
    "B.iv.Insumos [somatório]",
    "Suplemento mineral",
    "Suplemento-proteico/energético recria/terminação",
    "Suplemento cria (creep-feeding)",
    "Vacinas",
    "Vermífugos",
    "Outros medicamentos",
    "Combustível e lubrificantes",
    "Salários + encargos de empregados",
    "Serviços gerais e contador",
    "Assisência técnica",
    "B.vi. Outros custos [somatório]",
    "Energia elétrica, telefone e transporte interno",
    "C - Custo Total  (A+B)",
    
    "D - Receita total",
    "I - Impostos e taxas (Funrural)",
    "J - Receita líquida",
    "E - Custo operacional fixo",
    "F - Custo operacional variável",
    "G - Margem bruta - (D - F)",
    "H - Custo total",
    "Lucro operacional - margem líquida - (D - E)",
    "Lucro total - (J - H)",
    "Lucratividade (R$ ha-1 ano-1)",
    "VPL R$",
    
    "Produtividade [@/ha-1 ano-1]",
    "Número inicial de matrizes [cabeça]",
    "Área disponível de pasto [ADP ha]",
    "Módulo Mínimo [área total ha]",
    
    "Vendas Vacas 37 a 48 meses",
    "Vendas Novilhas 29 a 36 meses",
    "Vendas Novilhas 21 a 28 meses",
    "Vendas Novilhas 18 a 20 meses",
    "Vendas Bezerras 12 meses",
    "Vendas Desmama fêmeas (7 a 8 meses)",
    "Vendas Desmama macho (7 a 8 meses)",
    "Vendas Bezerros 12 meses",
    "Vendas Garrote de 18 meses",
    "Vendas Boi Magro (12 a 13 arrobas)",
    "Vendas Boi gordo 18 a 20 meses ( dente de leite - DL)",
    "Vendas Boi gordo 21 a 28 meses (até 2 dentes permanentes)",
    "Vendas Boi gordo 29 a 36 meses (até 4 dentes permanentes)",
    "Vendas Boi gordo 37 a 48 meses (adulto)",
    "Vendas Touros",
    
    "Quant Vacas 37 a 48 meses",
    "Quant Novilhas 29 a 36 meses",
    "Quant Novilhas 21 a 28 meses",
    "Quant Novilhas 18 a 20 meses",
    "Quant Bezerras 12 meses",
    "Quant Desmama fêmeas (7 a 8 meses)",
    "Quant Desmama macho (7 a 8 meses)",
    "Quant Bezerros 12 meses",
    "Quant Garrote de 18 meses",
    "Quant Boi Magro (12 a 13 arrobas)",
    "Quant Boi gordo 18 a 20 meses ( dente de leite - DL)",
    "Quant Boi gordo 21 a 28 meses (até 2 dentes permanentes)",
    "Quant Boi gordo 29 a 36 meses (até 4 dentes permanentes)",
    "Quant Boi gordo 37 a 48 meses (adulto)",
    "Quant Touros",
    
    "Valor do rebanho - R$ total",
    "Despesas - R$",
    "Despesas - R$ ha -1 ano-1",
    "Receitas total - R$",
    "Receitas total - R$ ha -1 ano-1",
    "Custo operacional por cabeça - R$ cab. ano-1" ,
    "Custo operacional por hectare - R$ ha -1 ano-1", 
    "Custo total  R$ @-1",
    "Custo total R$ cab. an.",
    "Custo total  R$ ha -1 ano-1",
    "Custo total por kg vivo produzio - R$ kg peso vivo-1",
    "Custo operacional por kg vivo produzido - R$ kg p.v-1",
    "Custo da dieta por dia - R$ cab.an.",
    "Margem bruta - R$",
    "Margem bruta - R$ ha-1 ano-1",
    "Remuneração do empreendedor - R$ - mês",
    "Ponto de equilíbrio - cabeça animal",
    
    "Quantidades de vacas - animais",
    "Taxa de lotação - UA ha-1",
    "Taxa de natalidade - %",
    
    "Remuneração mínima requerida pelo pecuarista R$ ano-1",
    "Área disponível (ha-1) [ADP]",
    "Unidade Animal (UA) Total - UAT",
    "Capacidade de suporte (Cs)",
    "Área total (ha) [Módulo Mínimo]",
    "Área de RL (20%)",
    "Área de APP (10%)",
    "Nível de Intensificação alta (1); média (2); baixo (3)",
    
    "Taxa de nascimento",
    "Número de matrizes",
    "Custo de Oportunidade (Arrend. R$ ha-1 ano-1)",
    "Taxa de Arrendamento",
    "Produtividade @ ha-1ano-1",
    "Custo Operacional Total  (COT R$ ha-1ano-1)",
    "Receita Total - Rt ano-1",
    "Preço do boi gordo R$ @-1",
    
    "Preço vaca gorda boiadeira R$ @-1",
    "Valor total do Rebanho R$ cab. an.",
    "Ponto de equilíbrio econômico (cabeça animal)",
    "Taxa real de desconto",
    "Lucro R$ ha-1ano-1",
    "Investimento total - Capex - R$",
    "Taxa interna de retorno modificada - MTIR",
    "PayBack - ano",
    
    "Valor Presente Líquido - VPL - R$"
  )
  
  # se MMBC for NA, retorna array vazio nomeado
  if (anyNA(m)) {
    res <- rep(NA, 117)
    names(res) <- colunas
    return(res)
  }
  
  # Variáveis usadas na análise de Monte Carlo ---------------------------------

  # Custos
  
  res_01 <- c(
    m$custos$fixo, # A - Custo Fixo
    m$custos$all$fixo$pastagem, # A.i. Custo de Pastagem (remuneração da terra)
    m$custos$all$fixo$instalacoes, # A.ii. Instalações e benfeitorias
    m$custos$all$fixo$instalacoes_dep, # Deprecições
    m$custos$all$fixo$maquinas, # A.iii. Maquinas e equipamentos
    m$custos$all$fixo$maquinas_dep, # Deprecições
    m$custos$all$fixo$prolabore, # A.iv.Pró-labore do Produtor
    
    m$custos$variavel, # B - Custo Variável
    m$custos$all$variavel$pasture$total, # B.i. Pastagem [somatório]
    m$custos$all$variavel$pasture$pasto_formacao, # Formação de Pastagem
    m$custos$all$variavel$pasture$pasto_manutencao, # Manutenção de Pastagem
    m$custos$all$variavel$instalacao_manutencao, # B.ii.Manutenção de instalações e benfeitorias
    m$custos$all$variavel$maquina_manutencao, # B.iii. Manutenção de máquinas e equipamentos
    m$custos$all$variavel$supplies$total, # B.iv.Insumos [somatório]
    m$custos$all$variavel$supplies$mineral, # Suplemento mineral
    m$custos$all$variavel$supplies$protein, # Suplemento-proteico/energético recria/terminação
    m$custos$all$variavel$supplies$creep, # Suplemento cria (creep-feeding)
    m$custos$all$variavel$supplies$vaccines, # Vacinas
    m$custos$all$variavel$supplies$vermifuge, # Vermífugos
    m$custos$all$variavel$supplies$other_medication, # Outros medicamentos
    m$custos$all$variavel$supplies$fuel_lubricant, # Combustível e lubrificantes
    m$custos$all$variavel$labor$employees, # Salários + encargos de empregados
    m$custos$all$variavel$labor$general_costs, # Serviços gerais e contador
    m$custos$all$variavel$labor$assistance, # Assisência técnica
    m$custos$all$variavel$other$total, # B.vi. Outros custos [somatório]
    m$custos$all$variavel$other$other_costs, # Energia elétrica, telefone e transporte interno
    
    m$custos$total # C - Custo Total  (A+B)
  )
  
  # Análise econômica
  
  res_02 <- c(
    m$propriedade$evolution$pVdT, # D - Receita total
    as.numeric(m$viabilidade$fluxos$impostos["Ano 1"]), # I - Impostos e taxas (Funrural)
    m$propriedade$evolution$pVdT - as.numeric(m$viabilidade$fluxos$impostos["Ano 1"]), # J - Receita líquida
    m$custos$fixo, # E - Custo operacional fixo
    m$custos$variavel, # F - Custo operacional variável
    m$propriedade$evolution$pVdT - m$custos$variavel, # G - Margem bruta - (D - F)
    m$custos$total, # H - Custo total
    m$propriedade$evolution$pVdT - m$custos$fixo, # Lucro operacional - margem líquida - (D - E)
    (m$propriedade$evolution$pVdT - as.numeric(m$viabilidade$fluxos$impostos["Ano 1"])) - m$custos$total, # Lucro total - (J - H)
    ((m$propriedade$evolution$pVdT - as.numeric(m$viabilidade$fluxos$impostos["Ano 1"])) - m$custos$total) / 
      m$propriedade$productive_unit$pasture_disp, # Lucratividade (R$ ha-1 ano-1)
    m$viabilidade$VPL # VPL R$
  )
  
  # Módulo mínimo
  
  res_03 <- c(
    m$propriedade$evolution$prod_ha / m$propriedade$productive_unit$pasture_disp, # Produtividade [@/ha-1 ano-1]
    m$propriedade$productive_unit$num_matrizes, # Número inicial de matrizes [cabeça]
    m$propriedade$productive_unit$pasture_disp, # Área disponível de pasto [ADP ha]
    
    ifelse(m$bioma == "Mata Atlântica", 
           m$propriedade$productive_unit$pasture_disp / 0.8,
           ifelse(m$bioma == "Amazônia", 
                  m$propriedade$productive_unit$pasture_disp / 0.2,
                  ifelse(m$bioma == "Cerrado", 
                         m$propriedade$productive_unit$pasture_disp / 0.7,
                         NA))) # Módulo Mínimo [área total ha]
  )
  
  # Preço médio (categoria animal) (não é output)
  
  # QUANTIDADE DE ANIMAIS
  
  # Vendas Líquidas
  
  res_04 <- c(
    m$propriedade$evolution$VdF6, # Vendas Vacas 37 a 48 meses 
    m$propriedade$evolution$VdF5, # Vendas Novilhas 29 a 36 meses
    m$propriedade$evolution$VdF4, # Vendas Novilhas 21 a 28 meses
    m$propriedade$evolution$VdF3, # Vendas Novilhas 18 a 20 meses
    m$propriedade$evolution$VdF2, # Vendas Bezerras 12 meses
    m$propriedade$evolution$VdF1, # Vendas Desmama fêmeas (7 a 8 meses)
    m$propriedade$evolution$VdM1, # Vendas Desmama macho (7 a 8 meses)
    m$propriedade$evolution$VdM2, # Vendas Bezerros 12 meses
    m$propriedade$evolution$VdM3g, # Vendas Garrote de 18 meses
    m$propriedade$evolution$VdM3bm, # Vendas Boi Magro (12 a 13 arrobas)
    m$propriedade$evolution$VdM3, # Vendas Boi gordo 18 a 20 meses ( dente de leite - DL)
    m$propriedade$evolution$VdM4, # Vendas Boi gordo 21 a 28 meses (até 2 dentes permanentes)
    m$propriedade$evolution$VdM5, # Vendas Boi gordo 29 a 36 meses (até 4 dentes permanentes)
    m$propriedade$evolution$VdM6, # Vendas Boi gordo 37 a 48 meses (adulto)
    m$propriedade$evolution$VdM6t # Vendas Touros
  )
  
  # Evolução do rebanho
  
  res_05 <- c(
    m$propriedade$evolution$Ncfc6, # Quant Vacas 37 a 48 meses
    m$propriedade$evolution$Ncfc5, # Quant Novilhas 29 a 36 meses
    m$propriedade$evolution$Ncfc4, # Quant Novilhas 21 a 28 meses
    m$propriedade$evolution$Ncfc3, # Quant Novilhas 18 a 20 meses
    m$propriedade$evolution$Ncfc2, # Quant Bezerras 12 meses
    m$propriedade$evolution$Ncfc1, # Quant Desmama fêmeas (7 a 8 meses)
    m$propriedade$evolution$Ncmc1, # Quant Desmama macho (7 a 8 meses)
    m$propriedade$evolution$Ncmc2, # Quant Bezerros 12 meses
    m$propriedade$evolution$Ncmc3g, # Quant Garrote de 18 meses
    m$propriedade$evolution$Ncmc3bm, # Quant Boi Magro (12 a 13 arrobas)
    m$propriedade$evolution$Ncmc3, # Quant Boi gordo 18 a 20 meses ( dente de leite - DL)
    m$propriedade$evolution$Ncmc4, # Quant Boi gordo 21 a 28 meses (até 2 dentes permanentes)
    m$propriedade$evolution$Ncmc5, # Quant Boi gordo 29 a 36 meses (até 4 dentes permanentes)
    m$propriedade$evolution$Ncmc6, # Quant Boi gordo 37 a 48 meses (adulto)
    m$propriedade$evolution$Ncmc6t # Quant Touros
  )

  # Outras variáveis dos Dashboards --------------------------------------------
  
  # Indicadores - Econômicos
  
  res_06 <- c(
    m$propriedade$evolution$pTot, # Valor do rebanho - R$ total
    m$custos$total, # Despesas - R$
    m$custos$total / m$propriedade$productive_unit$pasture_disp, # Despesas - R$ ha -1 ano-1
    m$propriedade$evolution$pVdT, # Receitas total - R$
    m$propriedade$evolution$pVdT / m$propriedade$productive_unit$pasture_disp, # Receitas total - R$ ha -1 ano-1
    m$custos$variavel / m$propriedade$evolution$Qr, # Custo operacional por cabeça - R$ cab. ano-1
    m$custos$variavel / m$propriedade$productive_unit$pasture_disp, # Custo operacional por hectare - R$ ha -1 ano-1
    m$custos$total / m$propriedade$evolution$prod_ha, # Custo total  R$ @-1
    m$custos$total / m$propriedade$evolution$ini, # Custo total R$ cab. an.
    m$custos$total / m$propriedade$productive_unit$pasture_disp, # Custo total  R$ ha -1 ano-1
    m$custos$total / (m$propriedade$evolution$prod_ha * 30), # Custo total por kg vivo produzio - R$ kg peso vivo-1
    m$custos$variavel / (m$propriedade$evolution$prod_ha * 30), # Custo operacional por kg vivo produzido - R$ kg p.v-1
    m$custos$all$variavel$supplies$total / m$propriedade$evolution$ini, # Custo da dieta por dia - R$ cab.an.
    m$propriedade$evolution$pVdT - m$custos$variavel, # Margem bruta - R$
    (m$propriedade$evolution$pVdT - m$custos$variavel) / m$propriedade$productive_unit$pasture_disp, # Margem bruta - R$ ha-1 ano-1 
    m$custos$all$fixo$prolabore / 12, # Remuneração do empreendedor - R$ - mês
    800 # Ponto de equilíbrio - cabeça animal
  )
  
  # Indicadores técnicos/gerencias
  res_07 <- c(
    m$propriedade$productive_unit$num_matrizes, # Quantidades de vacas - animais
    m$propriedade$productive_unit$cap_sup, # Taxa de lotação - UA ha-1
    m$propriedade$ref_zoo$Tn # Taxa de natalidade - %
  )
  
  # MMEBCCC Módulo Mínimo da Exploração da Bovinocultura de Corte Ciclo Completo
  res_08 <- c(
    m$custos$all$fixo$prolabore, # Remuneração mínima requerida pelo pecuarista R$ ano-1
    m$propriedade$productive_unit$pasture_disp, # Área disponível (ha-1) [ADP]
    m$propriedade$productive_unit$UA, # Unidade Animal (UA) Total - UAT
    
    m$propriedade$productive_unit$cap_sup, # Capacidade de suporte (Cs)
    
    # Área total (ha) [Módulo Mínimo]
    ifelse(m$bioma == "Mata Atlântica", 
           m$propriedade$productive_unit$pasture_disp / 0.8,
           ifelse(m$bioma == "Amazônia", 
                  m$propriedade$productive_unit$pasture_disp / 0.2,
                  ifelse(m$bioma == "Cerrado", 
                         m$propriedade$productive_unit$pasture_disp / 0.7,
                         NA))), # Área total (ha) [Módulo Mínimo]
    # Área de RL
    ifelse(m$bioma == "Mata Atlântica", 
           (m$propriedade$productive_unit$pasture_disp / 0.8) * 0.15,
           ifelse(m$bioma == "Amazônia", 
                  (m$propriedade$productive_unit$pasture_disp / 0.2) * 0.7,
                  ifelse(m$bioma == "Cerrado", 
                         (m$propriedade$productive_unit$pasture_disp / 0.7) * 0.2,
                         NA))), # Área de RL (20%)
    # Área de APP
    ifelse(m$bioma == "Mata Atlântica", 
           (m$propriedade$productive_unit$pasture_disp / 0.8) * 0.05,
           ifelse(m$bioma == "Amazônia", 
                  (m$propriedade$productive_unit$pasture_disp / 0.2) * 0.1,
                  ifelse(m$bioma == "Cerrado", 
                         (m$propriedade$productive_unit$pasture_disp / 0.7) * 0.1,
                         NA))), # Área de APP (10%)
    
    m$propriedade$productive_unit$nivel_intensificacao, # Nível de Intensificação alta (1); média (2); baixo (3)
    
    100 * m$propriedade$ref_zoo$Tn, # Taxa de nascimento
    m$propriedade$productive_unit$num_matrizes, # Número de matrizes
    m$propriedade$productive_unit$arrend_total, # Custo de Oportunidade (Arrend. R$ ha-1 ano-1)
    100 * m$propriedade$productive_unit$arrend_tax, # Taxa de Arrendamento
    m$propriedade$evolution$prod_ha / m$propriedade$productive_unit$pasture_disp, # Produtividade @ ha-1ano-1
    m$custos$total, # Custo Operacional Total  (COT R$ ha-1ano-1)
    as.numeric(m$viabilidade$fluxos$receita["Ano 1"]), # Receita Total - Rt ano-1
    m$propriedade$boi_gordo_18_20$P11, # Preço do boi gordo R$ @-1
    
    m$propriedade$vaca$P1, # Preço vaca gorda boiadeira R$ @-1
    m$propriedade$evolution$pTot, # Valor total do Rebanho R$ cab. an.
    800, # Ponto de equilíbrio econômico (cabeça animal)
    m$viabilidade$taxa_desconto_real * 100, # Taxa real de desconto
    m$viabilidade$lucro_hectare, # Lucro R$ ha-1ano-1
    m$viabilidade$investimento_total, # Investimento total - Capex - R$
    100 * m$viabilidade$MTIR, # Taxa interna de retorno modificada - MTIR
    m$viabilidade$payback, # PayBack - ano
    
    m$viabilidade$VPL # Valor Presente Líquido - VPL - R$
  )
  
  # Saída ----------------------------------------------------------------------
  
  res <- round(c(res_01, res_02, res_03, res_04, res_05, res_06, res_07, res_08), 2)
  names(res) <- colunas
  return(res)
}
