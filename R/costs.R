#' @title Planilha de custos
#'
#' @param propriedade Propriedade simulada
#' @param ambiente_de_producao Ambiente de produção
#' @param producao_vegetal Módulo de preodução vegetal
#' @param economico Módulo econômico
#'
#' @return A list
#' @export
#'
#' @examples
costs <- function(ambiente_de_producao, producao_vegetal, propriedade, economico) {
  
  # Custo fixo -----------------------------------------------------------------
  
  fixo <- list()
  
  ## A.i. Custo de Pasto (remuneração da terra)
  fixo$pastagem <- propriedade$productive_unit$arrend_total
  
  ## A.ii. Instalações e benfeitorias 
  fixo$instalacoes <- ambiente_de_producao$inventario$custo$valor_inicial_instalacao
  fixo$instalacoes_dep <- ambiente_de_producao$inventario$custo$valor_depreciacao_instalacao
  
  ## A.iii. Maquinas e equipamentos
  fixo$maquinas <- ambiente_de_producao$inventario$custo$valor_inicial_maquinas
  fixo$maquinas_dep <- ambiente_de_producao$inventario$custo$valor_depreciacao_maquinas
  
  ## A.iv.Pró-labore do Produtor
  fixo$prolabore <- economico$remuneracao_min
  
  ## total
  fixo$total <- Reduce("+", fixo)
  
  # Custo variável -------------------------------------------------------------
  
  variavel <- list()
  
  # B.i. Pasto
  pasture <- list()
  
  ## Formação de pasto
  pasture$pasto_formacao <- ifelse(
    propriedade$productive_unit$nivel_intensificacao == 1,
    producao_vegetal$pasture$formation$total_value * propriedade$productive_unit$pasture_disp * 0.05,
    ifelse(
      propriedade$productive_unit$nivel_intensificacao == 2,
      producao_vegetal$pasture$formation$total_value * propriedade$productive_unit$pasture_disp * 0.02,
      ifelse(
        propriedade$productive_unit$nivel_intensificacao == 3,
        producao_vegetal$pasture$formation$total_value * propriedade$productive_unit$pasture_disp * 0.01,
        NA # valor padrão caso não seja 1, 2 ou 3
      )
    )
  )
  
  ## Manutenção de pasto
  pasture$pasto_manutencao <- producao_vegetal$pasture$maintenance$sub_total *
    propriedade$productive_unit$pasture_disp * producao_vegetal$pasture$maintenance$total_10_area
  
  ## total
  pasture$total <- Reduce("+", pasture)
  variavel$pasture <- pasture
  
  # B.ii.Manutenção de instalações e benfeitorias
  variavel$instalacao_manutencao <- economico$outros_custos$instalacoes_maintenance
  
  # B.iii. Manutenção de máquinas e equipamentos
  variavel$maquina_manutencao <- economico$outros_custos$maquinas_maintenance
  
  # B.iv.Insumos
  supplies <- list()
  
  ## Suplemento mineral
  supplies$mineral <- 
    propriedade$planning$feeding$mineral_salt_reproduction$total_cost +
    propriedade$planning$feeding$mineral_salt$total_cost
  
  ## Suplemento-proteico/energético recria/terminação
  supplies$protein <- 
    propriedade$planning$feeding$protein_mineral$total_cost +
    propriedade$planning$feeding$energy_mineral$total_cost +
    ifelse(propriedade$productive_unit$nivel_intensificacao == 1,
           with(propriedade$planning$feeding$ccp,
                total_cost_18_20 + total_cost_21_28),
           0)
  
  ## Suplemento cria (creep-feeding)
  supplies$creep <- propriedade$planning$feeding$creep$total_cost
  
  ## Vacinas
  supplies$vaccines <- 
    propriedade$planning$sanitary$vax_af$total_cost +
    propriedade$planning$sanitary$vax_carb$total_cost +
    propriedade$planning$sanitary$vax_bru$total_cost
  
  ## Vermífugos
  supplies$vermifuge <- propriedade$planning$sanitary$vermifuge$total_cost
  
  ## Outros medicamentos
  supplies$other_medication <- economico$outros_custos$other_medication
  
  ## Combustível e lubrificantes
  if(propriedade$productive_unit$nivel_intensificacao == 1) {
    supplies$fuel_lubricant <- economico$outros_custos$fuel_lubricant * 3000 # alto
  } else if(propriedade$productive_unit$nivel_intensificacao == 2) {
    supplies$fuel_lubricant <- economico$outros_custos$fuel_lubricant * 2000 # médio
  } else {
    supplies$fuel_lubricant <- economico$outros_custos$fuel_lubricant * 1000 # baixo
  }
  
  ## total
  supplies$total <- Reduce("+", supplies)
  variavel$supplies <- supplies
  
  # B.v. Serviços e mão de obra
  labor <- list()
  
  ## Salários + encargos de empregados
  labor$employees <- economico$labor$total
  
  ## Serviços gerais e contador
  labor$general_costs <- economico$general_costs$Dg3tot
  
  ## Assisência técnica
  labor$assistance <- economico$general_costs$Dg2tot
  
  ## total
  labor$total <- Reduce("+", labor)
  variavel$labor <- labor
  
  # B.vi. Outros custos
  other <- list()
  
  ## Energia elétrica, telefone e transporte interno
  other$other_costs <- economico$general_costs$Dg1tot + economico$general_costs$Dg4tot
  
  ## total
  other$total <- Reduce("+", other)
  variavel$other <- other
  
  # total
  variavel$total <- pasture$total + supplies$total + labor$total + other$total +
    variavel$instalacao_manutencao + variavel$maquina_manutencao
  
  # Custo total ----------------------------------------------------------------
  
  cost <- list()
  
  ## A - Custo Fixo 
  cost$fixo <- fixo$total
  
  ## B - Custo Variável
  cost$variavel <- variavel$total
  
  ## C - Custo Total  (A+B)
  cost$total <- Reduce("+", cost)
  
  # Participação no Custo Total (%)
  cost$part_fixo <- with(cost, fixo / total)
  cost$part_variavel <- with(cost, variavel / total)
  
  # Todos os itens
  cost$all <- list(
    "variavel" = variavel,
    "fixo" = fixo
  )
  
  # Saída ----------------------------------------------------------------------
  
  return(cost)
}