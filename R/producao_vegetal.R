#################################################################################################################################################
# O script define um conjunto de funções para calcular os custos e processos envolvidos na produção vegetal dentro de uma unidade produtiva 
# de bovinocultura. As funções utilizam dados de atividades específicas (preparo do solo, formação e manutenção de pastagens) e preços unitários para 
# calcular valores totais e custos associados, retornando essas informações de forma estruturada para posterior análise ou integração com outras
#  partes do sistema.
#################################################################################################################################################

#' Title
#'
#' @param soil_preparation Preparo do solo
#' @param pasture_formation Custos de Formação de Pasto
#' @param pasture_maintenance Custo Anual de Manutenção de Pasto
#'
#' @return A list
#' @export
#'
#' @examples
producao_vegetal <- function(productive_unit, soil_preparation, pasture_formation,
                             pasture_maintenance) {
  
  # Pasto ----------------------------------------------------------------------
  
  ## Preparo do solo -----------------------------------------------------------
  
  soil_systematization <- with(
    soil_preparation,
    sum(destoca * p_destoca,
        topografia * p_topografia,
        terraceamento * p_terraceamento,
        carreador * p_carreador,
        destruicao_quimica * p_destruicao_quimica,
        distrib_calcario * p_distrib_calcario,
        carreg_calcario * p_carreg_calcario)
  )
  
  ## Formação ------------------------------------------------------------------
  
  # aloca feritilizante em função do nível de intensificação (B, M ou A)
  if(productive_unit$nivel_intensificacao == 3) {
    pasture_formation$fertilizante <- pasture_formation$fertilizante_Bni
  } else if(productive_unit$nivel_intensificacao == 2) {
    pasture_formation$fertilizante <- pasture_formation$fertilizante_Mni
  } else {
    pasture_formation$fertilizante <- pasture_formation$fertilizante_Ani
  }
  
  pasture_formation$total_value <- with(
    pasture_formation,
    sum(gradagem * p_gradagem,
        aracao * p_aracao,
        niveladora * p_niveladora,
        semeacao_adubacao * p_semeacao_adubacao,
        aplicacao_herbicida * p_aplicacao_herbicida,
        semente * p_semente,
        calcario_dolomitico * p_calcario_dolomitico,
        fertilizante * p_fertilizante,
        herbicida_oleo_mineral * p_herbicida_oleo_mineral)
  ) + soil_systematization
  
  pasture_formation$total_cost <- with(
    pasture_formation,
    total_value / vida_util_pasto
  )
  
  ## Manutenção ----------------------------------------------------------------
  
  pasture_maintenance$total_10_area <- ifelse(
    productive_unit$nivel_intensificacao == 1, 
    0.10, 
    ifelse(
      productive_unit$nivel_intensificacao == 2, 
      0.05, 
      ifelse(
        productive_unit$nivel_intensificacao == 3, 
        0.015, 
        NA # valor padrão caso não seja 1, 2 ou 3
      )
    )
  )
  
  # aloca feritilizante em função do nível de intensificação (B, M ou A)
  if(productive_unit$nivel_intensificacao == 3) {
    pasture_maintenance$fertilizante <- pasture_maintenance$fertilizante_Bni
  } else if(productive_unit$nivel_intensificacao == 2) {
    pasture_maintenance$fertilizante <- pasture_maintenance$fertilizante_Mni
  } else {
    pasture_maintenance$fertilizante <- pasture_maintenance$fertilizante_Ani
  }
  
  pasture_maintenance$sub_total <- with(
    pasture_maintenance,
    sum(distribuicao_calcario * p_distribuicao_calcario,
        gradagem * p_gradagem,
        distribuicao_fertilizante * p_distribuicao_fertilizante,
        aplicacao_herbicida * p_aplicacao_herbicida,
        fertilizante * p_fertilizante,
        calcario * p_calcario,
        herbicida_oleo * p_herbicida_oleo)
  )
  
  pasture_maintenance$total_cost <- with(
    pasture_maintenance, sub_total * total_10_area
  )
  
  # módulo de pasto
  pasture <- list("soil_systematization" = soil_systematization,
                  "formation" = pasture_formation,
                  "maintenance" = pasture_maintenance)
  
  # Saída ----------------------------------------------------------------------
  
  out <- list("pasture" = pasture)
  return(out)
}

#' @title Preparo do solo
#'
#' @param destoca Unidade - Destoca
#' @param topografia Unidade - Topografia
#' @param terraceamento Unidade - Terraceamento
#' @param carreador Unidade - Adequação de carreadores
#' @param destruicao_quimica Unidade - Destruição química
#' @param distrib_calcario Unidade - Distribuição de calcário
#' @param carreg_calcario Unidade - Carregamento de calcário
#' @param p_destoca Preço unitário - Destoca
#' @param p_topografia Preço unitário - Topografia
#' @param p_terraceamento Preço unitário - Terraceamento
#' @param p_carreador Preço unitário - Adequação de carreadores
#' @param p_destruicao_quimica Preço unitário - Destruição química
#' @param p_distrib_calcario Preço unitário - Distribuição de calcário
#' @param p_carreg_calcario Preço unitário - Carregamento de calcário
#'
#' @return A list
#' @export
#'
#' @examples
soil_preparation <- function(destoca, topografia, terraceamento,
                             carreador, destruicao_quimica,
                             distrib_calcario, carreg_calcario,
                             p_destoca, p_topografia, p_terraceamento,
                             p_carreador, p_destruicao_quimica,
                             p_distrib_calcario, p_carreg_calcario) {
  x <- list(destoca, topografia, terraceamento,
            carreador, destruicao_quimica,
            distrib_calcario, carreg_calcario,
            p_destoca, p_topografia, p_terraceamento,
            p_carreador, p_destruicao_quimica,
            p_distrib_calcario, p_carreg_calcario)
  names(x) <- c("destoca", "topografia", "terraceamento",
                "carreador", "destruicao_quimica",
                "distrib_calcario", "carreg_calcario",
                "p_destoca", "p_topografia", "p_terraceamento",
                "p_carreador", "p_destruicao_quimica",
                "p_distrib_calcario", "p_carreg_calcario")
  return(x)
}

#' @title Custos de Formação de Pasto
#'
#' @param gradagem Unidade - Gradagem
#' @param aracao Unidade - Aração
#' @param niveladora Unidade - Niveladora
#' @param semeacao_adubacao Unidade - Semeação/adubação
#' @param aplicacao_herbicida Unidade - Aplicação de herbicida
#' @param fertilizante_Bni Unidade - Fertilizante ureia (Baixo nível)
#' @param fertilizante_Mni Unidade - Fertilizante ureia (Médio nível)
#' @param fertilizante_Ani Unidade - Fertilizante ureia (Alto nível)
#' @param semente Unidade - Semente 
#' @param calcario_dolomitico Unidade - Calcário dolomítico
#' @param herbicida_oleo_mineral Unidade - Herbicida Tordon + óleo mineral
#' @param p_gradagem Preço unitário - Gradagem
#' @param p_aracao Preço unitário - Aração
#' @param p_niveladora Preço unitário - Niveladora
#' @param p_semeacao_adubacao Preço unitário - Semeação/adubação
#' @param p_aplicacao_herbicida Preço unitário - Aplicação de herbicida
#' @param p_semente Preço unitário - Semente
#' @param p_calcario_dolomitico Preço unitário - Calcário dolomítico
#' @param p_fertilizante Preço unitário - Fertilizante 04-14-08
#' @param p_herbicida_oleo_mineral Preço unitário - Herbicida Tordon + óleo mineral
#' @param vida_util_pasto Vida útil da pasto (em anos)
#'
#' @return A list
#' @export
#'
#' @examples
pasture_formation <- function(gradagem, aracao, niveladora,
                              semeacao_adubacao, aplicacao_herbicida,
                              fertilizante_Bni, fertilizante_Mni,
                              fertilizante_Ani, semente,
                              calcario_dolomitico,
                              herbicida_oleo_mineral,
                              p_gradagem, p_aracao, p_niveladora,
                              p_semeacao_adubacao, p_aplicacao_herbicida, p_semente,
                              p_calcario_dolomitico, p_fertilizante,
                              p_herbicida_oleo_mineral, vida_util_pasto) {
  x <- list(gradagem, aracao, niveladora,
            semeacao_adubacao, aplicacao_herbicida,
            fertilizante_Bni, fertilizante_Mni, fertilizante_Ani,
            semente, calcario_dolomitico, herbicida_oleo_mineral,
            p_gradagem, p_aracao, p_niveladora,
            p_semeacao_adubacao, p_aplicacao_herbicida, p_semente,
            p_calcario_dolomitico, p_fertilizante,
            p_herbicida_oleo_mineral, vida_util_pasto)
  names(x) <- c("gradagem", "aracao", "niveladora",
                "semeacao_adubacao", "aplicacao_herbicida",
                "fertilizante_Bni", "fertilizante_Mni", "fertilizante_Ani",
                "semente", "calcario_dolomitico", "herbicida_oleo_mineral",
                "p_gradagem", "p_aracao", "p_niveladora",
                "p_semeacao_adubacao", "p_aplicacao_herbicida", "p_semente",
                "p_calcario_dolomitico", "p_fertilizante",
                "p_herbicida_oleo_mineral", "vida_util_pasto")
  return(x)
}

#' @title Custo Anual de Manutenção de Pasto
#'
#' @param distribuicao_calcario Unidade - Distribuição de calcário
#' @param gradagem Unidade - Gradagem
#' @param distribuicao_fertilizante Unidade - Distribuição de fertilizante
#' @param aplicacao_herbicida Unidade - Aplicação de herbicida
#' @param fertilizante_Bni Unidade - Fertilizante ureia (Baixo nível)
#' @param fertilizante_Mni Unidade - Fertilizante ureia (Médio nível)
#' @param fertilizante_Ani Unidade - Fertilizante ureia (Alto nível)
#' @param calcario Unidade - Calcario
#' @param herbicida_oleo Unidade - herbicida tordon + óleo
#' @param p_distribuicao_calcario Preço unitário - Distribuição de calcário
#' @param p_gradagem Preço unitário - Gradagem
#' @param p_distribuicao_fertilizante Preço unitário - Distribuição de fertilizante
#' @param p_aplicacao_herbicida Preço unitário - Aplicação de herbicida
#' @param p_fertilizante Preço unitário - Fertilizante ureia
#' @param p_calcario Preço unitário - Calcario
#' @param p_herbicida_oleo Preço unitário - herbicida tordon + óleo
#'
#' @return A list
#' @export
#'
#' @examples
pasture_maintenance <- function(distribuicao_calcario, gradagem,
                                distribuicao_fertilizante,
                                aplicacao_herbicida, fertilizante_Bni,
                                fertilizante_Mni, fertilizante_Ani,
                                calcario, herbicida_oleo,
                                p_distribuicao_calcario, p_gradagem,
                                p_distribuicao_fertilizante,
                                p_aplicacao_herbicida, p_fertilizante,
                                p_calcario, p_herbicida_oleo) {
  x <- list(distribuicao_calcario, gradagem,
            distribuicao_fertilizante,
            aplicacao_herbicida, fertilizante_Bni,
            fertilizante_Mni, fertilizante_Ani,
            calcario, herbicida_oleo,
            p_distribuicao_calcario, p_gradagem,
            p_distribuicao_fertilizante,
            p_aplicacao_herbicida, p_fertilizante,
            p_calcario, p_herbicida_oleo)
  names(x) <- c("distribuicao_calcario", "gradagem",
                "distribuicao_fertilizante",
                "aplicacao_herbicida", "fertilizante_Bni",
                "fertilizante_Mni", "fertilizante_Ani",
                "calcario", "herbicida_oleo",
                "p_distribuicao_calcario", "p_gradagem",
                "p_distribuicao_fertilizante",
                "p_aplicacao_herbicida", "p_fertilizante",
                "p_calcario", "p_herbicida_oleo")
  return(x)
}
