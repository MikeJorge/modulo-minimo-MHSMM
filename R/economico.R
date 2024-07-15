############################################################################################################################################
# O script define um conjunto abrangente de funções para calcular e retornar parâmetros econômicos e financeiros de uma propriedade de 
# bovinocultura. A função principal economico realiza cálculos detalhados de custos de mão de obra, despesas gerais e viabilidade econômica, 
# enquanto as funções auxiliares fornecem suporte para a estruturação dos dados de entrada. Essas funções são essenciais para avaliar a 
# viabilidade econômica e gerenciar os custos de uma operação de bovinocultura de forma eficiente e detalhada.
#############################################################################################################################################

#' @title Econômico 
#'
#' @param labor Mão de obra
#' @param general_costs Despesas gerais
#' @param remuneracao_min 
#' @param outros_custos 
#' @param viabilidade 
#'
#' @return
#' @export
#'
#' @examples
economico <- function(remuneracao_min, labor, precos, general_costs, outros_custos,
                      viabilidade) {
  
  # Custos ---------------------------------------------------------------------
  
  ## Mão de obra ---------------------------------------------------------------
  
  # cálculos gerais
  cbasic_cost_h <- with(labor, cbasic / hours_m)        # cesta básica R$/h
  uniform_cost_y <- with(labor, uniform * uniform_cost) # uniforme custo/ano
  hours_y <- labor$hours_m * 12                         # horas trabalhadas/ano
  uniform_cost_h <- uniform_cost_y / hours_y            # uniforme R$/ano
  
  # custos mão de obra comum  
  Mo1_costs <- list(
    "s13" = with(labor, cMo1 * s13),
    "vacation" = with(labor, cMo1 * vacation),
    "fgts" = with(labor, cMo1 * fgts),
    "dsr" = with(labor, cMo1 * dsr)
  )
  Mo1_costs$subtotal <- labor$cMo1 + with(Mo1_costs, sum(s13, vacation, fgts, dsr))
  Mo1_costs$cs <- Mo1_costs$subtotal * labor$cs
  Mo1_costs$total_h <- with(Mo1_costs, subtotal + cs) + cbasic_cost_h + uniform_cost_h
  Mo1_costs$salary <- Mo1_costs$total_h * labor$hours_m
  Mo1_costs$cost_y <- labor$Mo1 * Mo1_costs$salary * 12
  
  # custos mão de obra tratorista
  Mo2_costs <- list(
    "s13" = with(labor, cMo2 * s13),
    "vacation" = with(labor, cMo2 * vacation),
    "fgts" = with(labor, cMo2 * fgts),
    "dsr" = with(labor, cMo2 * dsr)
  )
  Mo2_costs$subtotal <- labor$cMo2 + with(Mo2_costs, sum(s13, vacation, fgts, dsr))
  Mo2_costs$cs <- Mo2_costs$subtotal * labor$cs
  Mo2_costs$total_h <- with(Mo2_costs, subtotal + cs) + cbasic_cost_h + uniform_cost_h
  Mo2_costs$salary <- Mo2_costs$total_h * labor$hours_m
  Mo2_costs$cost_y <- labor$Mo2 * Mo2_costs$salary * 12
  
  # atualiza objeto mão de obra
  labor$Mo1_costs <- Mo1_costs
  labor$Mo2_costs <- Mo2_costs
  
  # custo total
  labor$total <- Mo2_costs$cost_y + Mo1_costs$cost_y
  
  ## Despesas gerais -----------------------------------------------------------
  
  # totais de depesas gerais
  general_costs$Dg1tot <- with(general_costs, Dg1 * Dg11)
  general_costs$Dg2tot <- with(general_costs, Dg2 * Dg21)
  general_costs$Dg3tot <- with(general_costs, Dg3 * Dg31)
  general_costs$Dg4tot <- with(general_costs, Dg4 * Dg41)
  
  # Saída ----------------------------------------------------------------------
  
  out <- list("remuneracao_min" = remuneracao_min,
              "precos" = precos,
              "labor" = labor,
              "general_costs" = general_costs,
              "outros_custos" = outros_custos,
              "viabilidade" = viabilidade)
  return(out)
}

#' @title Mão de obra
#'
#' @param Mo1 Tratorista (quantidade)
#' @param Mo2 Campeiro (quantidade)
#' @param cMo1 Valor da mão de obra Tratorista (custo)
#' @param cMo2 Valor da mão de obra Campeiro (custo)
#' @param s13 13o salario
#' @param vacation Férias
#' @param fgts FGTS
#' @param dsr Descanso semanal remunerado
#' @param hours_m Horas Trabalhadas/mês
#' @param cs Contribuição social
#' @param cbasic Cesta básica
#' @param uniform Uniforme (quantidade)
#' @param uniform_cost Uniforme (custo)
#'
#' @return A list
#' @export
#'
#' @examples
bio_labor <- function(Mo1, Mo2, cMo1, cMo2, s13, vacation, fgts,
                      dsr, cs, hours_m, cbasic, uniform, uniform_cost) {
  x <- list(Mo1, Mo2, cMo1, cMo2, s13, vacation, fgts,
            dsr, cs, hours_m, cbasic, uniform, uniform_cost)
  names(x) <- c("Mo1", "Mo2", "cMo1", "cMo2", "s13", "vacation", "fgts",
                "dsr", "cs", "hours_m", "cbasic", "uniform", "uniform_cost")
  return(x)
}

#' @title Despesas gerais
#'
#' @param Dg1 Transporte interno (quantidade)
#' @param Dg11 Transporte interno (custo unitário)
#' @param Dg2 Assistência veterinária (quantidade)
#' @param Dg21 Assistência veterinária (custo unitário)
#' @param Dg3 Despesas contabilidade (quantidade)
#' @param Dg31 Despesas contabilidade (custo unitário)
#' @param Dg4 Energia elétrica (quantidade)
#' @param Dg41 Energia elétrica (custo unitário)
#'
#' @return A list
#' @export
#'
#' @examples
bio_general_costs <- function(Dg1, Dg11, Dg2, Dg21, Dg3, Dg31, Dg4, Dg41) {
  x <- list(Dg1, Dg11, Dg2, Dg21, Dg3, Dg31, Dg4, Dg41)
  names(x) <- c("Dg1", "Dg11", "Dg2", "Dg21", "Dg3", "Dg31", "Dg4", "Dg41")
  return(x)
}

#' Title
#'
#' @param instalacoes_maintenance 
#' @param maquinas_maintenance 
#' @param other_medication 
#' @param fuel_lubricant 
#'
#' @return
#' @export
#'
#' @examples
outros_custos <- function(instalacoes_maintenance, maquinas_maintenance,
                          other_medication, fuel_lubricant) {
  x <- list(instalacoes_maintenance, maquinas_maintenance,
            other_medication, fuel_lubricant)
  names(x) <- c("instalacoes_maintenance", "maquinas_maintenance",
                "other_medication", "fuel_lubricant")
  return(x)
}

#' Title
#'
#' @param VTN Valor da terra nua no município de exploração (R$/ha)
#' @param GU Grau de utilização (GU) em %
#' @param p_inv_socios Invest. dos sócios (% ano)
#' @param p_inv_terceiro Aquisição de capital de terceiro sobre o valor do rebanho (%)
#' @param parcelas_ano Total de parcelas (ano)
#' @param carencia_ano Carência para pag. passivo (ano)
#' @param taxa_juros_passivo Taxa de juros (a.a)
#' @param taxa_desconto_real Taxa real de desconto real já expurgando a inflação
#' @param taxa_MTIR Taxa Financiamento MTIR
#' @param taxa_inflacao_aa Taxa de inflação (a.a)
#'
#' @return
#' @export
#'
#' @examples
viabilidade <- function(VTN, GU, p_inv_socios, p_inv_terceiro, parcelas_ano,
                        carencia_ano, taxa_juros_passivo, taxa_desconto_real,
                        taxa_MTIR, taxa_inflacao_aa) {
  x <- list(VTN, GU, p_inv_socios, p_inv_terceiro, parcelas_ano, carencia_ano,
            taxa_juros_passivo, taxa_desconto_real, taxa_MTIR, taxa_inflacao_aa)
  names(x) <- c("VTN", "GU", "p_inv_socios", "p_inv_terceiro", "parcelas_ano",
                "carencia_ano", "taxa_juros_passivo", "taxa_desconto_real",
                "taxa_MTIR", "taxa_inflacao_aa")
  return(x)
}

#' Title
#'
#' @param P1 
#' @param P2 
#' @param P3 
#' @param P4 
#' @param P5 
#' @param P6 
#' @param P7 
#' @param P8 
#' @param P9 
#' @param P10 
#' @param P11 
#' @param P12 
#' @param P13 
#' @param P14 
#' @param P15 
#' @param PAqF1 
#' @param PAqF2 
#' @param PAqF3 
#' @param PAqF4 
#' @param PAqF5 
#' @param PAqF6 
#' @param PAqM1 
#' @param PAqM2 
#' @param PAqM3g 
#' @param PAqM6t 
#' @param PAqAcomp 
#' @param pC1 
#' @param pC2 
#' @param pC3 
#' @param pC4 
#' @param pC5 
#' @param pTc6 
#' @param pCc18_20 
#' @param pCc21_28 
#' @param vacaftosa_unit_price 
#' @param vaccarb_unit_price 
#' @param vacbruc_unit_price 
#' @param vermifugo_unit_price 
#'
#' @return
#' @export
#'
#' @examples
precos <- function(P1, P2, P3, P4, P5, P6, P7, P8,
                   P9, P10, P11, P12, P13, P14, P15,
                   PAqF1, PAqF2, PAqF3, PAqF4, PAqF5, PAqF6,
                   PAqM1, PAqM2, PAqM3g, PAqM6t, PAqAcomp,
                   pC1, pC2, pC3, pC4, pC5, pTc6, pCc18_20, pCc21_28,
                   vacaftosa_unit_price, vaccarb_unit_price,
                   vacbruc_unit_price, vermifugo_unit_price) {
  x <- list(P1, P2, P3, P4, P5, P6, P7, P8,
            P9, P10, P11, P12, P13, P14, P15,
            PAqF1, PAqF2, PAqF3, PAqF4, PAqF5, PAqF6,
            PAqM1, PAqM2, PAqM3g, PAqM6t, PAqAcomp,
            pC1, pC2, pC3, pC4, pC5, pTc6, pCc18_20, pCc21_28,
            vacaftosa_unit_price, vaccarb_unit_price,
            vacbruc_unit_price, vermifugo_unit_price)
  names(x) <- c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9",
                "P10", "P11", "P12", "P13", "P14", "P15",
                "PAqF1", "PAqF2", "PAqF3", "PAqF4", "PAqF5", "PAqF6",
                "PAqM1", "PAqM2", "PAqM3g", "PAqM6t", "PAqAcomp",
                "pC1", "pC2", "pC3", "pC4", "pC5", "pTc6", "pCc18_20", "pCc21_28",
                "vacaftosa_unit_price", "vaccarb_unit_price",
                "vacbruc_unit_price", "vermifugo_unit_price")
  return(x)
}
