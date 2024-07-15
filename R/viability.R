#######################################################################################################################################################
# O script realiza uma análise da viabilidade econômica de uma propriedade rural, considerando variáveis de entrada do ambiente de produção e bioma, 
# da propriedade simulada e produção animal e econômicos, resultando em 23 variáveis de fluxo de caixa e os ndicadores financeiros finais (MTIR (Taxa 
# Interna de Retorno Modificada), VPL (Valor Presente Líquido) e Payback, fornecendo uma avaliação detalhada dos fluxos de caixa e indicadores financeiros ao longo do tempo.
########################################################################################################################################################

library(FinCal)

#' @title Análise de Viabilidade Econômica
#'
#' @param ambiente_de_producao Ambiente de produção
#' @param propriedade Propriedade simulada
#' @param economico 
#' @param costs Custos
#'
#' @return A list
#' @export
#'
#' @examples
viability <- function(ambiente_de_producao, propriedade, economico, costs, bioma) {

  # Calcula variáveis iniciais -------------------------------------------------
  
  # Área total (ha) Módulo Mínimo
  area_total <- ifelse(bioma == "Mata Atlântica",
                       propriedade$productive_unit$pasture_disp / 0.8,
                       ifelse(bioma == "Amazônia", 
                              propriedade$productive_unit$pasture_disp / 0.2,
                              ifelse(bioma == "Cerrado", 
                                     propriedade$productive_unit$pasture_disp / 0.7,
                                     NA))) # Módulo Mínimo [área total ha]
  
  # Valor do invest. dos sócios (R$)
  inv_socios <- propriedade$evolution$pTot * economico$viabilidade$p_inv_socios
  # cat("inv_socios:", inv_socios, "\n")
  
  # Valor do passivo
  passivo <- propriedade$evolution$pTot * economico$viabilidade$p_inv_terceiro
  # cat("passivo:", passivo, "\n")
  
  # anos pagamento passivo
  # monta vetor com os anos para pagamento
  # ex.: Ano 2, Ano 3, ..., Ano 12
  anos_carencia <- paste(
    "Ano",
    (economico$viabilidade$carencia_ano + 1):(economico$viabilidade$carencia_ano + economico$viabilidade$parcelas_ano)
  )
  # carencia_ano = 2, parcelas_ano = 10, anos_carencia = "Ano 3" até "Ano 12"
  # cat("carencia_ano:", economico$viabilidade$carencia_ano, "\n")
  # cat("parcelas_ano:", economico$viabilidade$parcelas_ano, "\n")
  # cat("anos_carencia:", anos_carencia, "\n")
  
  # Valor por parcela (s/juros)
  valor_parcela <- passivo / economico$viabilidade$parcelas_ano
  # cat("valor_parcela:", valor_parcela, "\n")
  
  # Valor do juros do passivo
  juros_passivo <- valor_parcela * economico$viabilidade$taxa_juros_passivo
  # cat("taxa_juros_passivo:", economico$viabilidade$taxa_juros_passivo, "\n")
  # cat("juros_passivo:", juros_passivo, "\n")
  
  # Valor total do passivo (ano)
  total_passivo <- valor_parcela + juros_passivo
  # cat("total_passivo:", total_passivo, "\n")
  
  # Taxa nominal requerida (a.a)
  taxa_nominal_aa <- (1 + economico$viabilidade$taxa_desconto_real) * (1 + economico$viabilidade$taxa_inflacao_aa) - 1
  # cat("taxa_desconto_real:", economico$viabilidade$taxa_desconto_real, "\n")
  # cat("taxa_inflacao_aa:", economico$viabilidade$taxa_inflacao_aa, "\n")
  # cat("taxa_nominal_aa:", taxa_nominal_aa, "\n")
  # cat("taxa_MTIR:", economico$viabilidade$taxa_MTIR, "\n")
  
  # Inicializa variáveis para o fluxo ------------------------------------------
  
  anos <- paste0("Ano ", seq(0, 20))
  
  sucata <- c()
  receita <- c()
  impostos <- c()
  receita_liquida <- c()
  administracao <- c()
  custo_oportunidade <- c()
  custo_total <- c()
  ebitda <- c()
  depreciacao <- c()
  ebit <- c()
  itr <- c()
  lair <- c()
  imposto_renda <- c()
  
  maquinas_implementos <- c()
  benfeitorias <- c()
  reposicao_desgaste <- c()
  rebanho <- c()
  reposicao_animal <- c()
  investimentos_capex <- c()
  pagamento_dividas <- c()
  fluxo_caixa_socios <- c()
  fluxo_caixa_livre_acumulado <- c()
  fluxo_caixa_hectare <- c()
  
  # Calcula fluxo usando loop --------------------------------------------------
  
  # cat("valor_sucata_tot:", ambiente_de_producao$inventario$fluxo_caixa$valor_sucata_tot, "\n")
  # cat("valor_sucata_tot:", ambiente_de_producao$inventario$valor_sucata_tot, "\n")
  # cat("pVdT:", propriedade$evolution$pVdT , "\n")
  
  # cat("funrural:", propriedade$productive_unit$funrural , "\n")
  
  # cat("parcela_vendas:", propriedade$desmama$pVdM1 +
  #       propriedade$desmama$pVdF1 +
  #       propriedade$bezerro$pVdM2 +
  #       propriedade$bezerro$pVdF2 +
  #       propriedade$garrote$pVdM3g +
  #       propriedade$boi_magro$pVdM3bm +
  #       propriedade$novilha_29_36$pVdF5 , "\n")
  # (717532.674-266049.6-0)*0.015
  
  # cat("GU:", economico$viabilidade$GU , "\n")
  # cat("VTN:", economico$viabilidade$VTN , "\n")
  
  # cat("aquisicao:", propriedade$aquisicao$total , "\n")
  
  for (ano in anos) {
    
    # Ano zero
    if (ano == "Ano 0") {
      
      sucata[ano] <- 0
      receita[ano] <- 0
      impostos[ano] <- 0
      receita_liquida[ano] <- 0
      administracao[ano] <- 0
      custo_oportunidade[ano] <- 0
      custo_total[ano] <- 0
      ebitda[ano] <- 0
      depreciacao[ano] <- 0
      ebit[ano] <- 0
      itr[ano] <- 0
      lair[ano] <- 0
      imposto_renda[ano] <- 0
      
    } else {
      
      # 2	Outros (sucata)
      if (ano == "Ano 10") {#} | ano == "Ano 30") {
        sucata[ano] <- ambiente_de_producao$inventario$fluxo_caixa$valor_sucata_tot
      } else if (ano == "Ano 20") { #| ano == "Ano 30") {
        sucata[ano] <- ambiente_de_producao$inventario$valor_sucata_tot
      } else {
        sucata[ano] <- 0
      }
      
      # 1	Receita 
      receita[ano] <- propriedade$evolution$pVdT + sucata[ano]
      
      # 3	Impostos sobre a receita
      impostos[ano] <- (
        receita[ano]
        - propriedade$desmama$pVdM1
        - propriedade$desmama$pVdF1
        - propriedade$bezerro$pVdM2
        - propriedade$bezerro$pVdF2
        - propriedade$garrote$pVdM3g
        - propriedade$boi_magro$pVdM3bm
        - propriedade$novilha_29_36$pVdF5
        - sucata[ano]
      ) * propriedade$productive_unit$funrural
      
      # 6	Administração (remuneração do Produtor)
      administracao[ano] <- economico$remuneracao_min
      
      # 7	Custo de oportunidade (ex.arrendamento da terra)
      custo_oportunidade[ano] <- propriedade$productive_unit$arrend_total
      
      # 4	Receita Líquida
      receita_liquida[ano] <- receita[ano] - impostos[ano]
      
      # 5	Custo total - CT
      custo_total[ano] <- costs$total
      
      # 8	EBITDA
      ebitda[ano] <- receita_liquida[ano] - custo_total[ano]
      
      # 9	Depreciação
      depreciacao[ano] <- 0
      
      # 10	EBIT
      ebit[ano] <- ebitda[ano] - depreciacao[ano]
      
      # 11	ITR
      itr[ano] <- economico$viabilidade$VTN * (area_total * 0.5) * economico$viabilidade$GU
      # itr[ano] <- economico$viabilidade$VTN * 1000 * economico$viabilidade$GU
      
      # 12	LAIR
      lair[ano] <- ebit[ano] - itr[ano]
      
      # 13	Imposto de Renda
      imposto_renda[ano] <- IRPF(lair[ano])
    }
    
    # 15	Máquinas e Implementos
    if (ano == "Ano 0" | ano == "Ano 10" ) {#| ano == "Ano 20" | ano == "Ano 30") {
      maquinas_implementos[ano] <- ambiente_de_producao$inventario$fluxo_caixa$valor_inicial_maquinas
    } else {
      maquinas_implementos[ano] <- 0
    }
    
    # 16	Benfeitorias (Barracão)
    if (ano == "Ano 0") {
      benfeitorias[ano] <- ambiente_de_producao$inventario$valor_inicial_barracao
    } else {
      benfeitorias[ano] <- 0
    }
    
    # 17	Reposição pelo desgaste
    if (ano == "Ano 0") {
      reposicao_desgaste[ano] <- 0
    } else {
      reposicao_desgaste[ano] <- ambiente_de_producao$inventario$valor_depreciacao_tot
    }
    
    # 18	Rebanho inicial
    if (ano == "Ano 0") {
      rebanho[ano] <- propriedade$evolution$pTot
    } else {
      rebanho[ano] <- 0
    }
    
    # 19	Reposição de animais
    if (ano == "Ano 0") {
      reposicao_animal[ano] <- 0
    } else {
      reposicao_animal[ano] <- propriedade$aquisicao$total
    }
    
    # 14	Investimentos - CAPEX
    investimentos_capex[ano] <- sum(
      maquinas_implementos[ano],
      benfeitorias[ano],
      reposicao_desgaste[ano],
      rebanho[ano],
      reposicao_animal[ano])
    
    # 20	Pagamento de Dívidas + juros
    if (ano == "Ano 0") {
      pagamento_dividas[ano] <- -passivo
    } else if (ano %in% anos_carencia) {
      pagamento_dividas[ano] <- total_passivo
    } else {
      pagamento_dividas[ano] <- 0
    }
    
    # 21	Fluxo de Caixa dos Sócios
    if (ano == "Ano 0") {
      fluxo_caixa_socios[ano] <- sum(
        lair[ano],
        - imposto_renda[ano],
        - investimentos_capex[ano],
        - pagamento_dividas[ano],
        inv_socios)
    } else {
      fluxo_caixa_socios[ano] <- sum(
        lair[ano],
        - imposto_renda[ano],
        - investimentos_capex[ano],
        - pagamento_dividas[ano])
    }
    
    # 23	Resultado por hectare
    fluxo_caixa_hectare[ano] <- fluxo_caixa_socios[ano] / propriedade$productive_unit$pasture_disp
    
  }
  
  # 22	Fluxo de Caixa Livre Acumulado
  fluxo_caixa_livre_acumulado <- cumsum(fluxo_caixa_socios)
  
  # Calcula variáveis finais ---------------------------------------------------
  
  # MTIR real já expurgando a inflação
  mtir_real <- MIRR(fluxo_caixa_socios,
                    economico$viabilidade$taxa_MTIR,
                    economico$viabilidade$taxa_desconto_real)
  
  # VPL
  # vpl <- VPL(as.numeric(fluxo_caixa_socios[-1]),
  #            economico$viabilidade$taxa_desconto_real) + as.numeric(fluxo_caixa_socios[1])
  vpl <- FinCal::npv(r = economico$viabilidade$taxa_desconto_real,
                     cf = as.numeric(fluxo_caixa_socios))
  # cat("vpl:", vpl, "\n")
  
  # PAYBACK
  payback <- sum(fluxo_caixa_livre_acumulado[-1] < 0) + 1
  # payback <- which(fluxo_caixa_livre_acumulado[-1] >= 0)[1] - 1
  
  # INVESTIMENTO TOTAL
  investimento_total <- inv_socios + passivo
  
  # Lucro/ha
  lucro_ha <- mean(fluxo_caixa_socios[-1]) / propriedade$productive_unit$pasture_disp
  
  # Saída ----------------------------------------------------------------------
  
  out <- list("MTIR" = mtir_real,
              "VPL" = vpl,
              "payback" = payback,
              "taxa_desconto_real" = economico$viabilidade$taxa_desconto_real,
              "investimento_total" = investimento_total,
              "lucro_hectare" = lucro_ha,
              "area_total" = area_total,
              "investimento_socios" = inv_socios,
              "valor_passivo" = passivo,
              "valor_parcela" = valor_parcela,
              "Juros_passivo" = juros_passivo,
              "total_passivo" = total_passivo,
              "taxa_nominal" = taxa_nominal_aa,
              "fluxos" = list(
                "sucata" = sucata,
                "receita" = receita,
                "impostos" = impostos,
                "receita_liquida" = receita_liquida,
                "administracao" = administracao,
                "custo_oportunidade" = custo_oportunidade,
                "custo_total" = custo_total,
                "ebitda" = ebitda,
                "depreciacao" = depreciacao,
                "ebit" = ebit,
                "itr" = itr,
                "lair" = lair,
                "imposto_renda" = imposto_renda,
                
                "maquinas_implementos" = maquinas_implementos,
                "benfeitorias" = benfeitorias,
                "reposicao_desgaste" = reposicao_desgaste,
                "rebanho" = rebanho,
                "reposicao_animal" = reposicao_animal,
                "investimentos_capex" = investimentos_capex,
                "pagamento_dividas" = pagamento_dividas,
                "fluxo_caixa_socios" = fluxo_caixa_socios,
                "fluxo_caixa_livre_acumulado" = fluxo_caixa_livre_acumulado,
                "fluxo_caixa_hectare" = fluxo_caixa_hectare))
  
  return(out)
}

library(FinCal)

#' @title Valor Presente Líquido (VPL)
#'
#' @param cashflow Fluxo de caixa
#' @param rate Taxa de investimento
#'
#' @return
#' @export
#'
#' @examples
VPL <- function(cashflow, rate) {
  # Case when cash outflow (initial investment) is present
  period <- 1:length(cashflow)
  if (cashflow[[1]] < 0) {
    period <- period - 1
  }
  
  # compute vpl
  return(sum(cashflow / (1 + rate)^period))
}

#' @title Taxa Interna de Retorno Modificada (TIRM)
#' 
#' @details
#' PRATES, W. R. O que é TIRM (Taxa Interna de Retorno Modificada)?. 2017. Disponível em: https://cienciaenegocios.com/o-que-e-tirm-taxa-interna-de-retorno-modificada/
#'
#' @param x Fluxo de caixa
#' @param discount.rate Taxa de financiamento
#' @param investment.rate Taxa de reinvestimento
#'
#' @return Numeric
#' @export
#'
#' @examples
MIRR <- function(x, discount.rate = 0.1, investment.rate = 0.05){
  
  # move cash flows
  # negative
  cf.neg <- (x < 0) * x
  pv.cf.neg <- cf.neg / (1 + discount.rate)^{0:(length(x)-1)}
  pv <- sum(pv.cf.neg)
  
  # positive
  cf.pos <- (x > 0) * x
  fv.cf.pos <- cf.pos * (1 + investment.rate)^{rev(0:(length(x)-1))}
  fv <- sum(fv.cf.pos)
  
  mirr.per.period <- ( fv / abs(pv) )^{1 / (length(x) - 1)} - 1
  
  return(mirr.per.period)
}

#' @title Base de cálculo do Imposto sobre a Renda das Pessoas Físicas (IRPF)
#'
#' @param x Valor (R$)
#'
#' @return Numeric
#' @export
#'
#' @examples
IRPF <- function(x) {
  if (x > 55976.16) {
    x_out <- 0.275 * x
  } else if(x > 45012.6) {
    x_out <- 0.225 * x
  } else if(x > 33919.8) {
    x_out <- 0.15 * x
  } else if(x > 22847.76) {
    x_out <- 0.075 * x
  } else {
    x_out <- 0
  }
  
  return(x_out)
}
