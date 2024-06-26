ambiente_de_producao <- function(productive_unit, inventario) {
  x <- list(productive_unit, inventario)
  names(x) <- c("productive_unit", "inventario")
  return(x)
}

#' @title Dados da unidade produtiva
#'
#' @param nivel_intensificacao Nível de Intensificação alto: 1, média: 2, baixo: 3
#' @param num_matrizes Número de matrizes 
#' @param cap_sup Capacidade de suporte
#' @param funrural Funrural
#' @param arrend_tax Taxa de arrendamento
#'
#' @return A list
#' @export
#'
#' @examples
bio_productive_unit <- function(nivel_intensificacao, num_matrizes, cap_sup,
                                funrural, arrend_tax) {
  x <- list(nivel_intensificacao, num_matrizes, cap_sup, funrural, arrend_tax)
  names(x) <- c("nivel_intensificacao", "num_matrizes", "cap_sup",
                "funrural", "arrend_tax")
  return(x)
}

#' @title Inventário
#'
#' @param nivel_intensificacao Nível de intensificação
#' @param distribuidor_semente_Vi Valor Inicial (R$) - Distribuidor de sementes/fertilizantes
#' @param distribuidor_calcario_Vi Valor Inicial (R$) - Distribuidor de Calcário
#' @param grade_aradora_Vi Valor Inicial (R$) - Grade Aradora
#' @param grade_niveladora_Vi Valor Inicial (R$) - Grade Niveladora
#' @param carreta_Vi Valor Inicial (R$) - Carreta (4 a 6 toneladas)
#' @param matabroto_Vi Valor Inicial (R$) - Matabroto Hidráulico EBT M
#' @param conj_hidr_diant_Vi Valor Inicial (R$) - Conjunto Hidráulico dianteiro
#' @param trator75 Valor (R$/cv) - Trator 75 cv 
#' @param trator180 Valor (R$/cv) - Trator 180 cv
#' @param trator65 Valor (R$/cv) - Trator 65 cv
#' @param trator140 Valor (R$/cv) - Trator 140 cv
#' @param trator75_Sp Valor Sucata (%) - Trator 75 cv 
#' @param trator180_Sp Valor Sucata (%) - Trator 180 cv
#' @param trator65_Sp Valor Sucata (%) - Trator 65 cv
#' @param trator140_Sp Valor Sucata (%) - Trator 140 cv
#' @param distribuidor_semente_Sp Valor Sucata (%) - Distribuidor de sementes/fertilizantes
#' @param distribuidor_calcario_Sp Valor Sucata (%) - Distribuidor de Calcário
#' @param terracador_Sp Valor Sucata (%) - Terraçador
#' @param pulverizador_Sp Valor Sucata (%) - Pulverizador de Barras 6m Montado
#' @param grade_aradora_Sp Valor Sucata (%) - Grade Aradora
#' @param grade_niveladora_Sp Valor Sucata (%) - Grade Niveladora
#' @param carreta_Sp Valor Sucata (%) - Carreta (4 a 6 toneladas)
#' @param matabroto_Sp Valor Sucata (%) - Matabroto Hidráulico EBT M
#' @param conj_hidr_diant_Sp Valor Sucata (%) - Conjunto Hidráulico dianteiro
#' @param barracao_Sp Valor Sucata (%) - Barracão
#' @param trator75_Vu Vida Útil (anos) - Trator 75 cv
#' @param trator180_Vu Vida Útil (anos) - Trator 180 cv
#' @param trator65_Vu Vida Útil (anos) - Trator 65 cv
#' @param trator140_Vu Vida Útil (anos) - Trator 140 cv
#' @param distribuidor_semente_Vu Vida Útil (anos) - Distribuidor de sementes/fertilizantes
#' @param distribuidor_calcario_Vu Vida Útil (anos) - Distribuidor de Calcário
#' @param terracador_Vu Vida Útil (anos) - Terraçador
#' @param pulverizador_Vu Vida Útil (anos) - Pulverizador de Barras 6m Montado
#' @param grade_aradora_Vu Vida Útil (anos) - Grade Aradora
#' @param grade_niveladora_Vu Vida Útil (anos) - Grade Niveladora
#' @param carreta_Vu Vida Útil (anos) - Carreta (4 a 6 toneladas)
#' @param matabroto_Vu Vida Útil (anos) - Matabroto Hidráulico EBT M
#' @param conj_hidr_diant_Vu Vida Útil (anos) - Conjunto Hidráulico dianteiro
#' @param barracao_Vu Vida Útil (anos) - Barracão
#' @param price_m2 Preco do  m²
#'
#' @return A list
#' @export
#'
#' @examples
inventario <- function(nivel_intensificacao,
                      
                      distribuidor_semente_Vi, distribuidor_calcario_Vi,
                      grade_aradora_Vi, grade_niveladora_Vi, carreta_Vi,
                      matabroto_Vi, conj_hidr_diant_Vi,
                      
                      trator75, trator180, trator65, trator140,
                      trator75_Sp, trator180_Sp, trator65_Sp, trator140_Sp,
                      distribuidor_semente_Sp, distribuidor_calcario_Sp,
                      terracador_Sp, pulverizador_Sp, grade_aradora_Sp,
                      grade_niveladora_Sp, carreta_Sp, matabroto_Sp,
                      conj_hidr_diant_Sp, barracao_Sp,
                      
                      trator75_Vu, trator180_Vu, trator65_Vu, trator140_Vu,
                      distribuidor_semente_Vu, distribuidor_calcario_Vu,
                      terracador_Vu, pulverizador_Vu, grade_aradora_Vu,
                      grade_niveladora_Vu, carreta_Vu, matabroto_Vu,
                      conj_hidr_diant_Vu, barracao_Vu,
                      
                      price_m2) {
  
  # Valor inicial --------------------------------------------------------------
  
  # valor inicial por item do inventário
  if (nivel_intensificacao <= 2) {
    # maquina/implemento
    trator1_Sp <- trator75_Sp # valor de sucata
    trator1_cv <- 65          # potencia
    trator1_vcv <- trator75   # valor / cv
    trator1_Vu <- trator75_Vu # vida util
    # cat("trator1_Sp:", trator1_Sp, "\n")
    # cat("trator1_cv:", trator1_cv, "\n")
    # cat("trator1_vcv:", trator1_vcv, "\n")
    # cat("trator1_Vu:", trator1_Vu, "\n")
    
    trator2_Sp <- trator180_Sp
    trator2_cv <- 140
    trator2_vcv <- trator180
    trator2_Vu <- trator180_Vu
    # cat("trator2_Sp:", trator2_Sp, "\n")
    # cat("trator2_cv:", trator2_cv, "\n")
    # cat("trator2_vcv:", trator2_vcv, "\n")
    # cat("trator2_Vu:", trator2_Vu, "\n")
    
    # descrição
    barracao_area <- 400
    # valor inicial
    trator1_Vi <- trator1_cv * trator1_vcv  # cv * (valor / cv)
    trator2_Vi <- trator2_cv * trator2_vcv   # cv * (valor / cv)
    terracador_Vi <- 15000
    pulverizador_Vi <- 8000
  } else {
    # maquina/implemento
    trator1_Sp <- trator65_Sp # valor de sucata
    trator1_cv <- 50          # potencia
    trator1_vcv <- trator65   # valor / cv
    trator1_Vu <- trator65_Vu # vida util
    
    trator2_Sp <- trator140_Sp
    trator2_cv <- 110
    trator2_vcv <- trator140
    trator2_Vu <- trator140_Vu
    
    # descrição
    barracao_area <- 120
    # valor inicial
    trator1_Vi <- trator1_cv * trator1_vcv   # cv * (valor / cv)
    trator2_Vi <- trator2_cv * trator2_vcv    # cv * (valor / cv)
    terracador_Vi <- 0
    pulverizador_Vi <- 0
  }
  
  barracao_Vi <- barracao_area * price_m2
  
  # valor inicial total
  valor_inicial <- c(trator1_Vi, trator2_Vi, distribuidor_semente_Vi,
                     distribuidor_calcario_Vi, terracador_Vi, pulverizador_Vi,
                     grade_aradora_Vi, grade_niveladora_Vi, carreta_Vi,
                     matabroto_Vi, conj_hidr_diant_Vi, barracao_Vi)
  Vi_tot <- sum(trator1_Vi, trator2_Vi, distribuidor_semente_Vi,
                distribuidor_calcario_Vi, terracador_Vi, pulverizador_Vi,
                grade_aradora_Vi, grade_niveladora_Vi, carreta_Vi, matabroto_Vi,
                conj_hidr_diant_Vi, barracao_Vi)
  
  # Valor sucata ---------------------------------------------------------------
  
  # valor sucata por item do inventário
  trator1_Sv <- trator1_Sp * trator1_Vi
  trator2_Sv <- trator2_Sp * trator2_Vi
  distribuidor_semente_Sv <- distribuidor_semente_Sp * distribuidor_semente_Vi
  distribuidor_calcario_Sv <- distribuidor_calcario_Sp * distribuidor_calcario_Vi
  terracador_Sv <- terracador_Sp * terracador_Vi
  pulverizador_Sv <- pulverizador_Sp * pulverizador_Vi
  grade_aradora_Sv <- grade_aradora_Sp * grade_aradora_Vi
  grade_niveladora_Sv <- grade_niveladora_Sp * grade_niveladora_Vi
  carreta_Sv <- carreta_Sp * carreta_Vi
  matabroto_Sv <- matabroto_Sp * matabroto_Vi
  conj_hidr_diant_Sv <- conj_hidr_diant_Sp * conj_hidr_diant_Vi
  barracao_Sv <- barracao_Sp * barracao_Vi
  
  # valor sucata total
  valor_sucata_perc <- c(trator1_Sp, trator2_Sp, distribuidor_semente_Sp,
                         distribuidor_calcario_Sp, terracador_Sp, pulverizador_Sp,
                         grade_aradora_Sp, grade_niveladora_Sp, carreta_Sp,
                         matabroto_Sp, conj_hidr_diant_Sp, barracao_Sp)
  valor_sucata <- c(trator1_Sv, trator2_Sv, distribuidor_semente_Sv,
                    distribuidor_calcario_Sv, terracador_Sv, pulverizador_Sv,
                    grade_aradora_Sv, grade_niveladora_Sv, carreta_Sv,
                    matabroto_Sv, conj_hidr_diant_Sv, barracao_Sv)
  Sv_tot <- sum(trator1_Sv, trator2_Sv, distribuidor_semente_Sv,
                distribuidor_calcario_Sv, terracador_Sv, pulverizador_Sv,
                grade_aradora_Sv, grade_niveladora_Sv, carreta_Sv, matabroto_Sv,
                conj_hidr_diant_Sv, barracao_Sv)
  
  # Depreciação ----------------------------------------------------------------
  
  # depreciação por item do inventário
  trator1_dep <- (trator1_Vi - trator1_Sv) / trator1_Vu
  trator2_dep <- (trator2_Vi - trator2_Sv) / trator2_Vu
  distribuidor_semente_dep <- (distribuidor_semente_Vi - distribuidor_semente_Sv) / distribuidor_semente_Vu
  distribuidor_calcario_dep <- (distribuidor_calcario_Vi - distribuidor_calcario_Sv) / distribuidor_calcario_Vu
  terracador_dep <- (terracador_Vi - terracador_Sv) / terracador_Vu
  pulverizador_dep <- (pulverizador_Vi - pulverizador_Sv) / pulverizador_Vu
  grade_aradora_dep <- (grade_aradora_Vi - grade_aradora_Sv) / grade_aradora_Vu
  grade_niveladora_dep <- (grade_niveladora_Vi - grade_niveladora_Sv) / grade_niveladora_Vu
  carreta_dep <- (carreta_Vi - carreta_Sv) / carreta_Vu
  matabroto_dep <- (matabroto_Vi - matabroto_Sv) / matabroto_Vu
  conj_hidr_diant_dep <- (conj_hidr_diant_Vi - conj_hidr_diant_Sv) / conj_hidr_diant_Vu
  barracao_dep <- (barracao_Vi - barracao_Sv) / barracao_Vu
  
  # depreciação total
  vida_util <- c(trator1_Vu, trator2_Vu, distribuidor_semente_Vu,
                 distribuidor_calcario_Vu, terracador_Vu, pulverizador_Vu,
                 grade_aradora_Vu, grade_niveladora_Vu, carreta_Vu,
                 matabroto_Vu, conj_hidr_diant_Vu, barracao_Vu)
  depreciacao <- c(trator1_dep, trator2_dep, distribuidor_semente_dep,
                   distribuidor_calcario_dep, terracador_dep, pulverizador_dep,
                   grade_aradora_dep, grade_niveladora_dep, carreta_dep,
                   matabroto_dep, conj_hidr_diant_dep, barracao_dep)
  dep_tot <- sum(trator1_dep, trator2_dep, distribuidor_semente_dep,
                 distribuidor_calcario_dep, terracador_dep, pulverizador_dep,
                 grade_aradora_dep, grade_niveladora_dep, carreta_dep, matabroto_dep,
                 conj_hidr_diant_dep, barracao_dep)
  
  # Informações complementares -------------------------------------------------
  
  ## Custo ---------------------------------------------------------------------
  
  # instalações e benfeitorias
  custo_inst_benf_Vi <- barracao_Vi - barracao_Sv
  custo_inst_benf_dep <- barracao_dep
  
  # máquinas e equipamentos
  custo_maq_equip_Vi <- (Vi_tot - barracao_Vi) - (Sv_tot - barracao_Sv)
  custo_maq_equip_dep <- dep_tot - barracao_dep
  
  ## Fluxo de caixa ------------------------------------------------------------
  fc_maq_equip_Vi <- Vi_tot - barracao_Vi
  fc_Sv <- Sv_tot - barracao_Sv
  fc_Sv_integral <- Sv_tot
  
  # Saída ----------------------------------------------------------------------
  
  out <- list("valor_inicial" = valor_inicial,
              "valor_inicial_tot" = Vi_tot,
              "valor_sucata_perc" = valor_sucata_perc,
              "valor_sucata" = valor_sucata,
              "valor_sucata_tot" = Sv_tot,
              "vida_util" = vida_util,
              "depreciacao" = depreciacao,
              "valor_depreciacao_tot" = dep_tot,
              "valor_inicial_barracao" = barracao_Vi,
              "custo" = list(
                "valor_inicial_instalacao" = custo_inst_benf_Vi,
                "valor_depreciacao_instalacao" = custo_inst_benf_dep,
                "valor_inicial_maquinas" = custo_maq_equip_Vi,
                "valor_depreciacao_maquinas" = custo_maq_equip_dep),
              "fluxo_caixa" = list(
                "valor_inicial_maquinas" = fc_maq_equip_Vi,
                "valor_sucata_tot" = fc_Sv,
                "valor_sucata_integral" = fc_Sv_integral))
  return(out)
}

simula_propriedade <- function(producao_animal, economico) {
  
  productive_unit <- producao_animal$productive_unit
  
  vaca <- bio_vaca(
    P1 = economico$precos$P1,
    W1 = producao_animal$pesos$W1,
    R1 = producao_animal$rendimentos$R1,
    Ps1 = producao_animal$pressao$Ps1,
    wF6 = producao_animal$pesos$wF6,
    UAF6 = producao_animal$coeficientes$UAF6,
    AqF6 = producao_animal$aquisicao$AqF6,
    WAqF6 = producao_animal$pesos$WAqF6,
    PAqF6 = economico$precos$PAqF6,
    AqAcomp = producao_animal$aquisicao$AqAcomp,
    PAqAcomp = economico$precos$PAqAcomp,
    Mt_ini = producao_animal$mortalidade$Mt_ini
  )
  
  desmama <- bio_desmama(
    P2 = economico$precos$P2,
    W2 = producao_animal$pesos$W2,
    Ps2 = producao_animal$pressao$Ps2,
    wM1 = producao_animal$pesos$wM1,
    UAM1 = producao_animal$coeficientes$UAM1,
    AqM1 = producao_animal$aquisicao$AqM1,
    WAqM1 = producao_animal$pesos$WAqM1,
    PAqM1 = economico$precos$PAqM1,
    
    P3 = economico$precos$P3,
    W3 = producao_animal$pesos$W3,
    Ps3 = producao_animal$pressao$Ps3,
    wF1 = producao_animal$pesos$wF1,
    UAF1 = producao_animal$coeficientes$UAF1,
    AqF1 = producao_animal$aquisicao$AqF1,
    WAqF1 = producao_animal$pesos$WAqF1,
    PAqF1 = economico$precos$PAqF1
  )
  
  bezerro <- bio_bezerro(
    P4 = economico$precos$P4,
    W4 = producao_animal$pesos$W4,
    Ps4 = producao_animal$pressao$Ps4,
    wM2 = producao_animal$pesos$wM2,
    UAM2 = producao_animal$coeficientes$UAM2,
    AqM2 = producao_animal$aquisicao$AqM2,
    WAqM2 = producao_animal$pesos$WAqM2,
    PAqM2 = economico$precos$PAqM2,
    
    P5 = economico$precos$P5,
    W5 = producao_animal$pesos$W5,
    Ps5 = producao_animal$pressao$Ps5,
    wF2 = producao_animal$pesos$wF2,
    UAF2 = producao_animal$coeficientes$UAF2,
    AqF2 = producao_animal$aquisicao$AqF2,
    WAqF2 = producao_animal$pesos$WAqF2,
    PAqF2 = economico$precos$PAqF2
  )
  
  novilha_18_20 <- bio_novilha_18_20(
    P6 = economico$precos$P6,
    Pp6 = producao_animal$premio$Pp6,
    W6 = producao_animal$pesos$W6,
    R6 = producao_animal$rendimentos$R6,
    Ps6 = producao_animal$pressao$Ps6,
    wF3 = producao_animal$pesos$wF3,
    UAF3 = producao_animal$coeficientes$UAF3,
    AqF3 = producao_animal$aquisicao$AqF3,
    WAqF3 = producao_animal$pesos$WAqF3,
    PAqF3 = economico$precos$PAqF3,
    ccF3Wo = producao_animal$ciclo_curto$ccF3Wo,
    ccF3dg = producao_animal$ciclo_curto$ccF3dg,
    ccF3pf = producao_animal$ciclo_curto$ccF3pf
  )
  
  novilha_21_28 <- bio_novilha_21_28(
    P7 = economico$precos$P7,
    Pp7 = producao_animal$premio$Pp7,
    W7 = producao_animal$pesos$W7,
    R7 = producao_animal$rendimentos$R7,
    Ps7 = producao_animal$pressao$Ps7,
    wF4 = producao_animal$pesos$wF4,
    UAF4 = producao_animal$coeficientes$UAF4,
    AqF4 = producao_animal$aquisicao$AqF4,
    WAqF4 = producao_animal$pesos$WAqF4,
    PAqF4 = economico$precos$PAqF4
  )
  
  novilha_29_36 <- bio_novilha_29_36(
    P8 = economico$precos$P8,
    Pp8 = producao_animal$premio$Pp8,
    W8 = producao_animal$pesos$W8,
    R8 = producao_animal$rendimentos$R8,
    Ps8 = producao_animal$pressao$Ps8,
    wF5 = producao_animal$pesos$wF5,
    UAF5 = producao_animal$coeficientes$UAF5,
    AqF5 = producao_animal$aquisicao$AqF5,
    WAqF5 = producao_animal$pesos$WAqF5,
    PAqF5 = economico$precos$PAqF5
  )
  
  garrote <- bio_garrote(
    P9 = economico$precos$P9,
    W9 = producao_animal$pesos$W9,
    R9 = producao_animal$rendimentos$R9,
    Ps9 = producao_animal$pressao$Ps9,
    wM3g = producao_animal$pesos$wM3g,
    UAM3g = producao_animal$coeficientes$UAM3g,
    AqM3g = producao_animal$aquisicao$AqM3g,
    WAqM3g = producao_animal$pesos$WAqM3g,
    PAqM3g = economico$precos$PAqM3g
  )
  
  boi_magro <- bio_boi_magro(
    P10 = economico$precos$P10,
    W10 = producao_animal$pesos$W10,
    R10 = producao_animal$rendimentos$R10,
    Ps10 = producao_animal$pressao$Ps10,
    wM3bm = producao_animal$pesos$wM3bm,
    UAM3bm = producao_animal$coeficientes$UAM3bm,
    AqM3bm = producao_animal$aquisicao$AqM3bm
  )
  
  boi_gordo_18_20 <- bio_boi_gordo_18_20(
    P11 = economico$precos$P11,
    W11 = producao_animal$pesos$W11,
    Pp11 = producao_animal$premio$Pp11,
    R11 = producao_animal$rendimentos$R11,
    Ps11 = producao_animal$pressao$Ps11,
    wM3 = producao_animal$pesos$wM3,
    UAM3 = producao_animal$coeficientes$UAM3,
    AqM3 = producao_animal$aquisicao$AqM3,
    ccM3Wo = producao_animal$ciclo_curto$ccM3Wo,
    ccM3dg = producao_animal$ciclo_curto$ccM3dg,
    ccM3pf = producao_animal$ciclo_curto$ccM3pf
  )
  
  boi_gordo_21_28 <- bio_boi_gordo_21_28(
    P12 = economico$precos$P12,
    W12 = producao_animal$pesos$W12,
    Pp12 = producao_animal$premio$Pp12,
    R12 = producao_animal$rendimentos$R12,
    Ps12 = producao_animal$pressao$Ps12,
    wM4 = producao_animal$pesos$wM4,
    UAM4 = producao_animal$coeficientes$UAM4,
    AqM4 = producao_animal$aquisicao$AqM4,
    ccM4Wo = producao_animal$ciclo_curto$ccM4Wo,
    ccM4dg = producao_animal$ciclo_curto$ccM4dg,
    ccM4pf = producao_animal$ciclo_curto$ccM4pf
  )
  
  boi_gordo_29_36 <- bio_boi_gordo_29_36(
    P13 = economico$precos$P13,
    W13 = producao_animal$pesos$W13,
    Pp13 = producao_animal$premio$Pp13,
    R13 = producao_animal$rendimentos$R13,
    Ps13 = producao_animal$pressao$Ps13,
    wM5 = producao_animal$pesos$wM5,
    UAM5 = producao_animal$coeficientes$UAM5,
    AqM5 = producao_animal$aquisicao$AqM5,
    ccM5Wo = producao_animal$ciclo_curto$ccM5Wo,
    ccM5dg = producao_animal$ciclo_curto$ccM5dg,
    ccM5pf = producao_animal$ciclo_curto$ccM5pf
  )
  
  boi_gordo_37_48 <- bio_boi_gordo_37_48(
    P14 = economico$precos$P14,
    W14 = producao_animal$pesos$W14,
    Pp14 = producao_animal$premio$Pp14,
    R14 = producao_animal$rendimentos$R14,
    Ps14 = producao_animal$pressao$Ps14,
    wM6 = producao_animal$pesos$wM6,
    UAM6 = producao_animal$coeficientes$UAM6,
    AqM6 = producao_animal$aquisicao$AqM6
  )
  
  boi_gordo_toruno <- bio_boi_gordo_toruno(
    P15 = economico$precos$P15,
    W15 = producao_animal$pesos$W15,
    R15 = producao_animal$rendimentos$R15,
    Ps15 = producao_animal$pressao$Ps15,
    wM6bgt = producao_animal$pesos$wM6bgt,
    UAM6bgt = producao_animal$coeficientes$UAM6bgt,
    AqM6bgt = producao_animal$aquisicao$AqM6bgt
  )
  
  touro <- bio_touro(
    wM6t = producao_animal$pesos$wM6t,
    UAM6t = producao_animal$coeficientes$UAM6t,
    AqM6t = producao_animal$aquisicao$AqM6t,
    WAqM6t = producao_animal$pesos$WAqM6t,
    PAqM6t = economico$precos$PAqM6t
  )
  
  impactos <- bio_impact(
    Mt1 = producao_animal$mortalidade$Mt1,
    Mt2 = producao_animal$mortalidade$Mt2,
    Mt3 = producao_animal$mortalidade$Mt3,
    Mt4 = producao_animal$mortalidade$Mt4,
    Mt5 = producao_animal$mortalidade$Mt5,
    Mt6 = producao_animal$mortalidade$Mt6
  )
  
  ref_zoo <- bio_zoo_ref(
    Rs1 = producao_animal$referencias_zootecnicas$Rs1,
    Rp2 = producao_animal$referencias_zootecnicas$Rp2,
    Rp3 = producao_animal$referencias_zootecnicas$Rp3,
    Tn = producao_animal$referencias_zootecnicas$Tn
  )
  
  daily_feeding_consumption <- bio_daily_feeding_consumption(
    C1 = producao_animal$consumo$C1,
    pC1 = economico$precos$pC1,
    C2 = producao_animal$consumo$C2,
    pC2 = economico$precos$pC2,
    C3 = producao_animal$consumo$C3,
    pC3 = economico$precos$pC3,
    C4 = producao_animal$consumo$C4,
    pC4 = economico$precos$pC4,
    C5 = producao_animal$consumo$C5,
    pC5 = economico$precos$pC5,
    Tc6 = producao_animal$consumo$Tc6,
    C7 = producao_animal$consumo$C7,
    pTc6 = economico$precos$pTc6,
    creep_days = producao_animal$consumo$creep_days,
    pCc18_20 = economico$precos$pCc18_20,
    pCc21_28 = economico$precos$pCc21_28
  )
  
  vax_af <- bio_vax_af(
    F6 = producao_animal$vacinas$vacaftosa_F6,
    F5 = producao_animal$vacinas$vacaftosa_F5,
    F4 = producao_animal$vacinas$vacaftosa_F4,
    F3 = producao_animal$vacinas$vacaftosa_F3,
    F2 = producao_animal$vacinas$vacaftosa_F2,
    F1 = producao_animal$vacinas$vacaftosa_F1,
    M1 = producao_animal$vacinas$vacaftosa_M1,
    M2 = producao_animal$vacinas$vacaftosa_M2,
    M3g = producao_animal$vacinas$vacaftosa_M3g,
    M3 = producao_animal$vacinas$vacaftosa_M3,
    M3bm = producao_animal$vacinas$vacaftosa_M3bm,
    M4 = producao_animal$vacinas$vacaftosa_M4,
    M5 = producao_animal$vacinas$vacaftosa_M5,
    M6 = producao_animal$vacinas$vacaftosa_M6,
    M6bgt = producao_animal$vacinas$vacaftosa_M6bgt,
    M6t = producao_animal$vacinas$vacaftosa_M6t,
    unit_price = economico$precos$vacaftosa_unit_price
  )
  
  vax_carb <- bio_vax_carb(
    F6 = producao_animal$vacinas$vaccarb_F6,
    F5 = producao_animal$vacinas$vaccarb_F5,
    F4 = producao_animal$vacinas$vaccarb_F4,
    F3 = producao_animal$vacinas$vaccarb_F3,
    F2 = producao_animal$vacinas$vaccarb_F2,
    F1 = producao_animal$vacinas$vaccarb_F1,
    M1 = producao_animal$vacinas$vaccarb_M1,
    M2 = producao_animal$vacinas$vaccarb_M2,
    M3g = producao_animal$vacinas$vaccarb_M3g,
    M3 = producao_animal$vacinas$vaccarb_M3,
    M3bm = producao_animal$vacinas$vaccarb_M3bm,
    M4 = producao_animal$vacinas$vaccarb_M4,
    M5 = producao_animal$vacinas$vaccarb_M5,
    M6 = producao_animal$vacinas$vaccarb_M6,
    M6bgt = producao_animal$vacinas$vaccarb_M6bgt,
    M6t = producao_animal$vacinas$vaccarb_M6t,
    unit_price = economico$precos$vaccarb_unit_price
  )
  
  vax_bru <- bio_vax_bru(
    F6 = producao_animal$vacinas$vacbruc_F6,
    F5 = producao_animal$vacinas$vacbruc_F5,
    F4 = producao_animal$vacinas$vacbruc_F4,
    F3 = producao_animal$vacinas$vacbruc_F3,
    F2 = producao_animal$vacinas$vacbruc_F2,
    F1 = producao_animal$vacinas$vacbruc_F1,
    M1 = producao_animal$vacinas$vacbruc_M1,
    M2 = producao_animal$vacinas$vacbruc_M2,
    M3g = producao_animal$vacinas$vacbruc_M3g,
    M3 = producao_animal$vacinas$vacbruc_M3,
    M3bm = producao_animal$vacinas$vacbruc_M3bm,
    M4 = producao_animal$vacinas$vacbruc_M4,
    M5 = producao_animal$vacinas$vacbruc_M5,
    M6 = producao_animal$vacinas$vacbruc_M6,
    M6bgt = producao_animal$vacinas$vacbruc_M6bgt,
    M6t = producao_animal$vacinas$vacbruc_M6t,
    unit_price = economico$precos$vacbruc_unit_price
  )
  
  vermifuge <- vermifuge(
    F6 = producao_animal$vacinas$vermifugo_F6,
    F5 = producao_animal$vacinas$vermifugo_F5,
    F4 = producao_animal$vacinas$vermifugo_F4,
    F3 = producao_animal$vacinas$vermifugo_F3,
    F2 = producao_animal$vacinas$vermifugo_F2,
    F1 = producao_animal$vacinas$vermifugo_F1,
    M1 = producao_animal$vacinas$vermifugo_M1,
    M2 = producao_animal$vacinas$vermifugo_M2,
    M3g = producao_animal$vacinas$vermifugo_M3g,
    M3 = producao_animal$vacinas$vermifugo_M3,
    M3bm = producao_animal$vacinas$vermifugo_M3bm,
    M4 = producao_animal$vacinas$vermifugo_M4,
    M5 = producao_animal$vacinas$vermifugo_M5,
    M6 = producao_animal$vacinas$vermifugo_M6,
    M6bgt = producao_animal$vacinas$vermifugo_M6bgt,
    M6t = producao_animal$vacinas$vermifugo_M6t,
    unit_price = economico$precos$vermifugo_unit_price
  )
  
  # Biológico ------------------------------------------------------------------
  
  # Wc1 Peso da vaca de descarte (vaca boiadeira)
  # Pc1 Preço da vaca de descarte (vaca boiadeira)
  vaca$Wc1 <- with(vaca, (W1*R1)/15)
  vaca$Pc1 <- with(vaca, Wc1*P1)
  
  # Wc2 Peso médio da carcaça do macho
  # Wc3 Peso médio da carcaça da fêmea
  # Pc2 Preço de venda do macho de descarte (<180 kg)
  # Pc3 Preço de venda da fêmea de descarte (<160 kg)
  desmama$Wc2 <- with(desmama, W2/30)
  desmama$Wc3 <- with(desmama, W3/30)
  desmama$Pc2 <- with(desmama, W2*P2)
  desmama$Pc3 <- with(desmama, W3*P3)
  
  # Wc4 Peso médio da carcaça do macho
  # Wc5 Peso médio da carcaça da fêmea
  # Pc4 Preço de venda do macho de descarte (<195 kg)
  # Pc5 Preço de venda da fêmea de descarte (<180 kg)
  bezerro$Wc4 <- with(bezerro, W4/30)
  bezerro$Wc5 <- with(bezerro, W5/30)
  bezerro$Pc4 <- with(bezerro, W4*P4)
  bezerro$Pc5 <- with(bezerro, W5*P5)
  
  # Wc6, Wc7, Wc8 Peso médio da novilha
  # Pc6, Pc7, Pc8 Preço de venda da novilha de descarte
  novilha_18_20$Wc6 <- with(novilha_18_20, (W6*R6)/15)
  novilha_21_28$Wc7 <- with(novilha_21_28, (W7*R7)/15)
  novilha_29_36$Wc8 <- with(novilha_29_36, (W8*R8)/15)
  
  novilha_18_20$Pc6 <- with(novilha_18_20, Wc6*(P6+Pp6))
  novilha_21_28$Pc7 <- with(novilha_21_28, Wc7*(P7+Pp7))
  novilha_29_36$Pc8 <- with(novilha_29_36, Wc8*(P8+Pp8))
  
  # Wc9 Peso médio do Garrote
  # Pc9 Preço de venda do Garrote de descarte (<285 kg)
  garrote$Wc9 <- with(garrote, (W9*R9)/15)
  garrote$Pc9 <- with(garrote, Wc9*P9)
  
  # Wc10 Peso médio do boi magro
  # Pc10 Preço de venda do boi magro de descarte (<360 kg)
  boi_magro$Wc10 <- with(boi_magro, (W10*R10)/15)
  boi_magro$Pc10 <- with(boi_magro, Wc10*P10)
  
  # Wc11, Wc12, Wc13, Wc14, Wc15 Peso médio do boi gordo
  # Pc11, Pc12, Pc13, Pc14, Pc15 Preço de venda do boi gordo
  boi_gordo_18_20$Wc11 <- with(boi_gordo_18_20, (W11*R11)/15)
  boi_gordo_21_28$Wc12 <- with(boi_gordo_21_28, (W12*R12)/15)
  boi_gordo_29_36$Wc13 <- with(boi_gordo_29_36, (W13*R13)/15)
  boi_gordo_37_48$Wc14 <- with(boi_gordo_37_48, (W14*R14)/15)
  boi_gordo_toruno$Wc15 <- with(boi_gordo_toruno, (W15*R15)/15)
  
  boi_gordo_18_20$Pc11 <- with(boi_gordo_18_20, Wc11*(P11+Pp11))
  boi_gordo_21_28$Pc12 <- with(boi_gordo_21_28, Wc12*(P12+Pp12))
  boi_gordo_29_36$Pc13 <- with(boi_gordo_29_36, Wc13*(P13+Pp13))
  boi_gordo_37_48$Pc14 <- with(boi_gordo_37_48, Wc14*(P14+Pp14))
  boi_gordo_toruno$Pc15 <- with(boi_gordo_toruno, Wc15*P15)
  
  # Aquisição/Reposição --------------------------------------------------------
  
  aquisicao <- list()
  aquisicao$quantidade <- vaca$AqF6 + vaca$AqAcomp + novilha_29_36$AqF5 +
    novilha_21_28$AqF4 + novilha_18_20$AqF3 + bezerro$AqF2 + desmama$AqF1 +
    desmama$AqM1 + bezerro$AqM2 + garrote$AqM3g + touro$AqM6t
  
  # Total (R$)
  vaca$tAqF6 <- with(vaca, AqF6 * WAqF6 * PAqF6)
  vaca$tAqAcomp <- with(vaca, AqAcomp * PAqAcomp * 260)
  novilha_29_36$tAqF5 <- with(novilha_29_36, AqF5 * WAqF5 * PAqF5)
  novilha_21_28$tAqF4 <- with(novilha_21_28, AqF4 * WAqF4 * PAqF4)
  novilha_18_20$tAqF3 <- with(novilha_18_20, AqF3 * WAqF3 * PAqF3)
  bezerro$tAqF2 <- with(bezerro, AqF2 * WAqF2 * PAqF2)
  desmama$tAqF1 <- with(desmama, AqF1 * WAqF1 * PAqF1)
  
  desmama$tAqM1 <- with(desmama, AqM1 * WAqM1 * PAqM1)
  bezerro$tAqM2 <- with(bezerro, AqM2 * WAqM2 * PAqM2)
  garrote$tAqM3g <- with(garrote, AqM3g * WAqM3g * PAqM3g)
  touro$tAqM6t <- with(touro, AqM6t * WAqM6t * PAqM6t)
  
  aquisicao$total <- vaca$tAqF6 + vaca$tAqAcomp + novilha_29_36$tAqF5 +
    novilha_21_28$tAqF4 + novilha_18_20$tAqF3 + bezerro$tAqF2 + desmama$tAqF1 +
    desmama$tAqM1 + bezerro$tAqM2 + garrote$tAqM3g + touro$tAqM6t
  
  # Preço/animal
  vaca$PAqF6_ani <- with(vaca, tAqF6 / AqF6)
  vaca$PAqAcomp_ani <- with(vaca, tAqAcomp / AqAcomp)
  novilha_29_36$PAqF5_ani <- with(novilha_29_36, tAqF5 / AqF5)
  novilha_21_28$PAqF4_ani <- with(novilha_21_28, tAqF4 / AqF4)
  novilha_18_20$PAqF3_ani <- with(novilha_18_20, tAqF3 / AqF3)
  bezerro$PAqF2_ani <- with(bezerro, tAqF2 / AqF2)
  desmama$PAqF1_ani <- with(desmama, tAqF1 / AqF1)
  
  desmama$PAqM1_ani <- with(desmama, tAqM1 / AqM1)
  bezerro$PAqM2_ani <- with(bezerro, tAqM2 / AqM2)
  garrote$PAqM3g_ani <- with(garrote, tAqM3g / AqM3g)
  touro$PAqM6t_ani <- with(touro, tAqM6t / AqM6t)
  
  aquisicao$media <- mean(c(
    vaca$PAqF6_ani, vaca$PAqAcomp_ani, novilha_29_36$PAqF5_ani,
    novilha_21_28$PAqF4_ani, novilha_18_20$PAqF3_ani, bezerro$PAqF2_ani,
    desmama$PAqF1_ani, desmama$PAqM1_ani, bezerro$PAqM2_ani, garrote$PAqM3g_ani,
    touro$PAqM6t_ani), na.rm = TRUE)
  
  # Evolução -------------------------------------------------------------------
  
  evolution <- list()
  evolution$aquisicao <- vaca$AqAcomp #aquisicao$quantidade
  evolution$nascimento <- ref_zoo$Tn * productive_unit$num_matrizes
  evolution$mortalidade <- vaca$Mt_ini
  evolution$num_cabecas <- with(evolution, nascimento - mortalidade + aquisicao)
  
  # femea
  evolution$F1 <- evolution$num_cabecas / 2
  
  evolution$UAF1_i <- evolution$F1 * desmama$UAF1
  evolution$MtF1 <- impactos$Mt1 * evolution$F1
  evolution$VdF1 <- with(evolution, F1 - MtF1) * desmama$Ps3
  evolution$Ncfc1 <- evolution$F2 <-  with(evolution, F1 - MtF1 - VdF1) + desmama$AqF1
  
  evolution$UAF2_i <- evolution$F2 * bezerro$UAF2
  evolution$MtF2 <- impactos$Mt2 * evolution$F2
  evolution$VdF2 <- with(evolution, F2 - MtF2) * bezerro$Ps5
  evolution$Ncfc2 <- evolution$F3 <- with(evolution, F2 - MtF2 - VdF2) + bezerro$AqF2
  
  evolution$UAF3_i <- evolution$F3 * novilha_18_20$UAF3
  evolution$MtF3 <- impactos$Mt3 * evolution$F3
  evolution$VdF3 <- with(evolution, F3 - MtF3) * novilha_18_20$Ps6
  evolution$Ncfc3 <- evolution$F4 <- with(evolution, F3 - MtF3 - VdF3) + novilha_18_20$AqF3
  
  evolution$UAF4_i <- evolution$F4 * novilha_21_28$UAF4
  evolution$MtF4 <- impactos$Mt4 * evolution$F4
  evolution$VdF4 <- evolution$F4 * novilha_21_28$Ps7 - evolution$MtF4
  evolution$Ncfc4 <- evolution$F5 <- with(evolution, F4 - MtF4 - VdF4) + novilha_21_28$AqF4
  
  evolution$UAF5_i <- evolution$F5 * novilha_29_36$UAF5
  evolution$MtF5 <- impactos$Mt5 * evolution$F5
  evolution$VdF5 <- evolution$F5 * novilha_29_36$Ps8 - evolution$MtF5
  evolution$Ncfc5 <- (ref_zoo$Rp2 + impactos$Mt6)*productive_unit$num_matrizes + evolution$F5 - evolution$VdF5
  
  evolution$UAF6_i <- productive_unit$num_matrizes * vaca$UAF6
  evolution$MtF6 <- impactos$Mt6 * productive_unit$num_matrizes
  evolution$VdF6 <- productive_unit$num_matrizes * vaca$Ps1 - evolution$MtF6
  evolution$Ncfc6 <- productive_unit$num_matrizes - evolution$MtF6 - evolution$VdF6 + (vaca$AqF6 + vaca$AqAcomp)
  evolution$UAF6_f <- with(evolution, Ncfc5 + Ncfc6)
  
  evolution$F6 <- productive_unit$num_matrizes
  
  # macho
  evolution$M1 <- evolution$num_cabecas / 2
  
  evolution$UAM1_i <- evolution$M1 * desmama$UAM1
  evolution$MtM1 <- impactos$Mt1 * evolution$M1
  evolution$VdM1 <- with(evolution, M1 - MtM1) * desmama$Ps2
  evolution$Ncmc1 <- evolution$M2 <- with(evolution, M1 - MtM1 - VdM1) + desmama$AqM1
  
  evolution$UAM2_i <- evolution$M2 * bezerro$UAM2
  evolution$MtM2 <- impactos$Mt2 * evolution$M2
  evolution$VdM2 <- with(evolution, M2 - MtM2) * bezerro$Ps4
  evolution$Ncmc2 <- evolution$M3g <- with(evolution, M2 - MtM2 - VdM2) + bezerro$AqM2
  
  evolution$UAM3g_i <- evolution$M3g * garrote$UAM3g
  evolution$MtM3g <- impactos$Mt3 * evolution$M3g
  evolution$VdM3g <- with(evolution, M3g - MtM3g) * garrote$Ps9
  evolution$Ncmc3g <- evolution$M3bm <- with(evolution, M3g - MtM3g - VdM3g) + garrote$AqM3g
  
  evolution$UAM3bm_i <- evolution$M3bm * boi_magro$UAM3bm
  evolution$MtM3bm <- impactos$Mt3 * evolution$M3bm
  evolution$VdM3bm <- with(evolution, M3bm - MtM3bm) * boi_magro$Ps10
  evolution$Ncmc3bm <- evolution$M3 <- with(evolution, M3bm - MtM3bm - VdM3bm) + boi_magro$AqM3bm
  
  evolution$UAM3_i <- evolution$M3 * boi_gordo_18_20$UAM3
  evolution$MtM3 <- impactos$Mt3 * evolution$M3
  evolution$VdM3 <- with(evolution, M3 - MtM3) * boi_gordo_18_20$Ps11
  evolution$Ncmc3 <- evolution$M4 <- with(evolution, M3 - MtM3 - VdM3) + boi_gordo_18_20$AqM3
  
  evolution$UAM4_i <- evolution$M4 * boi_gordo_21_28$UAM4
  evolution$MtM4 <- impactos$Mt4 * evolution$M4
  evolution$VdM4 <- with(evolution, M4 - MtM4) * boi_gordo_21_28$Ps12
  evolution$Ncmc4 <- evolution$M5 <- with(evolution, M4 - MtM4 - VdM4) + boi_gordo_21_28$AqM4
  
  evolution$UAM5_i <- evolution$M5 * boi_gordo_29_36$UAM5
  evolution$MtM5 <- impactos$Mt5 * evolution$M5
  evolution$VdM5 <- ifelse(productive_unit$nivel_intensificacao < 2,
                           with(evolution, M5 - MtM5),
                           with(evolution, M5 - MtM5)) / 2
  evolution$Ncmc5 <- evolution$M6 <- with(evolution, M5 - MtM5 - VdM5) + boi_gordo_29_36$AqM5
  
  evolution$UAM6_i <- evolution$M6 * boi_gordo_37_48$UAM6
  evolution$MtM6 <- impactos$Mt6 * evolution$M6
  evolution$VdM6 <- ifelse(productive_unit$nivel_intensificacao <= 3,
                           with(evolution, M6 - MtM6), 0)
  evolution$Ncmc6 <- evolution$M6bgt <- with(evolution, M6 - MtM6 - VdM6) + boi_gordo_37_48$AqM6
  
  evolution$UAM6bgt_i <- evolution$M6bgt * boi_gordo_toruno$UAM6bgt
  evolution$MtM6bgt <- impactos$Mt6 * evolution$M6bgt
  evolution$VdM6bgt <- 0
  evolution$Ncmc6bgt <- with(evolution, M6bgt - VdM6bgt) + boi_gordo_toruno$AqM6bgt
  
  evolution$M6t <- productive_unit$num_matrizes / ref_zoo$Rs1
  evolution$UAM6t_i <- evolution$M6t * touro$UAM6t
  evolution$MtM6t <- impactos$Mt6 * evolution$M6t
  evolution$VdM6t <- with(evolution, M6t - MtM6t) * ref_zoo$Rp3
  evolution$Ncmc6t <- with(evolution, M6t - VdM6t) + touro$AqM6t
  
  # mortalidade total, TMA
  evolution$TMA <- with(
    evolution,
    sum(MtF1, MtF2, MtF3, MtF4, MtF5, MtF6,
        MtM1, MtM2, MtM3, MtM4, MtM5, MtM6,
        MtM3g, MtM3bm, MtM6bgt, MtM6t))
  
  # quantidade de rebanho / número de cabeças total, Qr (Mudança de era)
  evolution$Qr <- with(
    evolution,
    sum(Ncfc1, Ncfc2, Ncfc3, Ncfc4, Ncfc5, Ncfc6,
        Ncmc1, Ncmc2, Ncmc3, Ncmc4, Ncmc5, Ncmc6,
        Ncmc3g, Ncmc3bm, Ncmc6bgt, Ncmc6t, num_cabecas))
  
  # vendas total, VdT
  evolution$VdT <- with(
    evolution,
    sum(VdF1, VdF2, VdF3, VdF4, VdF5, VdF6,
        VdM1, VdM2, VdM3, VdM4, VdM5, VdM6,
        VdM3g, VdM3bm, VdM6bgt, VdM6t))
  
  # UA total, UAt
  evolution$UAt <- with(
    evolution,
    sum(UAF1_i, UAF2_i, UAF3_i, UAF4_i, UAF5_i, UAF6_i,
        UAM1_i, UAM2_i, UAM3_i, UAM4_i, UAM5_i, UAM6_i,
        UAM3g_i, UAM3bm_i, UAM6bgt_i, UAM6t_i))
  
  # inicio do ano, ini
  evolution$ini <- with(
    evolution,
    sum(F1, F2, F3, F4, F5, F6,
        M1, M2, M3, M4, M5, M6,
        M3g, M3bm, M6bgt, M6t))
  
  # aquisição total, AqT
  evolution$AqT <- sum(
    vaca$AqF6 + vaca$AqAcomp,
    novilha_29_36$AqF5,
    novilha_21_28$AqF4,
    novilha_18_20$AqF3,
    bezerro$AqF2,
    desmama$AqF1,
    desmama$AqM1,
    
    vaca$AqAcomp,
    
    bezerro$AqM2,
    garrote$AqM3g,
    boi_magro$AqM3bm,
    boi_gordo_18_20$AqM3,
    boi_gordo_21_28$AqM4,
    boi_gordo_29_36$AqM5,
    boi_gordo_37_48$AqM6,
    boi_gordo_toruno$AqM6bgt,
    touro$AqM6t
  )
  
  ## Variáveis calculadas ------------------------------------------------------
  
  # peso total (kg)
  evolution$wtF6 <- evolution$F6 * with(vaca, wF6 * R1)
  evolution$wtF5 <- evolution$F5 * with(novilha_29_36, wF5 * R8)
  evolution$wtF4 <- evolution$F4 * with(novilha_21_28, wF4 * R7)
  evolution$wtF3 <- evolution$F3 * with(novilha_18_20, wF3 * R6)
  evolution$wtF2 <- evolution$F2 * bezerro$wF2 * 0.5
  evolution$wtF1 <- evolution$F1 * desmama$wF1 * 0.5
  
  evolution$wtM1 <- evolution$M1 * desmama$wM1 * 0.5
  evolution$wtM2 <- evolution$M2 * bezerro$wM2 * 0.5
  evolution$wtM3g <- evolution$M3g * garrote$wM3g * 0.5
  evolution$wtM3bm <- evolution$M3bm * with(boi_magro, wM3bm * R10)
  evolution$wtM3 <- evolution$M3 * with(boi_gordo_18_20, wM3 * R11)
  evolution$wtM4 <- evolution$M4 * with(boi_gordo_21_28, wM4 * R12)
  evolution$wtM5 <- evolution$M5 * with(boi_gordo_29_36, wM5 * R13)
  evolution$wtM6 <- evolution$M6 * with(boi_gordo_37_48, wM6 * R14)
  evolution$wtM6t <- evolution$M6t * touro$wM6t * boi_gordo_toruno$R15
  
  # peso total (arroba)
  evolution$wtArF6 <- evolution$wtF6 / 15
  evolution$wtArF5 <- evolution$wtF5 / 15
  evolution$wtArF4 <- evolution$wtF4 / 15
  evolution$wtArF3 <- evolution$wtF3 / 15
  evolution$wtArF2 <- evolution$wtF2 / 15
  evolution$wtArF1 <- evolution$wtF1 / 15
  
  evolution$wtArM1 <- evolution$wtM1 / 15
  evolution$wtArM2 <- evolution$wtM2 / 15
  evolution$wtArM3g <- evolution$wtM3g / 15
  evolution$wtArM3bm <- evolution$wtM3bm / 15
  evolution$wtArM3 <- evolution$wtM3 / 15
  evolution$wtArM4 <- evolution$wtM4 / 15
  evolution$wtArM5 <- evolution$wtM5 / 15
  evolution$wtArM6 <- evolution$wtM6 / 15
  evolution$wtArM6t <- evolution$wtM6t / 15
  
  # preço / kg
  evolution$wkgF6 <- vaca$P1 / 15
  evolution$wkgF5 <- novilha_29_36$P8 / 15
  evolution$wkgF4 <- novilha_21_28$P7 / 15
  evolution$wkgF3 <- novilha_18_20$P6 / 15
  evolution$wkgF2 <- bezerro$P5
  evolution$wkgF1 <- desmama$P3
  
  evolution$wkgM1 <- desmama$P2
  evolution$wkgM2 <- bezerro$P4
  evolution$wkgM3g <- garrote$P9 / 15
  evolution$wkgM3bm <- boi_magro$P10 / 15
  evolution$wkgM3 <- boi_gordo_18_20$P11 / 15
  evolution$wkgM4 <- boi_gordo_21_28$P12 / 15
  evolution$wkgM5 <- boi_gordo_29_36$P13 / 15
  evolution$wkgM6 <- boi_gordo_37_48$P14 / 15
  evolution$wkgM6t <- boi_gordo_toruno$P15 / 15
  
  # valor total
  evolution$pF6 <- evolution$wtArF6 * vaca$P1
  evolution$pF5 <- evolution$wtArF5 * novilha_29_36$P8
  evolution$pF4 <- evolution$wtArF4 * novilha_21_28$P7
  evolution$pF3 <- evolution$wtArF3 * novilha_18_20$P6
  evolution$pF2 <- evolution$wtArF2 * bezerro$P5 * 30
  evolution$pF1 <- evolution$wtArF1 * desmama$P3 * 30
  
  evolution$pM1 <- evolution$wtArM1 * desmama$P2 * 30
  evolution$pM2 <- evolution$wtArM2 * bezerro$P4 * 30
  evolution$pM3g <- evolution$wtArM3g * garrote$P9
  evolution$pM3bm <- evolution$wtArM3bm * boi_magro$P10
  evolution$pM3 <- evolution$wtArM3 * boi_gordo_18_20$P11
  evolution$pM4 <- evolution$wtArM4 * boi_gordo_21_28$P12
  evolution$pM5 <- evolution$wtArM5 * boi_gordo_29_36$P13
  evolution$pM6t <- with(evolution, wtM6t * wkgM6t)
  
  # valor total do rebanho
  evolution$pTot <- with(
    evolution,
    sum(pF6, pF5, pF4, pF3, pF2, pF1,
        pM1, pM2, pM3g, pM3bm, pM3, pM4, pM5, pM6t))
  
  # preço unitário
  evolution$pUF6 <- with(evolution, pF6 / F6)
  evolution$pUF5 <- with(evolution, pF5 / F5)
  evolution$pUF4 <- with(evolution, pF4 / F4)
  evolution$pUF3 <- with(evolution, pF3 / F3)
  evolution$pUF2 <- with(evolution, pF2 / F2)
  evolution$pUF1 <- with(evolution, pF1 / F1)
  
  evolution$pUM1 <- with(evolution, pM1 / M1)
  evolution$pUM2 <- with(evolution, pM2 / M2)
  evolution$pUM3g <- with(evolution, pM3g / M3g)
  evolution$pUM3bm <- with(evolution, pM3bm / M3bm)
  evolution$pUM3 <- with(evolution, pM3 / M3)
  evolution$pUM4 <- with(evolution, pM4 / M4)
  evolution$pUM5 <- with(evolution, pM5 / M5)
  evolution$pUM6t <- with(evolution, pM6t / M6t)
  
  # preço unitário Total
  evolution$pUTot <- with(evolution, pTot / Qr)
  
  # taxa de desfrute
  evolution$taxa_desfrute <- with(
    evolution,
    (Qr - ini - AqT + VdT) / ini
  )
  
  # Produtividade total por hectare
  evolution$prod_ha <-
    evolution$VdF6 * vaca$Wc1 +
    evolution$VdF5 * novilha_29_36$Wc8 +
    evolution$VdF4 * novilha_21_28$Wc7 +
    evolution$VdF3 * novilha_18_20$Wc6 +
    evolution$VdF2 * bezerro$Wc5 +
    evolution$VdF1 * desmama$Wc3 +
    
    evolution$VdM1 * desmama$Wc2 +
    evolution$VdM2 * bezerro$Wc4 +
    evolution$VdM3g * garrote$Wc9 +
    evolution$VdM3bm * boi_magro$Wc10 +
    evolution$VdM3 * boi_gordo_18_20$Wc11 +
    evolution$VdM4 * boi_gordo_21_28$Wc12 +
    evolution$VdM5 * boi_gordo_29_36$Wc13 +
    evolution$VdM6 * boi_gordo_37_48$Wc14 +
    evolution$VdM6bgt * boi_gordo_toruno$Wc15 +
    evolution$VdM6t * boi_gordo_toruno$Wc15
  
  # valor de venda para cada categoria de animal
  vaca$pVdF6 <- vaca$Pc1 * evolution$VdF6
  novilha_29_36$pVdF5 <- novilha_29_36$Pc8 * evolution$VdF5
  novilha_21_28$pVdF4 <- novilha_21_28$Pc7 * evolution$VdF4
  novilha_18_20$pVdF3 <- novilha_18_20$Pc6 * evolution$VdF3
  bezerro$pVdF2 <- bezerro$Pc5 * evolution$VdF2
  desmama$pVdF1 <- desmama$Pc3 * evolution$VdF1
  
  desmama$pVdM1 <- desmama$Pc2 * evolution$VdM1
  bezerro$pVdM2 <- bezerro$Pc4 * evolution$VdM2
  garrote$pVdM3g <- garrote$Pc9 * evolution$VdM3g
  boi_magro$pVdM3bm <- boi_magro$Pc10 * evolution$VdM3bm
  boi_gordo_18_20$pVdM3 <- boi_gordo_18_20$Pc11 * evolution$VdM3
  boi_gordo_21_28$pVdM4 <- boi_gordo_21_28$Pc12 * evolution$VdM4
  boi_gordo_29_36$pVdM5 <- boi_gordo_29_36$Pc13 * evolution$VdM5
  boi_gordo_37_48$pVdM6 <- boi_gordo_37_48$Pc14 * evolution$VdM6
  boi_gordo_toruno$pVdM6bgt <- boi_gordo_toruno$Pc15 * evolution$VdM6t
  
  # receita anual: valor de vendas total
  evolution$pVdT <- vaca$pVdF6 + novilha_29_36$pVdF5 + novilha_21_28$pVdF4 +
    novilha_18_20$pVdF3 + bezerro$pVdF2 +desmama$pVdF1 +
    desmama$pVdM1 + bezerro$pVdM2 + garrote$pVdM3g + boi_magro$pVdM3bm + 
    boi_gordo_18_20$pVdM3 + boi_gordo_21_28$pVdM4 + boi_gordo_29_36$pVdM5 +
    boi_gordo_37_48$pVdM6 + boi_gordo_toruno$pVdM6bgt
  
  # Planejamento ---------------------------------------------------------------
  
  ## Manejo sanitário ----------------------------------------------------------
  
  sanitary <- list("vax_af" = vax_af,
                   "vax_carb" = vax_carb,
                   "vax_bru" = vax_bru,
                   "vermifuge" = vermifuge)
  
  # custo total vacina aftosa
  sanitary$vax_af$total_cost <- sum(
    unlist(evolution[
      c("F6", "F5", "F4", "F3", "F2", "F1",
        "M1", "M2", "M3g", "M3bm", "M3", "M4", "M5", "M6", "M6bgt", "M6t")]) *
      unlist(sanitary$vax_af[
        c("F6", "F5", "F4", "F3", "F2", "F1",
          "M1", "M2", "M3g", "M3bm", "M3", "M4", "M5", "M6", "M6bgt", "M6t")])) *
    sanitary$vax_af$unit_price
  
  # custo total vacina carbunculo sintomático
  sanitary$vax_carb$total_cost <- sum(
    unlist(evolution[
      c("F6", "F5", "F4", "F3", "F2", "F1",
        "M1", "M2", "M3g", "M3bm", "M3", "M4", "M5", "M6", "M6bgt", "M6t")]) *
      unlist(sanitary$vax_carb[
        c("F6", "F5", "F4", "F3", "F2", "F1",
          "M1", "M2", "M3g", "M3bm", "M3", "M4", "M5", "M6", "M6bgt", "M6t")])) *
    sanitary$vax_carb$unit_price
  
  # custo total vacina brucelose
  sanitary$vax_bru$total_cost <- sum(
    unlist(evolution[
      c("F6", "F5", "F4", "F3", "F2", "F1",
        "M1", "M2", "M3g", "M3bm", "M3", "M4", "M5", "M6", "M6bgt", "M6t")]) *
      unlist(sanitary$vax_bru[
        c("F6", "F5", "F4", "F3", "F2", "F1",
          "M1", "M2", "M3g", "M3bm", "M3", "M4", "M5", "M6", "M6bgt", "M6t")])) *
    sanitary$vax_bru$unit_price
  
  # custo total vermifugo
  sanitary$vermifuge$total_cost <- sum(
    unlist(evolution[
      c("F6", "F5", "F4", "F3", "F2", "F1",
        "M1", "M2", "M3g", "M3bm", "M3", "M4", "M5", "M6", "M6bgt", "M6t")]) *
      unlist(sanitary$vermifuge[
        c("F6", "F5", "F4", "F3", "F2", "F1",
          "M1", "M2", "M3g", "M3bm", "M3", "M4", "M5", "M6", "M6bgt", "M6t")])) *
    sanitary$vermifuge$unit_price
  
  ## Suplementação nutritional -------------------------------------------------
  
  feeding <- list("daily_feeding_consumption" = daily_feeding_consumption)
  
  ### Sal mineral --------------------------------------------------------------
  
  # função auxiliar que reproduz a fórmula para cálculo do planejamento da
  # programação anual de sal mineral para algumas categorias de animal
  # a: número de cabeças calculadas na evolução
  mineral_salt_by_animal <- function(a) {
    ifelse(productive_unit$nivel_intensificacao == 3,
           1 * daily_feeding_consumption$C1 * a * 300,
           0)
  }
  
  # custo sal mineral por categorial animal
  feeding$mineral_salt <- list(
    "F4" = mineral_salt_by_animal(evolution$F4),
    "F3" = mineral_salt_by_animal(evolution$F3),
    "F2" = mineral_salt_by_animal(evolution$F2),
    "F1" = mineral_salt_by_animal(evolution$F1),
    "M1" = mineral_salt_by_animal(evolution$M1),
    "M2" = mineral_salt_by_animal(evolution$M2),
    "M3g" = mineral_salt_by_animal(evolution$M3g),
    "M3bm" = mineral_salt_by_animal(evolution$M3bm),
    "M3" = mineral_salt_by_animal(evolution$M3),
    "M4" = mineral_salt_by_animal(evolution$M4),
    "M5" = mineral_salt_by_animal(evolution$M5),
    "M6" = mineral_salt_by_animal(evolution$M6),
    "M6bgt" = mineral_salt_by_animal(evolution$M6bgt)
  )
  
  # custo total sal mineral
  feeding$mineral_salt$total_cost <- ifelse(
    productive_unit$nivel_intensificacao == 3,
    sum(unlist(feeding$mineral_salt)) * daily_feeding_consumption$pC1,
    0)
  
  ### Sal mineral reprodução ---------------------------------------------------
  
  # custo sal mineral reprodução por categorial animal
  feeding$mineral_salt_reproduction <- list(
    "F6" = (daily_feeding_consumption$C2 * vaca$wF6 / 100) * (1 * 360),
    "F5" = (daily_feeding_consumption$C2 * novilha_29_36$wF5 / 100) * (1 * 360)
  )
  
  # custo total sal mineral reprodução
  feeding$mineral_salt_reproduction$total_cost <- sum(
    feeding$mineral_salt_reproduction$F6 * evolution$F6,
    feeding$mineral_salt_reproduction$F5 * evolution$F5
  ) * daily_feeding_consumption$pC2
  
  ### Suplemento mineral proteico ----------------------------------------------
  
  # função auxiliar que reproduz a fórmula para cálculo do planejamento da
  # programação anual de suplemento mineral proteico e energético
  # para algumas categorias de animal
  # c: nível de intensificação
  # a: número de cabeças calculadas na evolução
  supp_mineral_by_animal <- function(c, a) {
    ifelse(productive_unit$nivel_intensificacao == 1,
           (c * (a / 100)) * (1 * 360),
           (c * (a / 100)) * (1 * 210))
  }
  
  # custo suplemento mineral proteico por categorial animal
  feeding$protein_mineral <- list(
    "F2" = supp_mineral_by_animal(daily_feeding_consumption$C3, bezerro$wF2),
    "F1" = supp_mineral_by_animal(daily_feeding_consumption$C3, desmama$wF1),
    "M1" = supp_mineral_by_animal(daily_feeding_consumption$C3, desmama$wM1),
    "M2" = supp_mineral_by_animal(daily_feeding_consumption$C3, bezerro$wM2)
  )
  
  # custo total suplemento mineral proteico
  feeding$protein_mineral$total_cost <- ifelse(
    productive_unit$nivel_intensificacao < 3,
    sum(feeding$protein_mineral$F2 * evolution$F2,
        feeding$protein_mineral$F1 * evolution$F1,
        feeding$protein_mineral$M1 * evolution$M1,
        feeding$protein_mineral$M2 * evolution$M2) * daily_feeding_consumption$pC3,
    0)
  
  ### Suplemento mineral energético --------------------------------------------
  
  # custo suplemento mineral energético por categorial animal
  feeding$energy_mineral <- list(
    "F4" = supp_mineral_by_animal(daily_feeding_consumption$C4, novilha_21_28$wF4),
    "F3" = ifelse(
      productive_unit$nivel_intensificacao == 2,
      (daily_feeding_consumption$C4 * evolution$F3 / 100) * (1 * 210),
      0
    ),
    "M3g" = supp_mineral_by_animal(daily_feeding_consumption$C4, garrote$wM3g),
    "M3bm" = supp_mineral_by_animal(daily_feeding_consumption$C4, boi_magro$wM3bm),
    "M3" = ifelse(
      productive_unit$nivel_intensificacao == 2,
      (daily_feeding_consumption$C4 * evolution$M3 / 100) * (1 * 210),
      0
    ),
    "M4" = supp_mineral_by_animal(daily_feeding_consumption$C4, boi_gordo_21_28$wM4),
    "M5" = supp_mineral_by_animal(daily_feeding_consumption$C4, boi_gordo_29_36$wM5),
    "M6t" = supp_mineral_by_animal(daily_feeding_consumption$C4, touro$wM6t)
  )
  
  # custo total suplemento mineral energético
  feeding$energy_mineral$total_cost <- ifelse(
    productive_unit$nivel_intensificacao < 3,
    sum(feeding$energy_mineral$F4 * evolution$F4,
        feeding$energy_mineral$F3 * (evolution$F3 - evolution$VdF3),
        feeding$energy_mineral$M3g * evolution$M3g,
        feeding$energy_mineral$M3bm * evolution$M3bm,
        feeding$energy_mineral$M3 * evolution$M3,
        feeding$energy_mineral$M4 * evolution$M4,
        feeding$energy_mineral$M5 * evolution$M5,
        feeding$energy_mineral$M6t * evolution$M6t) * daily_feeding_consumption$pC4,
    0)
  
  ### Creep-Feeding ------------------------------------------------------------
  
  # custo creep-feeding por categorial animal
  feeding$creep <- list(
    "AN" = with(daily_feeding_consumption, creep_days * C5)
  )
  
  # custo total creep-feeding
  feeding$creep$total_cost <- ifelse(
    productive_unit$nivel_intensificacao == 1,
    evolution$num_cabecas * feeding$creep$AN * daily_feeding_consumption$pC5,
    0
  )
  
  ### Pecuária de ciclo curto --------------------------------------------------
  
  if (productive_unit$nivel_intensificacao == 1) {
    # Dias de semi confinamento
    # dscF3 <- with(novilha_18_20, (ccF3Wo - wF3) / ccF3dg)
    dscF3 <- (novilha_18_20$ccF3Wo - desmama$wF1) / novilha_18_20$ccF3dg
    # dscM3 <- with(boi_gordo_18_20, (ccM3Wo - wM3) / ccM3dg)
    dscM3 <- (boi_gordo_18_20$ccM3Wo - desmama$wM1) / boi_gordo_18_20$ccM3dg
    dscM4 <- with(boi_gordo_21_28, (ccM4Wo - wM4) / ccM4dg)
    dscM5 <- with(boi_gordo_29_36, (ccM5Wo - wM5) / ccM5dg)
    
    # kg/consumido/dia
    # wcdF3 <- with(novilha_18_20, (ccF3pf / 100) * wF3)
    wcdF3 <- with(novilha_18_20, (ccF3pf / 100) * desmama$wF1)
    # wcdM3 <- with(boi_gordo_18_20, (ccM3pf / 100) * wM3)
    wcdM3 <- with(boi_gordo_18_20, (ccM3pf / 100) * desmama$wM1)
    wcdM4 <- with(boi_gordo_21_28, (ccM4pf / 100) * wM4)
    wcdM5 <- with(boi_gordo_29_36, (ccM5pf / 100) * wM5)
    
    # final
    novilha_18_20$ccF3 <- evolution$VdF3 * dscF3 * wcdF3
    boi_gordo_18_20$ccM3 <- evolution$VdM3 * dscM3 * wcdM3
    boi_gordo_21_28$ccM4 <- evolution$VdM4 * dscM4 * wcdM4
    boi_gordo_29_36$ccM5 <- evolution$VdM5 * dscM5 * wcdM5
    
    feeding$ccp <- list(
      "novilha_18_20" = novilha_18_20$ccF3,
      "boi_gordo_18_20" = boi_gordo_18_20$ccM3,
      "boi_gordo_21_28" = boi_gordo_21_28$ccM4,
      "boi_gordo_29_36" = boi_gordo_29_36$ccM5,
      "total_cost_18_20" = (novilha_18_20$ccF3 + boi_gordo_18_20$ccM3) * feeding$daily_feeding_consumption$pCc18_20,
      "total_cost_21_28" = (boi_gordo_21_28$ccM4 + boi_gordo_29_36$ccM5) * feeding$daily_feeding_consumption$pCc21_28
    )
  }
  
  # módulo de planejamento da programação anual
  planning <- list("sanitary" = sanitary,
                   "feeding" = feeding)
  
  # Dados da unidade produtiva -------------------------------------------------
  
  # variáveis calculadas após realizar a evolução do rebanho
  
  # UA corresponde total
  productive_unit$UA <- evolution$UAt
  
  # disponibilidade de pasto (hectares)
  productive_unit$pasture_disp <- with(productive_unit, UA / cap_sup)
  
  # preço do arroba sem funrural
  productive_unit$pArr <- boi_gordo_18_20$P11 - (boi_gordo_18_20$P11 * productive_unit$funrural)
  
  # preço de arrendamento por animal
  productive_unit$arrend_animal <- with(productive_unit, arrend_tax * pArr)
  
  # arrendamento total
  productive_unit$arrend_total <- productive_unit$pasture_disp*0.4*productive_unit$arrend_animal*12
  
  # Saída ----------------------------------------------------------------------
  
  out <- list("productive_unit" = productive_unit,
              "vaca" = vaca,
              "desmama" = desmama,
              "bezerro" = bezerro,
              "novilha_18_20" = novilha_18_20,
              "novilha_21_28" = novilha_21_28,
              "novilha_29_36" = novilha_29_36,
              "garrote" = garrote,
              "boi_magro" = boi_magro,
              "boi_gordo_18_20" = boi_gordo_18_20,
              "boi_gordo_21_28" = boi_gordo_21_28,
              "boi_gordo_29_36" = boi_gordo_29_36,
              "boi_gordo_37_48" = boi_gordo_37_48,
              "boi_gordo_toruno" = boi_gordo_toruno,
              "touro" = touro,
              "aquisicao" = aquisicao,
              "impactos" = impactos, 
              "ref_zoo" = ref_zoo,
              "evolution" = evolution,
              "planning" = planning)
  return(out)
}