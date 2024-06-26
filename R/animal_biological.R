#' @title Vaca (37 a 48 meses) biological and economic performance
#'
#' @param P1 Preço da vaca gorda com Funrural
#' @param W1 Peso médio da vaca gorda
#' @param R1 Rendimento de carcaça da vaca de descarte (vaca boiadeira)
#' @param Ps1 Pressão de seleção vaca
#' @param wF6 Peso inicial da vaca gorda
#' @param UAF6 UA/Cab
#' @param AqF6 Aquisição
#' @param WAqF6 Peso comercial (aquisição/reposição)
#' @param PAqF6 Preço comercial (aquisição/reposição)
#' @param AqAcomp Aquisição vaca acompanhada
#' @param PAqAcomp Preço comercial (aquisição/reposição) vaca acompanhada
#' @param Mt_ini Mortalidade inicial a ser usada na evolução
#'
#' @return A list
#' @export
#'
#' @examples
bio_vaca <- function(P1, W1, R1, Ps1, wF6, UAF6,
                     AqF6, WAqF6, PAqF6, AqAcomp, PAqAcomp, Mt_ini) {
  x <- list(P1, W1, R1, Ps1, wF6, UAF6,
            AqF6, WAqF6, PAqF6, AqAcomp, PAqAcomp, Mt_ini)
  names(x) <- c("P1", "W1", "R1", "Ps1", "wF6", "UAF6",
                "AqF6", "WAqF6", "PAqF6", "AqAcomp", "PAqAcomp", "Mt_ini")
  return(x)
}

#' @title Desmama (7 a 8 meses) biological and economic performance
#'
#' @param P2 Preço do macho vivo com Funrural
#' @param W2 Peso médio do macho vivo
#' @param Ps2 Pressão de seleção macho
#' @param wM1 Peso inicial do macho
#' @param UAM1 UA/Cab do macho
#' @param AqM1 Aquisição do macho
#' @param WAqM1 Peso comercial (aquisição/reposição) do macho
#' @param PAqM1 Preço comercial (aquisição/reposição) do macho
#' @param P3 Preço da fêmea viva com Funrural
#' @param W3 Peso médio da fêmea viva
#' @param Ps3 Pressão de seleção fêmea
#' @param wF1 Peso inicial da fêmea
#' @param UAF1 UA/Cab da fêmea
#' @param AqF1 Aquisição da fêmea
#' @param WAqF1 Peso comercial (aquisição/reposição) da fêmea
#' @param PAqF1 Preço comercial (aquisição/reposição) da fêmea
#'
#' @return A list
#' @export
#'
#' @examples
bio_desmama <- function(P2, W2, Ps2, wM1, UAM1, AqM1, WAqM1, PAqM1,
                        P3, W3, Ps3, wF1, UAF1, AqF1, WAqF1, PAqF1) {
  x <- list(P2, W2, Ps2, wM1, UAM1, AqM1, WAqM1, PAqM1,
            P3, W3, Ps3, wF1, UAF1, AqF1, WAqF1, PAqF1)
  names(x) <- c("P2", "W2", "Ps2", "wM1", "UAM1", "AqM1", "WAqM1", "PAqM1",
                "P3", "W3", "Ps3", "wF1", "UAF1", "AqF1", "WAqF1", "PAqF1")
  return(x)
}

#' @title Bezerro(a) (12 meses) biological and economic performance
#'
#' @param P4 Preço do macho vivo com Funrural
#' @param W4 Peso médio do macho vivo
#' @param Ps4 Pressão de seleção macho
#' @param wM2 Peso inicial do macho
#' @param UAM2 UA/Cab do macho
#' @param AqM2 Aquisição do macho
#' @param WAqM2 Peso comercial (aquisição/reposição) do macho
#' @param PAqM2 Preço comercial (aquisição/reposição) do macho
#' @param P5 Preço da fêmea viva com Funrural
#' @param W5 Peso médio da fêmea viva
#' @param Ps5 Pressão de seleção fêmea
#' @param wF2 Peso inicial da fêmea
#' @param UAF2 UA/Cab da fêmea
#' @param AqF2 Aquisição da fêmea
#' @param WAqF2 Peso comercial (aquisição/reposição) da fêmea
#' @param PAqF2 Preço comercial (aquisição/reposição) da fêmea
#'
#' @return A list
#' @export
#'
#' @examples
bio_bezerro <- function(P4, W4, Ps4, wM2, UAM2, AqM2, WAqM2, PAqM2,
                        P5, W5, Ps5, wF2, UAF2, AqF2, WAqF2, PAqF2) {
  x <- list(P4, W4, Ps4, wM2, UAM2, AqM2, WAqM2, PAqM2,
            P5, W5, Ps5, wF2, UAF2, AqF2, WAqF2, PAqF2)
  names(x) <- c("P4", "W4", "Ps4", "wM2", "UAM2", "AqM2", "WAqM2", "PAqM2",
                "P5", "W5", "Ps5", "wF2", "UAF2", "AqF2", "WAqF2", "PAqF2")
  return(x)
}

#' @title Novilha (18 a 20 meses) biological and economic performance
#'
#' @param P6 Preço da novilha com Funrural
#' @param Pp6 Premiação da carcaça
#' @param W6 Peso médio da novilha gorda
#' @param R6 Rendimento de carcaça da novilha
#' @param Ps6 Pressão de seleção
#' @param wF3 Peso inicial
#' @param UAF3 UA/Cab
#' @param AqF3 Aquisição
#' @param WAqF3 Peso comercial (aquisição/reposição)
#' @param PAqF3 Preço comercial (aquisição/reposição)
#' @param ccF3Wo Pecuária de ciclo curto ABATE - Peso na saída (kg)
#' @param ccF3dg Pecuária de ciclo curto ABATE - Ganho diário (kg)
#' @param ccF3pf Pecuária de ciclo curto ABATE - % de ração consumida do peso
#'
#' @return A list
#' @export
#'
#' @examples
bio_novilha_18_20 <- function(P6, Pp6, W6, R6, Ps6, wF3, UAF3,
                              AqF3, WAqF3, PAqF3, ccF3Wo, ccF3dg, ccF3pf) {
  x <- list(P6, Pp6, W6, R6, Ps6, wF3, UAF3,
            AqF3, WAqF3, PAqF3, ccF3Wo, ccF3dg, ccF3pf)
  names(x) <- c("P6", "Pp6", "W6", "R6", "Ps6", "wF3", "UAF3",
                "AqF3", "WAqF3", "PAqF3", "ccF3Wo", "ccF3dg", "ccF3pf")
  return(x)
}

#' @title Novilha (21 a 28 meses) biological and economic performance
#'
#' @param P7 Preço da novilha com Funrural
#' @param Pp7 Premiação da carcaça
#' @param W7 Peso médio da novilha gorda
#' @param R7 Rendimento de carcaça da novilha
#' @param Ps7 Pressão de seleção
#' @param wF4 Peso inicial
#' @param UAF4 UA/Cab
#' @param AqF4 Aquisição
#' @param WAqF4 Peso comercial (aquisição/reposição)
#' @param PAqF4 Preço comercial (aquisição/reposição)
#'
#' @return A list
#' @export
#'
#' @examples
bio_novilha_21_28 <- function(P7, Pp7, W7, R7, Ps7, wF4, UAF4, AqF4, WAqF4, PAqF4) {
  x <- list(P7, Pp7, W7, R7, Ps7, wF4, UAF4, AqF4, WAqF4, PAqF4)
  names(x) <- c("P7", "Pp7", "W7", "R7", "Ps7", "wF4", "UAF4", "AqF4", "WAqF4", "PAqF4")
  return(x)
}

#' @title Novilha (29 a 36 meses) biological and economic performance
#'
#' @param P8 Preço da novilha com Funrural
#' @param Pp8 Premiação da carcaça
#' @param W8 Peso médio da novilha gorda
#' @param R8 Rendimento de carcaça da novilha
#' @param Ps8 Pressão de seleção
#' @param wF5 Peso inicial
#' @param UAF5 UA/Cab
#' @param AqF5 Aquisição
#' @param WAqF5 Peso comercial (aquisição/reposição)
#' @param PAqF5 Preço comercial (aquisição/reposição)
#'
#' @return A list
#' @export
#'
#' @examples
bio_novilha_29_36 <- function(P8, Pp8, W8, R8, Ps8, wF5, UAF5, AqF5, WAqF5, PAqF5) {
  x <- list(P8, Pp8, W8, R8, Ps8, wF5, UAF5, AqF5, WAqF5, PAqF5)
  names(x) <- c("P8", "Pp8", "W8", "R8", "Ps8", "wF5", "UAF5", "AqF5", "WAqF5", "PAqF5")
  return(x)
}

#' @title Garrote (18 meses) biological and economic performance
#'
#' @param P9 Preço do garrote com Funrural
#' @param W9 Peso médio do garrote
#' @param R9 Rendimento de carcaça do garrote
#' @param Ps9 Pressão de seleção
#' @param wM3g Peso inicial
#' @param UAM3g UA/Cab
#' @param AqM3g Aquisição
#' @param WAqM3g Peso comercial (aquisição/reposição)
#' @param PAqM3g Preço comercial (aquisição/reposição)
#'
#' @return A list
#' @export
#'
#' @examples
bio_garrote <- function(P9, W9, R9, Ps9, wM3g, UAM3g, AqM3g, WAqM3g, PAqM3g) {
  x <- list(P9, W9, R9, Ps9, wM3g, UAM3g, AqM3g, WAqM3g, PAqM3g)
  names(x) <- c("P9", "W9", "R9", "Ps9", "wM3g", "UAM3g", "AqM3g", "WAqM3g", "PAqM3g")
  return(x)
}

#' @title Boi magro (12 a 13 arrobas) biological and economic performance
#'
#' @param P10 Preço do boi magro com Funrural
#' @param W10 Peso médio do boi magro
#' @param R10 Rendimento de carcaça do boi magro
#' @param Ps10 Pressão de seleção
#' @param wM3bm Peso inicial
#' @param UAM3bm UA/Cab
#' @param AqM3bm Aquisição
#'
#' @return A list
#' @export
#'
#' @examples
bio_boi_magro <- function(P10, W10, R10, Ps10, wM3bm, UAM3bm, AqM3bm) {
  x <- list(P10, W10, R10, Ps10, wM3bm, UAM3bm, AqM3bm)
  names(x) <- c("P10", "W10", "R10", "Ps10", "wM3bm", "UAM3bm", "AqM3bm")
  return(x)
}

#' @title Boi gordo (18 a 20 meses - dente de leite - DL) biological and economic performance
#'
#' @param P11 Preço do boi gordo com Funrural
#' @param W11 Peso médio do boi gorda
#' @param Pp11 Premiação da carcaça
#' @param R11 Rendimento de carcaça do boi gordo
#' @param Ps11 Pressão de seleção
#' @param wM3 Peso inicial
#' @param UAM3 UA/Cab
#' @param AqM3 Aquisição
#' @param ccM3Wo Pecuária de ciclo curto ABATE - Peso na saída (kg)
#' @param ccM3dg Pecuária de ciclo curto ABATE - Ganho diário (kg)
#' @param ccM3pf Pecuária de ciclo curto ABATE - % de ração consumida do peso
#'
#' @return A list
#' @export
#'
#' @examples
bio_boi_gordo_18_20 <- function(P11, W11, Pp11, R11, Ps11, wM3, UAM3, AqM3,
                                ccM3Wo, ccM3dg, ccM3pf) {
  x <- list(P11, W11, Pp11, R11, Ps11, wM3, UAM3, AqM3,
            ccM3Wo, ccM3dg, ccM3pf)
  names(x) <- c("P11", "W11", "Pp11", "R11", "Ps11", "wM3", "UAM3", "AqM3",
                "ccM3Wo", "ccM3dg", "ccM3pf")
  return(x)
}

#' @title Boi gordo (21 a 28 meses - até dois dentes permanentes) biological and economic performance
#'
#' @param P12 Preço do boi gordo com Funrural
#' @param W12 Peso médio do boi gorda
#' @param Pp12 Premiação da carcaça
#' @param R12 Rendimento de carcaça do boi gordo
#' @param Ps12 Pressão de seleção
#' @param wM4 Peso inicial
#' @param UAM4 UA/Cab
#' @param AqM4 Aquisição
#' @param ccM4Wo Pecuária de ciclo curto ABATE - Peso na saída (kg)
#' @param ccM4dg Pecuária de ciclo curto ABATE - Ganho diário (kg)
#' @param ccM4pf Pecuária de ciclo curto ABATE - % de ração consumida do peso
#'
#' @return A list
#' @export
#'
#' @examples
bio_boi_gordo_21_28 <- function(P12, W12, Pp12, R12, Ps12, wM4, UAM4, AqM4,
                                ccM4Wo, ccM4dg, ccM4pf) {
  x <- list(P12, W12, Pp12, R12, Ps12, wM4, UAM4, AqM4,
            ccM4Wo, ccM4dg, ccM4pf)
  names(x) <- c("P12", "W12", "Pp12", "R12", "Ps12", "wM4", "UAM4", "AqM4",
                "ccM4Wo", "ccM4dg", "ccM4pf")
  return(x)
}

#' @title Boi gordo (29 a 36 meses - até quatro dentes permanentes) biological and economic performance
#'
#' @param P13 Preço do boi gordo com Funrural
#' @param W13 Peso médio do boi gorda
#' @param Pp13 Premiação da carcaça
#' @param R13 Rendimento de carcaça do boi gordo
#' @param Ps13 Pressão de seleção
#' @param wM5 Peso inicial
#' @param UAM5 UA/Cab
#' @param AqM5 Aquisição
#' @param ccM5Wo Pecuária de ciclo curto ABATE - Peso na saída (kg)
#' @param ccM5dg Pecuária de ciclo curto ABATE - Ganho diário (kg)
#' @param ccM5pf Pecuária de ciclo curto ABATE - % de ração consumida do peso
#'
#' @return A list
#' @export
#'
#' @examples
bio_boi_gordo_29_36 <- function(P13, W13, Pp13, R13, Ps13, wM5, UAM5, AqM5,
                                ccM5Wo, ccM5dg, ccM5pf) {
  x <- list(P13, W13, Pp13, R13, Ps13, wM5, UAM5, AqM5,
            ccM5Wo, ccM5dg, ccM5pf)
  names(x) <- c("P13", "W13", "Pp13", "R13", "Ps13", "wM5", "UAM5", "AqM5",
                "ccM5Wo", "ccM5dg", "ccM5pf")
  return(x)
}

#' @title Boi gordo (37 a 48 meses - adulto) biological and economic performance
#'
#' @param P14 Preço do boi gordo com Funrural
#' @param W14 Peso médio do boi gorda
#' @param Pp14 Premiação da carcaça
#' @param R14 Rendimento de carcaça do boi gordo
#' @param Ps14 Pressão de seleção
#' @param wM6 Peso inicial
#' @param UAM6 UA/Cab
#' @param AqM6 Aquisição
#'
#' @return A list
#' @export
#'
#' @examples
bio_boi_gordo_37_48 <- function(P14, W14, Pp14, R14, Ps14, wM6, UAM6, AqM6) {
  x <- list(P14, W14, Pp14, R14, Ps14, wM6, UAM6, AqM6)
  names(x) <- c("P14", "W14", "Pp14", "R14", "Ps14", "wM6", "UAM6", "AqM6")
  return(x)
}

#' @title Boi gordo toruno (acima de 60 meses) biological and economic performance
#'
#' @param P15 Preço do boi gordo com Funrural
#' @param W15 Peso médio do boi gorda
#' @param R15 Rendimento de carcaça do boi gordo
#' @param Ps15 Pressão de seleção
#' @param wM6bgt Peso inicial
#' @param UAM6bgt UA/Cab
#' @param AqM6bgt Aquisição
#'
#' @return A list
#' @export
#'
#' @examples
bio_boi_gordo_toruno <- function(P15, W15, R15, Ps15, wM6bgt, UAM6bgt, AqM6bgt) {
  x <- list(P15, W15, R15, Ps15, wM6bgt, UAM6bgt, AqM6bgt)
  names(x) <- c("P15", "W15", "R15", "Ps15", "wM6bgt", "UAM6bgt", "AqM6bgt")
  return(x)
}

#' @title Touro
#'
#' @param wM6t Peso inicial
#' @param UAM6t UA/Cab
#' @param AqM6t Aquisição
#' @param WAqM6t Peso comercial (aquisição/reposição)
#' @param PAqM6t Preço comercial (aquisição/reposição)
#'
#' @return A list
#' @export
#'
#' @examples
bio_touro <- function(wM6t, UAM6t, AqM6t, WAqM6t, PAqM6t) {
  x <- list(wM6t, UAM6t, AqM6t, WAqM6t, PAqM6t)
  names(x) <- c("wM6t", "UAM6t", "AqM6t", "WAqM6t", "PAqM6t")
  return(x)
}

#' @title Impactos sobre a produção
#'
#' @param Mt1 Mortalidade até a desmama
#' @param Mt2 Mortalidade até 12 meses
#' @param Mt3 Mortalidade até 18 a 20 meses
#' @param Mt4 Mortalidade até 21 a 28 meses
#' @param Mt5 Mortalidade até 29 a 36 meses
#' @param Mt6 Mortalidade até 37 a 48 meses
#'
#' @return A list
#' @export
#'
#' @examples
bio_impact <- function(Mt1, Mt2, Mt3, Mt4, Mt5, Mt6) {
  x <- list(Mt1, Mt2, Mt3, Mt4, Mt5, Mt6)
  names(x) <- c("Mt1", "Mt2", "Mt3", "Mt4", "Mt5", "Mt6")
  return(x)
}

#' @title Referências zootécnicas
#'
#' @param Rs1 Relação touro:vaca
#' @param Rp2 Reposição de vacas
#' @param Rp3 Reposição de touros
#' @param Tn Taxa de natalidade
#'
#' @return A list
#' @export
#'
#' @examples
bio_zoo_ref <- function(Rs1, Rp2, Rp3, Tn) {
  x <- list(Rs1, Rp2, Rp3, Tn)
  names(x) <- c("Rs1", "Rp2", "Rp3", "Tn")
  return(x)
}

#' @title Consumo diário nutricional
#'
#' @param C1 Consumo diário Sal mineral
#' @param C2 Consumo diário Sal mineral vaca reprodução - cada 100 kg de peso vivo
#' @param C3 Consumo diário Suplemento mineral proteico - cada 100 kg de peso vivo
#' @param C4 Consumo diário Suplemento mineral energético - cada 100 kg de peso vivo
#' @param C5 Consumo diário Creep-Feeding
#' @param Tc6 Trava de consumo Creep-Feeding - sal mineral. Não utilizado na versão atual
#' @param C7 Consumo diário Dieta do semiconfinamento à pasto - do peso vivo
#' @param pC1 Preço Sal mineral
#' @param pC2 Preço Sal mineral vaca reprodução
#' @param pC3 Preço Suplemento mineral proteico
#' @param pC4 Preço Suplemento mineral energético
#' @param pC5 Preço Creep-Feeding
#' @param pTc6 Preço Trava de consumo Creep-Feeding. Não utilizado na versão atual
#' @param creep_days Dias de confinamento de creep feeding
#' @param pCc18_20 Preço Pecuária de ciclo curto - Dieta 18 a 20 meses
#' @param pCc21_28 Preço Pecuária de ciclo curto - Dieta 21 a 28 meses
#'
#' @return A list
#' @export
#'
#' @examples
bio_daily_feeding_consumption <- function(C1, C2, C3, C4, C5, Tc6, C7,
                                          pC1, pC2, pC3, pC4, pC5, pTc6,
                                          creep_days, pCc18_20, pCc21_28) {
  x <- list(C1, C2, C3, C4, C5, Tc6, C7,
            pC1, pC2, pC3, pC4, pC5, pTc6, creep_days,
            pCc18_20, pCc21_28)
  names(x) <- c("C1", "C2", "C3", "C4", "C5", "Tc6", "C7",
                "pC1", "pC2", "pC3", "pC4", "pC5", "pTc6", "creep_days",
                "pCc18_20", "pCc21_28")
  return(x)
}

#' @title Vacina contra febre aftosa
#'
#' @param F6 Vacas 37 a 48 meses
#' @param F5 Novilhas 29 a 36 meses
#' @param F4 Novilhas 21 a 28 meses
#' @param F3 Novilhas 18 a 20 meses
#' @param F2 Bezerras 12 meses
#' @param F1 Desmama fêmeas 7 a 8 meses
#' @param M1 Desmama machos 7 a 8 meses
#' @param M2 Bezerros 12 meses
#' @param M3g Garrote 18 meses
#' @param M3bm Boi Magro (12 a 13 arrobas)
#' @param M3 Boi gordo 18 a 20 meses
#' @param M4 Boi gordo 21 a 28 meses
#' @param M5 Boi gordo 29 a 36 meses
#' @param M6 Boi gordo 37 a 48 meses
#' @param M6bgt Boi gordo "toruno" acima de 60 meses
#' @param M6t Touros
#' @param unit_price Preço unitário
#'
#' @return A list
#' @export
#'
#' @examples
bio_vax_af <- function(F6, F5, F4, F3, F2, F1,
                       M1, M2, M3g, M3bm, M3, M4, M5, M6, M6bgt, M6t,
                       unit_price) {
  x <- list(F6, F5, F4, F3, F2, F1,
            M1, M2, M3g, M3, M3bm, M4, M5, M6, M6bgt, M6t,
            unit_price)
  names(x) <- c("F6", "F5", "F4", "F3", "F2", "F1",
                "M1", "M2", "M3g", "M3", "M3bm", "M4", "M5", "M6", "M6bgt", "M6t",
                "unit_price")
  return(x)
}

#' @title Vacina contra carbúnculo sintomático
#'
#' @param F6 Vacas 37 a 48 meses
#' @param F5 Novilhas 29 a 36 meses
#' @param F4 Novilhas 21 a 28 meses
#' @param F3 Novilhas 18 a 20 meses
#' @param F2 Bezerras 12 meses
#' @param F1 Desmama fêmeas 7 a 8 meses
#' @param M1 Desmama machos 7 a 8 meses
#' @param M2 Bezerros 12 meses
#' @param M3g Garrote 18 meses
#' @param M3bm Boi Magro (12 a 13 arrobas)
#' @param M3 Boi gordo 18 a 20 meses
#' @param M4 Boi gordo 21 a 28 meses
#' @param M5 Boi gordo 29 a 36 meses
#' @param M6 Boi gordo 37 a 48 meses
#' @param M6bgt Boi gordo "toruno" acima de 60 meses
#' @param M6t Touros
#' @param unit_price Preço unitário
#'
#' @return A list
#' @export
#'
#' @examples
bio_vax_carb <- function(F6, F5, F4, F3, F2, F1,
                       M1, M2, M3g, M3bm, M3, M4, M5, M6, M6bgt, M6t,
                       unit_price) {
  x <- list(F6, F5, F4, F3, F2, F1,
            M1, M2, M3g, M3, M3bm, M4, M5, M6, M6bgt, M6t,
            unit_price)
  names(x) <- c("F6", "F5", "F4", "F3", "F2", "F1",
                "M1", "M2", "M3g", "M3", "M3bm", "M4", "M5", "M6", "M6bgt", "M6t",
                "unit_price")
  return(x)
}

#' @title Vacina contra brucelose
#'
#' @param F6 Vacas 37 a 48 meses
#' @param F5 Novilhas 29 a 36 meses
#' @param F4 Novilhas 21 a 28 meses
#' @param F3 Novilhas 18 a 20 meses
#' @param F2 Bezerras 12 meses
#' @param F1 Desmama fêmeas 7 a 8 meses
#' @param M1 Desmama machos 7 a 8 meses
#' @param M2 Bezerros 12 meses
#' @param M3g Garrote 18 meses
#' @param M3bm Boi Magro (12 a 13 arrobas)
#' @param M3 Boi gordo 18 a 20 meses
#' @param M4 Boi gordo 21 a 28 meses
#' @param M5 Boi gordo 29 a 36 meses
#' @param M6 Boi gordo 37 a 48 meses
#' @param M6bgt Boi gordo "toruno" acima de 60 meses
#' @param M6t Touros
#' @param unit_price Preço unitário
#'
#' @return A list
#' @export
#'
#' @examples
bio_vax_bru <- function(F6, F5, F4, F3, F2, F1,
                       M1, M2, M3g, M3bm, M3, M4, M5, M6, M6bgt, M6t,
                       unit_price) {
  x <- list(F6, F5, F4, F3, F2, F1,
            M1, M2, M3g, M3, M3bm, M4, M5, M6, M6bgt, M6t,
            unit_price)
  names(x) <- c("F6", "F5", "F4", "F3", "F2", "F1",
                "M1", "M2", "M3g", "M3", "M3bm", "M4", "M5", "M6", "M6bgt", "M6t",
                "unit_price")
  return(x)
}

#' @title Vermífugo
#'
#' @param F6 Vacas 37 a 48 meses
#' @param F5 Novilhas 29 a 36 meses
#' @param F4 Novilhas 21 a 28 meses
#' @param F3 Novilhas 18 a 20 meses
#' @param F2 Bezerras 12 meses
#' @param F1 Desmama fêmeas 7 a 8 meses
#' @param M1 Desmama machos 7 a 8 meses
#' @param M2 Bezerros 12 meses
#' @param M3g Garrote 18 meses
#' @param M3bm Boi Magro (12 a 13 arrobas)
#' @param M3 Boi gordo 18 a 20 meses
#' @param M4 Boi gordo 21 a 28 meses
#' @param M5 Boi gordo 29 a 36 meses
#' @param M6 Boi gordo 37 a 48 meses
#' @param M6bgt Boi gordo "toruno" acima de 60 meses
#' @param M6t Touros
#' @param unit_price Preço unitário
#'
#' @return A list
#' @export
#'
#' @examples
vermifuge <- function(F6, F5, F4, F3, F2, F1,
                      M1, M2, M3g, M3bm, M3, M4, M5, M6, M6bgt, M6t,
                      unit_price) {
  x <- list(F6, F5, F4, F3, F2, F1,
            M1, M2, M3g, M3, M3bm, M4, M5, M6, M6bgt, M6t,
            unit_price)
  names(x) <- c("F6", "F5", "F4", "F3", "F2", "F1",
                "M1", "M2", "M3g", "M3", "M3bm", "M4", "M5", "M6", "M6bgt", "M6t",
                "unit_price")
  return(x)
}
