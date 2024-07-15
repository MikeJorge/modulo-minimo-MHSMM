########################################################################################################################################
# O script calcula variáveis de saída do módulo de produção animal de um sistema de produção pecuária. Ele define diversas funções auxiliares 
# que criam listas de dados estruturados, e uma função principal producao_animal que agrupa essas listas em uma única lista mais abrangente.
# Funções Auxiliares - Cada função auxiliar cria uma lista com nomeação específica para diferentes aspectos do sistema de produção animal:
# 1. 'pesos': Define os pesos iniciais e comerciais de diferentes categorias de animais.
# 2. 'rendimentos': Define os rendimentos de carcaça para diferentes categorias de animais.
# 3. 'pressao': Define a pressão de seleção para diferentes categorias de animais.
# 4. 'coeficientes': Define os coeficientes de uso animal (UA) para diferentes categorias de animais.
# 5. 'aquisicao': Define os números de aquisição de diferentes categorias de animais.
# 6. 'premio': Define os prêmios de carcaça para diferentes categorias de animais.
# 7. 'mortalidade': Define as taxas de mortalidade para diferentes fases do ciclo de vida animal.
# 8. 'ciclo_curto': Define parâmetros relacionados à produção de ciclo curto.
# 9. 'referencias_zootecnicas': Define referências zootécnicas importantes, como taxa de natalidade e reposição.
# 10. 'consumo': Define o consumo diário nutricional para diferentes categorias de animais.
# 11. 'vacinas': Define os diferentes tipos de vacinas e vermífugos administrados às diferentes categorias de animais.
# A função producao_animal recebe os dados de diferentes aspectos do sistema de produção animal, organizados em listas criadas pelas 
# funções auxiliares, e os agrupa em uma lista final, nomeando cada componente da lista de acordo com sua função.
##########################################################################################################################################

#' @title Módulo de produção animal
#' 
#' @description
#' Calcula variáveis de saída do módulo de produção animal
#' 
#' @param vaca Vaca (37 a 48 meses)
#' @param desmama Desmama (7 a 8 meses)
#' @param bezerro Bezerro(a) (12 meses)
#' @param novilha_18_20 Novilha (18 a 20 meses)
#' @param novilha_21_28 Novilha (21 a 28 meses)
#' @param novilha_29_36 Novilha (29 a 36 meses)
#' @param garrote Garrote (18 meses)
#' @param boi_magro Boi magro (12 a 13 arrobas)
#' @param boi_gordo_18_20 Boi gordo (18 a 20 meses - dente de leite - DL)
#' @param boi_gordo_21_28 Boi gordo (21 a 28 meses - até dois dentes permanentes)
#' @param boi_gordo_29_36 Boi gordo (29 a 36 meses - até quatro dentes permanentes)
#' @param boi_gordo_37_48 Boi gordo (37 a 48 meses - adulto)
#' @param boi_gordo_toruno Boi gordo toruno (acima de 60 meses)
#' @param touro Touro
#' @param impactos Impactos sobre a produção
#' @param ref_zoo Referências zootécnicas
#' @param daily_feeding_consumption Consumo diário nutricional
#' @param productive_unit Dados da unidade produtiva
#' @param vax_af Vacina aftosa
#' @param vax_carb Vacina carbunculo sintomático
#' @param vax_bru Vacina brucelose
#' @param vermifuge Vermifugo
#'
#' @return A list
#' @export
#'
#' @examples
producao_animal <- function(productive_unit, pesos, rendimentos, pressao,
                            coeficientes, aquisicao, premio, mortalidade,
                            ciclo_curto, referencias_zootecnicas,
                            consumo, vacinas) {
  x <- list(productive_unit, pesos, rendimentos, pressao,
            coeficientes, aquisicao, premio, mortalidade,
            ciclo_curto, referencias_zootecnicas,
            consumo, vacinas)
  names(x) <- c("productive_unit", "pesos", "rendimentos", "pressao",
                "coeficientes", "aquisicao", "premio", "mortalidade",
                "ciclo_curto", "referencias_zootecnicas",
                "consumo", "vacinas")
  return(x)
}

pesos <- function(W1, W2, W3, W4, W5, W6, W7, W8,
                  W9, W10, W11, W12, W13, W14, W15,
                  wF6, wF5, wF4, wF3, wF2, wF1,
                  wM1, wM2, wM3g, wM3bm, wM3, wM4, wM5, wM6, wM6bgt, wM6t,
                  WAqF6, WAqF5, WAqF4, WAqF3, WAqF2, WAqF1,
                  WAqM1, WAqM2, WAqM3g, WAqM6t) {
  x <- list(W1, W2, W3, W4, W5, W6, W7, W8,
            W9, W10, W11, W12, W13, W14, W15,
            wF6, wF5, wF4, wF3, wF2, wF1,
            wM1, wM2, wM3g, wM3bm, wM3, wM4, wM5, wM6, wM6bgt, wM6t,
            WAqF6, WAqF5, WAqF4, WAqF3, WAqF2, WAqF1,
            WAqM1, WAqM2, WAqM3g, WAqM6t)
  names(x) <- c("W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8",
                "W9", "W10", "W11", "W12", "W13", "W14", "W15",
                "wF6", "wF5", "wF4", "wF3", "wF2", "wF1",
                "wM1", "wM2", "wM3g", "wM3bm", "wM3", "wM4", "wM5", "wM6", "wM6bgt", "wM6t",
                "WAqF6", "WAqF5", "WAqF4", "WAqF3", "WAqF2", "WAqF1",
                "WAqM1", "WAqM2", "WAqM3g", "WAqM6t")
  return(x)
}

rendimentos <- function(R1, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15) {
  x <- list(R1, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15)
  names(x) <- c("R1", "R6", "R7", "R8", "R9", "R10", "R11", "R12", "R13", "R14", "R15")
  return(x)
}

pressao <- function(Ps1, Ps2, Ps3, Ps4, Ps5, Ps6, Ps7, Ps8,
                    Ps9, Ps10, Ps11, Ps12, Ps13, Ps14, Ps15) {
  x <- list(Ps1, Ps2, Ps3, Ps4, Ps5, Ps6, Ps7, Ps8,
            Ps9, Ps10, Ps11, Ps12, Ps13, Ps14, Ps15)
  names(x) <- c("Ps1", "Ps2", "Ps3", "Ps4", "Ps5", "Ps6", "Ps7", "Ps8",
                "Ps9", "Ps10", "Ps11", "Ps12", "Ps13", "Ps14", "Ps15")
  return(x)
}

coeficientes <- function(UAF6, UAF5, UAF4, UAF3, UAF2, UAF1,
                         UAM1, UAM2, UAM3g, UAM3bm, UAM3, UAM4, UAM5, UAM6,
                         UAM6bgt, UAM6t) {
  x <- list(UAF6, UAF5, UAF4, UAF3, UAF2, UAF1,
            UAM1, UAM2, UAM3g, UAM3bm, UAM3, UAM4, UAM5, UAM6,
            UAM6bgt, UAM6t)
  names(x) <- c("UAF6", "UAF5", "UAF4", "UAF3", "UAF2", "UAF1",
                "UAM1", "UAM2", "UAM3g", "UAM3bm", "UAM3", "UAM4", "UAM5", "UAM6",
                "UAM6bgt", "UAM6t")
  return(x)
}

aquisicao <- function(AqF6, AqF5, AqF4, AqF3, AqF2, AqF1,
                      AqM1, AqM2, AqM3g, AqM3bm, AqM3, AqM4, AqM5, AqM6,
                      AqM6bgt, AqM6t, AqAcomp) {
  x <- list(AqF6, AqF5, AqF4, AqF3, AqF2, AqF1,
            AqM1, AqM2, AqM3g, AqM3bm, AqM3, AqM4, AqM5, AqM6,
            AqM6bgt, AqM6t, AqAcomp)
  names(x) <- c("AqF6", "AqF5", "AqF4", "AqF3", "AqF2", "AqF1",
                "AqM1", "AqM2", "AqM3g", "AqM3bm", "AqM3", "AqM4", "AqM5", "AqM6",
                "AqM6bgt", "AqM6t", "AqAcomp")
  return(x)
}

premio <- function(Pp6, Pp7, Pp8, Pp11, Pp12, Pp13, Pp14) {
  x <- list(Pp6, Pp7, Pp8, Pp11, Pp12, Pp13, Pp14)
  names(x) <- c("Pp6", "Pp7", "Pp8", "Pp11", "Pp12", "Pp13", "Pp14")
  return(x)
}

mortalidade <- function(Mt1, Mt2, Mt3, Mt4, Mt5, Mt6, Mt_ini) {
  x <- list(Mt1, Mt2, Mt3, Mt4, Mt5, Mt6, Mt_ini)
  names(x) <- c("Mt1", "Mt2", "Mt3", "Mt4", "Mt5", "Mt6", "Mt_ini")
  return(x)
}

ciclo_curto <- function(ccF3Wo, ccF3dg, ccF3pf, ccM3Wo, ccM3dg, ccM3pf,
                        ccM4Wo, ccM4dg, ccM4pf, ccM5Wo, ccM5dg, ccM5pf) {
  x <- list(ccF3Wo, ccF3dg, ccF3pf, ccM3Wo, ccM3dg, ccM3pf,
            ccM4Wo, ccM4dg, ccM4pf, ccM5Wo, ccM5dg, ccM5pf)
  names(x) <- c("ccF3Wo", "ccF3dg", "ccF3pf", "ccM3Wo", "ccM3dg", "ccM3pf",
                "ccM4Wo", "ccM4dg", "ccM4pf", "ccM5Wo", "ccM5dg", "ccM5pf")
  return(x)
}

referencias_zootecnicas <- function(Rs1, Rp2, Rp3, Tn) {
  x <- list(Rs1, Rp2, Rp3, Tn)
  names(x) <- c("Rs1", "Rp2", "Rp3", "Tn")
  return(x)
}

consumo <- function(C1, C2, C3, C4, C5, Tc6, C7, creep_days) {
  x <- list(C1, C2, C3, C4, C5, Tc6, C7, creep_days)
  names(x) <- c("C1", "C2", "C3", "C4", "C5", "Tc6", "C7", "creep_days")
  return(x)
}

vacinas <- function(vacaftosa_F6, vacaftosa_F5, vacaftosa_F4, vacaftosa_F3,
                    vacaftosa_F2, vacaftosa_F1, vacaftosa_M1, vacaftosa_M2,
                    vacaftosa_M3g, vacaftosa_M3, vacaftosa_M3bm, vacaftosa_M4,
                    vacaftosa_M5, vacaftosa_M6, vacaftosa_M6bgt, vacaftosa_M6t,
                    vaccarb_F6, vaccarb_F5, vaccarb_F4, vaccarb_F3, vaccarb_F2,
                    vaccarb_F1, vaccarb_M1, vaccarb_M2, vaccarb_M3g, vaccarb_M3,
                    vaccarb_M3bm, vaccarb_M4, vaccarb_M5, vaccarb_M6, vaccarb_M6bgt,
                    vaccarb_M6t, vacbruc_F6, vacbruc_F5, vacbruc_F4, vacbruc_F3,
                    vacbruc_F2, vacbruc_F1, vacbruc_M1, vacbruc_M2, vacbruc_M3g,
                    vacbruc_M3, vacbruc_M3bm, vacbruc_M4, vacbruc_M5, vacbruc_M6,
                    vacbruc_M6bgt, vacbruc_M6t, vermifugo_F6, vermifugo_F5,
                    vermifugo_F4, vermifugo_F3, vermifugo_F2, vermifugo_F1,
                    vermifugo_M1, vermifugo_M2, vermifugo_M3g, vermifugo_M3,
                    vermifugo_M3bm, vermifugo_M4, vermifugo_M5, vermifugo_M6,
                    vermifugo_M6bgt, vermifugo_M6t) {
  x <- list(vacaftosa_F6, vacaftosa_F5, vacaftosa_F4, vacaftosa_F3,
            vacaftosa_F2, vacaftosa_F1, vacaftosa_M1, vacaftosa_M2,
            vacaftosa_M3g, vacaftosa_M3, vacaftosa_M3bm, vacaftosa_M4,
            vacaftosa_M5, vacaftosa_M6, vacaftosa_M6bgt, vacaftosa_M6t,
            vaccarb_F6, vaccarb_F5, vaccarb_F4, vaccarb_F3, vaccarb_F2,
            vaccarb_F1, vaccarb_M1, vaccarb_M2, vaccarb_M3g, vaccarb_M3,
            vaccarb_M3bm, vaccarb_M4, vaccarb_M5, vaccarb_M6, vaccarb_M6bgt,
            vaccarb_M6t, vacbruc_F6, vacbruc_F5, vacbruc_F4, vacbruc_F3,
            vacbruc_F2, vacbruc_F1, vacbruc_M1, vacbruc_M2, vacbruc_M3g,
            vacbruc_M3, vacbruc_M3bm, vacbruc_M4, vacbruc_M5, vacbruc_M6,
            vacbruc_M6bgt, vacbruc_M6t, vermifugo_F6, vermifugo_F5,
            vermifugo_F4, vermifugo_F3, vermifugo_F2, vermifugo_F1,
            vermifugo_M1, vermifugo_M2, vermifugo_M3g, vermifugo_M3,
            vermifugo_M3bm, vermifugo_M4, vermifugo_M5, vermifugo_M6,
            vermifugo_M6bgt, vermifugo_M6t)
  names(x) <- c("vacaftosa_F6", "vacaftosa_F5", "vacaftosa_F4", "vacaftosa_F3",
                "vacaftosa_F2", "vacaftosa_F1", "vacaftosa_M1", "vacaftosa_M2",
                "vacaftosa_M3g", "vacaftosa_M3", "vacaftosa_M3bm", "vacaftosa_M4",
                "vacaftosa_M5", "vacaftosa_M6", "vacaftosa_M6bgt", "vacaftosa_M6t",
                "vaccarb_F6", "vaccarb_F5", "vaccarb_F4", "vaccarb_F3", "vaccarb_F2",
                "vaccarb_F1", "vaccarb_M1", "vaccarb_M2", "vaccarb_M3g", "vaccarb_M3",
                "vaccarb_M3bm", "vaccarb_M4", "vaccarb_M5", "vaccarb_M6",
                "vaccarb_M6bgt", "vaccarb_M6t", "vacbruc_F6", "vacbruc_F5",
                "vacbruc_F4", "vacbruc_F3", "vacbruc_F2", "vacbruc_F1",
                "vacbruc_M1", "vacbruc_M2", "vacbruc_M3g", "vacbruc_M3",
                "vacbruc_M3bm", "vacbruc_M4", "vacbruc_M5", "vacbruc_M6",
                "vacbruc_M6bgt", "vacbruc_M6t", "vermifugo_F6", "vermifugo_F5",
                "vermifugo_F4", "vermifugo_F3", "vermifugo_F2", "vermifugo_F1",
                "vermifugo_M1", "vermifugo_M2", "vermifugo_M3g", "vermifugo_M3",
                "vermifugo_M3bm", "vermifugo_M4", "vermifugo_M5", "vermifugo_M6",
                "vermifugo_M6bgt", "vermifugo_M6t")
  return(x)
}
