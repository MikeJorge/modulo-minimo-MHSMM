# Prepara o ambiente -----------------------------------------------------------
library(readxl)
library(openxlsx)
library(dplyr)

# compila o arquivo com funções criadas
source('./R/MMBC.R', encoding = "UTF-8")
source('./R/animal_biological.R', encoding = "UTF-8")
source('./R/ambiente_de_producao.R', encoding = "UTF-8")
source('./R/producao_vegetal.R', encoding = "UTF-8")
source('./R/producao_animal.R', encoding = "UTF-8")
source('./R/economico.R', encoding = "UTF-8")
source('./R/costs.R', encoding = "UTF-8")
source('./R/viability.R', encoding = "UTF-8")
source('./R/analysis_pe.R', encoding = "UTF-8")
source('./R/results.R', encoding = "UTF-8")

#' Função para prepara as entradas do modelo por praça
#' 
#' Pega as variáveis da praça e complementa as restantes usando o nacional
#'
#' @param input data.frame com os inputs geral e por praça
#' @param praca nome da praça para selecionar
#' @param uf nome do uf para selecionar
#'
#' @return um data.frame tipo o do input geral
#'
#' @examples
input_por_praca <- function(input, praca, uf) {
  # praça de preços = praca
  df1 <- input %>%
    filter(`Praça de preços` == praca)
  
  # praça de preços = "Estadual" e UF = uf, exceto variables selecionadas em 1
  df2 <- input %>%
    filter(`Praça de preços` == "Estadual") %>%
    filter(UF == uf) %>%
    filter(!variable %in% df1$variable)
  
  # praça de preços = "Nacional", exceto variables selecionadas em 1 e 2
  df3 <- input %>%
    filter(`Praça de preços` == "Nacional") %>%
    filter(!variable %in% c(df1$variable, df2$variable))
  
  # combina
  result <- bind_rows(df1, df2, df3)
  return(result)
}

# Carrega dados ----------------------------------------------------------------

# shapefile
df <- readRDS("./data/final.rds")

# caminho para os arquivos de entrada
input <- "./data/EntradaDeDadosLimpo.csv"
input_pracas <- "./data/unificado.xlsx"

# importa como dataframe
df_geral <- read.csv(input, header = FALSE, 
                     col.names = c("value", "variable"), encoding = "UTF-8")
df_pracas <- read_excel(input_pracas)

# atribui medias para pontos sem praça e praça "nacional" para input geral
df_input <- rbind(
  # unificado original
  df_pracas,
  # unificado média por estado de para todas as variáveis
  df_pracas %>%
    filter(`Praça de preços` != "Estadual" &
             variable != "Valor da terra nua no município de exploração (R$/ha)") %>%
    group_by(UF, variable) %>%
    summarise(value = mean(value, na.rm = TRUE)) %>% 
    mutate(UF = UF,
           `Praça de preços` = "Estadual",
           `Tipo região` = "Estado",
           `Nome região` = "Estado", .before = variable),
  # EntradaDeDadosLimpo com as outras variáveis que são global
  df_geral %>% 
    select(variable, value) %>% 
    mutate(UF = NA_character_,
           `Praça de preços` = "Nacional",
           `Tipo região` = NA_character_,
           `Nome região` = NA_character_, .before = variable))
df_input <- df_input %>%
  semi_join(df_geral, by = "variable")

# Define parâmetros da simulação -----------------------------------------------

# remuneração inicial
re <- 200000

# niveis de intensificação: Alto (1), Médio (2) ou Baixo (3)
ni <- 3

# cenários de produção vegetal
# "tl_xt_f" extensico
# "tl_1_f" baixo
# "tl_2_f" medio
# "tl_3_f" alto
# "tl_pt_f" potencial
cenario <- "tl_pt_f"
tl <- df[[cenario]]

# pontos, biomas e praças
pontos <- df$value
biomas <- df$bioma
pracas <- df$praca
ufs <- df$nome_uf

# inicializa variaveis auxiliares
n_pontos <- nrow(df)
list_resultados <- list()

entrada <- NULL
praca <- ""

# função para ser executada em loop
processar_ponto <- function(i) {
  if (is.na(tl[i])) {
    return(NULL) # ignora ponto sem Cs
  } else {
    # define entradas por ponto
    entrada <- input_por_praca(input = df_input, praca = pracas[i], uf = ufs[i])
    # calcula MMBC
    mmbc <- find_MMBC_seq(path = entrada,
                          remuneracao = re,
                          nivel_intensificacao = ni,
                          bioma = biomas[i],
                          cap_sup = tl[i])
    output_mmbc <- results_clean(mmbc$MMBC)
    resultados <- list("ponto" = pontos[i],
                       "bioma" = biomas[i],
                       "uf" = ufs[i],
                       "praça" = pracas[i],
                       "cenário" = cenario,
                       "tl" = tl[i],
                       "output" = output_mmbc)
    return(resultados)
  }
}

# executa análise em loop
for (i in 1:n_pontos) {
  cat("i:", i, "/", n_pontos, "ID:", pontos[i], "\n")
  list_resultados[[i]] <- processar_ponto(i)
}

# converte a lista de resultados em um dataframe
df_resultados <- do.call(rbind, lapply(list_resultados, function(x) {
  output_df <- as.data.frame(t(as.data.frame(x$output)), stringsAsFactors = FALSE)
  colnames(output_df) <- names(x$output)
  rownames(output_df) <- NULL
  
  base_df <- data.frame(
    "ponto" = x$ponto,
    "bioma" = x$bioma,
    "uf" = x$uf,
    "praça" = x$praça,
    "cenário" = x$cenário,
    "tl" = x$tl,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  final_df <- cbind(base_df, output_df)
  return(final_df)
}))

# salva em excel
nome_saida <- paste0("mmbc-", cenario, "-ni", ni, "-", Sys.Date())
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, sheet = 1, df_resultados)
numberStyle <- createStyle(numFmt = "#.##0,0")
addStyle(wb, sheet = 1, numberStyle,
         rows = 2:(nrow(df_resultados)+1),
         cols = c(1, 2, 4:ncol(df_resultados)), gridExpand = TRUE)
saveWorkbook(wb, file = paste0("./data/output tl/", nome_saida, ".xlsx"),
             overwrite = TRUE)
