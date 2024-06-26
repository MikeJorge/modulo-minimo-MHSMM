library(ggplot2)

#' @title Análise do Ponto de Equilíbrio Econômico
#'
#' @description
#' Realiza análise do Ponto de Equilíbrio Econômico e plota gráfico.
#'
#' @param propriedade Propriedade simulada
#' @param costs Custos
#'
#' @return A list
#' @export
#'
#' @examples
analysis_pe <- function(propriedade, costs) {
  
  cabeca_animal <- seq(0, 5000, 100)
  PE_cabeca <- length(cabeca_animal)
  
  # Chama variáveis calculadas -------------------------------------------------
  
  # Custos Fixo, Variável e Total
  custo_fixo <- rep(costs$fixo, PE_cabeca)
  custo_variavel <- rep(costs$variavel, PE_cabeca)
  custo_total <- rep(costs$total, PE_cabeca)
  
  # Preço de venda = preço unitário Total
  preco_venda <- rep(propriedade$evolution$pUTot, PE_cabeca)
  
  # CV unitário = Custo operacional por cabeça - R$ cab. ano-1
  # custo variável / total de cabeças
  custo_variavel_unitario <- rep(costs$variavel / propriedade$evolution$Qr, PE_cabeca)
  
  # Calcula variáveis da Análise do Ponto de Equilíbrio ------------------------
  
  # Receita Líquida
  receita_liquida <- cabeca_animal * preco_venda
  
  # Lucro
  lucro <- receita_liquida - custo_total
  
  # Salva em data.frame
  df_analise_PE <- data.frame(
    "Cabeça animal" = cabeca_animal,
    "Receita Líquida" = receita_liquida,
    "Custo Fixo" = custo_fixo,
    "Custo Variável" = custo_variavel,
    "Custo Total" = custo_total,
    "Lucro" = lucro,
    check.names = FALSE
  )
  
  # Plota gráfico --------------------------------------------------------------
  
  p <- ggplot(data = df_analise_PE, mapping = aes(x = `Cabeça animal`)) +
    geom_line(aes(y = `Receita Líquida`, color = "Receita Líquida"), linewidth = 1) +
    geom_line(aes(y = `Custo Fixo`, color = "Custo Fixo"), linewidth = 1) +
    geom_line(aes(y = `Custo Variável`, color = "Custo Variável"), linewidth = 1) +
    geom_line(aes(y = `Custo Total`, color = "Custo Total"), linewidth = 1) +
    geom_line(aes(y = `Lucro`, color = "Lucro"), linewidth = 1) +
    labs(title = "Ponto de Equilíbrio Econômico") +
    scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
    scale_color_manual(
      name = "",
      breaks = c("Receita Líquida",
                 "Custo Fixo",
                 "Custo Variável",
                 "Custo Total",
                 "Lucro"),
      values = c("Receita Líquida" = "#66c2a5",
                 "Custo Fixo" = "#fc8d62",
                 "Custo Variável" = "#8da0cb",
                 "Custo Total" = "#e78ac3",
                 "Lucro" = "#a6d854")) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  # Saída ----------------------------------------------------------------------
  
  out <- list("cabeca_animal" = cabeca_animal,
              "receita_liquida" = receita_liquida,
              "custo_fixo" = custo_fixo,
              "custo_variavel" = custo_variavel,
              "custo_total" = custo_total,
              "lucro" = lucro,
              "custo_variavel_unitario" = custo_variavel_unitario,
              "preco_venda" = preco_venda,
              "plot" = p)
  return(out)
}
