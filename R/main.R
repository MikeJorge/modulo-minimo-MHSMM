##########################################################################################################################################
# O script calcula a viabilidade econômica e produtiva de um sistema de produção pecuária, ajustando os parâmetros para encontrar a 
# configuração mínima necessária que atenda às exigências de renda financeira do produtor, considerando diferentes níveis de intensificação e 
# características do bioma.
##########################################################################################################################################

# Script para executar o MMBC --------------------------------------------------

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

# arquivo de entrada
input <- "./data/input/EntradaDeDadosLimpo.csv"

# cria MMBC
mmbc <- MMBC(file = input,
             num_matrizes = 500,
             remuneracao = 200000,
             nivel_intensificacao = 1,
             bioma = "Mata Atlântica",
             cap_sup = 1.5)

# Otimização do MMBC -----------------------------------------------------------

# calcula o número de matrizes e área dada a remuneração mínima requerida
mmbc <- find_MMBC_seq(path = input,
                      remuneracao = 200000,
                      nivel_intensificacao = 1,
                      cap_sup = 1.5,
                      bioma = "Mata Atlântica")
