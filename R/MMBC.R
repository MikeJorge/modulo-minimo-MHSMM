#########################################################################################################################################
# O script define um conjunto abrangente de funções para calcular e simular a produção de uma propriedade de bovinocultura de corte, 
# considerando diversos parâmetros econômicos, ambientais e produtivos. As funções principais (MMBC e find_MMBC_seq) são responsáveis por
# configurar o ambiente de produção, calcular custos, simular a produção e realizar análises de viabilidade econômica. A saída é uma lista 
# detalhada contendo todas as informações necessárias para avaliar a viabilidade e eficiência da produção.
###########################################################################################################################################


MMBC_core <- function(file, num_matrizes, remuneracao, nivel_intensificacao,
                      bioma, cap_sup = 1.5) {

  # Entradas -------------------------------------------------------------------
  
  ## Importa dados -------------------------------------------------------------
  
  # lê dados de entrada
  if (is.character(file)) {
    df <- read.csv(file, header = FALSE, col.names = c("value", "variable"),
                   encoding = "UTF-8")
  } else {
    df <- file
  }
  
  # salva como lista
  x <- as.list(df$value)
  names(x) <- df$variable
  
  # verifica biomas
  if (!bioma %in% c("Mata Atlântica", "Amazônia", "Cerrado")) {
    warning("'bioma' deve ser um dos seguintes: 'Mata Atlântica', 'Amazônia', 'Cerrado'.")
    return(NA)
    # stop("Erro: 'bioma' deve ser um dos seguintes: 'Mata Atlântica', 'Amazônia', 'Cerrado'.")
  }
  
  ## Ambiente de produção ------------------------------------------------------
  ambiente_de_producao <- ambiente_de_producao(
    
    productive_unit = bio_productive_unit(
      nivel_intensificacao = nivel_intensificacao,
      num_matrizes = num_matrizes,
      cap_sup = cap_sup, # taxa de lotação
      funrural = x$`Taxa de Funrural`,
      arrend_tax = ifelse(
        nivel_intensificacao == 1,
        x$`Taxa de arrendamento Ani`,
        ifelse(nivel_intensificacao == 2,
               x$`Taxa de arrendamento Mni`,
               ifelse(nivel_intensificacao == 3,
                      x$`Taxa de arrendamento Bni`, NA)))
    ),
    
    inventario = inventario(
      nivel_intensificacao = nivel_intensificacao,
      
      distribuidor_semente_Vi = x$`Distribuidor de sementes/fertilizantes (Disco Duplo Montado)`,
      distribuidor_calcario_Vi = x$`Distribuidor de Calcário (Disco Duplo)`,
      grade_aradora_Vi = x$`Grade aradora (20 a 28 discos - 26 a 28 pol)`,
      grade_niveladora_Vi = x$`Grade Niveladora (40 a 44 discos - 20 a 22 pol)`,
      carreta_Vi = x$`Carreta (4 a 6 toneladas - 4 rodas)`,
      matabroto_Vi = x$`Matabroto Hidráulico EBT M (tamanho 2.75 m)`,
      conj_hidr_diant_Vi = x$`Conjunto hidráulico dianteiro (Concha para trator - 1500 a 1800 kg)`,
      
      trator75 = x$`Trator 75 cv - Valor (R$/cv)`,
      trator180 = x$`Trator 180 cv - Valor (R$/cv)`,
      trator65 = x$`Trator 65 cv - Valor (R$/cv)`,
      trator140 = x$`Trator 140 cv - Valor (R$/cv)`,
      
      trator75_Sp = x$`Valor de sucata - Trator 75 cv`,
      trator180_Sp = x$`Valor de sucata - Trator 180 cv`,
      trator65_Sp = x$`Valor de sucata - Trator 65 cv`,
      trator140_Sp = x$`Valor de sucata - Trator 140 cv`,
      distribuidor_semente_Sp = x$`Valor_de_Sucata_INV(3)`,
      distribuidor_calcario_Sp = x$`Valor_de_Sucata_INV(4)`,
      terracador_Sp = x$`Valor_de_Sucata_INV(5)`,
      pulverizador_Sp = x$`Valor_de_Sucata_INV(6)`,
      grade_aradora_Sp = x$`Valor_de_Sucata_INV(7) - Grade aradora (20 a 28 discos - 26 a 28 pol)`,
      grade_niveladora_Sp = x$`Valor_de_Sucata_INV(8)`,
      carreta_Sp = x$`Valor_de_Sucata_INV(9)`,
      matabroto_Sp = x$`Valor_de_Sucata_INV(10)`,
      conj_hidr_diant_Sp = x$`Valor_de_Sucata_INV(11)`,
      barracao_Sp = x$`Valor_de_Sucata_INV(12) - barracão (600 m²)`,
      
      trator75_Vu = x$`Vida útil - Trator 75 cv`,
      trator180_Vu = x$`Vida útil - Trator 180 cv`,
      trator65_Vu = x$`Vida útil - Trator 65 cv`,
      trator140_Vu = x$`Vida útil - Trator 140 cv`,
      distribuidor_semente_Vu = x$`Vida_Util_INV(3)`,
      distribuidor_calcario_Vu = x$`Vida_Util_INV(4)`,
      terracador_Vu = x$`Vida_Util_INV(5)`,
      pulverizador_Vu = x$`Vida_Util_INV(6)`,
      grade_aradora_Vu = x$`Vida_Util_INV(7) - Grade aradora (20 a 28 discos - 26 a 28 pol)`,
      grade_niveladora_Vu = x$`Vida_Util_INV(8)`,
      carreta_Vu = x$`Vida_Util_INV(9)`,
      matabroto_Vu = x$`Vida_Util_INV(10)`,
      conj_hidr_diant_Vu = x$`Vida_Util_INV(11)`,
      barracao_Vu = x$`Vida_Util_INV(12)`,
      
      price_m2 = x$`Preço do m²`
    )
  )
  
  ## Produção vegetal ----------------------------------------------------------
  producao_vegetal <- producao_vegetal(
    
    productive_unit = ambiente_de_producao$productive_unit,
    
    soil_preparation = soil_preparation(
      destoca = x$`Preparação do Solo - Unidade - Destoca`,
      topografia = x$`Preparação do Solo - Unidade - Topografia`,
      terraceamento = x$`Preparação do Solo - Unidade - Terraceamento`,
      carreador = x$`Preparação do Solo - Unidade - Adequação de carreadores`,
      destruicao_quimica = x$`Preparação do Solo - Unidade - Destruição química`,
      distrib_calcario = x$`Preparação do Solo - Unidade - Distribuição de calcário`,
      carreg_calcario = x$`Preparação do Solo - Unidade - Carregamento de calcário`,
      p_destoca = x$`Preparação do Solo - Preço Unitário - Destoca`,
      p_topografia = x$`Preparação do Solo - Preço Unitário - Topografia`,
      p_terraceamento = x$`Preparação do Solo - Preço Unitário - Terraceamento`,
      p_carreador = x$`Preparação do Solo - Preço Unitário - Adequação de carreadores`,
      p_destruicao_quimica = x$`Preparação do Solo - Preço Unitário - Destruição química`,
      p_distrib_calcario = x$`Preparação do Solo - Preço Unitário - Distribuição de calcário`,
      p_carreg_calcario = x$`Preparação do Solo - Preço Unitário - Carregamento de calcário`
    ),
    
    pasture_formation = pasture_formation(
      gradagem = x$`Formação de Pasto - Unidade - Gradagem`,
      aracao = x$`Formação de Pasto - Unidade - Aração`,
      niveladora = x$`Formação de Pasto - Unidade - Niveladora`,
      semeacao_adubacao = x$`Formação de Pasto - Unidade - Semeação/adubação`,
      aplicacao_herbicida = x$`Formação de Pasto - Unidade - Aplicação de herbicida`,
      fertilizante_Bni = x$`Formação de PastoBni - Unidade - Fertilizante - ureia`,
      fertilizante_Mni = x$`Formação de PastoMni - Unidade - Fertilizante - ureia`,
      fertilizante_Ani = x$`Formação de PastoAni - Unidade - Fertilizante - ureia`,
      semente = x$`Formação de Pasto - Unidade - Semente`,
      calcario_dolomitico = x$`Formação de Pasto - Unidade - Calcário dolomítico`,
      herbicida_oleo_mineral = x$`Formação de Pasto - Unidade - Herbicida Tordon + óleo mineral`,
      p_gradagem = x$`Formação de Pasto - Preço Unitário - Gradagem`,
      p_aracao = x$`Formação de Pasto - Preço Unitário - Aração`,
      p_niveladora = x$`Formação de Pasto - Preço Unitário - Niveladora`,
      p_semeacao_adubacao = x$`Formação de Pasto - Preço Unitário - Semeação/adubação`,
      p_aplicacao_herbicida = x$`Formação de Pasto - Preço Unitário - Aplicação de herbicida`,
      p_semente = x$`Formação de Pasto - Preço Unitário - Semente`,
      p_calcario_dolomitico = x$`Formação de Pasto - Preço Unitário - Calcário dolomítico`,
      p_fertilizante = x$`Formação de Pasto - Preço Unitário - Fertilizante - ureia`,
      p_herbicida_oleo_mineral = x$`Formação de Pasto - Preço Unitário - Herbicida Tordon + óleo mineral`,
      vida_util_pasto = x$`Formação de Pasto - Vida útil da pasto (em anos)`
    ),
    
    pasture_maintenance = pasture_maintenance(
      distribuicao_calcario = x$`Manutenção de Pasto - Unidade - Distribuição de calcário`,
      gradagem = x$`Manutenção de Pasto - Unidade - Gradagem`,
      distribuicao_fertilizante = x$`Manutenção de Pasto - Unidade - Distribuição de fertilizante`,
      aplicacao_herbicida = x$`Manutenção de Pasto - Unidade - Aplicação de herbicida`,
      fertilizante_Bni = x$`Manutenção de PastoBni - Unidade - Fertilizante - ureia`,
      fertilizante_Mni = x$`Manutenção de PastoMni - Unidade - Fertilizante - ureia`,
      fertilizante_Ani = x$`Manutenção de PastoAni - Unidade - Fertilizante - ureia`,
      calcario = x$`Manutenção de Pasto - Unidade - Calcario`,
      herbicida_oleo = x$`Manutenção de Pasto - Unidade - herbicida tordon + óleo`,
      p_distribuicao_calcario = x$`Manutenção de Pasto - Preço Unitário - Distribuição de calcário`,
      p_gradagem = x$`Manutenção de Pasto - Preço Unitário - Gradagem`,
      p_distribuicao_fertilizante = x$`Manutenção de Pasto - Preço Unitário - Distribuição de fertilizante`,
      p_aplicacao_herbicida = x$`Manutenção de Pasto - Preço Unitário - Aplicação de herbicida`,
      p_fertilizante = x$`Formação de Pasto - Preço Unitário - Fertilizante - ureia`,
      p_calcario = x$`Manutenção de Pasto - Preço Unitário - Calcario`,
      p_herbicida_oleo = x$`Manutenção de Pasto - Preço Unitário - herbicida tordon + óleo`
    )
  )
  
  ## Produção animal -----------------------------------------------------------
  producao_animal <- producao_animal(
    
    productive_unit = ambiente_de_producao$productive_unit,
    
    pesos = pesos(
      W1 = x$`Peso de venda da vaca gorda - kg - Vaca (37 a 48 meses)`,
      W2 = x$`Peso de venda da bezerro - Desmama (7 a 8 meses)`,
      W3 = x$`Peso de venda da bezerra - Desmama (7 a 8 meses)`,
      W4 = x$`Peso de venda da bezerro - (12 meses)`,
      W5 = x$`Peso de venda da bezerra - (12 meses)`,
      W6 = x$`Peso de venda da novilha gorda (18 a 20 meses)`,
      W7 = x$`Peso de venda da novilha gorda (21 a 28 meses)`,
      W8 = x$`Peso de venda da novilha gorda (29 a 36 meses)`,
      W9 = x$`Peso de venda do Garrote (18 meses)`,
      W10 = x$`Peso de venda do boi magro (12 a 13 arrobas)`,
      W11 = x$`Peso de venda do boi gordo (18 a 20 meses - dente de leite - DL)`,
      W12 = x$`Peso de venda do boi gordo (21 a 28 meses - até dois dentes permanentes)`,
      W13 = x$`Peso de venda do boi gordo (29 a 36 meses - até quatro dentes permanentes)`,
      W14 = x$`Peso de venda do boi gordo (37 a 48 meses - adulto)`,
      W15 = x$`Peso de venda do boi gordo toruno (acima de 60 meses)`,
      
      wF6 = x$`Peso inicial da vaca gorda - kg - Vaca (37 a 48 meses)`,
      wF5 = x$`Peso inicial da novilha gorda (29 a 36 meses)`,
      wF4 = x$`Peso inicial da novilha gorda (21 a 28 meses)`,
      wF3 = x$`Peso inicial da novilha gorda (18 a 20 meses)`,
      wF2 = x$`Peso inicial da bezerra - (12 meses)`,
      wF1 = x$`Peso inicial da bezerra - Desmama (7 a 8 meses)`,
      wM1 = x$`Peso inicial da bezerro - Desmama (7 a 8 meses)`,
      wM2 = x$`Peso inicial da bezerro - (12 meses)`,
      wM3g = x$`Peso inicial do Garrote (18 meses)`,
      wM3bm = x$`Peso inicial do boi magro (12 a 13 arrobas)`,
      wM3 = x$`Peso inicial do boi gordo (18 a 20 meses - dente de leite - DL)`,
      wM4 = x$`Peso inicial do boi gordo (21 a 28 meses - até dois dentes permanentes)`,
      wM5 = x$`Peso inicial do boi gordo (29 a 36 meses - até quatro dentes permanentes)`,
      wM6 = x$`Peso inicial do boi gordo (37 a 48 meses - adulto)`,
      wM6bgt = x$`Peso inicial do boi gordo toruno (acima de 60 meses)`,
      wM6t = x$`Peso inicial do touro`,
      
      WAqF6 = x$`Peso comercial (aquisição/reposição) - Vaca (37 a 48 meses)`,
      WAqF5 = x$`Peso comercial (aquisição/reposição) - novilha gorda (29 a 36 meses)`,
      WAqF4 = x$`Peso comercial (aquisição/reposição) - novilha gorda (21 a 28 meses)`,
      WAqF3 = x$`Peso comercial (aquisição/reposição) - novilha gorda (18 a 20 meses)`,
      WAqF2 = x$`Peso comercial (aquisição/reposição) - bezerra`,
      WAqF1 = x$`Peso comercial (aquisição/reposição) - bezerra - Desmama`,
      WAqM1 = x$`Peso comercial (aquisição/reposição) - bezerro - Desmama`,
      WAqM2 = x$`Peso comercial (aquisição/reposição) - bezerro`,
      WAqM3g = x$`Peso comercial (aquisição/reposição) - Garrote`,
      WAqM6t = x$`Peso comercial (aquisição/reposição) - touro`
    ),
    
    rendimentos = rendimentos(
      R1 = x$`Rendimento de carcaça (kg/kg) - Peso inicial da vaca gorda - kg - Vaca (37 a 48 meses)`,
      R6 = x$`Rendimento de carcaça (kg/kg) - Peso inicial da novilha gorda (18 a 20 meses)`,
      R7 = x$`Rendimento de carcaça (kg/kg) - Peso inicial da novilha gorda (21 a 28 meses)`,
      R8 = x$`Rendimento de carcaça (kg/kg) - Peso inicial da novilha gorda (29 a 36 meses)`,
      R9 = x$`Rendimento de carcaça (kg/kg) - Peso inicial Garrote (18 meses)`,
      R10 = x$`Rendimento de carcaça (kg/kg) - Peso inicial do boi magro (12 a 13 arrobas)`,
      R11 = x$`Rendimento de carcaça (kg/kg) - Peso inicial do boi gordo (18 a 20 meses - dente de leite - DL)`,
      R12 = x$`Rendimento de carcaça (kg/kg) - Peso inicial do boi gordo (21 a 28 meses - até dois dentes permanentes)`,
      R13 = x$`Rendimento de carcaça (kg/kg) - Peso inicial do boi gordo (29 a 36 meses - até quatro dentes permanentes)`,
      R14 = x$`Rendimento de carcaça (kg/kg) - Peso inicial do boi gordo (37 a 48 meses - adulto)`,
      R15 = x$`Rendimento de carcaça (kg/kg) - Peso inicial do gordo toruno (acima de 60 meses)`
    ),
    
    pressao = pressao(
      Ps1 = x$`Pressão de seleção - vaca gorda - kg - Vaca (37 a 48 meses)`,
      Ps2 = x$`Pressão de seleção - Bezerro desmama (7 a 8 meses)`,
      Ps3 = x$`Pressão de seleção - Bezerra desmama (7 a 8 meses)`,
      Ps4 = x$`Pressão de seleção - Bezerro (12 meses)`,
      Ps5 = x$`Pressão de seleção - Bezerra (12 meses)`,
      Ps6 = x$`Pressão de seleção - novilha gorda (18 a 20 meses)`,
      Ps7 = x$`Pressão de seleção - novilha gorda (21 a 28 meses)`,
      Ps8 = x$`Pressão de seleção - novilha gorda (29 a 36 meses)`,
      Ps9 = x$`Pressão de seleção - Garrote (18 meses)`,
      Ps10 = x$`Pressão de seleção - Boi magro (12 a 13 arrobas)`,
      Ps11 = x$`Pressão de seleção - Boi gordo (18 a 20 meses) - DL`,
      Ps12 = x$`Pressão de seleção - Boi gordo (21 a 28 meses) até 2 dentes permanentes`,
      Ps13 = x$`Pressão de seleção - Boi gordo (29 a 36 meses) até 4 dentes permanentes`,
      Ps14 = x$`Pressão de seleção - Boi gordo (37 a 48 meses)`,
      Ps15 = x$`Pressão de seleção - Boi gordo toruno (acima de 60 meses)`
    ),
    
    coeficientes = coeficientes(
      UAF6 = x$`UA/Cab - vaca gorda - kg - Vaca (37 a 48 meses)`,
      UAF5 = x$`UA/Cab - novilha gorda (29 a 36 meses)`,
      UAF4 = x$`UA/Cab - novilha gorda (21 a 28 meses)`,
      UAF3 = x$`UA/Cab - novilha gorda (18 a 20 meses)`,
      UAF2 = x$`UA/Cab - bezerra - (12 meses)`,
      UAF1 = x$`UA/Cab - bezerra - Desmama (7 a 8 meses)`,
      UAM1 = x$`UA/Cab - Bezerro desmama (7 a 8 meses)`,
      UAM2 = x$`UA/Cab - Bezerro (12 meses)`,
      UAM3g = x$`UA/Cab - Garrote (18 meses)`,
      UAM3bm = x$`UA/Cab - Boi magro (12 a 13 arrobas)`,
      UAM3 = x$`UA/Cab - Boi gordo (18 a 20 meses) - DL`,
      UAM4 = x$`UA/Cab - Boi gordo (21 a 28 meses) até 2 dentes permanentes `,
      UAM5 = x$`UA/Cab - Boi gordo (29 a 36 meses) até 4 dentes permanentes `,
      UAM6 = x$`UA/Cab - Boi gordo (37 a 48 meses)`,
      UAM6bgt = x$`UA/Cab - Boi gordo toruno (acima de 60 meses)`,
      UAM6t = x$`UA/Cab - touro`
    ),
    
    aquisicao = aquisicao(
      AqF6 = x$`Número (compra) de Vacas (37 a 48 meses)`,
      AqF5 = x$`Aquisição - novilha gorda (29 a 36 meses)`,
      AqF4 = x$`Aquisição - novilha gorda (21 a 28 meses)`,
      AqF3 = x$`Aquisição - novilha gorda (18 a 20 meses)`,
      AqF2 = x$`Aquisição - bezerra - (12 meses)`,
      AqF1 = x$`Aquisição - bezerra - Desmama (7 a 8 meses)`,
      AqM1 = x$`Aquisição - Peso inicial da bezerro - Desmama (7 a 8 meses)`,
      AqM2 = x$`Aquisição - Peso inicial da bezerro - (12 meses)`,
      AqM3g = x$`Aquisição - Peso inicial do Garrote (18 meses)`,
      AqM3bm = x$`Aquisição - Peso inicial do boi magro (12 a 13 arrobas)`,
      AqM3 = x$`Aquisição - Peso inicial do boi gordo (18 a 20 meses - dente de leite - DL)`,
      AqM4 = x$`Aquisição - boi gordo (21 a 28 meses - até dois dentes permanentes)`,
      AqM5 = x$`Aquisição - boi gordo (29 a 36 meses - até quatro dentes permanentes)`,
      AqM6 = x$`Aquisição - boi gordo (37 a 48 meses - adulto)`,
      AqM6bgt = x$`Aquisição - boi gordo toruno (acima de 60 meses)`,
      AqM6t = x$`Aquisição - touro`,
      AqAcomp = x$`Vaca Acompanhada`
    ),
    
    premio = premio(
      Pp6 = x$`Prêmio (carcaça) da novilha gorda (18 a 20 meses)`,
      Pp7 = x$`Prêmio (carcaça) da novilha gorda (21 a 28 meses)`,
      Pp8 = x$`Prêmio (carcaça) da novilha gorda (29 a 36 meses)`,
      Pp11 = x$`Prêmio (carcaça) do boi gordo (18 a 20 meses - dente de leite - DL)`,
      Pp12 = x$`Prêmio (carcaça) do boi gordo (21 a 28 meses - até dois dentes permanentes)`,
      Pp13 = x$`Prêmio (carcaça) do boi gordo (29 a 36 meses - até quatro dentes permanentes)`,
      Pp14 = x$`Prêmio (carcaça) do boi gordo (37 a 48 meses - adulto)`
    ),
    
    mortalidade = mortalidade(
      Mt1 = x$`Mortalidade até a desmama`,
      Mt2 = x$`Mortalidade até 12 meses`,
      Mt3 = x$`Mortalidade até 18-20 meses`,
      Mt4 = x$`Mortalidade até  21-28 meses`,
      Mt5 = x$`Mortalidade até 29-36 meses`,
      Mt6 = x$`Mortalidade até 37-48 meses`,
      Mt_ini = x$`UA/Cab - Valor Inicial - Mortalidade Nascido`
    ),
    
    ciclo_curto = ciclo_curto(
      ccF3Wo = x$`Pecuária de ciclo curto - Peso na saída (kg) - novilha gorda (18 a 20 meses)`,
      ccF3dg = x$`Pecuária de ciclo curto - Ganho diário (kg) - novilha gorda (18 a 20 meses)`,
      ccF3pf = x$`C7 - Coeficiente de consumo da dieta`, # consumo
      
      ccM3Wo = x$`Pecuária de ciclo curto - Peso na saída (kg) - Boi gordo (18 a 20 meses) - DL`,
      ccM3dg = x$`Pecuária de ciclo curto - Ganho diário (kg) - Boi gordo (18 a 20 meses) - DL`,
      ccM3pf = x$`C7 - Coeficiente de consumo da dieta`, # consumo
      
      ccM4Wo = x$`Pecuária de ciclo curto - Peso na saída (kg) - Boi gordo (21 a 28 meses)`,
      ccM4dg = x$`Pecuária de ciclo curto - Ganho diário (kg) - Boi gordo (21 a 28 meses)`,
      ccM4pf = x$`C7 - Coeficiente de consumo da dieta`, # consumo
      
      ccM5Wo = x$`Pecuária de ciclo curto - Peso na saída (kg) - Boi gordo (29 a 36 meses)`,
      ccM5dg = x$`Pecuária de ciclo curto - Ganho diário (kg) - Boi gordo (29 a 36 meses)`,
      ccM5pf = x$`C7 - Coeficiente de consumo da dieta` # consumo
    ),
    
    referencias_zootecnicas = referencias_zootecnicas(
      Rs1 = x$`Relação entre touro e vaca`,
      Rp2 = x$`Reposição de vacas`,
      Rp3 = x$`Reposição de touros`,
      Tn = ifelse(
        nivel_intensificacao == 1,
        x$`Taxa de natalidade Ani`,
        ifelse(nivel_intensificacao == 2,
               x$`Taxa de natalidade Mni`,
               ifelse(nivel_intensificacao == 3,
                      x$`Taxa de natalidade Bni`, NA)))
    ),
    
    consumo = consumo(
      C1 = x$`C1 - Consumo diário nutricional`,
      C2 = x$`C2 - Consumo diário nutricional`,
      C3 = x$`C3 - Consumo diário nutricional`,
      C4 = x$`C4 - Consumo diário nutricional`,
      C5 = x$`C5 - Consumo diário nutricional`,
      Tc6 = x$`Tc6 - trava de consumo`,
      C7 = x$`C7 - Coeficiente de consumo da dieta`, # consumo
      creep_days = x$`Dias de confinamento de creep feeding`
    ),
    
    vacinas = vacinas(
      vacaftosa_F6 = x$`VacAftosa_F(6)`,
      vacaftosa_F5 = x$`VacAftosa_F(5)`,
      vacaftosa_F4 = x$`VacAftosa_F(4)`,
      vacaftosa_F3 = x$`VacAftosa_F(3)`,
      vacaftosa_F2 = x$`VacAftosa_F(2)`,
      vacaftosa_F1 = x$`VacAftosa_F(1)`,
      vacaftosa_M1 = x$`VacAftosa_M(1)`,
      vacaftosa_M2 = x$`VacAftosa_M(2)`,
      vacaftosa_M3g = x$`VacAftosa_Mg(3)`,
      vacaftosa_M3 = x$`VacAftosa_M(3)`,
      vacaftosa_M3bm = x$`VacAftosa_Mbm(3)`,
      vacaftosa_M4 = x$`VacAftosa_M(4)`,
      vacaftosa_M5 = x$`VacAftosa_M(5)`,
      vacaftosa_M6 = x$`VacAftosa_M(6)`,
      vacaftosa_M6bgt = x$`VacAftosa_Mbgt(6)`,
      vacaftosa_M6t = x$`VacAftosa_Mt(6)`,
      
      vaccarb_F6 = x$`VacCarb_F(6)`,
      vaccarb_F5 = x$`VacCarb_F(5)`,
      vaccarb_F4 = x$`VacCarb_F(4)`,
      vaccarb_F3 = x$`VacCarb_F(3)`,
      vaccarb_F2 = x$`VacCarb_F(2)`,
      vaccarb_F1 = x$`VacCarb_F(1)`,
      vaccarb_M1 = x$`VacCarb_M(1)`,
      vaccarb_M2 = x$`VacCarb_M(2)`,
      vaccarb_M3g = x$`VacCarb_Mg(3)`,
      vaccarb_M3 = x$`VacCarb_M(3)`,
      vaccarb_M3bm = x$`VacCarb_Mbm(3)`,
      vaccarb_M4 = x$`VacCarb_M(4)`,
      vaccarb_M5 = x$`VacCarb_M(5)`,
      vaccarb_M6 = x$`VacCarb_M(6)`,
      vaccarb_M6bgt = x$`VacCarb_Mbgt(6)`,
      vaccarb_M6t = x$`VacCarb_Mt(6)`,
      
      vacbruc_F6 = x$`VacBruc_F(6)`,
      vacbruc_F5 = x$`VacBruc_F(5)`,
      vacbruc_F4 = x$`VacBruc_F(4)`,
      vacbruc_F3 = x$`VacBruc_F(3)`,
      vacbruc_F2 = x$`VacBruc_F(2)`,
      vacbruc_F1 = x$`VacBruc_F(1)`,
      vacbruc_M1 = x$`VacBruc_M(1)`,
      vacbruc_M2 = x$`VacBruc_M(2)`,
      vacbruc_M3g = x$`VacBruc_Mg(3)`,
      vacbruc_M3 = x$`VacBruc_M(3)`,
      vacbruc_M3bm = x$`VacBruc_Mbm(3)`,
      vacbruc_M4 = x$`VacBruc_M(4)`,
      vacbruc_M5 = x$`VacBruc_M(5)`,
      vacbruc_M6 = x$`VacBruc_M(6)`,
      vacbruc_M6bgt = x$`VacBruc_Mbgt(6)`,
      vacbruc_M6t = x$`VacBruc_Mt(6)`,
      
      vermifugo_F6 = x$`Vermifugo_F(6)`,
      vermifugo_F5 = x$`Vermifugo_F(5)`,
      vermifugo_F4 = x$`Vermifugo_F(4)`,
      vermifugo_F3 = x$`Vermifugo_F(3)`,
      vermifugo_F2 = x$`Vermifugo_F(2)`,
      vermifugo_F1 = x$`Vermifugo_F(1)`,
      vermifugo_M1 = x$`Vermifugo_M(1)`,
      vermifugo_M2 = x$`Vermifugo_M(2)`,
      vermifugo_M3g = x$`Vermifugo_Mg(3)`,
      vermifugo_M3 = x$`Vermifugo_M(3)`,
      vermifugo_M3bm = x$`Vermifugo_Mbm(3)`,
      vermifugo_M4 = x$`Vermifugo_M(4)`,
      vermifugo_M5 = x$`Vermifugo_M(5)`,
      vermifugo_M6 = x$`Vermifugo_M(6)`,
      vermifugo_M6bgt = x$`Vermifugo_Mbgt(6)`,
      vermifugo_M6t = x$`Vermifugo_Mt(6)`
    )
  )
  
  ## Econômico -----------------------------------------------------------------
  economico <- economico(
    
    remuneracao_min = remuneracao,
    
    labor = bio_labor(
      Mo1 = x$`Número da mão de obra (tratorista)`,
      Mo2 = x$`Número da mão de obra (campeiro)`,
      cMo1 = x$`Valor da mão de obra (tratorista)`,
      cMo2 = x$`Valor da mão de obra (campeiro)`,
      s13 = x$`Décimo terceiro salário`,
      vacation = x$`Férias`,
      fgts = x$FGTS,
      dsr = x$`Descanso semanal remunerado`,
      hours_m = x$`Horas Trabalhadas/mês`,
      cs = x$`Contribuição social`,
      cbasic = x$`Valor da cesta básica`,
      uniform = x$Uniforme_Qt_ano,
      uniform_cost = x$Uniforme_RS
    ),
    
    precos = precos(
      
      # comercializar
      P1 = x$`Preço R$ arroba Vaca F6`, # R$ arroba
      P2 = x$`Preço R$ kgf peso vivo Desmama M1`, # R$ kgf peso vivo
      P3 = x$`Preço R$ kgf peso vivo Desmama F1`, # R$ kgf peso vivo
      P4 = x$`Preço R$ kgf peso vivo Bezerro M2`, # R$ kgf peso vivo
      P5 = x$`Preço R$ kgf peso vivo Bezerra F2`, # R$ kgf peso vivo
      P6 = x$`Preço R$ arroba Novilha F3`, # R$ arroba
      P7 = x$`Preço R$ arroba Novilha F4`, # R$ arroba
      P8 = x$`Preço R$ arroba Novilha F5`, # R$ arroba
      P9 = x$`Preço R$ arroba Garrote M3g`, # R$ arroba
      P10 = x$`Preço R$ arroba Boi magro M3bm`, # R$ arroba
      P11 = x$`Preço R$ arroba Boi gordo M3`, # R$ arroba
      P12 = x$`Preço R$ arroba Boi gordo M4`, # R$ arroba
      P13 = x$`Preço R$ arroba Boi gordo M5`, # R$ arroba
      P14 = x$`Preço R$ arroba Boi gordo M6`, # R$ arroba
      P15 = x$`Preço R$ arroba Boi gordo M6bgt`, # R$ arroba
      
      # R$ kgf peso vivo
      PAqF1 = x$`Preço R$ kgf peso vivo Desmama F1`,
      PAqF2 = x$`Preço R$ kgf peso vivo Bezerra F2`,
      PAqF3 = x$`Preço R$ kgf peso vivo Novilha F3`,
      PAqF4 = x$`Preço R$ kgf peso vivo Novilha F4`,
      PAqF5 = x$`Preço R$ kgf peso vivo Novilha F5`,
      PAqF6 = x$`Preço R$ kgf peso vivo Vaca F6`,
      PAqM1 = x$`Preço R$ kgf peso vivo Desmama M1`,
      PAqM2 = x$`Preço R$ kgf peso vivo Bezerro M2`,
      PAqM3g = x$`Preço R$ kgf peso vivo Garrote M3g`,
      PAqM6t = x$`Preço R$ kgf peso vivo Touro M6t`,
      
      # Preço da kg do animal (valor inicial - R$) - vaca gorda - kg - Vaca (37 a 48 meses) + min(bezerro, bezerra)*0.5
      PAqAcomp = x$`Preço R$ kgf peso vivo Vaca F6` + min(
        x$`Preço R$ kgf peso vivo Bezerro M2`,
        x$`Preço R$ kgf peso vivo Bezerra F2`
      )*0.5,
      
      pC1 = x$`Preço do sal mineral`,
      pC2 = x$`Preço do sal mineral para reprodução`,
      pC3 = x$PrecoSupProt,
      pC4 = x$PrecoSupEnerg,
      pC5 = x$PrecoCreepFeeding,
      pTc6 = x$PrecoTravaConsumo,
      pCc18_20 = x$PrecoDieta1820,
      pCc21_28 = x$PrecoDieta2128,
      
      vacaftosa_unit_price = x$PrecoVacAftosa,
      vaccarb_unit_price = x$PrecoVacCarb,
      vacbruc_unit_price = x$PrecoVacBruc,
      vermifugo_unit_price = x$PrecoVermifugo
    ),
    
    general_costs = bio_general_costs(
      Dg1 = x$`Dg1 Transporte interno (quantidade)`,
      Dg11 = x$`Dg11 Transporte interno (custo unitário)`,
      Dg2 = x$`Dg2 Assistência veterinária (quantidade)`,
      Dg21 = x$`Dg21 Assistência veterinária (custo unitário)`,
      Dg3 = x$`Dg3 Despesas contabilidade (quantidade)`,
      Dg31 = x$`Dg31 Despesas contabilidade (custo unitário)`,
      Dg4 = x$`Dg4 Energia elétrica (quantidade)`,
      Dg41 = x$`Dg41 Energia elétrica (custo unitário)`
    ),
    
    outros_custos = outros_custos(
      instalacoes_maintenance = x$`Manutenção e instalações de benfeitorias (RS/ano)`,
      maquinas_maintenance = x$`Manutenção de máquinas e equipamentos (RS/ano)`,
      other_medication = x$`Outros medicamentos`,
      fuel_lubricant = x$PrecoDiesel
    ),
    
    viabilidade = viabilidade(
      VTN = x$`Valor da terra nua no município de exploração (R$/ha)`,
      GU = x$`Grau de utilização (GU) em %`,
      p_inv_socios = x$`Invest. dos sócios (% ano)`,
      p_inv_terceiro = x$`Aquisição de capital de terceiro sobre o valor do rebanho (%)`,
      parcelas_ano = x$`Total de parcelas (ano)`,
      carencia_ano = x$`Carência para pag. passivo (ano)`,
      taxa_juros_passivo = x$`Taxa de juros (a.a)`,
      taxa_desconto_real = x$`Taxa real de desconto real já expurgando a inflação`,
      taxa_MTIR = x$`Taxa Financiamento MTIR`,
      taxa_inflacao_aa = x$`Taxa de inflação (a.a)`
    )
  )
  
  ## Simula propriedade --------------------------------------------------------
  propriedade <- simula_propriedade(
    producao_animal = producao_animal,
    economico = economico
  )
  
  # Análises -------------------------------------------------------------------
  
  ## Custos --------------------------------------------------------------------
  custos <- costs(
    ambiente_de_producao = ambiente_de_producao,
    producao_vegetal = producao_vegetal,
    propriedade = propriedade,
    economico = economico
  )
  
  ## Análise de viabilidade ----------------------------------------------------
  viab <- viability(
    ambiente_de_producao = ambiente_de_producao,
    propriedade = propriedade,
    economico = economico,
    costs = custos,
    bioma = bioma
  )
  
  # Saída ----------------------------------------------------------------------
  out <- list("producao_animal" = producao_animal,
              "ambiente_de_producao" = ambiente_de_producao,
              "producao_vegetal" = producao_vegetal,
              "propriedade" = propriedade,
              "custos" = custos,
              "viabilidade" = viab,
              "economico" = economico,
              "bioma" = bioma)
  return(out)
}

#' @title Função principal do MMBC
#'
#' @param file string indicando o caminho para o arquivo csv
#' @param num_matrizes Número de matrizes
#' @param remuneracao Administração (remuneração do Produtor)
#' @param nivel_intensificacao Nível de Intensificação
#' @param bioma 
#' @param cap_sup 
#'
#' @return A list
#' @export
#'
#' @examples
MMBC <- function(file, num_matrizes, remuneracao, nivel_intensificacao,
                 bioma, cap_sup = 1.5) {
  

  # MMBC -----------------------------------------------------------------------
  mmbc <- MMBC_core(file, num_matrizes, remuneracao, nivel_intensificacao,
                    bioma, cap_sup)
    
  # Análise do Ponto de Equilíbrio ---------------------------------------------
  pe <- analysis_pe(propriedade = mmbc$propriedade, costs = mmbc$custos)
  
  # Tabelas com resultados finais (dashboard) ----------------------------------
  dashboards <- results(propriedade = mmbc$propriedade,
                        costs = mmbc$custos,
                        viab = mmbc$viabilidade,
                        ambiente_de_producao = mmbc$ambiente_de_producao,
                        bioma = mmbc$bioma)
  
  # Saída ----------------------------------------------------------------------
  out <- list("producao_animal" = mmbc$producao_animal,
              "ambiente_de_producao" = mmbc$ambiente_de_producao,
              "producao_vegetal" = mmbc$producao_vegetal,
              "propriedade" = mmbc$propriedade,
              "custos" = mmbc$custos,
              "viabilidade" = mmbc$viabilidade,
              "economico" = mmbc$economico,
              "pe" = pe,
              "dashboards" = dashboards,
              "bioma" = mmbc$bioma)
  return(out)
}

#' @title Calcula o MMBC usando busca binária
#'
#' @param path string indicando o caminho para o arquivo csv
#' @param remuneracao Administração (remuneração do Produtor)
#' @param bioma 
#' @param cap_sup 
#' @param nivel_intensificacao Nível de Intensificação
#'
#' @return A list
#' @export
#'
#' @examples
find_MMBC <- function(path, remuneracao, nivel_intensificacao,
                      bioma, cap_sup = 1.5) {
  
  # verifica capacidade de suporte  
  if (is.na(cap_sup)) return(NA)
  # verifica biomas
  if (!bioma %in% c("Mata Atlântica", "Amazônia", "Cerrado")) {
    warning("'bioma' deve ser um dos seguintes: 'Mata Atlântica', 'Amazônia', 'Cerrado'.")
    return(NA)
  }
  
  l_inf <- 10
  l_sup <- 250000
  
  # verifica se MMBC está dento do intervalo: VPL troca de sinal nos limites
  mmbc_inf <- MMBC_core(file = path,
                        num_matrizes = l_inf,
                        remuneracao = remuneracao,
                        nivel_intensificacao = nivel_intensificacao,
                        bioma = bioma,
                        cap_sup = cap_sup)$viabilidade$VPL
  mmbc_sup <- MMBC_core(file = path,
                        num_matrizes = l_sup,
                        remuneracao = remuneracao,
                        nivel_intensificacao = nivel_intensificacao,
                        bioma = bioma,
                        cap_sup = cap_sup)$viabilidade$VPL
  
  if (mmbc_inf * mmbc_sup >= 0) {
    stop(paste0("MMBC não encontrado dentro dos limites fornecidos.\n",
                "VPL: ", mmbc_inf, ", ", mmbc_sup))
  }
  
  # faz busca binária
  while (l_inf <= l_sup) {
    meio <- l_inf + (l_sup - l_inf) %/% 2
    
    mmbc_01 <- MMBC_core(file = path,
                         num_matrizes = meio,
                         remuneracao = remuneracao,
                         nivel_intensificacao = nivel_intensificacao,
                         bioma = bioma,
                         cap_sup = cap_sup)
    mmbc_02 <- MMBC_core(file = path,
                         num_matrizes = meio - 1,
                         remuneracao = remuneracao,
                         nivel_intensificacao = nivel_intensificacao,
                         bioma = bioma,
                         cap_sup = cap_sup)
    
    if (mmbc_01$viabilidade$VPL > 0 && mmbc_02$viabilidade$VPL < 0) {
      
      area_total <- ifelse(
        bioma == "Mata Atlântica", 
        mmbc_01$propriedade$productive_unit$pasture_disp / 0.8,
        ifelse(bioma == "Amazônia", 
               mmbc_01$propriedade$productive_unit$pasture_disp / 0.2,
               ifelse(bioma == "Cerrado", 
                      mmbc_01$propriedade$productive_unit$pasture_disp / 0.7,
                      NA)))
      
      message(paste0("Remuneracao minima: ", sprintf("%.2f", remuneracao), "\n",
                     "Numero de matrizes: ", meio, "\n",
                     "Area total (ha): ", sprintf("%.2f", area_total), "\n",
                     "VPL: ", sprintf("%.2f", mmbc_01$viabilidade$VPL), "\n",
                     "Payback: ", sprintf("%.2f", mmbc_01$viabilidade$payback)))
      
      out <- list("MMBC" = mmbc_01, 
                  "area_total" = area_total,
                  "num_matrizes" = meio,
                  "bioma" = bioma,
                  "cap_sup" = cap_sup)
      
      return(out)
      
    } else if (mmbc_01$viabilidade$VPL <= 0) {
      l_inf <- meio + 1
    } else {
      l_sup <- meio - 1
    }
  }
  
  stop("MMBC não encontrado dentro dos limites fornecidos.")
}

find_MMBC_seq <- function(path, remuneracao, nivel_intensificacao,
                          bioma, cap_sup = 1.5) {

  # verifica capacidade de suporte  
  if (is.na(cap_sup)) return(list("MMBC" = NA, 
                                  "area_total" = NA,
                                  "num_matrizes" = NA,
                                  "bioma" = bioma,
                                  "cap_sup" = cap_sup))
  # verifica biomas
  if (!bioma %in% c("Mata Atlântica", "Amazônia", "Cerrado")) {
    warning("'bioma' deve ser um dos seguintes: 'Mata Atlântica', 'Amazônia', 'Cerrado'.")
    return(list("MMBC" = NA, 
                "area_total" = NA,
                "num_matrizes" = NA,
                "bioma" = bioma,
                "cap_sup" = cap_sup))
  }
  
  seq_points <- c(10, 100, 200, 500, 1000, 2000, 5000, 7000,
                  10000, 25000, 50000, 75000, 100000, 250000)
  
  # Vetores para armazenar os valores de num_matrizes e VPL
  num_matrizes_values <- c()
  vpl_values <- c()
  
  # Faz busca sequencial
  for (i in seq_along(seq_points)) {
    num_matrizes <- seq_points[i]
    
    mmbc_01 <- MMBC_core(file = path,
                         num_matrizes = num_matrizes,
                         remuneracao = remuneracao,
                         nivel_intensificacao = nivel_intensificacao,
                         bioma = bioma,
                         cap_sup = cap_sup)
    
    vpl_01 <- mmbc_01$viabilidade$VPL
    
    num_matrizes_values <- c(num_matrizes_values, num_matrizes)
    vpl_values <- c(vpl_values, vpl_01)
    
    if (i > 1) {
      
      mmbc_02 <- MMBC_core(file = path,
                           num_matrizes = seq_points[i - 1],
                           remuneracao = remuneracao,
                           nivel_intensificacao = nivel_intensificacao,
                           bioma = bioma,
                           cap_sup = cap_sup)
      
      vpl_02 <- mmbc_02$viabilidade$VPL
    
      # Mensagens de depuração
      # cat("Busca sequencial\n")
      # cat("l_inf:", seq_points[i - 1], "l_sup:", num_matrizes, "\n")
      # cat("VPL inf:", vpl_02, "VPL sup:", vpl_01, "\n")
    
      if (vpl_01 > 0 && vpl_02 < 0) {
        # Encontrou mudança de sinal, agora faz busca binária
        l_inf_bin <- seq_points[i - 1]
        l_sup_bin <- seq_points[i]
        
        while (l_inf_bin <= l_sup_bin) {
          meio <- l_inf_bin + (l_sup_bin - l_inf_bin) %/% 2
          
          mmbc_meio <- MMBC_core(file = path,
                                 num_matrizes = meio,
                                 remuneracao = remuneracao,
                                 nivel_intensificacao = nivel_intensificacao,
                                 bioma = bioma,
                                 cap_sup = cap_sup)
          mmbc_meio_menos1 <- MMBC_core(file = path,
                                        num_matrizes = meio - 1,
                                        remuneracao = remuneracao,
                                        nivel_intensificacao = nivel_intensificacao,
                                        bioma = bioma,
                                        cap_sup = cap_sup)
          
          vpl_meio <- mmbc_meio$viabilidade$VPL
          vpl_meio_menos1 <- mmbc_meio_menos1$viabilidade$VPL
          
          # Mensagens de depuração
          # cat("Busca binária\n")
          # cat("l_inf_bin:", l_inf_bin, "l_sup_bin:", l_sup_bin, "meio:", meio, "\n")
          # cat("VPL meio:", vpl_meio, "VPL meio-1:", vpl_meio_menos1, "\n")
          
          num_matrizes_values <- c(num_matrizes_values, meio)
          vpl_values <- c(vpl_values, vpl_meio)
          
          if (vpl_meio > 0 && vpl_meio_menos1 < 0) {
            
            area_total <- switch(bioma,
                                 "Mata Atlântica" = mmbc_meio$propriedade$productive_unit$pasture_disp / 0.8,
                                 "Amazônia" = mmbc_meio$propriedade$productive_unit$pasture_disp / 0.2,
                                 "Cerrado" = mmbc_meio$propriedade$productive_unit$pasture_disp / 0.7,
                                 NA)
            
            # message(paste0("Remuneração mínima: ", sprintf("%.2f", remuneracao), "\n",
            #                "Número de matrizes: ", meio, "\n",
            #                "Área total (ha): ", sprintf("%.2f", area_total), "\n",
            #                "VPL: ", sprintf("%.2f", mmbc_meio$viabilidade$VPL), "\n",
            #                "Payback: ", sprintf("%.2f", mmbc_meio$viabilidade$payback)))
            
            # plot(num_matrizes_values, vpl_values, type = "b", col = "blue",
            #      xlab = "Número de Matrizes", ylab = "VPL", main = "VPL vs Número de Matrizes")
            # abline(h = 0, lty = 1, col = "darkgray")
            # abline(v = meio, lty = 2, col = "red")
            
            out <- list("MMBC" = mmbc_meio, 
                        "area_total" = area_total,
                        "num_matrizes" = meio,
                        "bioma" = bioma,
                        "cap_sup" = cap_sup)
            
            return(out)
            
          } else if (vpl_meio <= 0) {
            l_inf_bin <- meio + 1
          } else {
            l_sup_bin <- meio - 1
          }
        }
      }
    }
  }
  
  warning("MMBC não encontrado dentro dos limites fornecidos. Retornando NA")
  return(list("MMBC" = NA, 
              "area_total" = NA,
              "num_matrizes" = NA,
              "bioma" = bioma,
              "cap_sup" = cap_sup))
}
