# Módulo mínimo da bovinocultura de corte em pasto no Brasil

Este repositório contém o código-fonte para a definição do módulo mínimo de exploração da bovinocultura de corte em pasto.

O modelo híbrido de simulação de Módulo Mínimo (MHSMM) apresenta uma arquitetura modular estruturada em quatro submodelos distintos, que são:

1. *Módulo 1: Ambiente de Produção*
   - Este módulo aborda as condições ambientais e os recursos disponíveis, fundamentais para a produção.
     
2. *Módulo 2: Produção Vegetal (Pasto)*
   - Focado na produção de pastagens, este submodelo simula o crescimento e a produtividade das forrageiras, considerando diferentes cenários de manejo e fertilização.

3. *Módulo 3: Produção Animal (Desempenho)*
   - Este módulo simula o desempenho dos animais, incluindo ganho de peso, taxas de crescimento e outras métricas zootécnicas relevantes.

4. *Módulo 4: Econômico (Viabilidade)*
   - Focado na análise econômica, este submodelo avalia a viabilidade financeira da exploração, considerando custos, receitas e indicadores de rentabilidade.

     ![image](https://github.com/user-attachments/assets/bf083bc0-bda3-4b48-97a9-1b95c4714cfc)


A integração desses submodelos permite uma análise abrangente e detalhada das diversas dimensões da bovinocultura de corte em pasto, proporcionando informações essenciais para a tomada de decisão e o planejamento estratégico.

Diagrama simplificado da modelagem do fluxo de dados de saída. Bovinocultura de corte de ciclo completo em pasto. Modelo Híbrido de Simulação Módulo Mínimo (MHSMM). Brasil.
 
 ![image](https://github.com/user-attachments/assets/b39c716f-f66b-4ea7-8d42-8b50cde28aae)



## Exemplo

Conferir o arquivo [main.R](R/main.R).
