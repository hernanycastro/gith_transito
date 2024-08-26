# gith_transito
Controle de versionamento do projeto "Transito Pop Rua"

INTRODUÇÃO 
O estudo explora a variação quantitativa de pessoas em situação de rua (PSR) nas regiões administrativas (RA) do Distrito Federal (DF) no ano 2023. 

PROBLEMA 
O problema proposto é sobre quais efeitos as políticas públicas e outras ações do Estado geram sobre a variação quantitativa de PSR. 

OBJETIVO 
O objetivo consiste em identificar os possíveis efeitos das políticas públicas, mapeando cada uma das ações em particular. Utilizou-se o método de Análise de Componentes Principais para analisar os dados, além do software requalify de Inteligência Artificial. 

RESULTADOS 
1) o grupo de renda média alta apresenta maior intolerância do que os demais, 2) o grupo de média alta renda apresenta maior intolerância do que os de média baixa e baixa renda, embora menor do que o de alta renda, como maior rejeição em relação aos demais; 3) o grupo de média baixa renda apresenta maior tolerância e menor rejeição do que os de alta e média alta renda, embora menor tolerância e maior rejeição do que o grupo de baixa renda; 4) o grupo de renda baixa apresenta maior tolerância e menor rejeição do que os demais. Os resultados sugerem que, 1) a posição das regiões administrativas no plano teórico da variação quantitativa das pessoas em situação de rua difere segundo a renda da população, 2) rendas mais altas convergem com o aumento da intolerância e da rejeição em relação à PSR, 3) rendas mais baixas convergem com o aumento da tolerância e redução da rejeição em relação à PSR.

REQUISITOS DO SISTEMA
O estudo foi realizado em linguagem R, versão 4.4.1, e software RStudio, com os pacotes tidyverse, dplyr, tidyr, haven, janitor, readxl, magrittr, psych, rmarkdown, tidymodels, FactoMineR e factoextra. O ambiente de trabalho foi configurado em sistema operacional Windows 11.

CONJUNTO DE DADOS
A maior parte dos dados foi obtida a partir de solicitações ao Governo do Distrito Federal, mediante Lei de Acesso à Informação (LAI), conforme a seguir:
DISTRITO FEDERAL, Secretaria de Estado de Justiça e Cidadania do. Pedido de acesso à informação registrado no Sistema Participa/DF (Despacho SUBED sei nº 133467000). Subsecretaria de Enfrentamento às Drogas (Subed), processo sei nº 00400-00009385/2024-71. Brasília, 15 de fevereiro de 2024.

DISTRITO FEDERAL, Secretaria de Estado de Saúde do. Solicitação – Lei de acesso à informação (Despacho GASPVP sei nº 133892202). Gerência de Atenção à Saúde de Populações em Situação Vulnerável e Programas Especiais (GASPVP), processo sei nº 00060-00077596/2024-06. Brasília, 20 de fevereiro de 2024.

DISTRITO FEDERAL, Controladoria-Geral do. Resposta LAI-002634/2024 (Despacho Diouv sei nº 134559789). Diretoria de Inteolgência em Ouvidoria, processo sei nº 00480-00000671/2024-82. Brasília, 28 de fevereiro de 2024.

DISTRITO FEDERAL, Secretaria de Estado de Desenvolvimento Social do. Lei de acesso à informação – LAI-003125 (Despacho GESEAS sei nº 134989690). Gerência de Serviço Especializados em Abordagem Social (GESEAS), processo sei nº 00431-00002984/2024-23. Brasília, 04 de março de 2024.

DISTRITO FEDERAL, Controladoria-Geral do. Recurso pedido de acesso à informação (Despacho Cigouv sei nº 135152942). Coordenação de Inovação e Governança em Ouvidoria, processo sei nº 00480-00000671/2024-82. Brasília, 06 de março de 2024.

DISTRITO FEDERAL, Polícia Civil do. Documento nº 76/2024 (Protocolo 241400/2024. Seção de Estatística, processo sei nº 00052-00003696/2024-79. Brasília, 09 de março de 2024.

DISTRITO FEDERAL, Secretaria de Estado de Desenvolvimento Social do. Solicitação de acesso à informação – LAI-003904/2024 (Despacho SUBSAS sei nº 136182343). Subsecretaria de Assistência Social (SUBSAS), processo sei nº 00431-00003899/2024-82. Brasília, 18 de março de 2024.

DISTRITO FEDERAL, Controladoria-Geral do. Recurso (Despacho Cigouv sei nº 136131869). Coordenação de Inovação e Governança em Ouvidoria, processo sei nº 00480-00000671/2024-82. Brasília, 18 de março de 2024.

IPEDF, Instituto de Pesquisa e Estatística do Distrito Federal. Informe demográfico: censo 2022 – população no território. IPEDF. Brasília, 21 de março de 2024.

DISTRITO FEDERAL, Secretaria de Estado de Desenvolvimento Social do. Solicitação de acesso à informação – LAI-005865/2024 (Despacho GESEAS sei nº 138416204). Gerência de Serviço Especializados em Abordagem Social (GESEAS), processo sei nº 00431-00006676/2024-77. Brasília, 23 de abril de 2024.

DISTRITO FEDERAL, Secretaria de Estado de Desenvolvimento Social do. Resposta à solicitação de dados para pesquisa (Despacho Unibs sei nº 139249694). Unidade de Benefícios Socioassistenciais (Unibs), processo sei nº 00431-00007847/2024-85. Brasília, 24 de abril de 2024.

DISTRITO FEDERAL, Secretaria de Estado de Saúde do. Dados sobre óbito (Despacho GIASS sei nº 140088782). Gerência de Informação e Análise de Situação em Saúde (GIASS), processo sei nº 00060-00230495/2024-61. Brasília, 05 de maio de 2024.

DISTRITO FEDERAL, Polícia Civil do. Informativo criminal nº 69/2024 – DATE: Vítimas fatais que se encontravam em situação de rua (Protocolo 1099550/2024 - DATE. Seção de Análise Técnica, processo sei nº 00052-00015049/2024-18. Brasília, 15 de maio de 2024.

DISTRITO FEDERAL, Secretaria de Estado de Desenvolvimento Social do. Lei de acesso à informação – Refeições no Centro pop Brasília (LAI-009315/2024, Despacho SUBSAN sei nº 141845315). Subsecretaria de Segurança Alimentar e Nutricional (SUBSAN), processo sei nº 00431-00010985/2024-41. Brasília, 24 de maio de 2024.

DISTRITO FEDERAL, Secretaria de Estado de Desenvolvimento Social do. Solicitação de acesso à informação – LAI-009650/2024 (Despacho CTRAR sei nº 143352067). Coordenação de Gestão de Transferência de Renda e Cadastro Único (CTRAR), processo sei nº 00431-00011534/2024-21. Brasília, 13 de junho de 2024.

Outra parte dos dados foi obtida mediante consulta aos sites de entidades ou órgãos responsáveis pelas ações, conforme a seguir:
SLU, Serviço de Limpeza Urbana do Distrito Federal. Relatório anual 2023. Brasília, 2023. In: < https://www.slu.df.gov.br/wp-content/uploads/2024/04/RELATORIO-ANUAL-SLU-2023.pdf> 26/08/2024, 10h

IPES, Instituto. Relatório de execução do objeto (anual) sobre a execução anual do objeto de termo de colaboração 04/2017. Brasília, 2023. In: < https://gerenciador.institutoipes.org.br/wp-content/uploads/2024/04/Relatorio-Anual-de-execucao-do-objeto-de-2023-SEAS.pdf> 26/08/2024, 15h

FORMATO E PRÉ-PROCESSAMENTO DOS DADOS
Os dados estão organizados em arquivos "xlsx", como pré-processados para importação para ambiente R, conforme a seguir:
populacao_ibge <- readxl::read_xlsx("populacao_df_ra.xlsx") #População RA's (Censo IBGE 2022)
densidade_ipedf <- readxl::read_xlsx("densidade_ra.xlsx") #Densidade RA's (IPEDF/Censo IBGE 2022)
pontos <- readxl::read_xlsx("as_seas_pontos.xlsx") #Concentração de Pop Rua (SEDES/LAI)
saude_ecr <- readxl::read_xlsx("saude_ecr.xlsx") #Consultório na Rua (SES/LAI)
bo <- readxl::read_xlsx("pcdf_regionais.xlsx") #Boletim de Ocorrências (PCDF/LAI)
ouvidoria <- readxl::read_xlsx("ouv_dist.xlsx") #Ouvidorias (CGDF/LAI)
saude_upa <- readxl::read_xlsx("saude_upa.xlsx") #UPA (SES/LAI)
residuos_lotes <- readxl::read_xlsx("lotes_residuos.xlsx") #SLU (Site SLU)
residuos_slu <- readxl::read_xlsx("residuos.xlsx") #SLU (Site SLU)
coleta_seletiva <- readxl::read_xlsx("coleta_seletiva.xlsx") #SLU (Site SLU)
b_geacaf <- readxl::read_xlsx("as_beneficios_geacaf_mes.xlsx") #Benefício Eventual GEACAF (SEDES/LAI)
seas_ativos <- readxl::read_xlsx("seas_ativos.xlsx") #Abordagem Social_pessoas_ativas (SEDES/LAI)
seas_atendidos <- readxl::read_xlsx("seas_atendidos.xlsx") #Abordagem Social_pessoas_atendidas (SEDES/LAI)
seas_abordagem <- readxl::read_xlsx("seas_abordagem.xlsx") #Abordagem Social (SEDES/LAI)
refeicoes <- readxl::read_xlsx("seguranca_alimentar_mes.xlsx") #Kit_refeição do Centro_pop (SEDES/LAI)
prato_cheio <- readxl::read_xlsx("prato_cheio.xlsx") #Prato_Cheio (SEDES/LAI) 
obitos <- readxl::read_xlsx("obitos.xlsx") #Obitos (PCDF/LAI)
cadunico <- readxl::read_xlsx("cadastro_unico_mes.xlsx") #CadÚnico (SEDES/LAI)
centropop <- readxl::read_xlsx("centro_pop.xlsx") #Centro_pop Pessoas (RMA)
centropop_ra <- readxl::read_xlsx("centro_pop_ra.xlsx") #Regionalização Centro_pop (SEDES)
creas_ra <- readxl::read_xlsx("creas_ra.xlsx") #Regionalização Creas (SEDES)
creas_rma <- readxl::read_xlsx("creas_rma.xlsx") #Creas Pessoas (RMA)
drogas <- readxl::read_xlsx("drogas_mj.xlsx") #Apreensao Cocaina e Maconha (MJ/site)
grupo_renda <- readxl::read_xlsx("grupo_renda.xlsx") #RA's por Grupos de Renda (IPEDF/site)

DISPONIBILIDADE
O código e os dados utilizados nesse estudo estão disponíveis no repositório público GitHub de Hernany Castro (https://github.com/hernanycastro/gith_transito). O arquivo do código “transito_pca.R” permite identificar os demais arquivos contidos no repositório e necessários para sua reproducibilidade.

ESTRUTURA DO CODIGO
O script contido no arquivo "transito_pca.R" está organizado, conforme o diagrama a seguir:
importação -> transformação -> visualização -> realização da pca
importação: cria os objetos necessários no ambiente R
transformação: 1) converte os objetos "concentracao", "ativos", "atendidos", "abordagem", "ocorrencia", "ouvidorias", "upa", "residuos", "coleta_seletiva", "creas", "centro_pop", "cadunico" e "seg_alim", mediante a função pivot_longer; 2) cria o objeto "transito", mediante a função "left_join", observando a seguinte ordem: 
transito <- left_join(ouvidorias, creas) #Unindo objetos com maior número de observações
transito <- na.omit(transito)
#transito <- left_join(transito, concentracao)
transito <- left_join(transito, cadunico)
transito <- na.omit(transito)
transito <- left_join(transito, centro_pop)
transito <- na.omit(transito)
transito <- left_join(transito, ocorrencia)
transito <- na.omit(transito)
transito <- left_join(transito, abordagem)
transito <- na.omit(transito)
transito <- left_join(transito, ativos)
transito <- na.omit(transito)
#transito <- left_join(transito, atendidos)
#transito <- na.omit(transito)
#transito <- left_join(transito, upa)
#transito <- left_join(transito, saude_ecr)
#transito <- na.omit(transito)
transito <- left_join(transito, populacao_ibge)
transito <- na.omit(transito)
transito <- left_join(transito, residuos_lotes)
transito <- na.omit(transito)
transito <- left_join(transito, residuos)
transito <- na.omit(transito)
transito <- left_join(transito, coleta_seletiva)
transito <- na.omit(transito)
transito <- left_join(transito, seg_alim)
transito <- na.omit(transito)
transito <- left_join(transito, obitos)
transito <- na.omit(transito)
transito <- left_join(transito, b_geacaf)
transito <- na.omit(transito)
transito <- left_join(transito, drogas)
transito <- na.omit(transito)
#transito <- left_join(transito, prato_cheio)
#transito <- na.omit(transito)
transito <- left_join(transito, grupo_renda)
transito <- na.omit(transito)
transito <- left_join(transito, densidade_ipedf)
transito <- na.omit(transito)
3) utilizando a função mutate transforma as variáveis do objeto transito em razão da população; 4) procedimentos da pca, iniciando com a normalização dos dados.

PROBLEMAS CONHECIDOS
Durante a execução do código houve problemas com a função select. Isso exigiu a substituição da função. Em seu lugar foi utilizada a função arrange. O resultado não foi o mesmo, mas isso possibilitou a execução do codigo.

AUTORES
Hernany Gomes de Castro, responsável pela coleta e organziação dos dados, elaboração e execução do código, produção do relatório
Frederico Bertholini, orientação e revisão do código
Bruna Guimarães, apoio na revisão

LICENÇA
O código é aberto, como os dados, e seu uso requer a citação da fonte.
