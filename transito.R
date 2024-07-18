#Transito da Pop Rua ----
##Regulação do Transito da População em Situação de Rua pelo Estado no DF em 2023
####As políticas públicas constragem a liberdade de ir e vir da Pop Rua? 

#Pacotes ----
library(pacman)
p_load("tidyverse", "tidyr", "haven", "lubridate","janitor",
       "readxl", "stringr", "magrittr", "psych", "gapminder","scales",
       "rmarkdown","writexl","infer","patchwork","car","ggpubr","tidymodels",
       "stats","jtools","huxtable","lmtest","tolerance","car","performance","arm",
       "lmtest")

#Importacao ----
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

#Limpeza ----
#Converti os arquivos de Oficio do PDF para excel, já realizando a limpeza

#EAD ----
##Transformação ----
### PIVOTAGEM ----
####Pontos Concent. ----
concentracao <- pivot_longer(pontos, #Formatando objeto para formato tidy
                             names_to = "mes",
                             values_to = "pontos",
                             cols = c("jan","fev","mar","abr","mai",
                             "jun","jul","ago","set","out","nov","dez")) %>%
  arrange(regional,ra,ano,mes, pontos)

concentracao <- left_join(concentracao,residuos_lotes)

####Pessoas_ativos ----
ativos <- pivot_longer(seas_ativos, #Formatando objeto para formato tidy
                             names_to = "mes",
                             values_to = "ativos",
                             cols = c("jan","fev","mar","abr","mai",
                                      "jun","jul","ago","set","out","nov","dez")) %>%
  arrange(ra,ano,mes,ativos)

ativos <- left_join(ativos,residuos_lotes)

####Pessoas_atendidos ----
atendidos <- pivot_longer(seas_atendidos, #Formatando objeto para formato tidy
                       names_to = "mes",
                       values_to = "atendidos",
                       cols = c("jan","fev","mar","abr","mai",
                                "jun","jul","ago","set","out","nov","dez")) %>%
  arrange(ra,ano,mes,atendidos)

atendidos <- left_join(atendidos,residuos_lotes)

####Abordagem/SEAS ----
abordagem <- pivot_longer(seas_abordagem, #Formatando objeto para formato tidy
               names_to = "mes",
               values_to = "abordagens",
               cols = c("jan","fev","mar","abr","mai",
                        "jun","jul","ago","set","out","nov","dez")) %>%
  select(ra,ano,mes,abordagens)

abordagem <- left_join(abordagem,residuos_lotes)

####B. Ocorrência ----
ocorrencia <- pivot_longer(bo, #Formatando objeto para formato tidy
                           names_to = "mes",
                           values_to = "bo",
                           cols = c("jan","fev","mar","abr","mai",
                                    "jun","jul","ago","set","out","nov","dez")) %>%
  select(ra,regional,ano,mes,bo)

ocorrencia <- left_join(ocorrencia,residuos_lotes)

####Ouvidorias ----
ouvidorias <- pivot_longer(ouvidoria, #Formatando objeto para formato tidy
                           names_to = "mes",
                           values_to = "ouvidoria",
                           cols = c("jan","fev","mar","abr","mai",
                                    "jun","jul","ago","set","out","nov","dez")) %>%
  arrange(ra, regional,ano,mes, ouvidoria)

ouvidorias <- left_join(ouvidorias,residuos_lotes)

####Upa ----
upa <- pivot_longer(saude_upa, #Formatando objeto para formato tidy
                           names_to = "mes",
                           values_to = "upa",
                           cols = c("jan","fev","mar","abr","mai",
                                    "jun","jul","ago","set","out","nov","dez")) %>%
  arrange(ra,ano,mes,upa)

upa <- left_join(upa,residuos_lotes)

####Residuos ----
residuos <- pivot_longer(residuos_slu,#Formatando objeto para formato tidy
                    names_to = "mes",
                    values_to = "residuos",
                    cols = c("jan","fev","mar","abr","mai",
                             "jun","jul","ago","set","out","nov","dez")) %>%
  arrange(lote,ano,mes,residuos)

####Coleta_Seletiva ----
coleta_seletiva <- pivot_longer(coleta_seletiva,#Formatando objeto para formato tidy
                         names_to = "mes",
                         values_to = "coleta_seletiva",
                         cols = c("jan","fev","mar","abr","mai",
                                  "jun","jul","ago","set","out","nov","dez")) %>%
  arrange(lote,ano,mes,coleta_seletiva)

####Creas ----
creas <- left_join(creas_rma,creas_ra) %>% #Unindo RMA com Regionalização do Creas
  filter(!is.na(creas_poprua)) %>%
  select(ano,mes,regional,ra,creas_poprua)

creas <- left_join(creas,residuos_lotes)

####Centro Pop ----
centro_pop <- left_join(centropop,centropop_ra) %>% #Unindo RMA com Regionalização do Centro_pop
  select(ano,mes,regional,ra,centropop_pessoas)

centro_pop <- left_join(centro_pop,residuos_lotes)

####CadÚnico ----
cadunico <- pivot_longer(cadunico, #Formatando objeto para formato tidy
               names_to = "mes",
               values_to = "cadunico",
               cols = c("jan","fev","mar","abr","mai",
                        "jun","jul","ago","set","out","nov","dez")) %>%
  select(ra,ano,mes,cadunico)

cadunico <- left_join(cadunico,residuos_lotes)

####Seg. Alimentar ----
seg_alim <- pivot_longer(refeicoes, #Formatando objeto para formato tidy
               names_to = "mes",
               values_to = "kit_refeicao",
               cols = c("jan","fev","mar","abr","mai",
                        "jun","jul","ago","set","out","nov","dez")) %>%
  select(ano,mes,kit_refeicao)

###JOIN - Trânsito----
transito <- left_join(ouvidorias, concentracao) #Unindo objetos com maior número de observações
transito <- left_join(transito, ocorrencia)
transito <- left_join(transito, upa)
transito <- left_join(transito, saude_ecr)
transito <- left_join(transito, populacao_ibge)
transito <- left_join(transito, residuos_lotes)
transito <- left_join(transito, residuos)
transito <- left_join(transito, coleta_seletiva)
transito <- left_join(transito, abordagem)
transito <- left_join(transito, seg_alim)
transito <- left_join(transito, cadunico)
transito <- left_join(transito, obitos)
transito <- left_join(transito, b_geacaf)
transito <- left_join(transito, drogas)
transito <- left_join(transito, ativos)
transito <- left_join(transito, atendidos)
transito <- left_join(transito, centro_pop)
transito <- left_join(transito, creas)
transito <- left_join(transito, prato_cheio)
transito <- left_join(transito, grupo_renda)
transito <- left_join(transito, densidade_ipedf)

#Organizar Transito
transito %<>% #Formatando objeto para formato tidy
  filter(!is.na(lote)) %>%
  filter(!is.na(pontos)) %>%
  select(lote,regional,ra,grupo_renda,ano,mes,populacao,densidade,
         pontos,ativos,atendidos,centropop_pessoas,creas_poprua,cadunico,
         abordagens,kit_refeicao,prato_cheio,aux_vulnerab,
         ecr,upa,residuos,coleta_seletiva,ouvidoria,bo,obitos,drogas_kg)

###MUTATE ----
####Razão ----
transito %<>% #Calculando razão em relação à população da RA, com 4 décimos
  mutate(pontos_razao = round(pontos / populacao *100,5)) %>%
  mutate(ativos_razao = round(ativos / populacao *100,5)) %>%
  mutate(cadunico_razao = round(cadunico / populacao *100,5)) %>%
  mutate(abordagem_razao = round(abordagens / populacao *100,5)) %>%
  mutate(atendidos_razao = round(atendidos / populacao *100,5)) %>%
  mutate(refeicao_razao = round(kit_refeicao / populacao *100,5)) %>%
  mutate(pratocheio_razao = round(prato_cheio / populacao *100,5)) %>%
  mutate(aux_vul_razao = round(aux_vulnerab / populacao *100,5)) %>%
  mutate(ecr_razao = round(ecr / populacao *100,5)) %>%
  mutate(upa_razao = round(upa / populacao *100,5)) %>%
  mutate(residuos_razao = round(residuos / populacao *100,5)) %>%
  mutate(seletiva_razao = round(coleta_seletiva / populacao *100,5)) %>%
  mutate(ouvidoria_razao = round(ouvidoria / populacao *100,5)) %>%
  mutate(ocorrencias_razao = round(bo / populacao *100,5)) %>%
  mutate(obitos_razao = round(obitos / populacao *100,5)) %>%
  mutate(drogas_razao = round(drogas_kg / populacao *100,5)) %>%
  mutate(centropop_razao = round(centropop_pessoas / populacao *100,5)) %>%
  mutate(creas_razao = round(creas_poprua / populacao *100,5)) %>%
  mutate(tx_coleta = round(coleta_seletiva / residuos *100,2)) %>%
  select(lote,regional,ra,grupo_renda,ano,mes,populacao,densidade,
         pontos_razao,ativos_razao,atendidos_razao,centropop_razao,creas_razao,
         refeicao_razao,pratocheio_razao,abordagem_razao,cadunico_razao,
         aux_vul_razao,ecr_razao,upa_razao,residuos_razao,seletiva_razao,
         tx_coleta,ouvidoria_razao,ocorrencias_razao,obitos_razao,drogas_razao)

#####Grupos_razao ----
######RA ----
transito_ra <- transito %>%
  group_by(ra,densidade,ano,mes,centropop_razao,creas_razao,
           refeicao_razao,pratocheio_razao,abordagem_razao,cadunico_razao,
           aux_vul_razao,ecr_razao,upa_razao,residuos_razao,seletiva_razao,
           tx_coleta,ouvidoria_razao,ocorrencias_razao,obitos_razao,
           drogas_razao) %>%
  summarise(mean_ativos_razao = mean(ativos_razao)) %>%
  select(ra,densidade,ano,mes,mean_ativos_razao,centropop_razao,creas_razao,
         refeicao_razao,pratocheio_razao,abordagem_razao,cadunico_razao,
         aux_vul_razao,ecr_razao,upa_razao,residuos_razao,seletiva_razao,
         tx_coleta,ouvidoria_razao,ocorrencias_razao,obitos_razao,drogas_razao)

#####Renda ----
transito_renda <- transito %>%
  group_by(grupo_renda,densidade,ano,mes,centropop_razao,creas_razao,
           refeicao_razao,pratocheio_razao,abordagem_razao,cadunico_razao,
           aux_vul_razao,ecr_razao,upa_razao,residuos_razao,seletiva_razao,
           tx_coleta,ouvidoria_razao,ocorrencias_razao,obitos_razao,
           drogas_razao) %>%
  summarise(mean_ativos_razao = mean(ativos_razao)) %>%
  select(grupo_renda,densidade,ano,mes,mean_ativos_razao,centropop_razao,creas_razao,
         refeicao_razao,pratocheio_razao,abordagem_razao,cadunico_razao,
         aux_vul_razao,ecr_razao,upa_razao,residuos_razao,seletiva_razao,
         tx_coleta,ouvidoria_razao,ocorrencias_razao,obitos_razao,drogas_razao)

#####Regional ----
transito_regional <- transito %>%
  group_by(regional,densidade,ano,mes,centropop_razao,creas_razao,
           refeicao_razao,pratocheio_razao,abordagem_razao,cadunico_razao,
           aux_vul_razao,ecr_razao,upa_razao,residuos_razao,seletiva_razao,
           tx_coleta,ouvidoria_razao,ocorrencias_razao,obitos_razao,
           drogas_razao) %>%
  summarise(mean_ativos_razao = mean(ativos_razao)) %>%
  select(regional,densidade,ano,mes,mean_ativos_razao,centropop_razao,creas_razao,
         refeicao_razao,pratocheio_razao,abordagem_razao,cadunico_razao,
         aux_vul_razao,ecr_razao,upa_razao,residuos_razao,seletiva_razao,
         tx_coleta,ouvidoria_razao,ocorrencias_razao,obitos_razao,drogas_razao)

#####Lote ----
transito_lote <- transito %>%
  group_by(lote,densidade,ano,mes,centropop_razao,creas_razao,
           refeicao_razao,pratocheio_razao,abordagem_razao,cadunico_razao,
           aux_vul_razao,ecr_razao,upa_razao,residuos_razao,seletiva_razao,
           tx_coleta,ouvidoria_razao,ocorrencias_razao,obitos_razao,
           drogas_razao) %>%
  summarise(mean_ativos_razao = mean(ativos_razao)) %>%
  select(lote,densidade,ano,mes,mean_ativos_razao,centropop_razao,creas_razao,
         refeicao_razao,pratocheio_razao,abordagem_razao,cadunico_razao,
         aux_vul_razao,ecr_razao,upa_razao,residuos_razao,seletiva_razao,
         tx_coleta,ouvidoria_razao,ocorrencias_razao,obitos_razao,drogas_razao)

###Visualizando ----
#VD vs VI's sob "Proporção" como Unidade de Medida e Variando os Níveis de Agregação 
p_cnr <- transito %>% #Criando objeto com o ggplot para visualizar uma amostra dos dados 
  filter(!is.na(ecr_razao)) %>%
  ggplot(aes(y = pontos_razao, x = ecr_razao, color = lote)) + #Colorir a maior unidade de agregação (lote)
  geom_point() + #geom_point para visualizar possível correlação das proporções
  geom_smooth(method = "lm", size = 0.1, alpha = 0.0) + #geom_smooth para visualizar possível tendência linear
  facet_grid(~ ra, drop = F) + #facet_grid() para criar subpainéis segundo a menor unidade de agregação (RA)
  labs(x = "Atendimentos CnR (1:100)", #Usando escala linear no eixo x
       y = "Pontos de Concentração (1:100)") + #Usando escala linear no eixo y
  labs(title = "Relação do Consultório na Rua (CnR) com os Pontos de Concentração da População em Situação de Rua no DF em 2023",
       subtitle = "Razão de Pontos de Concentração vs Razão de Atendimentos do Cnr, em relação à População das RA's do DF (Censo IBGE 2022)",
       caption = "Fonte dos dados: Ouvidoria do GDF") +
  theme_minimal() + #Usando tema "minimal"
  theme(legend.title = element_blank(), #retirando o título da legenda do ggplot
        legend.position = "bottom") + #posicionando a legenda centralizada e abaixo do ggplot
  theme(axis.text.x = element_text(angle = 90, size = 8,vjust = 0.5,hjust = 1))

p_cnr #visualizando o gráfico

#Visualizando a distribuição de Ativos por grupos de RA
p_renda <- transito_renda %>%
  group_by(grupo_renda) %>%
  filter(!is.na(grupo_renda)) %>%
  ggplot(aes(x = grupo_renda,y=mean_ativos_razao, color = grupo_renda)) +
  geom_point()+
  labs(x="",y= "Pessoas em Situação de Rua por habitante (1:100)") +
  facet_grid(~grupo_renda) +
  theme_minimal() +
  theme(legend.position = "none")+
  theme(axis.text.x = element_blank())

p_ra <- transito_ra %>%
  group_by(ra) %>%
  filter(!is.na(ra)) %>%
  ggplot(aes(x=ra,y=mean_ativos_razao,color = ra)) +
  geom_point()+
  labs(x="",y= "Média da Razão de Ativos") +
  facet_grid(~ra) +
  theme_minimal() +
  theme(legend.position = "none")+
  theme(axis.text.x = element_blank())

p_lote <- transito_lote %>%
  group_by(lote) %>%
  filter(!is.na(lote)) %>%
  ggplot(aes(x=lote,y=mean_ativos_razao,color = lote)) +
  geom_point()+
  labs(x="",y= "Média da Razão de Ativos") +
  facet_grid(~lote) +
  theme_minimal() +
  theme(legend.position = "none")+
  theme(axis.text.x = element_blank())

p_regional <- transito_regional %>%
  group_by(regional) %>%
  filter(!is.na(regional)) %>%
  ggplot(aes(x=regional,y=mean_ativos_razao,color = regional)) +
  geom_point()+
  labs(x="",y= "Média da Razão de Ativos") +
  facet_grid(~regional)+
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_blank())

plot_layout(p_renda+p_lote+p_regional+p_ra)

#testando coleta seletiva
transito_renda %>%
  group_by(grupo_renda) %>%
  filter(!is.na(grupo_renda)) %>%
  ggplot(aes(x=seletiva_razao,y=mean_ativos_razao)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0) +
  labs(x="Coleta Seletiva kilo/habitante",y= "Pessoa em Situação de Rua / habitante") +
  labs(title = "Pessoas em Situação de Rua por Grupos de RA (renda) no Distrito Federal no ano 2023",
       subtitle = "Proporção de Pessoas em Situação de Rua vs Coleta Seletiva, por habitante (1:100)",
       caption = "Elaborado por Hernany Castro, em 20/06/2024") +
  facet_grid(~grupo_renda) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, size = 8,vjust = 0.5,hjust = 1))


###CORRELAÇÃO ----
####RM ----

#Estágio 1: objetivos

#Objetivo geral: explicar e prever a variação quantitativa de 
                 #pessoas em situação de rua entre as RA's do Distrito Federal
#Objetivo especifico: identificar ações das políticas publicas que interferem no transito
#VD (1): quantidade de pessoas em situação de rua em cada RA
#VIs (15): #1 centropop_razao, 2 creas_razao,
      #3 refeicao_razao, 4 pratocheio_razao,5 abordagem_razao, 
      #6 cadunico_razao, #7 aux_vul_razao, 8 ecr_razao, 9 upa_razao,
      #10 residuos_razao, 11 seletiva_razao, 12 tx_coleta, 13 ouvidoria_razao, 
      #14 ocorrencias_razao,15 obitos_razao, 16 drogas_razao

#Obs.: As relaçoes entre as 16 VIs e a VD é considerada estatística, 
       #não funcional,
       #pq envolve ação independente e pode ter níveis de erro de medida

#Estágio 2: Planejamento de pesquisa de uma análise RM

#Parâmetros sugeridos (com base na literatura)
#r² mínimo 49 para 18 variáveis (p.167, tabela 4-7), com significância 0,01
#parâmetros limites para n = 50 de cada variável (nivelamento pelo menor n)

amostra_n_vis <- sapply(transito_renda,function(x)length(na.omit(x)))
mean(amostra_n_vis)
sum(amostra_n_vis)
#Menor N 60, maior N 360, media geral 1:301.42, soma total N 6.330

#Estágio 3: Suposições em análise de RM

#Teste de Linearidade, Homocedasticidade e Normalidade

#1 centropop_razao
#Lineraridade
transito_renda %>%
  group_by(grupo_renda) %>%
  filter(!is.na(grupo_renda)) %>%
  ggplot(aes(x=centropop_razao,y=mean_ativos_razao)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0) +
  labs(x="Atendimento Centro pop/habitante",y= "Pessoa em Situação de Rua / habitante") +
  labs(title = "Pessoas em Situação de Rua por Grupos de RA (renda) no Distrito Federal no ano 2023",
       subtitle = "Proporção de Pessoas em Situação de Rua vs Atendimento Centro pop, por habitante (1:100)",
       caption = "Elaborado por Hernany Castro, em 15/07/2024") +
  facet_grid(~grupo_renda) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, size = 8,vjust = 0.5,hjust = 1))

#Homocedasticidade e Normalidade
mod_centropop <- transito_renda %>%
  lm(mean_ativos_razao ~ centropop_razao, data = .)
par(mfrow=c(2,2))
plot(mod_centropop)

shapiro.test(mod_centropop$residuals)

#2 creas_razao
#Lineralidade
transito_renda %>%
  group_by(grupo_renda) %>%
  filter(!is.na(grupo_renda)) %>%
  ggplot(aes(x=creas_razao,y=mean_ativos_razao)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0) +
  labs(x="Atendimento Creas/habitante",y= "Pessoa em Situação de Rua / habitante") +
  labs(title = "Pessoas em Situação de Rua por Grupos de RA (renda) no Distrito Federal no ano 2023",
       subtitle = "Proporção de Pessoas em Situação de Rua vs Atendimento Creas, por habitante (1:100)",
       caption = "Elaborado por Hernany Castro, em 15/07/2024") +
  facet_grid(~grupo_renda) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, size = 8,vjust = 0.5,hjust = 1))

#Homocedasticidade e Normalidade
mod_creas <- transito_renda %>%
  lm(mean_ativos_razao ~ creas_razao, data = .)
par(mfrow=c(2,2))
plot(mod_creas)

shapiro.test(mod_creas$residuals)

#3 refeicao_razao
#Lineralidade
transito_renda %>%
  group_by(grupo_renda) %>%
  filter(!is.na(grupo_renda)) %>%
  ggplot(aes(x=refeicao_razao,y=mean_ativos_razao)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0) +
  labs(x="Kit Refeição no Centro pop /habitante",y= "Pessoa em Situação de Rua / habitante") +
  labs(title = "Pessoas em Situação de Rua por Grupos de RA (renda) no Distrito Federal no ano 2023",
       subtitle = "Proporção de Pessoas em Situação de Rua vs Kit Refeição Centro pop, por habitante (1:100)",
       caption = "Elaborado por Hernany Castro, em 15/07/2024") +
  facet_grid(~grupo_renda) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, size = 8,vjust = 0.5,hjust = 1))

#Homocedasticidade e Normalidade
mod_refeicao <- transito_renda %>%
  lm(mean_ativos_razao ~ refeicao_razao, data = .)
par(mfrow=c(2,2))
plot(mod_refeicao)

shapiro.test(mod_refeicao$residuals)

#4 pratocheio_razao
#Linearidade
transito_renda %>%
  group_by(grupo_renda) %>%
  filter(!is.na(grupo_renda)) %>%
  ggplot(aes(x=pratocheio_razao,y=mean_ativos_razao)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0) +
  labs(x="Benefício Prato Cheio/habitante",y= "Pessoa em Situação de Rua / habitante") +
  labs(title = "Pessoas em Situação de Rua por Grupos de RA (renda) no Distrito Federal no ano 2023",
       subtitle = "Proporção de Pessoas em Situação de Rua vs Prato Cheio, por habitante (1:100)",
       caption = "Elaborado por Hernany Castro, em 15/07/2024") +
  facet_grid(~grupo_renda) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, size = 8,vjust = 0.5,hjust = 1))

#Homocedasticidade e Normalidade
mod_pratocheio <- transito_renda %>%
  lm(mean_ativos_razao ~ pratocheio_razao, data = .)
par(mfrow=c(2,2))
plot(mod_pratocheio)

shapiro.test(mod_pratocheio$residuals)

#5 abordagem_razao
#Linearidade
transito_renda %>%
  group_by(grupo_renda) %>%
  filter(!is.na(grupo_renda)) %>%
  ggplot(aes(x=abordagem_razao,y=mean_ativos_razao)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0) +
  labs(x="Abordgaem Social/habitante",y= "Pessoa em Situação de Rua / habitante") +
  labs(title = "Pessoas em Situação de Rua por Grupos de RA (renda) no Distrito Federal no ano 2023",
       subtitle = "Proporção de Pessoas em Situação de Rua vs Abordagem Social, por habitante (1:100)",
       caption = "Elaborado por Hernany Castro, em 15/07/2024") +
  facet_grid(~grupo_renda) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, size = 8,vjust = 0.5,hjust = 1))

#Homocedasticidade e Normalidade
mod_abordagem <- transito_renda %>%
  lm(mean_ativos_razao ~ abordagem_razao, data = .)
par(mfrow=c(2,2))
plot(mod_abordagem)

shapiro.test(mod_abordagem$residuals)

#6 cadunico_razao
#Linearidade
transito_renda %>%
  group_by(grupo_renda) %>%
  filter(!is.na(grupo_renda)) %>%
  ggplot(aes(x=cadunico_razao,y=mean_ativos_razao)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0) +
  labs(x="Cadastro Único/habitante",y= "Pessoa em Situação de Rua / habitante") +
  labs(title = "Pessoas em Situação de Rua por Grupos de RA (renda) no Distrito Federal no ano 2023",
       subtitle = "Proporção de Pessoas em Situação de Rua vs CadÚnico, por habitante (1:100)",
       caption = "Elaborado por Hernany Castro, em 15/07/2024") +
  facet_grid(~grupo_renda) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, size = 8,vjust = 0.5,hjust = 1))

#Homocedasticidade e Normalidade
mod_cadunico <- transito_renda %>%
  lm(mean_ativos_razao ~ cadunico_razao, data = .)
par(mfrow=c(2,2))
plot(mod_cadunico)

shapiro.test(mod_cadunico$residuals)

#7 aux_vul_razao
#Linearidade
transito_renda %>%
  group_by(grupo_renda) %>%
  filter(!is.na(grupo_renda)) %>%
  ggplot(aes(x=aux_vul_razao,y=mean_ativos_razao)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0) +
  labs(x="Auxílio Vulnerabilidade/habitante",y= "Pessoa em Situação de Rua / habitante") +
  labs(title = "Pessoas em Situação de Rua por Grupos de RA (renda) no Distrito Federal no ano 2023",
       subtitle = "Proporção de Pessoas em Situação de Rua vs Auxílio Vulnerabilidade, por habitante (1:100)",
       caption = "Elaborado por Hernany Castro, em 15/07/2024") +
  facet_grid(~grupo_renda) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, size = 8,vjust = 0.5,hjust = 1))

#Homocedasticidade e Normalidade
mod_auxilio <- transito_renda %>%
  lm(mean_ativos_razao ~ aux_vul_razao, data = .)
par(mfrow=c(2,2))
plot(mod_auxilio)

shapiro.test(mod_auxilio$residuals)

#8 ecr_razao
#Linearidade
transito_renda %>%
  group_by(grupo_renda) %>%
  filter(!is.na(grupo_renda)) %>%
  ggplot(aes(x=ecr_razao,y=mean_ativos_razao)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0) +
  labs(x="Atendimento Consultório na Rua/habitante",y= "Pessoa em Situação de Rua / habitante") +
  labs(title = "Pessoas em Situação de Rua por Grupos de RA (renda) no Distrito Federal no ano 2023",
       subtitle = "Proporção de Pessoas em Situação de Rua vs Atendimento Consultório na Rua, por habitante (1:100)",
       caption = "Elaborado por Hernany Castro, em 15/07/2024") +
  facet_grid(~grupo_renda) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, size = 8,vjust = 0.5,hjust = 1))

#Homocedasticidade e Normalidade
mod_ecr <- transito_renda %>%
  lm(mean_ativos_razao ~ ecr_razao, data = .)
par(mfrow=c(2,2))
plot(mod_ecr)

shapiro.test(mod_ecr$residuals)

#9 upa_razao
#Linearidade
transito_renda %>%
  group_by(grupo_renda) %>%
  filter(!is.na(grupo_renda)) %>%
  ggplot(aes(x=upa_razao,y=mean_ativos_razao)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0) +
  labs(x="Atendimento Upa/habitante",y= "Pessoa em Situação de Rua / habitante") +
  labs(title = "Pessoas em Situação de Rua por Grupos de RA (renda) no Distrito Federal no ano 2023",
       subtitle = "Proporção de Pessoas em Situação de Rua vs Atendimento Upa, por habitante (1:100)",
       caption = "Elaborado por Hernany Castro, em 15/07/2024") +
  facet_grid(~grupo_renda) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, size = 8,vjust = 0.5,hjust = 1))

#Homocedasticidade e Normalidade
mod_upa <- transito_renda %>%
  lm(mean_ativos_razao ~ upa_razao, data = .)
par(mfrow=c(2,2))
plot(mod_upa)

shapiro.test(mod_upa$residuals)

#10 residuos_razao
#Linearidade
transito_renda %>%
  group_by(grupo_renda) %>%
  filter(!is.na(grupo_renda)) %>%
  ggplot(aes(x=residuos_razao,y=mean_ativos_razao)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0) +
  labs(x="Coleta Resíduos em Kilos/habitante",y= "Pessoa em Situação de Rua / habitante") +
  labs(title = "Pessoas em Situação de Rua por Grupos de RA (renda) no Distrito Federal no ano 2023",
       subtitle = "Proporção de Pessoas em Situação de Rua vs Coleta Total, por habitante (1:100)",
       caption = "Elaborado por Hernany Castro, em 15/07/2024") +
  facet_grid(~grupo_renda) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, size = 8,vjust = 0.5,hjust = 1))

#Homocedasticidade e Normalidade
mod_residuos <- transito_renda %>%
  lm(mean_ativos_razao ~ residuos_razao, data = .)
par(mfrow=c(2,2))
plot(mod_residuos)

shapiro.test(mod_residuos$residuals)

#11 seletiva_razao
transito_renda %>%
  group_by(grupo_renda) %>%
  filter(!is.na(grupo_renda)) %>%
  ggplot(aes(x=seletiva_razao,y=mean_ativos_razao)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0) +
  labs(x="Coleta Seletiva kilo/habitante",y= "Pessoa em Situação de Rua / habitante") +
  labs(title = "Pessoas em Situação de Rua por Grupos de RA (renda) no Distrito Federal no ano 2023",
       subtitle = "Proporção de Pessoas em Situação de Rua vs Coleta Seletiva, por habitante (1:100)",
       caption = "Elaborado por Hernany Castro, em 20/06/2024") +
  facet_grid(~grupo_renda) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, size = 8,vjust = 0.5,hjust = 1))

#Homocedasticidade e Normalidade
mod_seletiva <- transito_renda %>%
  lm(mean_ativos_razao ~ seletiva_razao, data = .)
par(mfrow=c(2,2))
plot(mod_seletiva)

shapiro.test(mod_seletiva$residuals)

#12 tx_coleta

#13 ouvidoria_razao
#Linearidade
transito_renda %>%
  group_by(grupo_renda) %>%
  filter(!is.na(grupo_renda)) %>%
  ggplot(aes(x=ouvidoria_razao,y=mean_ativos_razao)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0) +
  labs(x="Ouvidoria/habitante",y= "Pessoa em Situação de Rua / habitante") +
  labs(title = "Pessoas em Situação de Rua por Grupos de RA (renda) no Distrito Federal no ano 2023",
       subtitle = "Proporção de Pessoas em Situação de Rua vs Ouvidoria pop, por habitante (1:100)",
       caption = "Elaborado por Hernany Castro, em 15/07/2024") +
  facet_grid(~grupo_renda) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, size = 8,vjust = 0.5,hjust = 1))

#Homocedasticidade e Normalidade
mod_ouvidoria <- transito_renda %>%
  lm(mean_ativos_razao ~ ouvidoria_razao, data = .)
par(mfrow=c(2,2))
plot(mod_ouvidoria)

shapiro.test(mod_ouvidoria$residuals)

#14 ocorrencias_razao
#Linearidade
transito_renda %>%
  group_by(grupo_renda) %>%
  filter(!is.na(grupo_renda)) %>%
  ggplot(aes(x=ocorrencias_razao,y=mean_ativos_razao)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0) +
  labs(x="Ocorrências policiais/habitante",y= "Pessoa em Situação de Rua / habitante") +
  labs(title = "Pessoas em Situação de Rua por Grupos de RA (renda) no Distrito Federal no ano 2023",
       subtitle = "Proporção de Pessoas em Situação de Rua vs Ocorrências policiais, por habitante (1:100)",
       caption = "Elaborado por Hernany Castro, em 15/07/2024") +
  facet_grid(~grupo_renda) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, size = 8,vjust = 0.5,hjust = 1))

#Homocedasticidade e Normalidade
mod_ocorrencias <- transito_renda %>%
  lm(mean_ativos_razao ~ ocorrencias_razao, data = .)
par(mfrow=c(2,2))
plot(mod_ocorrencias)

shapiro.test(mod_ocorrencias$residuals)

#15 obitos_razao
#Linearidade
transito_renda %>%
  group_by(grupo_renda) %>%
  filter(!is.na(grupo_renda)) %>%
  ggplot(aes(x=obitos_razao,y=mean_ativos_razao)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0) +
  labs(x="Óbitos/habitante",y= "Pessoa em Situação de Rua / habitante") +
  labs(title = "Pessoas em Situação de Rua por Grupos de RA (renda) no Distrito Federal no ano 2023",
       subtitle = "Proporção de Pessoas em Situação de Rua vs Óbitos, por habitante (1:100)",
       caption = "Elaborado por Hernany Castro, em 15/07/2024") +
  facet_grid(~grupo_renda) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, size = 8,vjust = 0.5,hjust = 1))

#Homocedasticidade e Normalidade
mod_obitos <- transito_renda %>%
  lm(mean_ativos_razao ~ obitos_razao, data = .)
par(mfrow=c(2,2))
plot(mod_obitos)

shapiro.test(mod_obitos$residuals)

#16 drogas_razao
#Linearidade
transito_renda %>%
  group_by(grupo_renda) %>%
  filter(!is.na(grupo_renda)) %>%
  ggplot(aes(x=drogas_razao,y=mean_ativos_razao)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0) +
  labs(x="Drogas Apreendidas (kg) /habitante",y= "Pessoa em Situação de Rua / habitante") +
  labs(title = "Pessoas em Situação de Rua por Grupos de RA (renda) no Distrito Federal no ano 2023",
       subtitle = "Proporção de Pessoas em Situação de Rua vs Drogas apreendidas, por habitante (1:100)",
       caption = "Elaborado por Hernany Castro, em 15/07/2024") +
  facet_grid(~grupo_renda) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, size = 8,vjust = 0.5,hjust = 1))

#Homocedasticidade e Normalidade
mod_drogas <- transito_renda %>%
  lm(mean_ativos_razao ~ drogas_razao, data = .)
par(mfrow=c(2,2))
plot(mod_drogas)

shapiro.test(mod_drogas$residuals)

#Somente cadastro único e ecr atenderam ao requisitos linearidade, 
#homocedasticidade e normalidade

#Estagio 4: estimação do modelo e avaliação do ajuste geral do modelo

#VIs: 1 ecr_razao, 2 cadunico_razao

#Verificar correlações
summary(mod_cadunico) #apresenta maior correlação t = 5.989 e p = 8.28e-09
summary(mod_ecr) # t = -0.241 e p = 0.81

#Estimar modelo, considerando stepwise
modelo <- lm(mean_ativos_razao ~ cadunico_razao, data = transito_renda)
summary(modelo)
#R² multiplo = 0.1375, R² ajustado = 0.1337
#Erro padrão da estiamtiva = 0.3886
#F = 35.87 para 225 DF
#b = 0.09296
#Beta = 10.35726
#erro padrão do coeficiente (b) = 0.03444
#valor t na equação = 2.699

modelo_ajustado <- lm(lm(mean_ativos_razao ~ cadunico_razao + ecr_razao, 
                         data = transito_renda))
summary(modelo_ajustado)
#R² multiplo = 0.3111, R² ajustado = 0.2869
#Erro padrão da estimativa = 0.1792
#F = 12.87 para 57 DF
#b = 0.1784
#Beta_cad = 6.4111
#Beta_ecr = -0.4111
#erro padrão do coeficiente (b) = 0.0493
#valor t na equação = 3.617
#Correlações = ordem zero - cadunico: (r² ajustado) 0.1337 / ecr: -0.01622; 
              # parcial: cadunico: (r² ajustado) 0.1337  / ecr : -0.01622;
              # semiparcial: de (r² ajustado) 0.1337 para 0.2869,
              # ecr acrescentou 0.1532 ao r² ajustado

vif_ajustado <- vif(lm(mean_ativos_razao ~ cadunico_razao + ecr_razao, 
                       data = transito_renda))
vif_ajustado
#cadunico: 1.051936 e ecr: 1.051936

#Avaliação da variável estatística (modelo_ajustado)
#Lineralidade, Homocesdasticidade e Normalidade
par(mfrow=c(2,2))
plot(modelo_ajustado)
shapiro.test(modelo_ajustado$residuals)

#Independência dos resíduos
dw_test_modelo_ajustado <- durbinWatsonTest(modelo_ajustado)
dw_test_modelo_ajustado
#identificada correlação positiva entre os resíduos, VIs não independentes 
plot(modelo_ajustado$residuals)

#Tratamento

transito_renda %>%
  ggplot(aes(x = cadunico_razao, y = mean_ativos_razao)) +
  geom_jitter()

transito_renda %>%
  ggplot(aes(x = ecr_razao, y = mean_ativos_razao)) +
  geom_jitter()

transito_renda_outlier <- transito_renda %>%
  group_by(grupo_renda,ano,mes,mean_ativos_razao,cadunico_razao,
         ecr_razao) %>%
  filter(!is.na(cadunico_razao)) %>%
  filter(!is.na(ecr_razao)) %>%
  filter(cadunico_razao < "0.07") %>%
  filter(ecr_razao < "0.4") %>%
  select(grupo_renda,ano,mes,mean_ativos_razao,cadunico_razao,
         ecr_razao)
  
modelo_tratado <- lm(lm(mean_ativos_razao ~ cadunico_razao + ecr_razao, 
                         data = transito_renda_outlier))
summary(modelo_tratado)

plot(modelo_tratado)

dw_test_modelo_tratado <- durbinWatsonTest(modelo_tratado)
dw_test_modelo_tratado
par(mfrow=c(2,2))
plot(modelo_tratado)


#Testando modelo confirmatório

modelo_conf <- lm(mean_ativos_razao ~ abordagem_razao + ouvidoria_razao +
                    ocorrencias_razao +
                    ecr_razao + seletiva_razao, data = transito_renda)

summary(modelo_conf)

vif_conf <- vif(lm(mean_ativos_razao ~ abordagem_razao + ouvidoria_razao +
                     ocorrencias_razao +
                     ecr_razao + seletiva_razao, data = transito_renda))

vif_conf

plot(modelo_conf)

dw_test_modelo_conf <- durbinWatsonTest(modelo_conf)
dw_test_modelo_conf
par(mfrow=c(2,2))
plot(modelo_conf)

#Buscando observações influentes

influential <- car::infIndexPlot(modelo_conf)
influential <- car::outlierTest(modelo_conf)

# Resumo das observações influentes
summary(influential)
print(influential)

#Tratando a linha 32 (outlier) do transito_renda - testando exclusão

transito_norm <- transito_renda[-32, ]

modelo_conf_n <- lm(mean_ativos_razao ~ abordagem_razao + ouvidoria_razao +
                    ocorrencias_razao +
                    ecr_razao + seletiva_razao, data = transito_norm)

summary(modelo_conf_n)

vif_conf_n <- vif(lm(mean_ativos_razao ~ abordagem_razao + ouvidoria_razao +
                     ocorrencias_razao +
                     ecr_razao + seletiva_razao, data = transito_norm))

vif_conf_n

plot(modelo_conf_n)

dw_test_modelo_conf_n <- durbinWatsonTest(modelo_conf_n)
dw_test_modelo_conf_n
par(mfrow=c(2,2))
plot(modelo_conf_n)

influential_n <- car::infIndexPlot(modelo_conf_n)
influential_n <- car::outlierTest(modelo_conf)

# Resumo das observações influentes
summary(influential_n)
print(influential_n)

#Novo teste de tratamento, excluindo a linha 32

transito_n <- transito_norm[-32, ]

modelo_conf_norm <- lm(mean_ativos_razao ~ abordagem_razao + ouvidoria_razao +
                      ocorrencias_razao +
                      ecr_razao + seletiva_razao, data = transito_n)

summary(modelo_conf_norm)

vif_conf_norm <- vif(lm(mean_ativos_razao ~ abordagem_razao + ouvidoria_razao +
                       ocorrencias_razao +
                       ecr_razao + seletiva_razao, data = transito_n))

vif_conf_norm

plot(modelo_conf_norm)

dw_test_modelo_conf_norm <- durbinWatsonTest(modelo_conf_norm)
dw_test_modelo_conf_norm
par(mfrow=c(2,2))
plot(modelo_conf_norm)

influential_norm <- car::infIndexPlot(modelo_conf_norm)
influential_norm <- car::outlierTest(modelo_conf_norm)

# Resumo das observações influentes
print(influential_norm)


#Estagio 5: Interpretação da Variável Estatística

summary(modelo_conf_norm)
#p-value da variável estatística: 1.678e-14
#ouvidoria e seletiva tem sinal negativo, embora sem signifância estatística
#abordagem e ocorrencias tem sinal positivo (abordagem + do que ocorrencias)
#o maior VIF é ocorrencias: 6.07, seguido de abordagem: 3.92

#Estágio 6: validação dos resultados
# Defina a proporção da subamostra de validação (20% neste caso)
proporcao_validacao <- 0.2

# Selecione aleatoriamente os índices para a subamostra de validação
indices_validacao <- sample(1:nrow(transito_n), size = floor(nrow(transito_n) * proporcao_validacao))

# Crie os conjuntos de dados de treinamento e validação
dados_treinamento <- transito_n[-indices_validacao, ]
dados_validacao <- transito_n[indices_validacao, ]
#Teste do modelo de treinamento
modelo_treino <- lm(mean_ativos_razao ~ abordagem_razao + ouvidoria_razao +
                     ocorrencias_razao +
                     ecr_razao + seletiva_razao, data = dados_treinamento)

summary(modelo_treino)

vif_treino <- vif(lm(mean_ativos_razao ~ abordagem_razao + ouvidoria_razao +
                      ocorrencias_razao +
                      ecr_razao + seletiva_razao, data = dados_treinamento))

vif_treino

plot(modelo_treino)

dw_test_modelo_treino <- durbinWatsonTest(modelo_treino)
dw_test_modelo_treino
par(mfrow=c(2,2))
plot(modelo_treino)

influential_treino <- car::infIndexPlot(modelo_treino)
influential_treino <- car::outlierTest(modelo_treino)

# Resumo das observações influentes
print(influential_treino)

summary(modelo_treino)
#r²: 72.37 e r² ajustado: 68.42
#p-value da variável estatística: 6.493e-09
#ouvidoria e seletiva tem sinal negativo, embora sem signifância estatística
#abordagem e ocorrencias tem sinal positivo (abordagem + do que ocorrencias)
#o maior VIF é ocorrencias: 7.19, seguido de abordagem: 4.59


#Teste do modelo de validação
modelo_valid <- lm(mean_ativos_razao ~ abordagem_razao + ouvidoria_razao +
                         ocorrencias_razao +
                         ecr_razao + seletiva_razao, data = dados_validacao)

summary(modelo_valid)

vif_valid <- vif(lm(mean_ativos_razao ~ abordagem_razao + ouvidoria_razao +
                          ocorrencias_razao +
                          ecr_razao + seletiva_razao, data = dados_validacao))

vif_valid

plot(modelo_valid)

dw_test_modelo_valid <- durbinWatsonTest(modelo_valid)
dw_test_modelo_valid
par(mfrow=c(2,2))
plot(modelo_valid)

influential_valid <- car::infIndexPlot(modelo_valid)
influential_valid <- car::outlierTest(modelo_valid)

# Resumo das observações influentes
print(influential_valid)

summary(modelo_valid)
#r²: 97.91 e r² ajustado: 96.75
#p-value da variável estatística: 2.762e-07
#intercept tem sinal negativo, embora sem signifância estatística
#todas as variáveis tem sinal positivo
#o maior VIF é ocorrencias: 6.11, seguido de ouvidoria: 3.34

##Modelando ----
#Testando Hipótese nula
observed_fit <- transito_renda %>%
  specify(mean_ativos_razao ~ abordagem_razao + 
            ouvidoria_razao +
            ocorrencias_razao +
            ecr_razao + seletiva_razao
            ) %>%
  fit()

bootstrap_distribution <- transito_renda %>%
  specify(mean_ativos_razao ~ abordagem_razao + 
            ouvidoria_razao +
            ocorrencias_razao +
            ecr_razao + seletiva_razao
          ) %>% #especifica a VD e a VI ou VI's
  generate(reps = 1000, type = "bootstrap") %>% #bootstraping x1000
  fit()
  #calculate(stat = #"mean", #especifica a estatística desejada
                     #"median", 
                     #"sum", 
                     #"sd", 
                     #"prop", 
                     #"count", 
                     #"diff in means"#,
                     #"diff in medians", 
                     #"diff in props", 
                     #"Chisq", 
                     #"F", 
                     #"slope", 
                     #"correlation", 
                     #"t", 
                     #"z",
                     #"ratio of props", 
                     #"odds ratio", 
                     #"ratio of means") 

bootstrap_distribution

null_ativos <- transito_renda %>% 
  #filter(grupo_renda == "")
  specify(mean_ativos_razao ~ abordagem_razao + 
            ouvidoria_razao +
            ocorrencias_razao +
            ecr_razao + seletiva_razao #+ densidade
          ) %>%
  hypothesize(null = "point", mu = 0) %>% #sugere a hipótese que ativos_prop é igual a zero
  generate(reps = 1000, type = "bootstrap") %>%
  #calculate(stat = "mean") #compara médias
    fit()

null_ativos

dataci <- get_confidence_interval(
  bootstrap_distribution, 
  point_estimate = observed_fit, 
  level = .95
)

visualize(null_ativos) + 
  shade_p_value(observed_fit, direction = "both")
#entendendo o gráfico:
#a linha vermelha é a média de ativos
#a parte sombreada é o p_value - a parte signifitiva

#Lista 4
point_ci <- left_join(null_ativos,dataci)

lista4 <- lista4 <- ggplot(point_ci, aes(x = term, y = estimate, color = term, alpha = 1)) +
  geom_point(color = "black") +
  geom_jitter() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 2) +
  geom_hline(aes(yintercept = mean(estimate)), 
             linetype = "dashed", color = "red") +
  geom_text(aes(label = round(mean(estimate), 2), 
                y = mean(estimate) + 0.1), color = "red", size = 3) +
  facet_grid(~ term) +
  labs(title = "Estimativa Intervalar",
       subtitle = "Intervalo de Confiança - 95%",
       x = "", y = "Estimativa Intervalar") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  theme(legend.position = "none")

#Estimativa de Média de Ativos com IC de 95%
#Transito_renda
observed_fit_g1 <- transito_renda %>%
  filter(grupo_renda == "g1_alta_renda") %>%
  specify(mean_ativos_razao ~ abordagem_razao + 
            ouvidoria_razao +
            ocorrencias_razao +
            ecr_razao + seletiva_razao #+ densidade
          ) %>%
  fit()

bootstrap_distribution_g1 <- transito_renda %>%
  filter(grupo_renda == "g1_alta_renda") %>%
  specify(mean_ativos_razao ~ abordagem_razao + 
            ouvidoria_razao +
            ocorrencias_razao +
            ecr_razao + seletiva_razao
  ) %>% #especifica a VD e a VI ou VI's
  generate(reps = 1000, type = "bootstrap") %>% #bootstraping x1000
  fit() 

null_ativos_g1 <- transito_renda %>% 
  filter(grupo_renda == "g1_alta_renda") %>%
  specify(mean_ativos_razao ~ abordagem_razao + 
            ouvidoria_razao +
            ocorrencias_razao +
            ecr_razao + seletiva_razao
  ) %>%
  hypothesize(null = "point", mu = 0) %>% #sugere a hipótese que ativos_prop é igual a zero
  generate(reps = 1000, type = "bootstrap") %>%
  #calculate(stat = "mean") #compara médias
  fit()

data_ci_g1 <- get_confidence_interval(
  null_ativos_g1, 
  point_estimate = observed_fit_g1, 
  level = .95
)

g1 <- ggplot(bootstrap_distribution_g1, aes(x = estimate)) +
  geom_histogram(fill = NA, color = "black") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1) +
  geom_density(alpha = 0.2, fill = "lightgray") +
  #scale_x_log10() +
  #scale_y_log10() +
  scale_x_continuous(limits = c(-10,10, by = 2)) +
  #scale_y_log10() +
  annotate("point", x = mean(bootstrap_distribution_g1$estimate), y = 0, size = 5, color = "blue") +
  annotate("segment", x = mean(bootstrap_distribution_g1$estimate), xend = data_ci_g1$lower_ci, y = 0, yend = 0, color = "blue", size = 1, alpha = 0.5) +
  annotate("segment", x = mean(bootstrap_distribution_g1$estimate), xend = data_ci_g1$upper_ci, y = 0, yend = 0, color = "blue", size = 1, alpha = 0.5) +
  labs(title = "Teste de Hipótese nula e Estimativa de Média de População em Situação de Rua por Agrupamento de RA's por renda (IC 95%)",
       subtitle = "Grupo 1: Alta-renda") +
  labs(x = "", y = "Densidade") +
  theme_minimal()

observed_fit_g2 <- transito_renda %>%
  filter(grupo_renda == "g2_media_alta_renda") %>%
  specify(mean_ativos_razao ~ abordagem_razao + 
            ouvidoria_razao +
            ocorrencias_razao +
            ecr_razao + seletiva_razao
          ) %>%
  fit()

bootstrap_distribution_g2 <- transito_renda %>%
  filter(grupo_renda == "g2_media_alta_renda") %>%
  specify(mean_ativos_razao ~ abordagem_razao + 
            ouvidoria_razao +
            ocorrencias_razao +
            ecr_razao + seletiva_razao
  ) %>% #especifica a VD e a VI ou VI's
  generate(reps = 1000, type = "bootstrap") %>% #bootstraping x1000
  fit() 

null_ativos_g2 <- transito_renda %>% 
  filter(grupo_renda == "g2_media_alta_renda") %>%
specify(mean_ativos_razao ~ abordagem_razao + 
          ouvidoria_razao +
          ocorrencias_razao +
          ecr_razao + seletiva_razao
) %>%
  hypothesize(null = "point", mu = 0) %>% #sugere a hipótese que ativos_prop é igual a zero
  generate(reps = 1000, type = "bootstrap") %>%
  #calculate(stat = "mean") #compara médias
  fit()

data_ci_g2 <- get_confidence_interval(
  null_ativos_g2, 
  point_estimate = observed_fit_g2, 
  level = .95
)

g2 <- ggplot(bootstrap_distribution_g2, aes(x = estimate)) +
  geom_histogram(fill = NA, color = "black") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1) +
  geom_density(alpha = 0.2, fill = "lightgray") +
  #scale_x_log10() +
  #scale_y_log10() +
  scale_x_continuous(limits = c(-10,10, by = 2)) +
  #scale_y_log10() +
  annotate("point", x = mean(bootstrap_distribution_g2$estimate), y = 0, size = 5, color = "blue") +
  annotate("segment", x = mean(bootstrap_distribution_g2$estimate), xend = data_ci_g2$lower_ci, y = 0, yend = 0, color = "blue", size = 1, alpha = 0.5) +
  annotate("segment", x = mean(bootstrap_distribution_g2$estimate), xend = data_ci_g2$upper_ci, y = 0, yend = 0, color = "blue", size = 1, alpha = 0.5) +
  labs(#title = "Teste de Hipótese nula e Estimativa de Média de População em Situação de Rua por Agrupamento de RA's por renda (IC 95%)",
       subtitle = "Grupo 2: Média alta-renda") +
  labs(x = "", y = "") +
  theme_minimal()

observed_fit_g3 <- transito_renda %>%
  filter(grupo_renda == "g3_media_baixa_renda") %>%
  specify(mean_ativos_razao ~ abordagem_razao + 
            ouvidoria_razao +
            ocorrencias_razao +
            ecr_razao + seletiva_razao
          ) %>%
  fit()

bootstrap_distribution_g3 <- transito_renda %>%
  filter(grupo_renda == "g3_media_baixa_renda") %>%
  specify(mean_ativos_razao ~ abordagem_razao + 
            ouvidoria_razao +
            ocorrencias_razao +
            ecr_razao + seletiva_razao
  ) %>% #especifica a VD e a VI ou VI's
  generate(reps = 1000, type = "bootstrap") %>% #bootstraping x1000
  fit() 

null_ativos_g3 <- transito_renda %>% 
  filter(grupo_renda == "g3_media_baixa_renda") %>%
specify(mean_ativos_razao ~ abordagem_razao + 
          ouvidoria_razao +
          ocorrencias_razao +
          ecr_razao + seletiva_razao
) %>%
  hypothesize(null = "point", mu = 0) %>% #sugere a hipótese que ativos_prop é igual a zero
  generate(reps = 1000, type = "bootstrap") %>%
  #calculate(stat = "mean") #compara médias
  fit()

data_ci_g3 <- get_confidence_interval(
  null_ativos_g3, 
  point_estimate = observed_fit_g3, 
  level = .95
)

g3 <- ggplot(bootstrap_distribution_g3, aes(x = estimate)) +
  geom_histogram(fill = NA, color = "black") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1) +
  geom_density(alpha = 0.2, fill = "lightgray") +
  #scale_x_log10() +
  #scale_y_log10() +
  scale_x_continuous(limits = c(-10,10, by = 2)) +
  #scale_y_log10() +
  annotate("point", x = mean(bootstrap_distribution_g3$estimate), y = 0, size = 5, color = "blue") +
  annotate("segment", x = mean(bootstrap_distribution_g3$estimate), xend = data_ci_g3$lower_ci, y = 0, yend = 0, color = "blue", size = 1, alpha = 0.5) +
  annotate("segment", x = mean(bootstrap_distribution_g3$estimate), xend = data_ci_g3$upper_ci, y = 0, yend = 0, color = "blue", size = 1, alpha = 0.5) +
  labs(#title = "Teste de Hipótese nula e Estimativa de Média de População em Situação de Rua por Agrupamento de RA's por renda (IC 95%)",
       subtitle = "Grupo 3: Média baixa-renda") +
  labs(x = "", y = "Densidade") +
  theme_minimal()

observed_fit_g4 <- transito_renda %>%
  filter(grupo_renda == "g4_baixa_renda") %>%
  specify(mean_ativos_razao ~ abordagem_razao + 
            ouvidoria_razao +
            ocorrencias_razao +
            ecr_razao + seletiva_razao
          ) %>%
  fit()

bootstrap_distribution_g4 <- transito_renda %>%
  filter(grupo_renda == "g4_baixa_renda") %>%
  specify(mean_ativos_razao ~ abordagem_razao + 
            ouvidoria_razao +
            ocorrencias_razao +
            ecr_razao + seletiva_razao
  ) %>% #especifica a VD e a VI ou VI's
  generate(reps = 1000, type = "bootstrap") %>% #bootstraping x1000
  fit() 

null_ativos_g4 <- transito_renda %>% 
  filter(grupo_renda == "g4_baixa_renda") %>%
specify(mean_ativos_razao ~ abordagem_razao + 
          ouvidoria_razao +
          ocorrencias_razao +
          ecr_razao + seletiva_razao
) %>%
  hypothesize(null = "point", mu = 0) %>% #sugere a hipótese que ativos_prop é igual a zero
  generate(reps = 1000, type = "bootstrap") %>%
  #calculate(stat = "mean") #compara médias
  fit()

data_ci_g4 <- get_confidence_interval(
  null_ativos_g4, 
  point_estimate = observed_fit_g4, 
  level = .95
)

g4 <- ggplot(bootstrap_distribution_g4, aes(x = estimate)) +
  geom_histogram(fill = NA, color = "black") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1) +
  geom_density(alpha = 0.2, fill = "lightgray") +
  #scale_x_log10() +
  #scale_y_log10() +
  scale_x_continuous(limits = c(-10,10, by = 2)) +
  #scale_y_log10() +
  annotate("point", x = mean(bootstrap_distribution_g4$estimate), y = 0, size = 5, color = "blue") +
  annotate("segment", x = mean(bootstrap_distribution_g4$estimate), xend = data_ci_g4$lower_ci, y = 0, yend = 0, color = "blue", size = 1, alpha = 0.5) +
  annotate("segment", x = mean(bootstrap_distribution_g4$estimate), xend = data_ci_g4$upper_ci, y = 0, yend = 0, color = "blue", size = 1, alpha = 0.5) +
  labs(#title = "Teste de Hipótese nula e Estimativa de Média de População em Situação de Rua por Agrupamento de RA's por renda (IC 95%)",
       subtitle = "Grupo 4: Baixa-renda") +
  labs(x = "", y = "") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme_minimal()

plot_layout(g1 + g2 + g3 + g4)

#Resumo da hipótese nula
#Teste da hipótese nula por variável escolhida
visualize(null_ativos) + 
  shade_p_value(observed_fit, direction = "both")
#entendendo o gráfico:
#a linha vermelha é a média de ativos
#a parte sombreada é o p_value - a parte signifitiva

#Teste da hipótese nula do conjunto das variáveis nos grupos de renda, com IC
plot_layout(g1 + g2 + g3 + g4)

# testes para elaboração da lista 5
check_model(modelo_conf_norm)
coefplot(modelo_conf_norm)

tidy(mod_abordagem)
summary(modelo_conf)
summary(modelo_conf_norm)
tidy(modelo_valid)
vif_valid
vif_treino
check_model(modelo_conf_norm)
check_model(modelo_valid)

summary(modelo_valid)
par(mfrow=c(2,2)) 
plot(modelo_valid)

summary(modelo_treino)
par(mfrow=c(2,2)) 
plot(modelo_treino) 

vif(modelo_treino)
