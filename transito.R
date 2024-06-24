#Transito da Pop Rua ----
##Regulação do Transito da População em Situação de Rua pelo Estado no DF em 2023
####As políticas públicas constragem a liberdade de ir e vir da Pop Rua? 

#Pacotes ----
library(pacman)
p_load("tidyverse", "tidyr", "haven", "lubridate","janitor",
       "readxl", "stringr", "magrittr", "psych", "gapminder","scales",
       "rmarkdown","writexl","infer","patchwork")

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
transito <- left_join(transito, grupo_renda)
transito <- left_join(transito, densidade_ipedf)

#Organizar Transito
transito %<>% #Formatando objeto para formato tidy
  filter(!is.na(lote)) %>%
  filter(!is.na(pontos)) %>%
  select(lote,regional,ra,grupo_renda,ano,mes,populacao,densidade,
         pontos,ativos,atendidos,centropop_pessoas,creas_poprua,cadunico,abordagens,kit_refeicao,
         aux_vulnerab,ecr,upa,residuos,coleta_seletiva,ouvidoria,bo,obitos,drogas_kg)

###MUTATE ----
####Razão ----
transito %<>% #Calculando razão em relação à população da RA, com 4 décimos
  mutate(pontos_razao = round(pontos / populacao *100,5)) %>%
  mutate(ativos_razao = round(ativos / populacao *100,5)) %>%
  mutate(cadunico_razao = round(cadunico / populacao *100,5)) %>%
  mutate(abordagem_razao = round(abordagens / populacao *100,5)) %>%
  mutate(atendidos_razao = round(atendidos / populacao *100,5)) %>%
  mutate(refeicao_razao = round(kit_refeicao / populacao *100,5)) %>%
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
         refeicao_razao,abordagem_razao,cadunico_razao,
         aux_vul_razao,ecr_razao,upa_razao,residuos_razao,seletiva_razao,
         tx_coleta,ouvidoria_razao,ocorrencias_razao,obitos_razao,drogas_razao)

#####Grupos_razao ----
######RA ----
transito_ra <- transito %>%
  group_by(ra,densidade,ano,mes,centropop_razao,creas_razao,
           refeicao_razao,abordagem_razao,cadunico_razao,
           aux_vul_razao,ecr_razao,upa_razao,residuos_razao,seletiva_razao,
           tx_coleta,ouvidoria_razao,ocorrencias_razao,obitos_razao,
           drogas_razao) %>%
  summarise(mean_ativos_razao = mean(ativos_razao)) %>%
  select(ra,densidade,ano,mes,mean_ativos_razao,centropop_razao,creas_razao,
         refeicao_razao,abordagem_razao,cadunico_razao,
         aux_vul_razao,ecr_razao,upa_razao,residuos_razao,seletiva_razao,
         tx_coleta,ouvidoria_razao,ocorrencias_razao,obitos_razao,drogas_razao)

#####Renda ----
transito_renda <- transito %>%
  group_by(grupo_renda,densidade,ano,mes,centropop_razao,creas_razao,
           refeicao_razao,abordagem_razao,cadunico_razao,
           aux_vul_razao,ecr_razao,upa_razao,residuos_razao,seletiva_razao,
           tx_coleta,ouvidoria_razao,ocorrencias_razao,obitos_razao,
           drogas_razao) %>%
  summarise(mean_ativos_razao = mean(ativos_razao)) %>%
  select(grupo_renda,densidade,ano,mes,mean_ativos_razao,centropop_razao,creas_razao,
         refeicao_razao,abordagem_razao,cadunico_razao,
         aux_vul_razao,ecr_razao,upa_razao,residuos_razao,seletiva_razao,
         tx_coleta,ouvidoria_razao,ocorrencias_razao,obitos_razao,drogas_razao)

#####Regional ----
transito_regional <- transito %>%
  group_by(regional,densidade,ano,mes,centropop_razao,creas_razao,
           refeicao_razao,abordagem_razao,cadunico_razao,
           aux_vul_razao,ecr_razao,upa_razao,residuos_razao,seletiva_razao,
           tx_coleta,ouvidoria_razao,ocorrencias_razao,obitos_razao,
           drogas_razao) %>%
  summarise(mean_ativos_razao = mean(ativos_razao)) %>%
  select(regional,densidade,ano,mes,mean_ativos_razao,centropop_razao,creas_razao,
         refeicao_razao,abordagem_razao,cadunico_razao,
         aux_vul_razao,ecr_razao,upa_razao,residuos_razao,seletiva_razao,
         tx_coleta,ouvidoria_razao,ocorrencias_razao,obitos_razao,drogas_razao)

#####Lote ----
transito_lote <- transito %>%
  group_by(lote,densidade,ano,mes,centropop_razao,creas_razao,
           refeicao_razao,abordagem_razao,cadunico_razao,
           aux_vul_razao,ecr_razao,upa_razao,residuos_razao,seletiva_razao,
           tx_coleta,ouvidoria_razao,ocorrencias_razao,obitos_razao,
           drogas_razao) %>%
  summarise(mean_ativos_razao = mean(ativos_razao)) %>%
  select(lote,densidade,ano,mes,mean_ativos_razao,centropop_razao,creas_razao,
         refeicao_razao,abordagem_razao,cadunico_razao,
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

p_renda <- transito_renda %>%
  group_by(grupo_renda) %>%
  filter(!is.na(grupo_renda)) %>%
  ggplot(aes(x = grupo_renda,y=mean_ativos_razao, color = grupo_renda)) +
  geom_point()+
  labs(x="",y= "Média da Razão de Ativos") +
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
  ggplot(aes(x=coleta_seletiva,y=mean_ativos_razao)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0) +
  labs(x="Razão da Coleta Seletiva (1:100)",y= "Média da Razão de Ativos (1:100)") +
  labs(title = "Relação da Razão de Pessoas em Situação de Rua com Grupos de Renda no Distrito Federal no ano 2023",
       subtitle = "Média da Razão de Pessoas em Situação de Rua Ativos vs Média da Razão de Coleta Seletiva, em relação à População das RA's por grupos de renda",
       caption = "Elaborado por Hernany Castro, em 20/06/2024") +
  facet_grid(~grupo_renda) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, size = 8,vjust = 0.5,hjust = 1))


###CORRELAÇÃO ----
####ANOVA ----
#Testando correlação das variáveis independentes com a VD "Pessoas Ativas"
#VI's Assistência Social
anova_as <- aov(mean_ativos_razao ~ cadunico_razao + 
                      abordagem_razao + 
                      refeicao_razao + 
                      aux_vul_razao + 
                      creas_razao + 
                      centropop_razao, 
                    data = transito_renda)
summary(anova_as)

#VI's SLU
anova_slu <- aov(mean_ativos_razao ~ seletiva_razao +
                   residuos_razao + 
                   tx_coleta,
                          data = transito_renda)
summary(anova_slu)

#VI's Saúde
anova_saude <- aov(mean_ativos_razao ~ ecr_razao +
                         upa_razao, 
                       data = transito_renda)
summary(anova_saude)

#VI's Ouvidoria
anova_ouv <- aov(mean_ativos_razao ~ ouvidoria_razao, 
                       data = transito_renda)
summary(anova_ouv)

#VI's Policia
anova_bo <- aov(mean_ativos_razao ~ ocorrencias_razao +
                         drogas_razao + obitos_razao, 
                       data = transito_renda)
summary(anova_bo)

#VI's Populacionais
anova_densidade <- aov(mean_ativos_razao ~ densidade, 
                       data = transito_renda)
summary(anova_densidade)

####Coef.Cronbach ----
#####Alpha Positivo ----
#Testando a consistência dos modelos
#Assistência Social
alpha_as <- psych::alpha(transito_renda[, c("cadunico_razao", 
                                   "abordagem_razao",
                                   "creas_razao", 
                                   "refeicao_razao",
                                   #"aux_vul_razao", #retirar aumenta consisitência
                                   "centropop_razao"
                                   )])
print(alpha_as)

#SLU
alpha_slu <- psych::alpha(transito_renda[, c("seletiva_razao",
                                             "residuos_razao"
                                             )])
print(alpha_slu)

#Saúde
alpha_saude <- psych::alpha(transito_renda[, c("ecr_razao",
                                             "upa_razao"
                                             )])
print(alpha_saude)

#Policia
alpha_bo <- psych::alpha(transito_renda[, c("ocorrencias_razao", 
                                       "drogas_razao", 
                                       "obitos_razao"
                                       )])
print(alpha_bo)

#Teste da Proposta de modelo
alpha_modelo <- psych::alpha(transito_renda[, c("residuos_razao",
                                                  "seletiva_razao",
                                                  "abordagem_razao",
                                                  "ecr_razao"#,
                                                  #"densidade"
                                                )])
print(alpha_modelo)


##Visualização ----

##Modelando ----
#Testando Hipótese nula
observed_fit <- transito_renda %>%
  specify(mean_ativos_razao ~ residuos_razao + 
            seletiva_razao +
            abordagem_razao +
            ecr_razao + 
            densidade
            ) %>%
  fit()

bootstrap_distribution <- transito_renda %>%
  specify(mean_ativos_razao ~ residuos_razao + 
            seletiva_razao +
            abordagem_razao +
            ecr_razao + # propõe que razão de ativos é nulo
            densidade
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
  specify(mean_ativos_razao ~ residuos_razao + 
            seletiva_razao +
            abordagem_razao +
            ecr_razao #+ densidade
          ) %>%
  hypothesize(null = "point", mu = 0) %>% #sugere a hipótese que ativos_prop é igual a zero
  generate(reps = 1000, type = "bootstrap") %>%
  #calculate(stat = "mean") #compara médias
    fit()

null_ativos

get_confidence_interval(
  bootstrap_distribution, 
  point_estimate = observed_fit, 
  level = .95
)

visualize(null_ativos) + 
  shade_p_value(observed_fit, direction = "both")
#entendendo o gráfico:
#a linha vermelha é a média de ativos
#a parte sombreada é o p_value - a parte signifitiva

#Estimativa de Média de Ativos com IC de 95%
#Transito_renda
observed_fit_g1 <- transito_renda %>%
  filter(grupo_renda == "g1_alta_renda") %>%
  specify(mean_ativos_razao ~ residuos_razao + 
            seletiva_razao +
            abordagem_razao +
            ecr_razao #+ densidade
          ) %>%
  fit()

bootstrap_distribution_g1 <- transito_renda %>%
  filter(grupo_renda == "g1_alta_renda") %>%
  specify(mean_ativos_razao ~ residuos_razao + 
            seletiva_razao +
            abordagem_razao +
            ecr_razao #+ densidade # propõe que razão de ativos é nulo
  ) %>% #especifica a VD e a VI ou VI's
  generate(reps = 1000, type = "bootstrap") %>% #bootstraping x1000
  fit() 

null_ativos_g1 <- transito_renda %>% 
  filter(grupo_renda == "g1_alta_renda") %>%
  specify(mean_ativos_razao ~ residuos_razao + 
            seletiva_razao +
            abordagem_razao +
            ecr_razao #+ densidade
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
  scale_x_continuous() +
  scale_y_continuous() +
  annotate("point", x = mean(bootstrap_distribution_g1$estimate), y = 0, size = 5, color = "blue") +
  annotate("segment", x = mean(bootstrap_distribution_g1$estimate), xend = data_ci_g1$lower_ci, y = 0, yend = 0, color = "blue", size = 3, alpha = 0.5) +
  annotate("segment", x = mean(bootstrap_distribution_g1$estimate), xend = data_ci_g1$upper_ci, y = 0, yend = 0, color = "blue", size = 3, alpha = 0.5) +
  labs(title = "Teste de Hipótese nula e Estimativa de Média de População em Situação de Rua por Agrupamento de RA's por renda (IC 95%)",
       subtitle = "Grupo 1: renda alta") +
  labs(x = "", y = "Densidade") +
  theme_minimal()

observed_fit_g2 <- transito_renda %>%
  filter(grupo_renda == "g2_media_alta_renda") %>%
  specify(mean_ativos_razao ~ residuos_razao + 
            seletiva_razao +
            abordagem_razao +
            ecr_razao #+ densidade
          ) %>%
  fit()

bootstrap_distribution_g2 <- transito_renda %>%
  filter(grupo_renda == "g2_media_alta_renda") %>%
  specify(mean_ativos_razao ~ residuos_razao + 
            seletiva_razao +
            abordagem_razao +
            ecr_razao #+ densidade # propõe que razão de ativos é nulo
  ) %>% #especifica a VD e a VI ou VI's
  generate(reps = 1000, type = "bootstrap") %>% #bootstraping x1000
  fit() 

null_ativos_g2 <- transito_renda %>% 
  filter(grupo_renda == "g2_media_alta_renda") %>%
specify(mean_ativos_razao ~ residuos_razao + 
          seletiva_razao +
          abordagem_razao +
          ecr_razao #+ densidade
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
  scale_x_continuous() +
  scale_y_continuous() +
  annotate("point", x = mean(bootstrap_distribution_g2$estimate), y = 0, size = 5, color = "blue") +
  annotate("segment", x = mean(bootstrap_distribution_g2$estimate), xend = data_ci_g2$lower_ci, y = 0, yend = 0, color = "blue", size = 3, alpha = 0.5) +
  annotate("segment", x = mean(bootstrap_distribution_g2$estimate), xend = data_ci_g2$upper_ci, y = 0, yend = 0, color = "blue", size = 3, alpha = 0.5) +
  labs(#title = "Teste de Hipótese nula e Estimativa de Média de População em Situação de Rua por Agrupamento de RA's por renda (IC 95%)",
       subtitle = "Grupo 2: Média alta renda") +
  labs(x = "", y = "") +
  theme_minimal()

observed_fit_g3 <- transito_renda %>%
  filter(grupo_renda == "g3_media_baixa_renda") %>%
  specify(mean_ativos_razao ~ residuos_razao + 
            seletiva_razao +
            abordagem_razao +
            ecr_razao #+ densidade
          ) %>%
  fit()

bootstrap_distribution_g3 <- transito_renda %>%
  filter(grupo_renda == "g3_media_baixa_renda") %>%
  specify(mean_ativos_razao ~ residuos_razao + 
            seletiva_razao +
            abordagem_razao +
            ecr_razao #+ densidade # propõe que razão de ativos é nulo
  ) %>% #especifica a VD e a VI ou VI's
  generate(reps = 1000, type = "bootstrap") %>% #bootstraping x1000
  fit() 

null_ativos_g3 <- transito_renda %>% 
  filter(grupo_renda == "g3_media_baixa_renda") %>%
specify(mean_ativos_razao ~ residuos_razao + 
          seletiva_razao +
          abordagem_razao +
          ecr_razao #+ densidade
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
  scale_x_continuous() +
  scale_y_continuous() +
  annotate("point", x = mean(bootstrap_distribution_g3$estimate), y = 0, size = 5, color = "blue") +
  annotate("segment", x = mean(bootstrap_distribution_g3$estimate), xend = data_ci_g3$lower_ci, y = 0, yend = 0, color = "blue", size = 3, alpha = 0.5) +
  annotate("segment", x = mean(bootstrap_distribution_g3$estimate), xend = data_ci_g3$upper_ci, y = 0, yend = 0, color = "blue", size = 3, alpha = 0.5) +
  labs(#title = "Teste de Hipótese nula e Estimativa de Média de População em Situação de Rua por Agrupamento de RA's por renda (IC 95%)",
       subtitle = "Grupo 3: Média baixa renda") +
  labs(x = "", y = "Densidade") +
  theme_minimal()

observed_fit_g4 <- transito_renda %>%
  filter(grupo_renda == "g4_baixa_renda") %>%
  specify(mean_ativos_razao ~ residuos_razao + 
            seletiva_razao +
            abordagem_razao +
            ecr_razao #+ densidade
          ) %>%
  fit()

bootstrap_distribution_g4 <- transito_renda %>%
  filter(grupo_renda == "g4_baixa_renda") %>%
  specify(mean_ativos_razao ~ residuos_razao + 
            seletiva_razao +
            abordagem_razao +
            ecr_razao #+ densidade # propõe que razão de ativos é nulo
  ) %>% #especifica a VD e a VI ou VI's
  generate(reps = 1000, type = "bootstrap") %>% #bootstraping x1000
  fit() 

null_ativos_g4 <- transito_renda %>% 
  filter(grupo_renda == "g4_baixa_renda") %>%
specify(mean_ativos_razao ~ residuos_razao + 
          seletiva_razao +
          abordagem_razao +
          ecr_razao #+ densidade
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
  scale_x_continuous() +
  scale_y_continuous() +
  annotate("point", x = mean(bootstrap_distribution_g4$estimate), y = 0, size = 5, color = "blue") +
  annotate("segment", x = mean(bootstrap_distribution_g4$estimate), xend = data_ci_g4$lower_ci, y = 0, yend = 0, color = "blue", size = 3, alpha = 0.5) +
  annotate("segment", x = mean(bootstrap_distribution_g4$estimate), xend = data_ci_g4$upper_ci, y = 0, yend = 0, color = "blue", size = 3, alpha = 0.5) +
  labs(#title = "Teste de Hipótese nula e Estimativa de Média de População em Situação de Rua por Agrupamento de RA's por renda (IC 95%)",
       subtitle = "Grupo 4: baixa renda") +
  labs(x = "", y = "") +
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
