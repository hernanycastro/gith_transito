#Transito da Pop Rua ----
##Regulação do Transito da População em Situação de Rua pelo Estado no DF em 2023
####As políticas públicas constragem a liberdade de ir e vir da Pop Rua? 

#Pacotes ----
library(pacman)
p_load("tidyverse", "tidyr", "haven", "lubridate","janitor",
       "readxl", "stringr", "magrittr", "psych", "gapminder","scales",
       "rmarkdown","writexl")

#Importacao ----
populacao_ibge <- readxl::read_xlsx("populacao_df_ra.xlsx") #População RA's (Censo IBGE 2022)
pontos <- readxl::read_xlsx("as_seas_pontos.xlsx") #Concentração de Pop Rua (SEDES/LAI)
saude_ecr <- readxl::read_xlsx("saude_ecr.xlsx") #Consultório na Rua (SES/LAI)
bo <- readxl::read_xlsx("pcdf_regionais.xlsx") #Boletim de Ocorrências (PCDF/LAI)
ouvidoria <- readxl::read_xlsx("ouv_dist.xlsx") #Ouvidorias (CGDF/LAI)
saude_upa <- readxl::read_xlsx("saude_upa.xlsx") #UPA (SES/LAI)
residuos_lotes <- readxl::read_xlsx("lotes_residuos.xlsx") #SLU (Site SLU)
residuos_slu <- readxl::read_xlsx("residuos.xlsx") #SLU (Site SLU)
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

####Pessoas_ativos ----
ativos <- pivot_longer(seas_ativos, #Formatando objeto para formato tidy
                             names_to = "mes",
                             values_to = "ativos",
                             cols = c("jan","fev","mar","abr","mai",
                                      "jun","jul","ago","set","out","nov","dez")) %>%
  arrange(ra,ano,mes,ativos)

####Pessoas_atendidos ----
atendidos <- pivot_longer(seas_atendidos, #Formatando objeto para formato tidy
                       names_to = "mes",
                       values_to = "atendidos",
                       cols = c("jan","fev","mar","abr","mai",
                                "jun","jul","ago","set","out","nov","dez")) %>%
  arrange(ra,ano,mes,atendidos)

####Abordagem/SEAS ----
abordagem <- pivot_longer(seas_abordagem, #Formatando objeto para formato tidy
               names_to = "mes",
               values_to = "abordagens",
               cols = c("jan","fev","mar","abr","mai",
                        "jun","jul","ago","set","out","nov","dez")) %>%
  select(ra,ano,mes,abordagens)


####B. Ocorrência ----
ocorrencia <- pivot_longer(bo, #Formatando objeto para formato tidy
                           names_to = "mes",
                           values_to = "bo",
                           cols = c("jan","fev","mar","abr","mai",
                                    "jun","jul","ago","set","out","nov","dez")) %>%
  select(ra,regional,ano,mes,bo)

####Ouvidorias ----
ouvidorias <- pivot_longer(ouvidoria, #Formatando objeto para formato tidy
                           names_to = "mes",
                           values_to = "ouvidoria",
                           cols = c("jan","fev","mar","abr","mai",
                                    "jun","jul","ago","set","out","nov","dez")) %>%
  arrange(ra, regional,ano,mes, ouvidoria)

####Upa ----
upa <- pivot_longer(saude_upa, #Formatando objeto para formato tidy
                           names_to = "mes",
                           values_to = "upa",
                           cols = c("jan","fev","mar","abr","mai",
                                    "jun","jul","ago","set","out","nov","dez")) %>%
  arrange(ra,ano,mes,upa)

####Residuos ----
residuos <- pivot_longer(residuos_slu,#Formatando objeto para formato tidy
                    names_to = "mes",
                    values_to = "residuos",
                    cols = c("jan","fev","mar","abr","mai",
                             "jun","jul","ago","set","out","nov","dez")) %>%
  arrange(lote,ano,mes,residuos)

####Creas ----
creas <- left_join(creas_rma,creas_ra) %>% #Unindo RMA com Regionalização do Creas
  filter(!is.na(creas_poprua)) %>%
  select(ano,mes,ra,creas_poprua)

####Centro Pop ----
centro_pop <- left_join(centropop,centropop_ra) %>% #Unindo RMA com Regionalização do Centro_pop
  select(ano,mes,ra,centropop_pessoas)

####CadÚnico ----
cadunico <- pivot_longer(cadunico, #Formatando objeto para formato tidy
               names_to = "mes",
               values_to = "cadunico",
               cols = c("jan","fev","mar","abr","mai",
                        "jun","jul","ago","set","out","nov","dez")) %>%
  select(ra,ano,mes,cadunico)

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

#Organizar Transito
transito %<>% #Formatando objeto para formato tidy
  filter(!is.na(lote)) %>%
  filter(!is.na(pontos)) %>%
  select(lote,regional,ra,ano,mes,populacao,
         pontos,ativos,atendidos,centropop_pessoas,creas_poprua,cadunico,abordagens,kit_refeicao,
         aux_vulnerab,ecr,upa,residuos,ouvidoria,bo,obitos,drogas_kg)

###MUTATE ----
####Proporções ----
transito %<>% #Criando proporções em relação à população da RA x 1000, com 4 décimos
  mutate(pontos_prop = round(pontos / populacao * 1000,4)) %>%
  mutate(ativos_prop = round(ativos / populacao * 1000,4)) %>%
  mutate(cadunico_prop = round(cadunico / populacao * 1000,4)) %>%
  mutate(abordagem_prop = round(abordagens / populacao * 1000,4)) %>%
  mutate(atendidos_prop = round(atendidos / populacao * 1000,4)) %>%
  mutate(refeicao_prop = round(kit_refeicao / populacao * 1000,4)) %>%
  mutate(aux_vul_prop = round(aux_vulnerab / populacao * 1000,4)) %>%
  mutate(ecr_prop = round(ecr / populacao * 1000,4)) %>%
  mutate(upa_prop = round(upa / populacao * 1000,4)) %>%
  mutate(residuos_prop = round(residuos / populacao * 1000,4)) %>%
  mutate(ouvidoria_prop = round(ouvidoria / populacao * 1000,4)) %>%
  mutate(ocorrencias_prop = round(bo / populacao * 1000,4)) %>%
  mutate(obitos_prop = round(obitos / populacao * 1000,4)) %>%
  mutate(drogas_prop = round(drogas_kg / populacao * 1000,4)) %>%
  mutate(centropop_prop = round(centropop_pessoas / populacao * 1000,4)) %>%
  mutate(creas_prop = round(creas_poprua / populacao * 1000,4)) %>%
  select(lote,regional,ra,ano,mes,populacao,
         pontos_prop,ativos_prop,atendidos_prop,centropop_prop,creas_prop,
         refeicao_prop,abordagem_prop,cadunico_prop,
         aux_vul_prop,ecr_prop,upa_prop,residuos_prop,
         ouvidoria_prop,ocorrencias_prop,obitos_prop,drogas_prop)

###Visualizando ----
#VD vs VI's sob "Proporção" como Unidade de Medida e Variando os Níveis de Agregação 
p_cnr <- transito %>% #Criando objeto com o ggplot para visualizar uma amostra dos dados 
  filter(!is.na(ecr_prop)) %>%
  ggplot(aes(y = pontos_prop, x = ecr_prop, color = lote)) + #Colorir a maior unidade de agregação (lote)
  geom_point() + #geom_point para visualizar possível correlação das proporções
  geom_smooth(method = "lm", size = 0.1, alpha = 0.0) + #geom_smooth para visualizar possível tendência linear
  facet_grid(~ ra, drop = F) + #facet_grid() para criar subpainéis segundo a menor unidade de agregação (RA)
  labs(x = "Atendimentos CnR (1:1000)", #Usando escala linear no eixo x
       y = "Pontos de Concentração (1:1000)") + #Usando escala linear no eixo y
  labs(title = "Relação do Consultório na Rua (CnR) com os Pontos de Concentração da População em Situação de Rua no DF em 2023",
       subtitle = "Proporção de Pontos de Concentração vs Proporção de Atendimentos do Cnr, em relação à População das RA's do DF (Censo IBGE 2022)",
       caption = "Fonte dos dados: Ouvidoria do GDF") +
  theme_minimal() + #Usando tema "minimal"
  theme(legend.title = element_blank(), #retirando o título da legenda do ggplot
        legend.position = "bottom") #posicionando a legenda centralizada e abaixo do ggplot

p_cnr #visualizando o gráfico

###CORRELAÇÃO ----
####ANOVA ----
#Testando correlação das variáveis independentes com a VD de "Pessoas Ativas" por RA/Mês
modelo_anova <- aov(ativos_prop ~ ra + mes + 
                      residuos_prop + 
                      ouvidoria_prop + 
                      ocorrencias_prop + 
                      drogas_prop +
                      obitos_prop + 
                      ecr_prop +
                      #upa_prop + inviabiliza a análise
                      cadunico_prop + 
                      atendidos_prop +
                      #ativos_prop +
                      abordagem_prop + 
                      refeicao_prop + 
                      aux_vul_prop + 
                      creas_prop + 
                      centropop_prop, 
                    data = transito)

# Resumo da ANOVA
summary(modelo_anova)

####Coef.Cronbach ----
#####Alpha Positivo ----
#Testando a consistência do modelo de fixação da Pop Rua
alpha <- psych::alpha(transito[, c("residuos_prop", 
                                   #"ecr_prop",
                                   "cadunico_prop", 
                                   #"drogas_prop",
                                   #ouvidorias_prop",
                                   #ocorrencias_prop",
                                   "abordagem_prop",
                                   "creas_prop", 
                                   "refeicao_prop",
                                   "aux_vul_prop",
                                   "centropop_prop"
                                   )])
#ecr_prop está correlacionado negativamente com essas variáveis
#drogas_prop reduz o coeficiente
#residuos_prop e abordagem_prop elevam o coeficiente para 0.7
print(alpha)

#####Alpha Negativo ----
#Testando a consistência do modelo de nomadismo da Pop Rua
alpha_neg <- psych::alpha(transito[, c("residuos_prop", 
                                       #"ouvidoria_prop",
                                       "refeicao_prop"#, 
                                       #"ecr_prop",
                                       #"cadunico_prop",
                                       #"upa_prop",
                                       #"ocorrencias_prop", 
                                       #"drogas_prop"#, 
                                       #"obitos_prop",
                                       )])
#ecr_prop está correlacionado negativamente com essas variáveis
#"residuos_prop" e "refeicao_pro" tem melhor coeficiente (0.69)
#apesar do alto coeficiente não é confiável pelo alto sd de "refeicao_pro"
#drogas_prop mantém o valor do coeficiente
#retirar "ouvidorias_prop" e "ocorrencias_prop" aumenta o coeficiente (0.48)
print(alpha_neg)

#ecr_prop é independente em relação às demais variáveis?

##Visualização ----

##Modelando RM ----

transito$lote <- factor(transito$lote)
transito$ra <- factor(transito$ra)
transito$regional <- factor(transito$regional)

### Modelo ----
modelo <- lm(ativos_prop ~ ra + mes + 
               residuos_prop + 
               ouvidoria_prop + 
               ocorrencias_prop + 
               obitos_prop + 
               ecr_prop +
               cadunico_prop + 
               abordagem_prop + 
               refeicao_prop + 
               aux_vul_prop +
               drogas_prop +
               centropop_prop, 
                 data = transito)

# Resumo do modelo
summary(modelo)
plot(modelo)
qqnorm(modelo$residuals)

### Modelo_neg ----
modelo_neg <- lm(ativos_prop ~ ra + mes + 
                   residuos_prop + 
                   ouvidoria_prop + 
                   ocorrencias_prop + 
                   obitos_prop, 
             data = transito)

#Resumo do modelo_neg
summary(modelo_neg)
plot(modelo_neg)
qqnorm(modelo_neg$residuals)

### Modelo_pos ----
modelo_pos <- lm(pontos_prop ~ ra + mes + 
                   residuos_prop + 
                   cadunico_prop + 
                   abordagem_prop + 
                   refeicao_prop + 
                   aux_vul_prop, 
                 data = transito)

#Resumo Modelo_pos
summary(modelo_pos)
plot(modelo_pos)
qqnorm(modelo_pos$residuals)

###Modelo_ecr ----
modelo_ecr <- lm(ativos_prop ~ ra + mes + 
                   ecr_prop + 
                   residuos_prop, 
                 data = transito)

#Resumo do Modelo_ecr
summary(modelo_ecr)
plot(modelo_ecr)
qqnorm(modelo_ecr$residuals)

##Comunicando ----

#-----------------------------------------------------------------
#AVALIAR PARA A LISTA 4
data_ci <- transito %>%
  group_by(ra,ano,mes) %>%
  mutate( n = n(),
          prop = sum(ativos_prop) / n,
          se = sd(prop) / sqrt(prop),
          ci_lower = prop - 1.96 * se,
          ci_upper = prop + 1.96 * se) %>%
  select(ra,populacao,ano,mes,prop,se,ci_lower,ci_upper)

estimativa <- data_ci %>%
  filter(!se == "NaN") %>%
  filter(!is.na(prop)) %>%
  ggplot(aes(x = populacao,color = "red")) +
  geom_bar() +
  facet_grid(~ra) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
  labs(title = "Estimativa Intervalar da Proporção de Pessoas em Situação de Rua (ativos) nas Regiões Administrativas do Distrito Federal em 2023",
       subtitle = "Estimativa por Região Administrativa, com intervalo de confiança de 95%",
       caption = "Elaborado por Hernany Castro, em 06/2024, a partir de dados do Instituto Ipês (TC nº 04/2017)",
       x = "População da Região Administrativa (Censo IBGE 2022)",
       y = "IC Proporção de Pessoas em Situação de Rua (ativos) [1:10 = %]") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 200000, by = 50000)) +
  scale_y_continuous(limits = c(0,12, by = 3)) +
  theme(legend.key = element_blank(),
        legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, size = 8,vjust = 0.5,hjust = 1))

estimativa
#------------------------------------------------------------------------
