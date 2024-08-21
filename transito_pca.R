#Transito da Pop Rua ----
##Regulação do Transito da População em Situação de Rua pelo Estado no DF em 2023
####As políticas públicas constragem a liberdade de ir e vir da Pop Rua? 

#Pacotes ----
library(pacman)
p_load("tidyverse", "dplyr","tidyr", "haven", "lubridate","janitor",
       "readxl", "stringr", "magrittr", "psych", "gapminder","scales",
       "rmarkdown","writexl","infer","patchwork","car","ggpubr","tidymodels",
       "stats","jtools","huxtable","lmtest","tolerance","car","performance","arm",
       "lmtest","FactoMineR","factoextra","corrplot")

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
                                   "jun","jul","ago","set","out","nov","dez")) 

abordagem <- left_join(abordagem,residuos_lotes)

####B. Ocorrência ----
ocorrencia <- pivot_longer(bo, #Formatando objeto para formato tidy
                           names_to = "mes",
                           values_to = "bo",
                           cols = c("jan","fev","mar","abr","mai",
                                    "jun","jul","ago","set","out","nov","dez"))

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
                                  "jun","jul","ago","set","out","nov","dez"))

####Coleta_Seletiva ----
coleta_seletiva <- pivot_longer(coleta_seletiva,#Formatando objeto para formato tidy
                                names_to = "mes",
                                values_to = "coleta_seletiva",
                                cols = c("jan","fev","mar","abr","mai",
                                         "jun","jul","ago","set","out","nov","dez"))

####Creas ----
creas <- left_join(creas_rma,creas_ra) %>% #Unindo RMA com Regionalização do Creas
  filter(!is.na(creas_poprua))

creas <- left_join(creas,residuos_lotes)

####Centro Pop ----
centro_pop <- left_join(centropop,centropop_ra)

centro_pop <- left_join(centro_pop,residuos_lotes)

####CadÚnico ----
cadunico <- pivot_longer(cadunico, #Formatando objeto para formato tidy
                         names_to = "mes",
                         values_to = "cadunico",
                         cols = c("jan","fev","mar","abr","mai",
                                  "jun","jul","ago","set","out","nov","dez"))

cadunico <- left_join(cadunico,residuos_lotes)

####Seg. Alimentar ----
seg_alim <- pivot_longer(refeicoes, #Formatando objeto para formato tidy
                         names_to = "mes",
                         values_to = "kit_refeicao",
                         cols = c("jan","fev","mar","abr","mai",
                                  "jun","jul","ago","set","out","nov","dez"))

###JOIN - Trânsito----
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

#Organizar Transito
###MUTATE ----
####Razão ----
transito %<>% #Calculando razão em relação à população da RA, com 4 décimos
  #mutate(pontos_razao = round(pontos / populacao *100,5)) %>%
  mutate(ativos_razao = round(ativos / populacao *100,5)) %>%
  mutate(cadunico_razao = round(cadunico / populacao *100,5)) %>%
  mutate(abordagem_razao = round(abordagens / populacao *100,5)) %>%
  #mutate(atendidos_razao = round(atendidos / populacao *100,5)) %>%
  mutate(refeicao_razao = round(kit_refeicao / populacao *100,5)) %>%
  #mutate(pratocheio_razao = round(prato_cheio / populacao *100,5)) %>%
  mutate(aux_vul_razao = round(aux_vulnerab / populacao *100,5)) %>%
  #mutate(ecr_razao = round(ecr / populacao *100,5)) %>%
  #mutate(upa_razao = round(upa / populacao *100,5)) %>%
  mutate(residuos_razao = round(residuos / populacao *100,5)) %>%
  mutate(seletiva_razao = round(coleta_seletiva / populacao *100,5)) %>%
  mutate(ouvidoria_razao = round(ouvidoria / populacao *100,5)) %>%
  mutate(ocorrencias_razao = round(bo / populacao *100,5)) %>%
  # mutate(obitos_razao = round(obitos / populacao *100,5)) %>%
  mutate(drogas_razao = round(drogas_kg / populacao *100,5)) %>%
  mutate(centropop_razao = round(centropop_pessoas / populacao *100,5)) %>%
  mutate(creas_razao = round(creas_poprua / populacao *100,5)) #%>%
#mutate(tx_coleta = round(coleta_seletiva / residuos *100,2))

#### Normalização ----
transito <- transito %>%
  mutate_at(vars(densidade, ativos_razao:drogas_razao), scale)

##### Grupos_razao ----
##### Renda ----
transito_renda <- transito %>%
  drop_na() %>%
  group_by(grupo_renda,ano,mes,centropop_razao,creas_razao,
           refeicao_razao,abordagem_razao,cadunico_razao,
           aux_vul_razao,residuos_razao,seletiva_razao,
           ouvidoria_razao,ocorrencias_razao,#tx_coleta,
           drogas_razao) %>%
  summarise(mean_ativos_razao = mean(ativos_razao))

###Visualizando ----
# Distribuição de Ativos por grupos de RA
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

## PCA ----
### Subconjunto ----
transito_sub <- transito_renda %>%
  drop_na()

transito_sub <- transito_sub[c(1, 4, 5:15)]

head(transito_sub[, 1:7], 10)

#Testenado KMO

KMO(transito_sub[,2:13])

### Normalização ----
#transito_sub[,2:14] <- scale(transito_sub[,2:14])

### Computação PCA ----
transito_pca <- PCA(transito_sub[,2:12], graph = TRUE)

### Resultados ----
print.PCA(transito_pca)

### Visualização e Interpretação ----
eig.val <- get_eigenvalue(transito_pca)
eig.val

### Correlação e p-valor ----
dimdesc(transito_pca)

### Gráfico das Variáveis ----
var <- get_pca_var(transito_pca)
var

fviz_eig(transito_pca, addlabels = TRUE, ylim = c(0, 50))

fviz_pca_var(transito_pca, col.var = "black")

### Círculo de Correlação ----
head(var$cos2,15)
fviz_pca_var(transito_pca, col.var = "black")

### Qualidade da Representação ----
corrplot(var$cos2, is.corr=FALSE)

### Contribuição das Variáveis ----
corrplot(var$contrib, is.corr=FALSE)

fviz_contrib(transito_pca, choice = "var", axes = 1, top = 10)

fviz_contrib(transito_pca, choice = "var", axes = 2, top = 10)

fviz_contrib(transito_pca, choice = "var", axes = 3, top = 10)

### Gráfico Indivíduos ----
ind <- get_pca_ind(transito_pca)
ind

fviz_pca_ind(transito_pca,geom.ind = "point",
             col.ind = transito_sub$grupo_renda, 
             pointsize = 3,
             alpha = 0.5,
             legend.title = "Grupos")

### Gráfico Grupos ----
fviz_pca_ind(transito_pca,,
             geom.ind = "point",
             col.ind = transito_sub$grupo_renda,
             pointsize = 3,
             alpha = 0.5,
             addEllipses = TRUE,
             legend.title = "Grupos")

# Dim1: eixo de intolerância; Dim2: eixo de rejeição

# Reporte para trabalho final

#Exemplo de Estrutura de um Relatório:

##Introdução
##Metodologia
#Preparação dos dados
#Realização da PCA
#Seleção dos componentes principais
##Resultados
#Autovalores e variância explicada
#Carregamentos dos componentes
#Gráficos de biplot e scree plot
##Discussão
#Interpretação dos componentes
#Limitações da análise
##Conclusões
