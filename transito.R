#Transito Pop Rua ----
## O que é? ----
#

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
b_geacaf <- readxl::read_xlsx("as_beneficios_geacaf_mes.xlsx") #Benefício Eventual (SEDES/LAI)
seas <- readxl::read_xlsx("as_seas_qnt_mes.xlsx") #Abordagem Social (SEDES/LAI)
seg_alim <- readxl::read_xlsx("seguranca_alimentar_mes.xlsx") #Kit Refeição (SEDES/LAI)
obitos <- readxl::read_xlsx("obitos.xlsx") #Obitos (PCDF/LAI)
cadunico <- readxl::read_xlsx("cadastro_unico_mes.xlsx") #CadÚnico (SEDES/LAI)

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
  arrange(ra, mes, pontos)

####B. Ocorrência ----
ocorrencia <- pivot_longer(bo, #Formatando objeto para formato tidy
                           names_to = "mes",
                           values_to = "bo",
                           cols = c("jan","fev","mar","abr","mai",
                                    "jun","jul","ago","set","out","nov","dez")) %>%
  select(ra,regional,mes,bo)

####Ouvidorias ----
ouvidorias <- pivot_longer(ouvidoria, #Formatando objeto para formato tidy
                           names_to = "mes",
                           values_to = "ouvidoria",
                           cols = c("jan","fev","mar","abr","mai",
                                    "jun","jul","ago","set","out","nov","dez")) %>%
  arrange(ra, regional,mes, ouvidoria)

####Upa ----
upa <- pivot_longer(saude_upa, #Formatando objeto para formato tidy
                           names_to = "mes",
                           values_to = "upa",
                           cols = c("jan","fev","mar","abr","mai",
                                    "jun","jul","ago","set","out","nov","dez")) %>%
  arrange(ra,mes,upa)

####Residuos ----
residuos <- pivot_longer(residuos_slu,#Formatando objeto para formato tidy
                    names_to = "mes",
                    values_to = "residuos",
                    cols = c("jan","fev","mar","abr","mai",
                             "jun","jul","ago","set","out","nov","dez")) %>%
  arrange(lote,mes,residuos)

####Abordagem/SEAS ----
abordagem <- seas %>%
  pivot_longer(!'DADOS GERAIS', #Formatando objeto para formato tidy
               names_to = "mes",
               values_to = "abordagens") %>%
  select(mes,abordagens) %>%
  group_by(mes) %>%
  summarise(abordagem = sum(abordagens,na.rm=T))

####Seg. Alimentar ----
seg_alim <- seg_alim %>%
  pivot_longer(!refeicao_kit, #Formatando objeto para formato tidy
               names_to = "mes",
               values_to = "kit_refeicao") %>%
  select(mes,kit_refeicao)

###JOIN - Trânsito----
transito <- left_join(ouvidorias,concentracao) #Unindo objetos com maior número de observações
transito <- left_join(transito,ocorrencia)
transito <- left_join(transito,upa)
transito <- left_join(transito,saude_ecr)
transito <- left_join(transito, populacao_ibge)
transito <- left_join(transito, residuos_lotes)
transito <- left_join(transito, residuos)
transito <- left_join(transito, abordagem)
transito <- left_join(transito, seg_alim)
transito <- left_join(transito, cadunico)
transito <- left_join(transito, obitos)
transito <- left_join(transito, b_geacaf)

#Organizar Transito
transito %<>% #Formatando objeto para formato tidy
  filter(!is.na(lote)) %>%
  filter(!is.na(pontos)) %>%
  select(lote,regional,ra,mes,populacao,
         pontos,cadunico,abordagem,kit_refeicao,aux_vulnerab,
         ecr,upa,
         residuos,ouvidoria,bo,obitos)

###MUTATE ----
####Proporções ----
transito %<>% #Criando proporções em relação à população da RA x 1000, com 4 décimos
  mutate(pontos_prop = round(pontos / populacao * 1000,4)) %>%
  mutate(cadunico_prop = round(cadunico / populacao * 1000,4)) %>%
  mutate(abordagem_prop = round(abordagem / populacao * 1000,4)) %>%
  mutate(refeicao_prop = round(kit_refeicao / populacao * 1000,4)) %>%
  mutate(aux_vul_prop = round(aux_vulnerab / populacao * 1000,4)) %>%
  mutate(ecr_prop = round(ecr / populacao * 1000,4)) %>%
  mutate(upa_prop = round(upa / populacao * 1000,4)) %>%
  mutate(residuos_prop = round(residuos / populacao * 1000,4)) %>%
  mutate(ouvidoria_prop = round(ouvidoria / populacao * 1000,4)) %>%
  mutate(ocorrencias_prop = round(bo / populacao * 1000,4)) %>%
  mutate(obitos_prop = round(obitos / populacao * 1000,4)) %>%
  select(lote,regional,ra,mes,
         pontos_prop,cadunico_prop,abordagem_prop,refeicao_prop,aux_vul_prop,
         ecr_prop,upa_prop,
         residuos_prop,ouvidoria_prop,ocorrencias_prop,obitos_prop)

###Visualizando ----
#VD vs VI's sob "Proporção" como Unidade de Medida e Variando os Níveis de Agregação 
p_cnr <- transito %>% #Criando objeto com o ggplot para visualizar uma amostra dos dados 
  filter(!is.na(ecr_prop)) %>%
  ggplot(aes(y = pontos_prop, x = ecr_prop, color = lote)) + #Colorir a maior unidade de agregação (lote)
  geom_point() + #geom_point para visualizar possível correlação das proporções
  geom_smooth(method = "lm", size = 0.1, alpha = 0.0) + #geom_smooth para visualizar possível tendência linear
  facet_grid(~ ra, drop = F) + #facet_wrap() para criar subpainéis segundo a menor unidade de agregação (RA)
  labs(x = "Atendimentos CnR (1:1000)", #Usando escala linear no eixo x
       y = "Pontos de Concentração (1:1000)") + #Usando escala linear no eixo y
  labs(title = "Relação do Consultório na Rua (CnR) com os Pontos de Concentração da População em Situação de Rua no DF em 2023",
       subtitle = "Proporção de Pontos de Concentração vs Proporção de Atendimentos do Cnr, em relação à População das RA's do DF (Censo IBGE 2022)",
       caption = "Fonte dos dados: Ouvidoria do GDF") +
  theme_minimal() + #Usando tema "minimal"
  theme(legend.title = element_blank(), #retirando o título do ggplot
        legend.position = "bottom") #posicionando a legenda centralizada e abaixo do ggplot

p_cnr #visualizando o gráfico

###CORRELAÇÃO ----
####Teste T ----

####Coef.Cronbach ----

##Visualização ----

##Modelando ----

##Comunicando ----

