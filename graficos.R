# Gráficos descritivos dos tempos totais dos processos de homicídio do Estado de São Paulo, distribuídos após 
# 1989-06-30 e anterior à 2014-03-12.

library(ggplot2)
library(plyr)
library(dplyr)
if(!require(jurimetria)) {
  if(!require(devtools)) install.packages("devtools")
  install_github("jurimetria", "juliotrecenti")
  require(jurimetria)
}

# baixa infos de processos julgados em primeiro grau dos assuntos relacionados a homicídio com auxílio do crawler_cjpg (consulta de julgado em primeiro grau do site do TJSP).
assuntos = "3370,3371,3372"
homicidios <- pega_metadados_tjsp(assuntos=assuntos)

# retira pontuação do número do processo para ser usado como chave no inner_join mais tarde.
homicidios$processo <- gsub("[[:punct:]]", "", homicidios$n_processo)

# datas das movimentações baixadas previamente em virtude da demora que leva para baixar diretamente do site do TJSP
dados_homicidios_mov <- read.csv2("https://raw.githubusercontent.com/abjur/mjdata/master/dados_homicidios_mov.csv", colClasses = c("character", "character"))

# Resume os tempos totais dos processos de cada Assunto
tempo_por_Assunto <- tbl_df(dados_homicidios_mov) %.%
  mutate(data = as.character(as.Date(data, format="%d/%m/%Y"))) %.%
  group_by(processo) %.%
  arrange(data) %.%
  summarise(tempo = as.Date(last(data)) - as.Date(first(data))) %.%
  inner_join(homicidios[,c("processo", "Classe", "Assunto")]) %.%
  ungroup() %.%
  filter(tempo<10000) %.%
  group_by(Assunto) %.%
  summarise(n=n(), min = min(tempo), media = mean(tempo), mediana = median(tempo), max = max(tempo), desvPad = sd(tempo))

tempo_por_assunto

# Estudo sobre o efeito no estimador do tempo processual total ao se fazer um corte temporal na amostra pela data de distribuição de processos

# BD de processos com datas do primeiro e último andamento e a duração entre eles
homicidios_disp <- tbl_df(dados_homicidios_mov) %.%
  mutate(data = as.character(as.Date(data, format="%d/%m/%Y"))) %.%
  arrange(data) %.%
  group_by(processo) %.%
  summarise(primeiro=first(data), ultimo=last(data), tempo=as.Date(last(data)) - as.Date(first(data)), primeiro_ano = str_extract(primeiro, "[0-9]{4}")) %.%
  filter(tempo<10000) %.%
  filter(primeiro >= '2008-01-01') %.%
  ungroup() %.%
  mutate(corte = primeiro < '2019-01-01')

#summarise(n=n(), min = min(tempo) media = mean(tempo), mediana = median(tempo), max = max(tempo), desvPad = sd(tempo))
annotate_bd <- with(homicidios_disp, tapply(tempo, primeiro_ano, median))*1.04
ggplot(homicidios_disp, aes(x=tempo)) +
  geom_density(aes(fill=primeiro_ano), alpha=.4) +
  annotate("text", label=names(annotate_bd), x = annotate_bd, y = 0.0015) +
  theme_bw()  

# O efeito do corte temporal parece que se extendederia para anos ainda mais antigos. 


