
# ctrl + shift + r para criar uma secao
# Carregando os pacotes ---------------------------------------------------

library(tidyverse)
library(readxl)
library(readr)
library(skimr)
library(ggrepel)
library(GGally)
library(car)
library(rstatix)
library(summarytools)
library(psych)
library(gt)
library(DataExplorer)


# Leitura dos dados  ------------------------------------------------------

GEM_APS <- read_excel("02_dados/GEM_APS.xlsx", 
                      col_types = c("numeric", "text", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric"))


# Funcao glimpse ----------------------------------------------------------

glimpse(GEM_APS)

# usando a funcao select 
# o pipe |> ou %>% (atalho ctrl + shift + m)


GEM_APS_selecionadas <- 
  GEM_APS |> 
  select(code, economy, year, 
         tea, female_male_TEA)

GEM_APS_selecionadas |> 
  head()


# Funcao filter  ----------------------------------------------------------


Gem_brazil <- GEM_APS |> 
  filter(economy == "Brazil")

Gem_brazil_argentina <- 
  GEM_APS |> 
  filter(economy == "Brazil" | 
         economy == "Argentina")

america_sul <- c("Brazil","Argentina","Chile",
              "Colombia","Venezuela", "Peru")

Gem_aps_america <- 
  GEM_APS |> 
  filter(economy %in% america_sul)

## filtrar brasil e argentina e valores abaixo de < 2020

gem_br_arg_menor_2020 <- 
  GEM_APS |> 
  filter((economy == "Brazil" | 
         economy == "Argentina") & year < 2020)

# Funcao mutate -----------------------------------------------------------

GEM_aps_perc <- 
  GEM_APS |> 
  select(economy, year, female_male_TEA) |> 
  mutate(perc_fem_male_tea = female_male_TEA * 100) |> 
  filter(year >= 2010 & economy == "Brazil") |> 
  arrange(desc(year))

# Funcoes group by e summarise --------------------------------------------

unique(GEM_APS$economy)

americas <- c("Brazil","Chile","Uruguay","Colombia","Canada",
              "Ecuador","Mexico","United States","Venezuela")

GEM23_americas <- 
  GEM_APS |> 
  filter(year == 2023) |> 
  filter(economy %in% americas) |> 
  mutate(hemisferio = 
           if_else(economy == "Mexico"|
                 economy == "Canada"|
                 economy == "United States","Norte","Sul"),
         .after = economy)

GEM23_americas |> 
  group_by(hemisferio) |> 
  summarise(media = mean(tea),
            mediana = median(tea),
            desv_pad = sd(tea))


# joins -------------------------------------------------------------------

gem_aps19 <- read_delim("https://raw.githubusercontent.com/danielppagotto/r_unicamp/refs/heads/main/02_dados/gem_2019_aps.csv", 
                        delim = ";",
                        show_col_types = FALSE)

wgi <- read_delim("https://github.com/danielppagotto/r_unicamp/raw/refs/heads/main/02_dados/wgi.csv",
                  delim = ",",
                  show_col_types = FALSE) |> 
  select(-`...1`)


glimpse(gem_aps19)
glimpse(wgi)

# inner_join como exemplo 

juncao_inner <- 
  wgi |> 
  inner_join(gem_aps19,
             by = c("code"="abrev")) |> 
  select(-year, -cod_pais, -code)

juncao_left <- 
  wgi |> 
    left_join(gem_aps19,
              by = c("code"="abrev"))


# analise descritiva ------------------------------------------------------

juncao_inner |> 
  select(economy, 
         continent,  
         entrepreneurship_as_good_carrer_choice,
         tea,
         rule_of_law,
         regulatory_quality,
         political_stability,
         voice_accountability) |> 
  skim()
  
# Usando o pacote psych e gt

describe(juncao_inner) |> gt()

# usando o pacote summarytools

view(dfSummary(juncao_inner))

# usando o pacote Dataexplorer

DataExplorer::create_report(juncao_inner)
