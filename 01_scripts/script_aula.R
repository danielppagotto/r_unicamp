

# Carregando os pacotes ---------------------------------------------------

library(tidyverse)
library(readxl)
library(readr)
library(skimr)
library(ggrepel)
library(GGally)
library(car)
library(rstatix)


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


GEM_APS |> 
  select(economy, year, female_male_TEA) |> 
  mutate(perc_fem_male_tea = female_male_TEA * 100) |> 
  filter(year >= 2010 & economy == "Brazil") |> 
  arrange(desc(year))



# Funcoes group by e summarise --------------------------------------------




# joins -------------------------------------------------------------------


