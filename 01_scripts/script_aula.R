
# ctrl + shift + r para criar uma secao
# Carregando os pacotes ---------------------------------------------------

library(tidyverse)
library(ggplot2)
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
library(writexl)
library(haven)

# esse comando serve para voce acessar informacoes sobre a funcao
#?write_xlsx()


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

# Regepe - Pagotto & Borges 

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

# o link para outros pacotes de analise exploratoria de dados
# https://www.business-science.io/code-tools/2024/10/03/top-10-r-packages-for-eda.html


# como salvar uma base já tratada? ----------------------------------------

writexl::write_xlsx(juncao_inner, 
                    "03_outputs/exercicio.xlsx")

write.csv(juncao_inner,
          "03_outputs/exercicios.csv")

foreign::write.dta(juncao_inner,
                   "03_outputs/exercicios.dta")

haven::write_sav(juncao_inner,
                 "03_outputs/exercicios.sav")

# Visualização de dados ---------------------------------------------------


# histograma --------------------------------------------------------------

hist(gem_aps19$entrepreneurship_as_good_carrer_choice)

ggplot(data = gem_aps19,
       aes(x = entrepreneurship_as_good_carrer_choice)) +
  geom_histogram()

ggplot(data = gem_aps19,
       aes(x = entrepreneurship_as_good_carrer_choice)) +
  geom_histogram(bins = 20, 
                 fill = "#0b3748",
                 color = "black") + 
  xlab("Entrepreneurship as good carrer choice") +
  ylab("Frequência") +
  ggtitle("Variável do estudo","Fonte: GEM - 2019") +
  theme_minimal()


mean_val <- mean(gem_aps19$entrepreneurship_as_good_carrer_choice, na.rm = TRUE)
sd_val <- sd(gem_aps19$entrepreneurship_as_good_carrer_choice, na.rm = TRUE)

# Criar o gráfico com o histograma e adicionar a curva normal
ggplot(data = gem_aps19, 
       aes(x = entrepreneurship_as_good_carrer_choice)) +
  geom_histogram(aes(y = ..density..), bins = 20, 
                 fill = "#0b3748", color = "black") + 
  stat_function(fun = dnorm, args = list(mean = mean_val, sd = sd_val), 
                color = "red", size = 1) +
  xlab("Entrepreneurship as good career choice") +
  ylab("Density") +
  ggtitle("Variável do estudo", "Fonte: GEM - 2019") +
  theme_minimal()


# boxplot -----------------------------------------------------------------


grafico <- gem_aps19 |> 
      rename(Continente = continent) |> 
      ggplot(aes(y = entrepreneurship_as_good_carrer_choice,
               fill = Continente)) +
      geom_boxplot() + 
      ylab("Entrepreneurship as good carrer of choice") + 
      ggtitle("Variável de estudo","Fonte: GEM - 2019") +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            legend.position = "bottom",
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12))

ggsave("03_outputs/grafico1.svg", grafico, dpi = 300,
       height = 4, width = 5)


# grafico de linhas -------------------------------------------------------




# extra: tidyplots - https://tidyplots.org/
# https://jbengler.github.io/tidyplots/authors.html#citation