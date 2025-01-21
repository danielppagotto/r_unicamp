
# ctrl + shift + r para criar uma secao
# Carregando os pacotes ---------------------------------------------------

# caso o pacote nao esteja instalado ainda, instale usando o comando
# install.packages("tidyverse")

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
library(plotly)
library(sf)
library(rnaturalearth)

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

GEM23_americas <- 
  Gem_aps_america |> 
  filter(year == 2023)

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

x <- hist(gem_aps19$entrepreneurship_as_good_carrer_choice)

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

grafico

ggsave("03_outputs/grafico1.svg", grafico, dpi = 300,
       height = 4, width = 5)


# grafico de linhas -------------------------------------------------------

GEM_NES <- readr::read_delim("https://raw.githubusercontent.com/danielppagotto/r_unicamp/refs/heads/main/02_dados/GEM_NES.csv", 
           delim = ";", escape_double = FALSE, trim_ws = TRUE)

glimpse(GEM_NES)

br_chile <- GEM_NES |> 
                filter(economy == "Brazil" | economy == "Chile")


grafico2 <- 
  br_chile |> 
  filter(year >= 2005) |> 
  rename(País = economy) |> 
  ggplot(aes(x = year,
             y = governamental_support_policies,
             color = País)) + 
  geom_line(size = 1.5) +
  scale_x_continuous(breaks = seq(2005, 2023, by = 1)) +
  xlab("Ano") +
  ylab("Políticas de suporte ao empreendedorismo") +
  ggtitle("Percepção de especialistas sobre o suporte de políticas públicas ao \nempreendedorismo",
          "Fonte: NES-GEM") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90,
                                   hjust = 1)) 
  
plotly::ggplotly(grafico2)

# grafico de colunas ------------------------------------------------------


br_chile |> 
  filter(year >= 2005) |> 
  rename(País = economy) |> 
  ggplot(aes(x = year,
             y = governamental_support_policies,
             fill = País)) + 
  geom_col(position = position_dodge()) +
  geom_text(aes(label = governamental_support_policies),
            position = position_dodge(width = 0.9),
            angle = 90, 
            vjust = 0.3, 
            hjust = -0.3,
            size = 3) +
  scale_x_continuous(breaks = seq(2005, 2023, by = 1)) +
  scale_y_continuous(breaks = seq(0, 8, by = 1)) +
  xlab("Ano") +
  ylab("Políticas de suporte ao empreendedorismo") +
  ggtitle("Percepção de especialistas sobre o suporte de políticas públicas ao \nempreendedorismo") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90,
                                   hjust = 1)) + 
  coord_cartesian(ylim = c(0, 8)) 

# Gráfico de dispersão ----------------------------------------------------

grafico3 <- 
  juncao_inner |> 
  ggplot(aes(x = political_stability,
             y = entrepreneurship_as_good_carrer_choice)) +
  geom_point(size = 2, aes(col = continent)) +
  geom_text_repel(aes(label = economy),
                  max.overlaps = 20) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Estabilidade política") +
  ylab("Empreendedorismo como boa opção de carreira") +
  theme_minimal() + 
  facet_wrap(~continent)

grafico3

ggsave("03_outputs/grafico3.jpeg",grafico3,
       dpi = 500, height = 5, width = 8)



# Gráfico de diagrama de correlação ---------------------------------------

juncao_inner |> 
  select(tea, entrepreneurship_as_good_carrer_choice,
         rule_of_law, political_stability,
         regulatory_quality) |> 
  ggpairs()


juncao_inner  |> 
  ggcorr()


# mapa cloroplético -------------------------------------------------------

#geobr para baixar polígonos do Brasil

america_sul <- ne_countries(scale = "medium",
             continent = "South America",
             returnclass = "sf")

map_data <- america_sul |> 
  inner_join(GEM23_americas,
            by = c("sovereignt"="economy")) |> 
  filter(tea != "NA")


map_data |> 
  ggplot() + 
  geom_sf(data = america_sul, 
          fill = "grey70", color = "black") +
  geom_sf(aes(fill = tea)) +
  scale_fill_gradient(low = "lightgreen", 
                      high = "darkgreen", 
                      name = "TEA") +
  theme_minimal() +
  ggtitle("Total early stage entrepreneurial activity", 
          "Fonte: GEM (2023)")

# extra: tidyplots - https://tidyplots.org/
# https://jbengler.github.io/tidyplots/authors.html#citation

# onde aprender mais: 

# Curso de R do LAPEI: https://www.youtube.com/playlist?list=PLANciT1coAsTqQoPo5-3be0Qq07eU6ePI
# Datacamp: https://www.datacamp.com/


# ANOVA -------------------------------------------------------------------

juncao_inner |> 
  group_by(continent) |> 
  summarise(tea_medio = mean(tea))

modelo <- anova_test(formula = tea ~ continent, 
                     data = juncao_inner, 
                     effect.size = "ges")
modelo

modelo2 <- aov(formula = tea ~ continent, 
               data = juncao_inner)

summary(modelo2)

levene_test(tea ~ continent, data = juncao_inner)

shapiro.test(resid(modelo2))

hist(resid(modelo2))

# post-hoc test - onde existe diferencas significativas nos pares de grupos
TukeyHSD(modelo2)
