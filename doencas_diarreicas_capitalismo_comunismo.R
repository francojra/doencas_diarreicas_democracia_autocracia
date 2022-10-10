
# Doenças diarréicas em países capitalistas e comunistas -----------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 09/10/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/diarrheal-diseases --------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### Dados estimam que 1,6 milhões de pessoas morreram devido à doenças diarréicas em 2017. Doenças diarréicas
### estão entre as principais causas de morte global. Nesse registro nós observamos o peso dessa doença
### e o que podemos fazer para reduzir ela.

### A taxa de doenças diarréicas expressa o número de mortes por ano a cada 100 mil indivíduos.
### Essas taxas tem sido padronizadas por idade da população entre países e ao longo do tempo. 
### Isso nos permite fazer comparações entre países e ao longo do tempo de forma a não ser afetado
### por mudanças na estrutura de idade.

### Na década de 1990 as taxas de doenças diarréicas eram muito mais elevadas comparado a atualmente.

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

diarr <- read.csv("diarrheal-disease-death-rates.csv")
view(diarr)
names(diarr)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

diarr <- diarr %>%
  select(-Code) %>%
  rename(taxa_mort_diar = Deaths...Diarrheal.diseases...Sex..Both...Age..Age.standardized..Rate.) %>%
  view()

diarr1 <- diarr %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "China", "Cuba", "North Korea")) %>%
  group_by(Entity) %>%
  summarise(media = mean(taxa_mort_diar),
            sd = sd(taxa_mort_diar), n = n(),
            se = sd/sqrt(n)) %>%
  view()

diarr2 <- diarr %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "China", "Cuba", "North Korea")) %>%
  view()
