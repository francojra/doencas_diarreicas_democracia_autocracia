
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
library(ggthemes)

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

diarr3 <- diarr %>%
  filter(Entity %in% c("United States", "China")) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

c4a("safe", 6)

ggplot(diarr1, aes(x = fct_reorder(Entity, media), y = media, 
                   fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                size = 0.8, width = 0.2) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                               "#332288", "#AA4499")) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  scale_x_discrete(labels = c("Japão", "Alemanha", "Estados Unidos",
                              "Coreia do Norte", "Cuba", "China")) +
  labs(x = "Países", y = "Taxa de mortes por doenças diarréicas") +
  theme_ipsum(axis_title_size = 16, axis_text_size = 14) +
  theme(legend.position = "none", 
        axis.text = element_text(color = "black"))

ggplot(diarr2, aes(x = Year, y = taxa_mort_diar, 
                   group = Entity, color = Entity)) +
  geom_point(shape = 15, size = 2.5) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                               "#332288", "#AA4499"),
                     labels = c("China", "Cuba", "Alemanha",
                                "Japão", "Coreia do Norte", "Estados Unidos")) +
  labs(x = "Tempo (anos)", y = "Taxa de mortes por doenças diarréicas",
       color = "Países") +
  theme_ipsum(axis_title_size = 16, axis_text_size = 14) +
  theme(axis.text = element_text(color = "black"))

c4a("dark2", 2)

ggplot(diarr3, aes(x = Year, y = taxa_mort_diar, 
                  group = Entity, col = Entity)) +
  geom_line(size = 2.2) +
  scale_color_manual(values = c("#1B9E77", "#D95F02"),
                     labels = c("China", "Estados Unidos")) +
  labs(x = "Tempo (anos)", y = "Taxa de mortes por doenças diarréicas", 
       color = "Países") +
  theme_hc() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(color = "black", size = 15),
        legend.text = element_text(size = 12))










