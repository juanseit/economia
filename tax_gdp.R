library(tidyverse)
library(ggthemes)
library(wbstats)
library(ggrepel)
options(scipen = 9999)

indicadores <- c("tax" = "GC.TAX.TOTL.GD.ZS",
                 "gdp" = "NY.GDP.PCAP.PP.CD")

df_bank <- wb_data(
  country = "all",
  indicadores,
  start_date = 1960, end_date = 2020,
)

df_bank %>%
  filter(!is.na(gdp),!is.na(tax)) %>%
  group_by(country) %>%
  summarize(gdp_medio = mean(gdp),tax_medio=mean(tax)) %>%
  ungroup() %>%
  lm(gdp_medio ~ tax_medio, data =.) %>%
  summary()

df_bank %>%
  filter(!is.na(gdp),!is.na(tax)) %>%
  group_by(country) %>%
  summarize(gdp_medio = mean(gdp),tax_medio=mean(tax)) %>%
  ungroup() %>%
  mutate(label = case_when(
    country == "Brazil" | country == "China" |  country == "South Africa" |  country == "Russian Federation" |  country == "India" |
      country == "United States" ~ country
  )) %>%
  ggplot(mapping = aes(x=tax_medio,y=gdp_medio)) +
  geom_point(color = '#014d64') +
  geom_text_repel(aes(label = label)) +
  stat_smooth(method = "lm", se = FALSE, color = "#56B4E9", linetype = "dashed") +
  labs(x = "Receita de impostos (% do PIB).",
       y = "PIB per capita (PPP, preços atuais).",
       title = "Não há correlação entre carga tributária e crescimento.",
       subtitle = "PIB per capita e Receita dos impostos, em média, entre 1960 e 2020.",
       caption = "Feito por Juan Iturvide | Data by World Bank.") +
  theme_economist() +
  theme(axis.title.x = element_text(face = "bold",margin=margin(12,0,-12,0)),
        axis.title.y = element_text(face = "bold",margin=margin(0,12,0,0)),
        plot.title = element_text(face = "bold", hjust=0,size=18,margin=margin(-7,0,10,0)),
        plot.subtitle = element_text(hjust=0,size=12, margin=margin(-2,0,3,0)),
        plot.margin = margin(1,1.5,2,0.5, 'cm'),
        plot.caption = element_text(size=9, hjust = 0,margin=margin(34,0,-45,0)))

ggsave("world_bank.png", height = 9, width = 12, dpi = 300)
