library(rdbnomics)
library(dplyr)
library(stringr)
library(countrycode)

## tax revenues
# in domestic currency
rev_currency <- rdb(api_link = "https://api.db.nomics.world/v22/series/IMF/GFSR?dimensions=%7B%22CLASSIFICATION%22%3A%5B%22W0_S1_G11%22%5D%2C%22UNIT_MEASURE%22%3A%5B%22XDC%22%5D%2C%22REF_SECTOR%22%3A%5B%22S13%22%5D%7D&observations=1")
# as % of GDP
rev_gdp <- rdb(api_link = "https://api.db.nomics.world/v22/series/IMF/GFSR?dimensions=%7B%22CLASSIFICATION%22%3A%5B%22W0_S1_G11%22%5D%2C%22REF_SECTOR%22%3A%5B%22S13%22%5D%2C%22UNIT_MEASURE%22%3A%5B%22XDC_R_B1GQ%22%5D%7D&observations=1")

## monetary base
# in domestic currency
base_currency <- rdb(api_link = "https://api.db.nomics.world/v22/series/IMF/IFS?dimensions=%7B%22FREQ%22%3A%5B%22A%22%5D%2C%22INDICATOR%22%3A%5B%22FASMB_XDC%22%5D%7D&observations=1")

## GDP in domestic currency
gdp <- rdb(api_link = "https://api.db.nomics.world/v22/series/IMF/IFS?dimensions=%7B%22FREQ%22%3A%5B%22A%22%5D%2C%22INDICATOR%22%3A%5B%22NGDP_XDC%22%5D%7D&observations=1")
gdp <- gdp %>% select(REF_AREA, `Reference Area`, period, value)

## GDP per capita in USD
gdp_pc_usd <- rdb(api_link = "https://api.db.nomics.world/v22/series/IMF/WEO:2021-04?dimensions=%7B%22unit%22%3A%5B%22us_dollars%22%5D%2C%22weo-subject%22%3A%5B%22NGDPDPC%22%5D%7D&observations=1")
gdp_pc_usd <- gdp_pc_usd %>% select(`weo-country`, `WEO Country`, period, value)
colnames(gdp_pc_usd) <- c("code", "country", "period", "GDP_cap")
gdp_pc_usd$country <- if_else(gdp_pc_usd$country=="Russia", "Russian Federation", gdp_pc_usd$country)
gdp_pc_usd$country <- if_else(gdp_pc_usd$country=="Tanzania", "Tanzania, United Republic of", gdp_pc_usd$country)
gdp_pc_usd$country <- if_else(gdp_pc_usd$country=="Kyrgyz Republic", "Kyrgyzstan", gdp_pc_usd$country)
gdp_pc_usd$country <- if_else(gdp_pc_usd$country=="Korea", "Korea, Republic of", gdp_pc_usd$country)
gdp_pc_usd$country <- if_else(gdp_pc_usd$country=="Korea", "Korea, Republic of", gdp_pc_usd$country)
gdp_pc_usd$country <- if_else(gdp_pc_usd$country=="Moldova", "Moldova, Republic of", gdp_pc_usd$country)
gdp_pc_usd$country <- if_else(gdp_pc_usd$country=="North Macedonia", "North Macedonia, Republic of", gdp_pc_usd$country)
gdp_pc_usd$country <- if_else(gdp_pc_usd$country=="Macao SAR", "Macao", gdp_pc_usd$country)
gdp_pc_usd$country <- if_else(gdp_pc_usd$country=="Hong Kong SAR", "Hong Kong, China", gdp_pc_usd$country)
gdp_pc_usd$country <- if_else(gdp_pc_usd$country=="Côte d'Ivoire", "Cote d`Ivoire", gdp_pc_usd$country)

## Population
population <- read.csv2("rysunki\\population1.csv")
pop <- population %>% group_by(country) %>% summarise(pop=mean(pop))

## Obliczenia

rev <- rev_gdp %>% select(REF_AREA, `Reference Area`, period, value) %>% group_by(REF_AREA, `Reference Area`) 

monetary <- base_currency %>% select(REF_AREA, `Reference Area`, period, value) 
monetary_gdp <- left_join(monetary, gdp, by = c("REF_AREA" = "REF_AREA", "period" = "period")) %>% 
     filter(!is.na(value.x) & !is.na(value.y)) %>% 
     mutate(M0 = value.x/value.y) %>% 
     select(REF_AREA, `Reference Area.x`, period, M0) %>% 
     rename(country = `Reference Area.x`) 

razem <- left_join(rev, monetary_gdp, by = c("REF_AREA" = "REF_AREA", "period" = "period")) %>% 
     filter(!is.na(value) & !is.na(M0)) %>% 
     group_by(REF_AREA, country)

razem_gdp <- left_join(razem, gdp_pc_usd, by=c("country" = "country", "period" = "period")) %>% 
     select(-`Reference Area`, -code) %>% 
     summarise(M0_mean=mean(M0, na.rm = TRUE), revenues = mean(value, na.rm=TRUE), GDP_cap=mean(GDP_cap)) %>% 
     mutate(M0_mean = round(M0_mean, digits = 3),
            revenues = round(revenues, digits = 3))

razem_gdp$GDP_cap_b <- case_when(
     razem_gdp$GDP_cap <=3000 ~ "najmniej rozwinięte",
     razem_gdp$GDP_cap >3000 & razem_gdp$GDP_cap<=9000 ~ "rozwijające się",
     razem_gdp$GDP_cap >9000 & razem_gdp$GDP_cap<=17000 ~ "rozwinięte",
     razem_gdp$GDP_cap>17000 ~ "bogate"
)

razem_gdp$country <- countrycode(razem_gdp$country, origin = 'country.name.en', destination = 'cldr.name.pl')

razem_gdp %>% filter(REF_AREA !="HK" & REF_AREA !="CG" & GDP_cap_b == "najmniej rozwinięte") %>% ggplot(aes(revenues, M0_mean)) + geom_point(size=2) + 
     geom_smooth(method = "glm", se = FALSE) + 
     ggrepel::geom_text_repel(aes(label=country), size = 4, show.legend=FALSE) + 
     #scale_color_viridis_d() +
     geom_vline(xintercept=as.numeric(razem_gdp[razem_gdp$country=="Egipt","revenues"]), color = "red") +
     geom_text(aes(x=14.2, y = -0.01, label = paste(as.character(round(razem_gdp[razem_gdp$country=="Egipt","revenues"]), 2), "%")), color = "red", size = 4)+
     theme_minimal(base_size = 14)+
     labs(caption = "Źródło: MFW") + xlab("Dochody budżetowe jako % of PKB") + ylab("M0 jako % PKB")

plt <- razem_gdp %>% filter(REF_AREA !="HK" & REF_AREA !="CG" & GDP_cap_b == "najmniej rozwinięte")
plt$country <- ifelse(plt$country == "Côte d’Ivoire", "Wybrzeże \n Kości Słoniowej", plt$country)
plt$country <- ifelse(plt$country == "Mjanma (Birma)", "Mjanma \n (Birma)", plt$country)

write.csv2(plt, "budz_wyk2.csv")

fit <- lm(M0_mean ~ revenues, data = plt) %>% fitted.values()

vline <- function(x = 0, color = "green") {
     list(
          type = "line",
          y0 = 0,
          y1 = 1,
          yref = "paper",
          x0 = x,
          x1 = x,
          line = list(color = color, dash="dot")
     )
}
an <- list(
     x = 13,
     y = 0.02,
     text = "13 %",
     xref = "x",
     yref = "y",
     showarrow = TRUE,
     arrowhead = 6,
     arrowcolor = "green",
     ax = 20,
     ay = -20,
     font = list(color = "green")
)

plot_ly() %>% 
     add_trace(x = plt$revenues, y=plt$M0_mean, type = "scatter", mode = "markers") %>%
     add_trace(x = plt$revenues, y = fit, type = "scatter", mode = "lines") %>%
     add_text(x = plt$revenues+.5, y=plt$M0_mean, text = plt$country, textposition = 'right') %>% 
     layout(
          xaxis = list(title = "Dochody budżetowe jako % of PKB", range = c(5,41)),
          yaxis = list(title = "M0 jako % PKB"),
          shapes = list(vline(13)),
          showlegend = FALSE,
          annotations = list(an)
     )

