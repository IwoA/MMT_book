library(readxl)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(plotly)
# -----------------------------------------
# Wykres 1 Saldo transferów z budżetu UE do Polski a inflacja CPI 
dane1 <- read_excel("rysunki\\transfery_z_UE.xlsx", range = "A1:E19")
cor(dane1$`Saldo transferów z UE`, dane1$CPI, method = "pearson")

# ggplot(dane1,group=Rok) + geom_line(aes(x=Rok, y=`Saldo transferów z UE`), color="#8DA0CB", size=1.5) +
#      geom_line(aes(x=Rok, y=CPI), color="#FC8D62", size=1.5) + 
#      annotate("text",x=2013, y=0.005, color="#8DA0CB", size = 6, hjust=1, label = "Saldo transferów z UE (% PKB)",fontface = "bold")+
#      annotate("text",x=2006, y=0.035, color="#FC8D62", size = 6, label = "Inflacja CPI", fontface = "bold")+
#      ggthemes::theme_hc() + 
#      scale_y_continuous(labels = scales::percent) + xlab("") + ylab("")

an1 <- list(
     x = dane1$Rok[3],
     y = dane1$CPI[1],
     text = "<b>Inflacja CPI</b>",
     xref = "x",
     yref = "y",
     showarrow = FALSE,
     ax = -0,
     ay = 20,
     font = list(color = "#ff7f0e", size = 16)
)
an2 <- list(
     x = dane1$Rok[5],
     y = 0.005,
     text = "<b>Saldo transferów z UE (% PKB)</b>",
     xref = "x",
     yref = "y",
     showarrow = FALSE,
     ax = -0,
     ay = 20,
     font = list(color = "#1f77b4", size = 16)
)

plot_ly(dane1, x = ~Rok) %>% 
     add_trace(y = ~`Saldo transferów z UE`, name = 'Saldo transferów z UE (% PKB)',type = 'scatter', mode = 'lines+markers') %>% 
     add_trace(y = ~CPI, name = 'Inflacja CPI',type = 'scatter', mode = 'lines+markers') %>% 
     layout(legend = list(x = 0.1, y = 0.1),
            yaxis = list(tickformat = '.2%', title = "", range = c(-0.01,0.06), zeroline = FALSE),
            xaxis = list(title = "", showgrid = F),
            annotations = list(an1, an2),
            showlegend = FALSE)
     
# -----------------------------------------    
# Wykres 2 
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
population <- wbstats::wb_data("SP.POP.TOTL")#read.csv2("rysunki\\population1.csv")
#pop <- population %>% group_by(country) %>% summarise(pop=mean(pop))
pop <- population %>% filter(date==max(population$date))

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

write.csv2(plt, "rysunki\\m0_dochody.csv")

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

# -----------------------------------------
# Tabela 1
# K27; G423; P2622 (dochody własne); dochody podatkowe - podatek od nieruchomości
# K27; G423; P2621 (dochody ogółem)
library(bdl)
get_variables("P2621")
podatek <- get_data_by_variable("76077", year = 2021, unitLevel = 6) %>% filter(attrId==1 & year == 2021)
dochody <- get_data_by_variable("76037", year = c(2020,2021), unitLevel = 6) %>% filter(attrId==1 & year == 2021)
udzial <- left_join(podatek[,1:4], dochody[,1:4], by = c( "id" = "id")) %>% 
     mutate(udzial = val.x/val.y) %>% 
     filter(!is.nan(udzial)) %>% 
     select(name.x, udzial)
write.csv2(udzial, "rysunki\\pod_od_nieruchomosci.csv")

an1 <- list(
     x = 0,
     y = quantile(udzial$udzial)[2],
     text = paste(round(quantile(udzial$udzial)[2]*100, digits = 2),"%, Jedna czwarta gmin"),
     xref = "x",
     yref = "y",
     xanchor = 'middle',
     showarrow = FALSE,
     arrowhead = 6,
     ay = 0,
     font = list(color = "#1F77B4")
)
an2 <- list(
     x = 0,
     y = median(udzial$udzial),
     text = paste(round(median(udzial$udzial)*100, digits = 2), "%, Połowa gmin"),
     xref = "x",
     yref = "y",
     xanchor = 'middle',
     showarrow = FALSE,
     arrowhead = 6,
     ay = 0,
     font = list(color = "#1F77B4")
)
an3 <- list(
     x = 0,
     y = quantile(udzial$udzial)[4],
     text = paste(round(quantile(udzial$udzial)[4]*100, digits = 2),"%, Trzy czwarte gmin"),
     xref = "x",
     yref = "y",
     xanchor = 'middle',
     showarrow = FALSE,
     arrowhead = 6,
     ay = 0,
     font = list(color = "#1F77B4")
)
plot_ly(y = udzial$udzial, type = "box", name = "",
        text = paste(as.character(round(udzial$udzial*100, digits = 2)), "%\n", as.character(udzial$name.x)),
        hoverinfo = "text") %>% 
     layout(
          yaxis = list(tickformat = '.2%', zeroline = FALSE),
          annotations = list(an1, an2, an3)
     )
# -----------------------------------------
# wykres 3 dochody budżetowe

# https://api.dane.gov.pl/media/resources/20211109/20211108_Zalaczniki_do_zmiany_UB_2021.zip

dane <- read_excel("rysunki\\dochody_budz_2021.xlsx", skip = 11)
dane <- dane %>% select(1, 7) %>% 
     rename_with(~c("Podatek", "kwota")) %>% 
     filter(!is.na(Podatek)) %>% 
     filter(Podatek!="Dochody niepodatkowe") %>% 
     mutate(kwota = kwota/1000) #w mln zł

# wersja do ksiązki
# dane %>% mutate(Podatek = ifelse(kwota<10000, "Pozostałe", Podatek)) %>% group_by(Podatek) %>% summarise(kwota = sum(kwota)) %>% 
# ggplot(aes(area = kwota, fill = Podatek, label = Podatek)) + 
#      geom_treemap(start = "topleft") +
#      geom_treemap_text(colour = "white", place = "centre", reflow = TRUE, start = "topleft") +
#      theme(legend.position="none")+
#      scale_fill_viridis_d()


kolory <- c(brewer.pal(8, "Blues"), brewer.pal(7, "Purples"), "#c90076")
rev <- cbind(dane, kolory)
names(rev) <- c("name", "value","color")#highchart must have data frame with these names. It takes values accordingly.
library(highcharter)
highchart() %>%
     hc_title(text = "Dochody budżetowe w 2021 r. (mln zł)",
              style = list(fontSize = "15px")) %>% 
     hc_chart(type = "treemap") %>% 
     hc_add_series(rev)


# ------------------------------------------
# Wykres 4
dane4 <- readxl::read_excel("rysunki\\podatki.xlsx", sheet = 4) %>% 
     mutate(across(where(is.double), ~round(.,digits = 3)))

plot_ly(dane4, x = ~Rok) %>% 
     add_trace(y = ~`Udział podatku od dochodów kapitałowych w przychodach z PIT`, name = 'Udział podatku od dochodów kapitałowych w przychodach z PIT',type = 'scatter', mode = 'lines+markers') %>% 
     add_trace(y = ~`Udział podatników składających PIT-38*`, name = 'Udział podatników składających PIT-38*',type = 'scatter', mode = 'lines+markers') %>% 
     layout(legend = list(x = 0.1, y = 0.1),
            yaxis = list(tickformat = '.2%', title = "", range = c(0,0.035)),
            xaxis = list(title = ""))

# ------------------------------------------
# Wykres 5
dane4$Rok <- as.numeric(dane4$Rok)

an1 <- list(
     x = dane4$Rok[1],
     y = dane4$`Podatnicy w II przedziale podatkowym`[1],
     text = paste(dane4$`Podatnicy w II przedziale podatkowym`[1]*100, "%"),
     xref = "x",
     yref = "y",
     showarrow = TRUE,
     arrowhead = 6,
     arrowcolor = "#1F77B4",
     ax = -0,
     ay = 20,
     font = list(color = "#1F77B4")
)
an2 <- list(
     x = dane4$Rok[nrow(dane4)],
     y = dane4$`Podatnicy w II przedziale podatkowym`[nrow(dane4)],
     text = paste(dane4$`Podatnicy w II przedziale podatkowym`[nrow(dane4)]*100, "%"),
     xref = "x",
     yref = "y",
     showarrow = TRUE,
     arrowhead = 6,
     arrowcolor = "#1F77B4",
     ax = 0,
     ay = -20,
     font = list(color = "#1F77B4")
)

plot_ly(dane4, x = ~Rok, hoverinfo = 'text',
        text = ~paste0(Rok, ", ", `Podatnicy w II przedziale podatkowym`*100, " %")) %>% 
     add_trace(y = ~`Podatnicy w II przedziale podatkowym`, name = 'Podatnicy w II przedziale podatkowym',type = 'scatter', mode = 'lines') %>% 
     layout(legend = list(x = 0.1, y = 0.1),
            yaxis = list(tickformat = '.1%', showticklabels = FALSE, title = "", range = c(0,0.06), zeroline = FALSE),
            xaxis = list(title = ""),
            annotations = list(an1, an2))
