library(readxl)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(plotly)
# -----------------------------------------
# Wykres 1 Saldo transferów z budżetu UE do Polski a inflacja CPI 
dane1 <- read_excel("rysunki\\transfery_z_UE.xlsx", range = "A1:E18")
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
            yaxis = list(tickformat = '.2%', title = "", range = c(-0.01,0.05), zeroline = FALSE),
            yaxis = list(title = ""),
            xaxis = list(title = "", showgrid = F),
            annotations = list(an1, an2),
            showlegend = FALSE)
     
# -----------------------------------------    
# Wykres 2 
# Jako obraz z prezentacji


# -----------------------------------------
# Tabela 1
# K27; G423; P2622 (dochody własne); dochody podatkowe - podatek od nieruchomości
# K27; G423; P2621 (dochody ogółem)

# -----------------------------------------
# wykres 3 dochody budżetowe

# https://api.dane.gov.pl/media/resources/20211109/20211108_Zalaczniki_do_zmiany_UB_2021.zip

dane <- read_excel("rysunki\\dochody_budz_2021.xlsx", skip = 11)
dane <- dane %>% select(1, 7) %>% 
     rename_with(~c("Podatek", "kwota")) %>% 
     filter(!is.na(Podatek)) %>% 
     filter(Podatek!="Dochody niepodatkowe") %>% 
     mutate(kwota = kwota/1000) #w mln zł


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
dane4 <- read_excel("rysunki\\podatki.xlsx", sheet = 4) %>% 
     mutate(across(where(is.double), ~round(.,digits = 3)))

plot_ly(dane4, x = ~Rok) %>% 
     add_trace(y = ~`Udział podatku od dochodów kapitałowych w przychodach z PIT`, name = 'Udział podatku od dochodów kapitałowych w przychodach z PIT',type = 'scatter', mode = 'lines+markers') %>% 
     add_trace(y = ~`Udział podatników składających PIT-38*`, name = 'Udział podatników składających PIT-38*',type = 'scatter', mode = 'lines+markers') %>% 
     layout(legend = list(x = 0.1, y = 0.1),
            yaxis = list(tickformat = '.2%', title = "", range = c(0,0.03)),
            yaxis = list(title = ""),
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
     arrowcolor = "blue",
     ax = -0,
     ay = 20,
     font = list(color = "blue")
)
an2 <- list(
     x = dane4$Rok[nrow(dane4)],
     y = dane4$`Podatnicy w II przedziale podatkowym`[nrow(dane4)],
     text = paste(dane4$`Podatnicy w II przedziale podatkowym`[nrow(dane4)]*100, "%"),
     xref = "x",
     yref = "y",
     showarrow = TRUE,
     arrowhead = 6,
     arrowcolor = "blue",
     ax = 0,
     ay = -20,
     font = list(color = "blue")
)

plot_ly(dane4, x = ~Rok, hoverinfo = 'text',
        text = ~paste0(Rok, ", ", `Podatnicy w II przedziale podatkowym`*100, " %")) %>% 
     add_trace(y = ~`Podatnicy w II przedziale podatkowym`, name = 'Podatnicy w II przedziale podatkowym',type = 'scatter', mode = 'lines') %>% 
     layout(legend = list(x = 0.1, y = 0.1),
            yaxis = list(tickformat = '.1%', showticklabels = FALSE, title = "", range = c(0,0.05), zeroline = FALSE),
            yaxis = list(title = ""),
            xaxis = list(title = ""),
            annotations = list(an1, an2))
