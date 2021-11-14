library(rio)
library(dplyr)
library(ggplot2)
library(plotly)

url <- "https://www.nbp.pl/statystyka/dwn/rez_y.xlsx"
dane <- rio::import(url, skip=3)
colnames(dane) <- c("rok", "waluty")
dane <- dane[14:nrow(dane),]
#dane1 <- dane %>% select(data, M1)

# dane o długu publicznym: https://www.gov.pl/web/finanse/zadluzenie-skarbu-panstwa
# dane archiwalne zapisane są w katalogu głównym w plikach zadłużenie skarbu państwa ...
debt <- data.frame("rok" = as.Date(as.character(2005:2020), format = "%Y"), 
                   "dług" = c(124.7,  126.2, 121.1, 149.9, 168.8, 194.8, 246.4, 250.9, 253.8, 276.9, 261.3, 319.5, 283.9, 279.8, 256.9, 266))

debt1 <- bind_cols(debt, dane["waluty"]/1000)
write.csv2(debt1, "wykres_5.csv")

debt2 <- read.csv2("wykres_5.csv", colClasses = c("rok" = "Date"))

ggplot(debt1) + geom_area(aes(x=rok, waluty), fill="grey40") +
     geom_area(aes(x=rok, dług), fill="grey") +
     theme_minimal() +
     ylab("mld zł") + xlab("") +
     annotate("text", x = as.Date("2019", format = "%Y"), y=150, label="Zadłużenie w walutach obcych\nSkarbu Państwa", 
              hjust=1, size = 5) +
     annotate("text", x = as.Date("2019", format = "%Y"), y=350, label="Aktywa rezerwowe", 
              hjust=1, size = 5, color = "white")  

# https://dane.gov.pl/pl/dataset/164,zaduzenie-skarbu-panstwa/resource/33681/table
dane6 <- rio::import("https://api.dane.gov.pl/media/resources/20211008/Zadluzenie_Skarbu_Panstwa.xlsx", sheet=3, skip = 7, col_types="text")
dane6_t <-  as_tibble(t(dane6), rownames = "row_names")
dane6_t1 <- dane6_t[6:nrow(dane6_t),3]
month_names <- as.Date(format(seq(as.Date("2003-02-01"), as.Date("2021-08-01"), 
                          by = 'month')))
plot6 <- bind_cols(month_names, dane6_t1)
colnames(plot6) <- c("data", "PLN")
plot6 <- plot6 %>% mutate(PLN=as.numeric(PLN)) %>% mutate(zagr=round(1-PLN, digits = 3))
saveRDS(plot6, "rysunki\\plot28.RDS")

ggplot(plot6, aes(data, zagr)) + geom_line()

an1 <- list(
     x = plot6$data[1],
     y = plot6$zagr[1],
     text = paste(plot6$zagr[1]*100, "%"),
     xref = "x",
     yref = "y",
     showarrow = TRUE,
     arrowhead = 6,
     ax = -10,
     ay = 20
)
tmp <- plot6 %>% filter(zagr==max(zagr))
an2 <- list(
     x = tmp$data,
     y = tmp$zagr,
     text = paste(tmp$zagr*100, "%"),
     xref = "x",
     yref = "y",
     showarrow = TRUE,
     arrowhead = 6,
     ax = -10,
     ay = -20
)
tmp <- plot6 %>% filter(zagr==min(zagr))
an3 <- list(
     x = tmp$data,
     y = tmp$zagr,
     text = paste(tmp$zagr*100, "%"),
     xref = "x",
     yref = "y",
     showarrow = TRUE,
     arrowhead = 6,
     ax = 10,
     ay = 20
)
an4 <- list(
     x = plot6$data[nrow(plot6)],
     y = plot6$zagr[nrow(plot6)],
     text = paste(plot6$zagr[nrow(plot6)]*100, "%"),
     xref = "x",
     yref = "y",
     showarrow = TRUE,
     arrowhead = 6,
     ax = 10,
     ay = -20
)

plot_ly() %>% add_trace(x = plot6$data, y=plot6$zagr, type = "scatter", mode = "lines+markers", fill = "tonexty",
        #text = as.character(plot6$data),
        text = paste(as.character(plot6$zagr*100), "%\n", as.character(plot6$data)),
        hoverinfo = "text") %>% 
     layout(
          xaxis = list(title = ""),
          yaxis = list(title = "", color = "#eee", showticklabels=FALSE, range=list(0,1)), #range zmienia zakres osi
          annotations = list(an1, an2, an3, an4)
     )


#-----------------------------------------------
# wykres 7
url <- "https://www.nbp.pl/statystyka/bilans_platniczy/bop_q_pln.xlsx"

#handel, usługi i rachunek kapitałowy
bop1 <- rio::import(url, skip=8, sheet = 2) 
bop1_1 <- bop1[5:21,] %>% select(1, 4, 7, 16) %>% mutate(handel= `4`+`7`)

# wynagrodzenia pracowników
bop2 <- rio::import(url, skip=9, sheet = 6) 
bop2_1 <- bop2[5:21,] %>% select(1, 5)

# przekazy zarobków
bop3 <- rio::import(url, sheet = 'Dochody wtórne-Secondary income', skip=9)
bop3_1 <- bop3[5:21,] %>% select(1, 11)

# rachunek finansowy
bop_fin <- rio::import(url, skip=8, sheet = 1) 
bop_fin_1 <- bop_fin[5:21,] %>% select(1, 8)

bop <- bind_cols(bop1_1, bop2_1$`5`, bop3_1$`11`, bop_fin_1$`8`) %>% rename(
     rok = `1`, 
     towary = `4`, 
     usługi = `7`, 
     kapitałowy = `16`, 
     saldo = `handel`)
colnames(bop)[6:8] <- c("wynagrodzenia", "przekazy", "finansowy")
bop$finansowy <- -bop$finansowy
bop$wynagrodzenia <- bop$wynagrodzenia+bop$przekazy
bop_plot <- bop %>% select(-towary, -usługi, -przekazy) %>% tidyr::pivot_longer(cols = 2:5, names_to = "rachunek", values_to = "pln") %>% 
     mutate(pln = pln/1000)
ggplot(bop_plot, aes(x=rok, y=pln, fill = rachunek)) +geom_col() +
     # ggthemes::scale_fill_tableau(palette = "Classic 10 Medium", name = "", 
     #                              labels=c("Rachunek finansowy*", "Rachunek kapitałowy", "Towary i usługi", "Wynagrodzenia"))+
     scale_fill_brewer(type = "qual", palette = "Set2", name = "", 
                       labels=c("Rachunek finansowy*", "Rachunek kapitałowy", "Towary i usługi", "Wynagrodzenia"))+
     theme_minimal() +xlab("") + ylab("mld zł") + theme(legend.position="bottom") +
     theme(panel.grid.major.x = element_blank(),
           panel.grid.major.y = element_blank(),
           panel.grid.minor.y = element_blank())
saveRDS(bop_plot, "rysunki\\plot29.RDS")
