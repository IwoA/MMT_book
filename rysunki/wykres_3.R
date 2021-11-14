library(rio)
library(dplyr)
library(plotly)
url <- "https://www.nbp.pl/statystyka/pieniezna_i_bankowa/dwn/miarypieniadza_nowe.xlsx"
dane <- rio::import(url, skip=2)
colnames(dane) <- c("data", "M0", "M1", "M2", "M3")
dane1 <- dane %>% select(data, M1)

url <- "https://www.nbp.pl/statystyka/pieniezna_i_bankowa/dwn/bilans_nbp.xlsx"
dane <- rio::import(url, skip=5, sheet=3) 
colnames(dane)[1:2] <- c("data", "MM")
dane2 <- dane %>% select(1,2)
dane3 <- left_join(dane1, dane2) %>% mutate(M= round(MM/M1*100, digits = 1))

an1 <- list(
     x = dane3$data[1],
     y = dane3$M[1],
     text = paste(dane3$M[1], "%"),
     xref = "x",
     yref = "y",
     showarrow = TRUE,
     arrowhead = 6,
     ax = -10,
     ay = 20
)
tmp <- dane3 %>% filter(M==max(M))
an2 <- list(
     x = tmp$data,
     y = tmp$M,
     text = paste(tmp$M, "%"),
     xref = "x",
     yref = "y",
     showarrow = TRUE,
     arrowhead = 6,
     ax = -10,
     ay = -20
)
tmp <- dane3 %>% filter(M==min(M))
an3 <- list(
     x = tmp$data,
     y = tmp$M,
     text = paste(tmp$M, "%"),
     xref = "x",
     yref = "y",
     showarrow = TRUE,
     arrowhead = 6,
     ax = 10,
     ay = 20
)
an4 <- list(
     x = dane3$data[nrow(dane3)],
     y = dane3$M[nrow(dane3)],
     text = paste(dane3$M[nrow(dane3)], "%"),
     xref = "x",
     yref = "y",
     showarrow = TRUE,
     arrowhead = 6,
     ax = 10,
     ay = -20
)

plot_ly(dane3, x = ~data, y=~M, type = "scatter", mode = "lines+markers") %>% 
     layout(
          xaxis = list(title = ""),
          yaxis = list(title = "", color = "#eee"),
          annotations = list(an1, an2, an3, an4)
     )
