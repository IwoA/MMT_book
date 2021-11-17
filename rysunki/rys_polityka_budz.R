library(readxl)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
# -----------------------------------------
# Wykres 1 Saldo transferów z budżetu UE do Polski a inflacja CPI 
dane1 <- read_excel("rysunki\\transfery_z_UE.xlsx", range = "A1:E18")
cor(dane1$`Saldo transferów z UE`, dane1$CPI, method = "pearson")

ggplot(dane1,group=Rok) + geom_line(aes(x=Rok, y=`Saldo transferów z UE`), color="#8DA0CB", size=1.5) +
     geom_line(aes(x=Rok, y=CPI), color="#FC8D62", size=1.5) + 
     annotate("text",x=2013, y=0.005, color="#8DA0CB", size = 6, hjust=1, label = "Saldo transferów z UE (% PKB)",fontface = "bold")+
     annotate("text",x=2006, y=0.035, color="#FC8D62", size = 6, label = "Inflacja CPI", fontface = "bold")+
     ggthemes::theme_hc() + 
     scale_y_continuous(labels = scales::percent) + xlab("") + ylab("")
     
# -----------------------------------------    
# Wykres 2 
# Jako obraz z prezentacji



# -----------------------------------------
# wykres dochody budżetowe

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
