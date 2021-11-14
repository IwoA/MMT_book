# -----------------------------------------
# wykres dochody budżetowe

# https://api.dane.gov.pl/media/resources/20211109/20211108_Zalaczniki_do_zmiany_UB_2021.zip
library(readxl)
dane <- read_excel("rysunki\\dochody_budz_2021.xlsx", skip = 11)
dane <- dane %>% select(1, 7) %>% 
     rename_with(~c("Podatek", "kwota")) %>% 
     filter(!is.na(Podatek)) %>% 
     filter(Podatek!="Dochody niepodatkowe") %>% 
     mutate(kwota = kwota/1000) #w mln zł

library(RColorBrewer)
kolory <- c(brewer.pal(8, "Blues"), brewer.pal(7, "Purples"), "#c90076")
rev <- cbind(dane, kolory)
names(rev) <- c("name", "value","color")#highchart must have data frame with these names. It takes values accordingly.
library(highcharter)
highchart() %>%
     hc_title(text = "Dochody budżetowe w 2021 r. (mln zł)",
              style = list(fontSize = "15px")) %>% 
     hc_chart(type = "treemap") %>% 
     hc_add_series(rev)
