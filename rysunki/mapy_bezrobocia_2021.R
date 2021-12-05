library(readxl)
library(rgdal)
library(broom)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Wykres bezrobocia w Polsce

historia <- read_excel("RYNE_2392_XTAB_20191115120524.xlsx", 
                       sheet = "Arkusz1", col_types = c("text", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric"))

stopa <- historia %>% pivot_longer(-`rok/miesiąc`, names_to = "mies", values_to = "stopa")
stopa$`rok/miesiąc` <- paste(stopa$`rok/miesiąc`, stopa$mies, "01", sep = "-")
stopa <- select(stopa, -mies)
stopa$`rok/miesiąc` <- as.Date(stopa$`rok/miesiąc`)
colnames(stopa) <- c("rok", "stopa")
ggplot(stopa, aes(x = rok, y = stopa)) + geom_line() + theme_minimal() + ylab("stopa bezrobocia")


# Mapa bezrobocia.
# Na podstawie https://blog.prokulski.science/index.php/2017/09/08/liczba-bezrobotnych-a-liczba-mieszkancow/


bezrobocie <- read_delim("RYNE_3559_CREL_20200909105354.csv",
                         ";", escape_double = FALSE,
                         locale = locale(decimal_mark = ",",
                                         grouping_mark = "."),
                         col_types = cols(Atrybut = col_skip(),
                                          Rok = col_skip()),
                         trim_ws = TRUE)

bezrobocie_2021 <- read_delim("RYNE_3559_CTAB_20211130141419_.csv", 
                         ";", escape_double = FALSE, 
                         locale = locale(decimal_mark = ",", 
                                         grouping_mark = "."),
                         trim_ws = TRUE)


bezrobotni <- read_delim("RYNE_2961_CREL_20200909bezrobotni.csv", 
                         ";", escape_double = FALSE, 
                         locale = locale(decimal_mark = ",", 
                                         grouping_mark = "."),
                         col_types = cols(Atrybut = col_skip(), 
                                          Rok = col_skip()), 
                         trim_ws = TRUE)

bezrobotni_2021 <- read_delim("RYNE_2962_CTAB_20211130142607.csv", 
                              ";", escape_double = FALSE, 
                              locale = locale(decimal_mark = ",", 
                                              grouping_mark = "."),
                              trim_ws = TRUE)

# bezrobocie <- read.csv2("RYNE_2392_CREL_20191109201337.csv") - nie czyta polskich liter

bezrobocie_2020 <- bezrobocie %>% filter(Miesiące == "lipiec")

powiaty <- rgdal::readOGR("Powiaty.shp")  #muszą być dostępne wszystkie cztery pliki .shp, .shx, .prj, .dbf
powiaty <- spTransform(powiaty, CRS("+init=epsg:4326"))
powiaty_df <- broom::tidy(powiaty, region = "JPT_KOD_JE")

bezrobocie_2020$Teryt <- substr(bezrobocie_2020$Kod, 1, 4)
bezrobocie_2021$Teryt <- substr(bezrobocie_2021$Kod, 1, 4)

powiaty_mapa <- left_join(powiaty_df,
                          select(bezrobocie_2021, Teryt, Wartosc),
                          by = c("id" = "Teryt"))

# Mapa stopy bezrobocia
powiaty_mapa %>%
     ggplot() +
     geom_polygon(aes(long, lat, group=group, fill=Wartosc), color="gray45") +
     scale_fill_gradient(low = "white", high = "gray45", name = "Stopa bezrobocia w %") +
     coord_map() +
     theme_minimal() +
     theme(legend.position = "bottom", axis.text.x = element_blank(), axis.text.y = element_blank(),
           axis.title.x = element_blank(), axis.title.y = element_blank())


# Mapa liczby bezrobotnych
bezrobotni_2020 <- bezrobotni %>%filter(Miesiące == "lipiec") 
bezrobotni_2020$Teryt <- substr(bezrobotni_2020$Kod, 1, 4)
bezrobotni_2021$Teryt <- substr(bezrobotni_2021$Kod, 1, 4)

powiaty_mapa2 <- left_join(powiaty_df,
                          select(bezrobotni_2021, Teryt, Wartosc),
                          by = c("id" = "Teryt"))

powiaty_mapa2$Wartosc <- ifelse(powiaty_mapa2$Wartosc<=19000, powiaty_mapa2$Wartosc, NA) #Bo Warszawa i Łódź wyraźnie odstają
powiaty_mapa2 %>%
        ggplot() +
        geom_polygon(aes(long, lat, group=group, fill=Wartosc/1000), color="gray45") +
        scale_fill_gradient(low = "white", high = "gray45", na.value = "black", name = "Liczba bezrobotnych w tys. *") +
        coord_map() +
        theme_minimal() +
        theme(legend.position = "bottom", axis.text.x = element_blank(), axis.text.y = element_blank(),
              axis.title.x = element_blank(), axis.title.y = element_blank())


# Top 10 powiatów z największą liczbą bezrobotnych
bezrobotni_2020 %>% top_n(Wartosc,n = 10) %>% 
        ggplot() +
        geom_col(aes(x=reorder(Nazwa, Wartosc),y=Wartosc), fill = "gray") +
        geom_text(aes(x=reorder(Nazwa, Wartosc),y=Wartosc, label = Wartosc), hjust = 1.3) +
        coord_flip() + labs(x = "", y = "Liczba bezrobotnych") +
        theme_minimal() + theme(line = element_blank(), axis.text.x = element_blank())

 # Bottom 10 powiatów z największą liczbą bezrobotnych
bezrobotni_2020 %>% top_n(Wartosc,n = -10) %>% 
        ggplot() +
        geom_col(aes(x=reorder(Nazwa, Wartosc),y=Wartosc), fill = "gray") +
        geom_text(aes(x=reorder(Nazwa, Wartosc),y=Wartosc, label = Wartosc), hjust = -.4) +
        coord_flip() + labs(x = "", y = "Liczba bezrobotnych") +
        scale_y_continuous(limits = c(0, 20000)) +
        theme_minimal() + theme(line = element_blank(), axis.text.x = element_blank())

## Wykresy liniowe
top <- bezrobotni_2018 %>% top_n(Wartosc,n = 10)
bottom <- bezrobotni_2018 %>% top_n(Wartosc,n = -10)

# 10 powiatów z największą liczbą bezrobotnych
top10 <- filter(bezrobotni, Nazwa %in% top$Nazwa &  `Staż pracy` == "ogółem" & Płeć == "ogółem")
kolejnosc <- arrange(top, desc(Wartosc)) %>% select(Nazwa) 
top10$Nazwa <- factor(top10$Nazwa, levels = deframe(kolejnosc))

ggplot(top10,aes(x= Rok, y = Wartosc)) + 
        labs(x = "", y = "Liczba bezrobotnych") +
        geom_line() + facet_wrap(vars(Nazwa), nrow = 2) + theme_minimal() 


# 10 powiatów z najmniejszą liczbą bezrobotnych
bot10 <- filter(bezrobotni, Nazwa %in% bottom$Nazwa &  `Staż pracy` == "ogółem" & Płeć == "ogółem")
kolejnosc <- arrange(bottom, desc(Wartosc)) %>% select(Nazwa) 
bot10$Nazwa <- factor(bot10$Nazwa, levels = deframe(kolejnosc))

ggplot(bot10,aes(x= Rok, y = Wartosc)) + 
        labs(x = "", y = "Liczba bezrobotnych") +
        geom_line() + facet_wrap(vars(Nazwa), nrow = 2) + theme_minimal()


# Beeswarm
library(beeswarm)
bezrobocie_2021 <- bezrobocie_2021 %>% filter(stringr::str_detect(Nazwa, "Powiat"))

#par(mfrow = c(1,2), oma = c(1,1,1,1), mar = c(1,1,1,1))
par(mfrow = c(1,1), mar=rep(3,4), pin = c(4,5))
beeswarm(bezrobocie_2021$Wartosc, method = "center", pch = 16, ylab = "Stopa bezrobocia w %")
abline(h=median(bezrobocie_2021$Wartosc), col = "red")
abline(h=quantile(bezrobocie_2021$Wartosc, probs = .75), col = "blue", lty = 2)
text(x = 0.6, y = median(bezrobocie_2021$Wartosc)+0.8, "50%")
text(x = 0.6, y = 11, "75%")

bezrobotni_2021 <- bezrobotni_2021 %>% filter(stringr::str_detect(Nazwa, "Powiat"))

par(mfrow = c(1,1), mar=rep(3,4), pin = c(4,5))
beeswarm(bezrobotni_2021$Wartosc/1000, method = "center", pch = 16, ylab = "Liczba bezrobotnych w tys.")
abline(h=median(bezrobotni_2021$Wartosc/1000), col = "red")
abline(h=quantile(bezrobotni_2021$Wartosc/1000, probs = .75), col = "blue", lty = 2)
text(x = 0.55, y = median(bezrobotni_2021$Wartosc/1000)+0.8, "50%")
text(x = 0.55, y = quantile(bezrobotni_2021$Wartosc/1000, probs = .75)+0.8, "75%")


library(ggbeeswarm)
bezrobocie_2018 %>% 
        ggplot() +
        geom_quasirandom(aes(Rok, Wartosc),varwidth = TRUE)

library(ggplot2)

## Wykresy liniowe stopy bezrobocia
bezrobotni_miesiace <- read_delim("RYNE_2962_CREL_20211130145202.csv", 
                                 ";", escape_double = FALSE, 
                                  trim_ws = TRUE)
tmp <- bezrobotni_miesiace %>%
        filter(Miesiące %in% unique(bezrobotni_miesiace$Miesiące)[c(3,6,9,12)]) %>% 
        filter(!is.na(Wartosc)) %>% 
        mutate(
                Miesiące = case_when(
                        Miesiące == "marzec" ~ "I kw.",
                        Miesiące == "czerwiec" ~ "II kw.",
                        Miesiące == "wrzesień" ~ "III kw.",
                        Miesiące == "grudzień" ~ "IV kw.",
                )
        )
tmp$data <- paste(tmp$Rok, tmp$Miesiące )

bezr <- tmp %>% mutate(data = as.factor(data))

top <- bezr %>% filter(data =="2021 III kw.") %>% top_n(Wartosc,n = 10)
bottom <- bezr %>% filter(data =="2021 III kw.") %>% top_n(Wartosc,n = -10) %>% arrange(Wartosc)
bottom <- bottom[1:10,]  #są trzy powiaty o takiej samej stopie bezrobocia i robi się 11 zamiast 10

# 10 powiatów z największą liczbą bezrobotnych
top10 <- filter(tmp, Nazwa %in% top$Nazwa) #%>% filter(Rok %in% c("2018", "2019", "2020"))
kolejnosc <- arrange(top, desc(Wartosc)) %>% select(Nazwa) 
top10$Nazwa <- factor(top10$Nazwa, levels = tibble::deframe(kolejnosc))

ggplot(top10,aes(x= data, y = Wartosc, group=Nazwa)) + 
        labs(x = "", y = "Liczba bezrobotnych") +
        geom_line() + facet_wrap(vars(Nazwa), nrow = 2) + 
        theme_minimal() + 
        scale_x_discrete(breaks=c("2011 I kw.","2021 III kw.")) +
        theme(axis.text.x = element_text(angle = 90))

# 10 powiatów z najmniejszą liczbą bezrobotnych
bot10 <- filter(tmp, Nazwa %in% bottom$Nazwa) #%>% filter(Rok %in% c("2018", "2019", "2020"))
kolejnosc1 <- arrange(bottom, desc(Wartosc)) %>% select(Nazwa) 
bot10$Nazwa <- factor(bot10$Nazwa, levels = tibble::deframe(kolejnosc1))


ggplot(bot10,aes(x= data, y = Wartosc, group=Nazwa)) + 
        labs(x = "", y = "Liczba bezrobotnych") +
        geom_line() + facet_wrap(vars(Nazwa), nrow = 2) + 
        theme_minimal() + 
        scale_x_discrete(breaks=c("2011 I kw.","2021 III kw.")) +
        theme(axis.text.x = element_text(angle = 90))
