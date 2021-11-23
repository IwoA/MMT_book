# -------------------------------------------------------------------------
# Wykres 3
library(bdl)
library(dplry)
library(ggplot2)

# P2392  	Stopa bezrobocia rejestrowanego
get_variables("P2392")
bezrobocie <- get_data_by_variable("60270", year = 2004:2021, unitLevel = 0) %>% select(year, val)
miesiace <- get_variables("P3559") %>% select(n1) %>% slice_head(n=12)
bezrobocie_m <- get_data_by_variable(as.character(c(461680:461691)), year = 2020:2021, unitLevel = 0)
bezrobocie_m <- bezrobocie_m[,3:15]
colnames(bezrobocie_m)[2:13] <- miesiace$n1
bezrobocie_ml <- bezrobocie_m %>% tidyr::pivot_longer(cols = 2:13, names_to = "miesiac", values_to = "proc") %>% #values_to = "val", żeby ggplot działał
     filter(year==2021) %>% select(-year) %>% rename(year = miesiac)
bezrobocie_plt <- bind_rows(bezrobocie, bezrobocie_ml)
lev <- bezrobocie_plt$year
bezrobocie_plt$year <- factor(bezrobocie_plt$year, levels = lev)
col <- data.frame(col=c(rep("y", times =nrow(bezrobocie)-1), rep("m", times=13)))
bezrobocie_plt <- bind_cols(bezrobocie_plt,col)
ggplot(bezrobocie_plt, aes(x=year, y=val, group = 1)) +
     geom_rect(aes(xmin="2020", xmax="grudzień", ymin=0, ymax=20), fill = "grey80") +
     geom_line(aes(color=col))+ 
     theme_minimal() +
     scale_y_continuous(limits = c(0,20), labels = NULL, breaks = NULL)+
     scale_x_discrete(breaks = NULL)

# Mapa bezrobocia.
# Na podstawie https://blog.prokulski.science/index.php/2017/09/08/liczba-bezrobotnych-a-liczba-mieszkancow/

library(tidyverse)
library(rgdal)
library(broom)
library(readr)
bezrobocie <- read_delim("RYNE_2392_CREL_20191109201337.csv", 
                         ";", escape_double = FALSE, 
                         locale = locale(decimal_mark = ",", 
                                         grouping_mark = "."),
                         col_types = cols(Atrybut = col_skip(), 
                                                                       Rok = col_date(format = "%Y"), X8 = col_skip()), 
                         trim_ws = TRUE)


# bezrobocie <- read.csv2("RYNE_2392_CREL_20191109201337.csv") - nie czyta polskich liter

bezrobocie_2018 <- bezrobocie %>% filter(Rok == "2018-01-01")

powiaty <- readOGR("Powiaty.shp")  #muszą być dostępne wszystkie cztery pliki .shp, .shx, .prj, .dbf
powiaty <- spTransform(powiaty, CRS("+init=epsg:4326"))
powiaty_df <- broom::tidy(powiaty, region = "JPT_KOD_JE")

bezrobocie_2018$Teryt <- substr(bezrobocie_2018$Kod, 1, 4)
powiaty_mapa <- left_join(powiaty_df,
                          select(bezrobocie_2018, Teryt, Wartosc),
                          by = c("id" = "Teryt"))

powiaty_mapa %>%
     ggplot() +
     geom_polygon(aes(long, lat, group=group, fill=Wartosc), color="gray45") +
     scale_fill_gradient(low = "white", high = "gray45", name = "Stopa bezrobocia w %") +
     coord_map() +
     theme_minimal() +
     theme(legend.position = "bottom", axis.text.x = element_blank(), axis.text.y = element_blank(),
           axis.title.x = element_blank(), axis.title.y = element_blank())
