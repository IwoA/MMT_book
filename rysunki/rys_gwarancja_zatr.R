# -------------------------------------------------------------------------
# Wykres 3
library(bdl)
library(dplyr)
library(ggplot2)
library(plotly)

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
bezrobocie_plt$proc[17] <- bezrobocie_plt$val[17]
bezrobocie_plt$text <- ifelse(is.na(bezrobocie_plt$proc), bezrobocie_plt$val, bezrobocie_plt$proc)
write.csv2(bezrobocie_plt, "rysunki\\wykres_4_3.csv")
# ggplot(bezrobocie_plt, aes(x=year, y=val, group = 1)) +
#      geom_rect(aes(xmin="2020", xmax="grudzień", ymin=0, ymax=20), fill = "grey80") +
#      geom_line(aes(color=col))+ 
#      theme_minimal() +
#      scale_y_continuous(limits = c(0,20), labels = NULL, breaks = NULL)+
#      scale_x_discrete(breaks = NULL)

an1 <- list(
     x = 0,
     y = max(bezrobocie_plt$val/100, na.rm=TRUE),
     text = paste(round(max(bezrobocie_plt$val, na.rm=TRUE), digits = 1),"%"),
     xref = "x",
     yref = "y",
     showarrow = TRUE,
     arrowhead = 6,
     arrowcolor = "blue",
     ax = 10,
     ay = -20,
     font = list(color = "blue")
)
an2 <- list(
     x = 4,
     y =min(bezrobocie_plt$val[1:7]/100, na.rm=TRUE),
     text = paste(round(min(bezrobocie_plt$val[1:7], na.rm=TRUE), digits = 1),"%"),
     xref = "x",
     yref = "y",
     showarrow = TRUE,
     arrowhead = 6,
     arrowcolor = "blue",
     ax = 10,
     ay = 20,
     font = list(color = "blue")
)
an3 <- list(
     x = 15,
     y = min(bezrobocie_plt$val/100, na.rm=TRUE),
     text = paste(round(min(bezrobocie_plt$val, na.rm=TRUE), digits = 1),"%"),
     xref = "x",
     yref = "y",
     showarrow = TRUE,
     arrowhead = 6,
     arrowcolor = "blue",
     ax = -10,
     ay = 20,
     font = list(color = "blue")
)
an4 <- list(
     x = sum(!is.na(bezrobocie_plt$text)),
     y = min(bezrobocie_plt$proc/100, na.rm=TRUE),
     text = paste(round(min(bezrobocie_plt$proc[20:27], na.rm=TRUE), digits = 1),"%"),
     xref = "x",
     yref = "y",
     showarrow = TRUE,
     arrowhead = 6,
     arrowcolor = "blue",
     ax = -10,
     ay = -20,
     font = list(color = "blue")
)

plot_ly(bezrobocie_plt, x = ~year, text = paste(bezrobocie_plt$year, "\n", bezrobocie_plt$text,"%")) %>% 
     add_trace(y = ~val/100, type = 'scatter', mode = 'lines', hoverinfo = "text") %>% 
     add_trace(y = ~proc/100, type = 'scatter', mode = 'lines',  hoverinfo = "text",
               line = list(color = "blue", dash = 'dash')) %>% 
     layout(showlegend=FALSE,
            yaxis = list(showticklabels = FALSE, title = "", showgrid = F),
            xaxis = list(title = "", tickangle = 45),
            annotations = list(an1, an2, an3, an4))

# ------------------------------------------------------------------------
# Mapa bezrobocia.

bezrobocie_pow <- get_data_by_variable("60270", year = 2004:as.numeric(format(Sys.Date(), "%Y")), unitLevel = 5) %>% 
     filter(year == max(year)) %>% mutate(name = tolower(name))

load(url("https://github.com/statisticspoland/R_Package_to_API_BDL/releases/download/1.0.3/bdl.maps.2021.Rdata")) #mapy Polski z kodami NUTS
powiaty <- bdl.maps.2021$level5

powiaty_mapa <- left_join(powiaty,
                          select(bezrobocie_pow, id, val),
                          by = c("id" = "id"))

powiaty_mapa %>% select(geometry, val) %>% 
     ggplot() +
     geom_sf(aes(fill=val)) +
     scale_fill_gradient(low = "white", high = "gray45", name = "Stopa bezrobocia w %") +
     #coord_map() +
     theme_void() +
     theme(legend.position = "bottom", axis.text.x = element_blank(), axis.text.y = element_blank(),
           axis.title.x = element_blank(), axis.title.y = element_blank())

get_variables("P3897") #P1944
nr <- get_variables("P3897") %>% filter(n2 == "ogółem" & n3 == "ogółem")
nr[lubridate::month(Sys.Date())-2,1] #ostatni miesiąc, za który są dane

bezrobocie_pow_liczba <- get_data_by_variable(as.character(nr[lubridate::month(Sys.Date())-2,1]), unitLevel = 5) %>% select(id, name, val)
bezrobocie_pow_liczba$val <- ifelse(bezrobocie_pow_liczba$val==max(bezrobocie_pow_liczba$val), NA, bezrobocie_pow_liczba$val)
bezrobocie_pow_liczba$val <- ifelse(bezrobocie_pow_liczba$val==max(bezrobocie_pow_liczba$val, na.rm = TRUE), NA, bezrobocie_pow_liczba$val)
bezrobocie_pow_liczba$val <- ifelse(bezrobocie_pow_liczba$val==max(bezrobocie_pow_liczba$val, na.rm = TRUE), NA, bezrobocie_pow_liczba$val)

powiaty_mapa_liczba <- left_join(powiaty,
                          select(bezrobocie_pow_liczba, id, val),
                          by = c("id" = "id")) %>% mutate(val = val/1000)

powiaty_mapa_liczba %>% select(geometry, val) %>% 
     ggplot() +
     geom_sf(aes(fill=val)) +
     scale_fill_gradient(low = "white", high = "gray45", na.value = "grey10", name = "Liczba bezrobotnych w tys.*") +
     coord_sf() +
     theme_void() +
     theme(legend.position = "bottom", axis.text.x = element_blank(), axis.text.y = element_blank(),
           axis.title.x = element_blank(), axis.title.y = element_blank())

# ------------------------------------------------------
# wykres 6 Rozkład powiatów wg liczby oraz stopy bezrobocia rejestrowanego

beeswarm <- function(x){     
     plot_ly(x) %>% 
     add_trace(
          x = 0,
          y = ~val,
          type = "box", boxpoints = "all",
          text = paste0(x$name, ", ",x$val),
          hoverinfo = "text",
          width = .5
     ) %>% 
     add_text(x=-.28, y=quantile(x$val)[3], text = "50%",textfont = list(color = "orange")) %>% 
     add_text(x=-.28, y=quantile(x$val, probs = seq(0,1,0.01))[96], text = "95%",textfont = list(color = "orange")) %>% 
     add_text(x=-.28, y=quantile(x$val)[4], text = "75%",textfont = list(color = "orange")) %>% 
     add_text(x=-.48, y=quantile(x$val)[3], text = as.character(quantile(x$val)[3]),textfont = list(color = "orange")) %>% 
     add_text(x=-.48, y=quantile(x$val, probs = seq(0,1,0.01))[96], text = as.character(quantile(x$val, probs = seq(0,1,0.01))[96]),textfont = list(color = "orange")) %>% 
     add_text(x=-.48, y=quantile(x$val)[4], text = as.character(quantile(x$val)[4]),textfont = list(color = "orange")) %>% 
     add_lines(x=seq(from=-.45, to=-.3, by=.01), y=quantile(x$val)[3], text = "50%", color = I("orange")) %>% 
     add_lines(x=seq(from=-.45, to=-.3, by=.01), y=quantile(x$val, probs = seq(0,1,0.01))[96], text = "95%", color = I("orange")) %>% 
     add_lines(x=seq(from=-.45, to=-.3, by=.01), y=quantile(x$val)[4], text = "75%", color = I("orange")) %>% 
     layout(xaxis=list(range=c(-.5,-.26), showticklabels=FALSE),
            yaxis=list(zeroline=FALSE, title = ""),
            showlegend=FALSE)
}

beeswarm(bezrobocie_pow_liczba)
beeswarm(bezrobocie_pow)

library(beeswarm)

#par(mfrow = c(1,2), oma = c(1,1,1,1), mar = c(1,1,1,1))
par(mfrow = c(1,1), mar=rep(0,4), pin = c(4,5))
beeswarm(bezrobocie_pow_liczba$val, method = "center", pch = 16, ylab = "Liczba bezrobotnych")
abline(h=median(bezrobocie_pow_liczba$val), col = "red")
abline(h=quantile(bezrobocie_pow_liczba$val, probs = .75), col = "blue", lty = 2)
text(x = 0.6, y = median(bezrobocie_pow_liczba$val), "50%")
text(x = 0.6, y = quantile(bezrobocie_pow_liczba$val, probs = .75), "75%")
