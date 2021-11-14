library(eurostat)
library(dplyr)
library(ggplot2)
library(ggthemes)

rok<- as.character(Sys.Date())

gdp<-get_eurostat("nama_10_gdp") %>% 
     filter(time >= "2002-01-01") %>%
     filter(time <= rok) %>%
     filter(unit=="CP_MNAC") %>%
     subset(.,na_item %in% c("P3_S13", "P31_S14_S15", "P51G", "P52_P53",
                             "P6", "P7", "D1", "B2A3G", "D2X3")) %>%
     select(geo, time, na_item, values) %>% 
     filter(geo=="PL")

#SP=D1+B2A3G
#I=P51G+P52_P53
#G=P3_S13
#T=D2X3
#E=P6
#Im=P7
#
#(SP-I)=(G-T)+(E-IM)

tmp<-tidyr::spread(gdp,na_item, values) %>%
     mutate (.,SP=D1+B2A3G-P31_S14_S15) %>%
     mutate (.,I=P51G+P52_P53) %>%
     mutate (., dSP=SP-I) %>%
     mutate (., dG=P3_S13-D2X3) %>%
     mutate (., dE=P6-P7) %>%
     mutate (., ver=dSP-(dG+dE))

x<-select(tmp, geo, time, dSP, dG, dE, ver)
x$dG<--x$dG
x$dE<--x$dE
x1<-tidyr::gather(x, na_item, values, 3:5)
x1$na_item<-factor(x1$na_item, levels = c("dSP", "dG", "dE"), labels = c("prywatnego", "publicznego", "zagranicy"))

ggplot(x1, aes(x = time, y = values, group = na_item)) +
     geom_col(aes(fill = na_item), position = "stack") +
     theme_hc() + scale_fill_brewer(palette="Set2") +
     scale_y_continuous(labels=function(x)x/1000) +
     labs(fill = "Bilans sektora: ") + ylab("mld zł") + xlab("rok") +
     ggtitle (levels(x1$geo1)[as.numeric(as.character(x1[1,1]))]) +
     theme(plot.background = element_rect(fill = "#fcfcfc"),
           legend.background = element_rect(fill = "#fcfcfc"))