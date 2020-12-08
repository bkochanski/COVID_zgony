options(java.parameters = "-Xmx1024m")
library(XLConnect)
library(tidyverse)
library(rsdmx)
library(lubridate)
library(shadowtext)
library(httr)
library(jsonlite)
library(eurostat)
library(data.table)
library(ISOweek)
library(manipulate)
library(ggplot2)
library(forecast)


setwd("C:/Users/Kamil Pastor/Desktop/R - zgony")

download.file("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&file=data%2Fdemo_r_mwk_ts.tsv.gz",
              destfile = "demo_r_mwk_ts.tsv.gz")
dt = fread("demo_r_mwk_ts.tsv.gz")
dt1 <- dt %>%
      gather(key = "tydzien", value = "obsValue", - `sex,unit,geo\\time`) %>%
      filter(!grepl("W99", tydzien)) %>%
  mutate(obsValue = gsub(" p", "", obsValue),
         obsValue = gsub(" e", "", obsValue),
         obsValue = gsub(":", "", obsValue),
             obsValue = as.numeric(obsValue),
             
             tydzien = gsub("W","-W", tydzien),
             tydzien2 = paste(tydzien, "-7",sep="")) %>%

      separate(`sex,unit,geo\\time`, c("sex","unit","geo")) %>%
      mutate(data = ISOweek2date(tydzien2), 
             rok = year(data),
             tydzien_liczba = week(data))

dt1$geo <- gsub("AD", "Andora", dt1$geo)
dt1$geo <- gsub("AL", "Albania", dt1$geo)
dt1$geo <- gsub("AM", "Armenia", dt1$geo)
dt1$geo <- gsub("AT", "Austria", dt1$geo)
dt1$geo <- gsub("BE", "Belgia", dt1$geo)
dt1$geo <- gsub("BG", "Bułgaria", dt1$geo)
dt1$geo <- gsub("CH", "Szwajcaria", dt1$geo)
dt1$geo <- gsub("CY", "Cypr", dt1$geo)
dt1$geo <- gsub("CZ", "Czechy", dt1$geo)
dt1$geo <- gsub("DK", "Dania", dt1$geo)
dt1$geo <- gsub("EE", "Estonia", dt1$geo)
dt1$geo <- gsub("EL", "Grecja", dt1$geo)
dt1$geo <- gsub("ES", "Hiszpania", dt1$geo)
dt1$geo <- gsub("FI", "Finlandia", dt1$geo)
dt1$geo <- gsub("FR", "Francja", dt1$geo)
dt1$geo <- gsub("GE", "Gruzja", dt1$geo)
dt1$geo <- gsub("HR", "Chorwacja", dt1$geo)
dt1$geo <- gsub("HU", "Węgry", dt1$geo)
dt1$geo <- gsub("IS", "Islandia", dt1$geo)
dt1$geo <- gsub("IT", "Włochy", dt1$geo)
dt1$geo <- gsub("LI", "Lichtenstein", dt1$geo)
dt1$geo <- gsub("LT", "Litwa", dt1$geo)
dt1$geo <- gsub("LU", "Luksemburg", dt1$geo)
dt1$geo <- gsub("LV", "Łotwa", dt1$geo)
dt1$geo <- gsub("ME", "Czarnogóra", dt1$geo)
dt1$geo <- gsub("MT", "Malta", dt1$geo)
dt1$geo <- gsub("NL", "Holandia", dt1$geo )
dt1$geo <- gsub("NO", "Norwegia", dt1$geo)
dt1$geo <- gsub("PL", "Polska", dt1$geo)
dt1$geo <- gsub("PT", "Portugalia", dt1$geo)
dt1$geo <- gsub("RO", "Rumunia", dt1$geo)
dt1$geo <- gsub("RS", "Serbia", dt1$geo)
dt1$geo <- gsub("SE", "Szwecja", dt1$geo)
dt1$geo <- gsub("SI", "Słowenia", dt1$geo)
dt1$geo <- gsub("SK", "Słowacja", dt1$geo)
dt1$geo <- gsub("UK", "Wielka Brytania", dt1$geo)
dt1$geo <- gsub("DE", "Niemcy", dt1$geo) 

### Dodawanie danych dla Polski

dt1[dt1$sex == "T"& dt1$geo == "Polska" & dt1$tydzien_liczba == 45 & dt1$rok == 2020, "obsValue"] <- 16229
dt1[dt1$sex == "T"& dt1$geo == "Polska" & dt1$tydzien_liczba == 46 & dt1$rok == 2020, "obsValue"] <- 15612
dt1[dt1$sex == "T"& dt1$geo == "Polska" & dt1$tydzien_liczba == 47 & dt1$rok == 2020, "obsValue"] <- 14778
dt1[dt1$sex == "T"& dt1$geo == "Polska" & dt1$tydzien_liczba == 48 & dt1$rok == 2020, "obsValue"] <- 13502

saveRDS(dt1, file = "data/dt1.rds")

dt1 <- readRDS("data/dt1.rds")



#######################
### dane dla województw i podregionów


download.file("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&file=data%2Fdemo_r_mwk3_t.tsv.gz",
              destfile = "demo_r_mwk3_t.tsv.gz")
dt = fread("demo_r_mwk3_t.tsv.gz")

dt_NUTS <- dt %>%
  gather(key = "tydzien", value = "obsValue", - `unit,geo\\time`) %>%
  filter(!grepl("W99", tydzien)) %>%
  mutate(obsValue = gsub(" p", "", obsValue),
         obsValue = gsub(":", "", obsValue),
         obsValue = as.numeric(obsValue),
         
         tydzien = gsub("W","-W", tydzien),
         tydzien2 = paste(tydzien, "-7",sep="")) %>%
  
  separate(`unit,geo\\time`, c("unit","geo")) %>%
  mutate(data = ISOweek2date(tydzien2), 
         rok = year(data),
         tydzien_liczba = week(data))

dt_NUTS <- dt_NUTS %>%
           filter(str_detect(geo, "PL"),
                  nchar(geo) >= 4)  %>%
           mutate(kod = geo)

dt_NUTS_wsad <- dt_NUTS  

kody <- openxlsx::read.xlsx("nuts3_kody.xlsx")

kody$kody <- trimws(kody$kody)

dt_NUTS <- left_join(dt_NUTS, kody, by = c("geo" = "kody"))


wojewodztwa <- kody %>% filter(nchar(kody)==4) %>% arrange(nazwa)

saveRDS(dt_NUTS, file = "data/dt_NUTS.rds")

dt_NUTS <- readRDS("data/dt_NUTS.rds")


############ dodawanie danych z excela GUS

dane_GUS <- openxlsx::read.xlsx("zgony_regiony_gus.xlsx")

dane_GUS2 <- dane_GUS %>%
               gather(key = "data", value = "obsValue", -kto, -kod, -nazwa) %>%
               filter(!is.na(kod)) %>%
               mutate(data = openxlsx::convertToDate(data),
                      obsValue = as.numeric(obsValue), 
                      unit = "NR",
                      geo = kod,
                      rok = year(data),
                      tydzien_liczba = week(data),
                      tydzien = NA,
                      tydzien2 = NA) %>%
              filter(!is.na(obsValue),
                     tydzien_liczba %in% c(41,42,43,44)) %>%
             select(unit, geo, tydzien, obsValue, tydzien2, data, rok,
                    tydzien_liczba, kod)


dt_NUTS_wsad <- dt_NUTS_wsad %>% filter (!is.na(obsValue))

dt_NUTS_wsad <- rbind(dt_NUTS_wsad, dane_GUS2)

kody <- openxlsx::read.xlsx("nuts3_kody.xlsx")

kody$kody <- trimws(kody$kody)

dt_NUTS <- left_join(dt_NUTS_wsad, kody, by = c("geo" = "kody"))

saveRDS(dt_NUTS, file = "data/dt_NUTS.rds")



######### dane województwa i makroregion mazowiecki


dt_NUTS <- dt %>%
  gather(key = "tydzien", value = "obsValue", - `unit,geo\\time`) %>%
  filter(!grepl("W99", tydzien)) %>%
  mutate(obsValue = gsub(" p", "", obsValue),
         obsValue = gsub(":", "", obsValue),
         obsValue = as.numeric(obsValue),
         
         tydzien = gsub("W","-W", tydzien),
         tydzien2 = paste(tydzien, "-7",sep="")) %>%
  
  separate(`unit,geo\\time`, c("unit","geo")) %>%
  mutate(data = ISOweek2date(tydzien2), 
         rok = year(data),
         tydzien_liczba = week(data))

dt_NUTS <- dt_NUTS %>%
  filter(str_detect(geo, "PL"),
         nchar(geo) == 4 | geo =="PL9")  %>%
  mutate(kod = geo)

dt_NUTS_wsad <- dt_NUTS  


dane_GUS <- openxlsx::read.xlsx("zgony_regiony_gus.xlsx")

dane_GUS2 <- dane_GUS %>%
  gather(key = "data", value = "obsValue", -kto, -kod, -nazwa) %>%
  filter(!is.na(kod),
         nchar(kod) == 4 | kod == "PL9") %>%
  mutate(data = openxlsx::convertToDate(data),
         obsValue = as.numeric(obsValue), 
         unit = "NR",
         geo = kod,
         rok = year(data),
         tydzien_liczba = week(data),
         tydzien = NA,
         tydzien2 = NA) %>%
  filter(!is.na(obsValue),
         tydzien_liczba %in% c(41,42,43,44)) %>%
  select(unit, geo, tydzien, obsValue, tydzien2, data, rok,
         tydzien_liczba, kod)


dt_NUTS_wsad <- dt_NUTS_wsad %>% filter (!is.na(obsValue))

dt_NUTS_wsad <- rbind(dt_NUTS_wsad, dane_GUS2)

kody <- openxlsx::read.xlsx("nuts3_kody.xlsx")

kody$kody <- trimws(kody$kody)

dt_NUTS_wojewodztwa <- left_join(dt_NUTS_wsad, kody, by = c("geo" = "kody"))

dt_NUTS_wojewodztwa<- dt_NUTS_wojewodztwa %>%
                        filter(!geo =="PL91", !geo == "PL92") %>%
                        mutate(nazwa = gsub("Województwo ", "", nazwa),
                               nazwa = gsub("Makroregion ", "", nazwa))




saveRDS(dt_NUTS_wojewodztwa, file = "data/dt_NUTS_wojewodztwa.rds")





####


dt1_PL <- dt1 %>%
          filter(sex== "T", geo == "Polska") %>%
          mutate(rok = factor(rok)) %>%
          arrange(data)

dt1_PL_szare <- dt1_PL %>% filter(rok != "2020")
dt1_PL_2020 <- dt1_PL %>% filter(rok == "2020")

ggplot(dt1_PL) +
  geom_line(data = dt1_PL_szare, aes(x = tydzien_liczba, y = obsValue, group = rok), color = "grey") +
  geom_line(data = dt1_PL_2020, aes(x = tydzien_liczba, y = obsValue, group = rok)) +
  scale_x_continuous(breaks = 1:53)


wybrany_kraj <- "PL"

  data_robocze <- dt1 %>%
    filter(sex== "T", geo == wybrany_kraj) %>%
    mutate(rok = factor(rok)) 

  
    ggplot(data_robocze) +
    geom_line(aes(x = tydzien_liczba, y = obsValue, group = rok), color = "grey", data = data_robocze %>% filter(rok != "2020")) +
    geom_line(aes(x = tydzien_liczba, y = obsValue, group = rok), data = data_robocze %>% filter(rok == "2020")) +
    scale_x_continuous(breaks = 1:53)
    
  

dt1_PL <- dt1_PL[complete.cases(dt1_PL),]

rok_pocz <- min(year(dt1_PL$data))  
tydzien_pocz <- min(week(dt1_PL$data))


dt1_PL_ts <- ts(dt1_PL$obsValue, freq=52, start=2000+1/12)
dt1_PL_ts
dane_odsezonowane <- stl(dt1_PL_ts, s.window="periodic")

dane_odsezonowane %>% forecast::autoplot(range.bars = FALSE) + 
  labs(title = "sadas") + theme_bw()

dane_wykres <- as.data.frame( dane_odsezonowane$time.series)
dane_wykres <- cbind(dane_wykres, dt1_PL$data) %>%
               rename(data = `dt1_PL$data`) %>%
               mutate(szacunek = seasonal + trend,
                      dol = seasonal + trend - 2 * sd(remainder),
                      gora = seasonal + trend + 2 *sd(remainder)) %>%
               filter(data >= "2018-01-01") 

ggplot(dane_wykres, aes(x = data)) +
  geom_line(aes(y = szacunek), color = "red") +
  geom_ribbon(aes(ymin = dol, ymax = gora), fill = "grey", alpha = 0.5)


