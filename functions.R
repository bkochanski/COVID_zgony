
przyg_danych <- function(wybrany_kraj,dane) {
  
data_robocze <- dane %>%
  filter(sex== "T", geo == wybrany_kraj) %>%
  mutate(rok = factor(rok)) 
return(data_robocze)

}


ostatnia_data <- function(wybrany_kraj,dane) {
  
  robocze <- dane %>%
    filter(!is.na(obsValue)) %>%
    filter(sex== "T", geo == wybrany_kraj) %>%
    filter(data == max(data)) %>% select(data, tydzien_liczba)
  
  ostatnia_data <-   format(robocze$data, "%d %B %Y")
  ostatni_tydzien <- robocze$tydzien_liczba
  lista <- list(ostatni_tydzien, ostatnia_data)
  return(lista)
  
}

przyg_wyk_tygodniowego <- function(data_robocze, wybrany_kraj) {
  
ggplot(data_robocze) +
  geom_line(aes(x = tydzien_liczba, y = obsValue, group = rok), color = "grey",                data = data_robocze %>% filter(rok != "2020")) +
  geom_line(aes(x = tydzien_liczba, y = obsValue, group = rok), data =                         data_robocze %>% filter(rok == "2020"), size = 1.1, color =                        "darkblue") +
  scale_y_continuous(labels = scales::number) +
  labs(x = "Tygodnie roku", y = "Tygodniowa liczba zgonów",
       title = paste0(wybrany_kraj," - Tygodniowa liczba zgonów dla roku 2020 na tle poprzednich"),
       caption = "Autor: Kamil Pastor na podstawie Eurostatu") +
  theme_classic()

}

przyg_wyk_dekompozycja <- function(data_robocze, wybrany_kraj) {
  
data_robocze2 <- data_robocze[complete.cases(data_robocze),] %>%
  arrange(data)
rok_pocz <- min(year(data_robocze2$data))  
tydzien_pocz <- min(week(data_robocze2$data))
data_robocze_ts <- ts(data_robocze2$obsValue, freq=52, start=rok_pocz + tydzien_pocz /12)
dane_odsezonowane <- stl(data_robocze_ts, s.window="periodic")

wykres <- dane_odsezonowane %>%
          forecast::autoplot(range.bars = FALSE,
                 labels = c("trend", "sezonowość", "reszty")) + 
                 scale_y_continuous(labels = scales::number) +
                 labs(x = "", y = "",
                 title = paste0(wybrany_kraj," - Dekompozycja liczby zgonów na trend, sezonowość i reszty"),
                 subtitle = "Dekompozycja metodą STL.",
                 caption = "Autor: Kamil Pastor na podstawie Eurostatu.") +
  theme_classic()

return(wykres)
}

przyg_wyk_nadmiarowych_zgonow <- function(data_robocze, wybrany_kraj) {

data_robocze2 <- data_robocze[complete.cases(data_robocze),] %>%
  arrange(data)
rok_pocz <- min(year(data_robocze2$data))  
tydzien_pocz <- min(week(data_robocze2$data))
data_robocze_ts <- ts(data_robocze2$obsValue, freq=52, start=rok_pocz + tydzien_pocz/12)
dane_odsezonowane <- stl(data_robocze_ts, s.window="periodic")
  
dane_wykres <- as.data.frame(dane_odsezonowane$time.series)
dane_wykres <- cbind(dane_wykres, data_robocze2$data) %>% 
  mutate(szacunek = seasonal + trend,
         dol = seasonal + trend - 2 * sd(remainder),
         gora = seasonal + trend + 2 *sd(remainder),
         original = seasonal + trend + remainder) %>%
  rename(data = `data_robocze2$data`) %>%
  filter(data > "2016-01-01")

dane_max <- dane_wykres %>% 
  filter(data == max(data)) %>%
  mutate(data = data + months(3))

ggplot(dane_wykres, aes(x=data)) +
  geom_line(aes(y = szacunek), color = "darkgrey") +
  geom_line(aes(y = original), color = "darkblue", size = 1.2) +
  geom_ribbon(aes(ymin = dol, ymax = gora), fill = "grey", alpha = 0.5) +
  scale_y_continuous(labels = scales::number) +
  labs(x = "", y= "Zgony tygodniowo",
       title = paste0(wybrany_kraj," - Nadzwyczajna śmiertelność - od 2016 roku"),
       subtitle = "Szary obszar oznacza przedział trend + sezonowość ± dwa odchylenia standardowe reszt.",
       caption = "Autor: Kamil Pastor na podstawie Eurostatu") +
  theme_classic() +
  scale_x_date(date_labels = "%b %y", date_breaks = "6 months") +
  theme(panel.grid.major.x = element_line()) +
  geom_shadowtext(data = dane_max, aes(y = original, label = round(original,0)),
                  vjust= 0, color = "darkblue", bg.colour="white" ) +
  geom_shadowtext(data = dane_max, aes(y = szacunek, label = round(szacunek,0)),
                  vjust= 0, color = "dimgrey", bg.colour= "white" ) 
}



przyg_wyk_tygodniowego_kraje <- function(dt1) {
  
  dane_pomocnicze <- dt1 %>%
                      filter(sex == "T" & geo != "Cypr" & geo != "Andora" &
                               geo != "Andora" & geo != "Albania" & geo != "Armenia" &
                               geo != "Gruzja" & geo != "Lichtenstein" & geo != "Luksemburg" &
                               geo !=  "Czarnogóra" & geo != "Serbia" &
                               geo !=  "Litwa" & geo != "Łotwa" & geo != "Estonia" &
                               
                             data >= "2015-01-01" & data < "2020-01-01") %>%
                      mutate(rok = factor(rok)) %>%
                      group_by(geo, tydzien_liczba) %>%
                      mutate(srednia_15_19 = mean(obsValue, na.rm = TRUE)) %>%
                      ungroup()
  
  dane_pomocnicze2 <- dt1 %>%
    filter(sex == "T" & geo != "Cypr" & geo != "Andora" &
             geo != "Andora" & geo != "Albania" & geo != "Armenia" &
             geo != "Gruzja" & geo != "Lichtenstein" & geo != "Luksemburg" &
             geo !=  "Czarnogóra" & geo != "Serbia" &
             geo !=  "Litwa" & geo != "Łotwa" & geo != "Estonia" &
             
             data >= "2020-01-01") %>%
    mutate(rok = factor(rok)) %>%
    mutate(srednia_15_19 = NA)

                      
  dane_wykres <- rbind(dane_pomocnicze, dane_pomocnicze2) %>%
                  group_by(geo, tydzien_liczba) %>%
                  mutate(p_indeks = round(obsValue / mean(srednia_15_19,na.rm = TRUE) * 100 - 100, 1)) %>%
                  filter(data >= "2020-01-01") %>%
                  ungroup()
  
  dane_max <- dane_wykres %>% 
    filter(rok == 2020 ) %>%
    filter(tydzien_liczba == max(tydzien_liczba)) %>%
    mutate(tydzien_liczba = tydzien_liczba + 5)
  
  dane_szare <- dane_wykres %>%
    mutate(geo2 = geo) %>%
    select(!geo)
    
  ggplot(dane_wykres, aes(x = tydzien_liczba, y = p_indeks, group = geo)) +
    geom_line(aes(x = tydzien_liczba, y = p_indeks, group = geo2), color = "grey",
              data = dane_szare) +
    geom_line(aes(x = tydzien_liczba, y = p_indeks, group = geo), 
              data = dane_wykres, size = 1, color =  "darkblue") +
    facet_wrap(~geo, ncol = 5) +
    scale_y_continuous(labels = scales::number, limits = c(-50, 170)) +
    labs(x = "Tygodnie roku", y = "Nadwyżkowa umieralność w procentach",
         title = paste0("Procentowa nadwyżkowa umieralność w ujęciu tygodniowym"),
         subtitle = "Dane przedstawiają o ile procent więcej osób umarło w danym kraju w danym tygodniu \nwzględem średniej umieralności w danym tygodniu w ostatnich pięciu latach (2015-2019).\nDane dla krajów różnią się opóźnieniem w raportowaniu danych.",
         caption = "Autor: Kamil Pastor na podstawie danych Eurostatu") +
    theme_minimal() +
    theme(axis.text = element_text(size = 7),
          axis.title = element_text(size = 8),
          strip.text = element_text(face = "bold", size = 8),
          plot.title = element_text(),
          plot.subtitle = element_text(size = 8)) +
   geom_hline(yintercept = 0, color = "maroon")
  
  
}


przyg_wyk_tygodniowego_nadwyzkowa_smiertelnosc_kraj <- function(dt1, wybrany_kraj) {
  
  dane_pomocnicze <- dt1 %>%
    filter(sex == "T" & geo == wybrany_kraj &
             data >= "2015-01-01" & data < "2020-01-01") %>%
    mutate(rok = factor(rok)) %>%
    group_by(geo, tydzien_liczba) %>%
    mutate(srednia_15_19 = mean(obsValue, na.rm = TRUE)) %>%
    ungroup()
  
  dane_pomocnicze2 <- dt1 %>%
    filter(sex == "T" & geo == wybrany_kraj &
             data >= "2020-01-01") %>%
    mutate(rok = factor(rok)) %>%
    mutate(srednia_15_19 = NA) %>%
    filter(!is.na(obsValue))
   
  
  
  dane_wykres <- rbind(dane_pomocnicze, dane_pomocnicze2) %>%
    group_by(geo, tydzien_liczba) %>%
    mutate(p_indeks = round(obsValue / mean(srednia_15_19,na.rm = TRUE) * 100 - 100, 1)) %>%
    filter(data >= "2020-01-01") %>%
    ungroup()
  
  dane_max <- dane_wykres %>% 
    filter(rok == 2020 ) %>%
    filter(tydzien_liczba == max(tydzien_liczba)) %>%
    mutate(tydzien_liczba = tydzien_liczba + 4)
  
  
  ggplot(dane_wykres, aes(x = tydzien_liczba, y = p_indeks)) +
    geom_line(aes(x = tydzien_liczba, y = p_indeks, group = geo), 
              data = dane_wykres, size = 1.1, color =  "darkblue") +
    scale_y_continuous(labels = scales::number, limits = c(-50, 170)) +
    labs(x = "Tygodnie roku", y = "Nadwyżkowa umieralność w procentach",
         title = paste0(wybrany_kraj," - Procentowa nadwyżkowa umieralność w ujęciu tygodniowym"),
         subtitle = "Dane przedstawiają o ile procent więcej osób umarło w danym tygodniu względem średniej \numieralności w danym tygodniu w ostatnich pięciu latach (2015-2019).",
         caption = "Autor: Kamil Pastor na podstawie danych Eurostatu") +
    theme_minimal() +
    theme(axis.text = element_text(),
          strip.text = element_text(face = "bold"),
          plot.title = element_text()) +
    geom_hline(yintercept = 0, color = "maroon") +
    geom_shadowtext(data = dane_max, aes(x = tydzien_liczba, y = p_indeks, label = paste0(round(p_indeks,1)," %")),
                    vjust= 0, size = 4, color = "darkblue", bg.colour="white" ) 
  
}

heatmapa_dostepnosc <- function(dt1) {
  
  dane_wykres <- dt1 %>%
                 filter(sex == "T" & data >= "2020-01-01" & geo != "Andora") %>%
                 mutate(dostepnosc = if_else(is.na(obsValue),0,1))
  
  ggplot(dane_wykres, aes(x = tydzien_liczba, y = geo, fill = dostepnosc)) +
    geom_tile() +
    scale_y_discrete(limits = rev) +
    scale_fill_gradientn(colours=c("lightgrey", "darkblue"),
                         values=rescale(c(0,1)),
                         guide="colorbar") +
    labs(x = "Tygodnie roku", y = "",
         title = paste0("Dostępność danych o zgonach w 2020 roku wg tygodni"),
         subtitle = "Ciemnoniebieskim kolorem zaznaczo te tygodnie, dla których są dostępne dane o zgonach",
         caption = "Autor: Kamil Pastor na podstawie danych Eurostatu") +
    theme_minimal() +
    theme(axis.text = element_text(),
          strip.text = element_text(face = "bold"),
          plot.title = element_text(),
          plot.subtitle = element_text(size = 8),
          legend.position = "none") 
  
  
}
