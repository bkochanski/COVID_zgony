
przyg_danych <- function(wybrany_kraj,dane) {
  
  data_robocze <- dane %>%
    filter(nazwa == wybrany_kraj) %>%
    mutate(rok = factor(rok)) 
  return(data_robocze)
  
}


ostatnia_data <- function(wybrany_kraj,dane) {
  
  robocze <- dane %>%
    filter(!is.na(obsValue)) %>%
    filter(nazwa == wybrany_kraj) %>%
    filter(data == max(data)) %>% select(data, tydzien_liczba)
  
  ostatnia_data <-   format(robocze$data, "%d %B %Y")
  ostatni_tydzien <- robocze$tydzien_liczba
  lista <- list(ostatni_tydzien, ostatnia_data)
  return(lista)
  
}

przyg_wyk_tygodniowego <- function(data_robocze, wybrany_kraj) {
 
   dane_max <- data_robocze %>% 
    filter(rok == 2020 ) %>%
    filter(tydzien_liczba == max(tydzien_liczba)) %>%
    mutate(tydzien_liczba = tydzien_liczba + 2)
  
  ggplot(data_robocze) +
    geom_line(aes(x = tydzien_liczba, y = obsValue, group = rok), color = "grey",                data = data_robocze %>% filter(rok != "2020")) +
    geom_line(aes(x = tydzien_liczba, y = obsValue, group = rok), data =                         data_robocze %>% filter(rok == "2020"), size = 1.1, color =                        "darkblue") +
    scale_y_continuous(labels = scales::number, limits = c(0, NA)) +
    labs(x = "Tygodnie roku", y = "Tygodniowa liczba zgonów",
         title = paste0(wybrany_kraj," \nTygodniowa liczba zgonów dla roku 2020 na tle poprzednich"),
         caption = "Autor: Kamil Pastor na podstawie Eurostatu") +
    theme_classic() +
    theme(axis.text = element_text(size = 12)) +
    geom_shadowtext(data = dane_max, aes(x = tydzien_liczba, y = obsValue, label = round(obsValue,0)),
                    vjust= 0, color = "darkblue", bg.colour="white" ) 
  
}


przyg_wyk_nadmiarowych_zgonow <- function(data_robocze, wybrany_kraj) {
  
  data_robocze2 <- data_robocze %>% filter(!is.na(obsValue)) %>%
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
    mutate(data = data + months(2))
  
  ggplot(dane_wykres, aes(x=data)) +
    geom_line(aes(y = szacunek), color = "darkgrey") +
    geom_line(aes(y = original), color = "darkblue", size = 1.2) +
    geom_ribbon(aes(ymin = dol, ymax = gora), fill = "grey", alpha = 0.5) +
    scale_y_continuous(labels = scales::number, limits = c(0, NA)) +
    labs(x = "", y= "Zgony tygodniowo",
         title = paste0(wybrany_kraj," \nNadzwyczajna śmiertelność - od 2016 roku"),
         subtitle = "Szary obszar oznacza przedział trend + sezonowość ± dwa odchylenia standardowe reszt.",
         caption = "Autor: Kamil Pastor na podstawie Eurostatu") +
    theme_classic() +
    theme(axis.text = element_text(size = 12)) +
    scale_x_date(date_labels = "%b %y", date_breaks = "12 months") +
    theme(panel.grid.major.x = element_line()) +
    geom_shadowtext(data = dane_max, aes(y = original, label = round(original,0)),
                    vjust= 0, color = "darkblue", bg.colour="white" ) +
    geom_shadowtext(data = dane_max, aes(y = szacunek, label = round(szacunek,0)),
                    vjust= 0, color = "dimgrey", bg.colour= "white" ) 
}



przyg_wyk_tygodniowego_regiony <- function(data_robocze, wybrany_kraj) {
  
  dane_max <- data_robocze %>% 
    filter(rok == 2020 ) %>%
    filter(tydzien_liczba == max(tydzien_liczba)) %>%
    mutate(tydzien_liczba = tydzien_liczba + 2)
  
  ggplot(data_robocze) +
    geom_line(aes(x = tydzien_liczba, y = obsValue, group = rok), color = "grey",                data = data_robocze %>% filter(rok != "2020")) +
    geom_line(aes(x = tydzien_liczba, y = obsValue, group = rok), data =                         data_robocze %>% filter(rok == "2020"), size = 1.1, color =                        "maroon") +
    scale_y_continuous(labels = scales::number, limits = c(0, NA)) +
    labs(x = "Tygodnie roku", y = "Tygodniowa liczba zgonów",
         title = paste0(wybrany_kraj," \nTygodniowa liczba zgonów dla roku 2020 na tle poprzednich"),
         caption = "Autor: Kamil Pastor na podstawie Eurostatu") +
    theme_classic() +
    theme(axis.text = element_text(size = 12)) +
    geom_shadowtext(data = dane_max, aes(x = tydzien_liczba, y = obsValue, label = round(obsValue,0)),
                    vjust= 0, color = "maroon", bg.colour="white" ) 
  
}


przyg_wyk_nadmiarowych_zgonow_regiony <- function(data_robocze, wybrany_kraj) {
  
  data_robocze2 <- data_robocze %>% filter(!is.na(obsValue)) %>%
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
    mutate(data = data + months(2))
  
  ggplot(dane_wykres, aes(x=data)) +
    geom_line(aes(y = szacunek), color = "darkgrey") +
    geom_line(aes(y = original), color = "maroon", size = 1.2) +
    geom_ribbon(aes(ymin = dol, ymax = gora), fill = "grey", alpha = 0.5) +
    scale_y_continuous(labels = scales::number, limits = c(0, NA)) +
    labs(x = "", y= "Zgony tygodniowo",
         title = paste0(wybrany_kraj," \nNadzwyczajna śmiertelność - od 2016 roku"),
         subtitle = "Szary obszar oznacza przedział trend + sezonowość ± dwa odchylenia standardowe reszt.",
         caption = "Autor: Kamil Pastor na podstawie Eurostatu") +
    theme_classic() +
    theme(axis.text = element_text(size = 12)) +
    scale_x_date(date_labels = "%b %y", date_breaks = "12 months") +
    theme(panel.grid.major.x = element_line()) +
    geom_shadowtext(data = dane_max, aes(y = original, label = round(original,0)),
                    vjust= 0, color = "maroon", bg.colour="white" ) +
    geom_shadowtext(data = dane_max, aes(y = szacunek, label = round(szacunek,0)),
                    vjust= 0, color = "dimgrey", bg.colour= "white" ) 
}



przyg_wyk_tygodniowego_wojewodztwa <- function(data_robocze) {
  
  dane_max <- data_robocze %>% 
    filter(rok == 2020 ) %>%
    filter(tydzien_liczba == max(tydzien_liczba)) %>%
    mutate(tydzien_liczba = tydzien_liczba + 5)
  
  ggplot(data_robocze) +
    geom_line(aes(x = tydzien_liczba, y = obsValue, group = rok), color = "grey",                data = data_robocze %>% filter(rok != "2020")) +
    geom_line(aes(x = tydzien_liczba, y = obsValue, group = rok), data =                         data_robocze %>% filter(rok == "2020"), size = 1.1, color =                        "maroon") +
    facet_wrap(~nazwa, ncol = 4) +
    scale_y_continuous(labels = scales::number, limits = c(0, 2000)) +
    labs(x = "Tygodnie roku", y = "Tygodniowa liczba zgonów",
         title = paste0("Tygodniowa liczba zgonów dla roku 2020 na tle poprzednich"),
         caption = "Autor: Kamil Pastor na podstawie Eurostatu") +
    theme_minimal() +
    theme(axis.text = element_text(),
          strip.text = element_text(face = "bold"),
          plot.title = element_text()) +
    geom_shadowtext(data = dane_max, aes(x = tydzien_liczba, y = obsValue, label = round(obsValue,0)),
                    vjust= 0, size = 3, color = "maroon", bg.colour="white" ) 
  
}
