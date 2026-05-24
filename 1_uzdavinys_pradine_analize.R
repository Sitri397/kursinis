library(dplyr)
library(sf)
library(rlang)
library(geodata)
library(ggplot2)
library(patchwork)
library(purrr)
library(stringr)
library(zoo)
library(scales)

data <- read.csv("duomenys.csv", TRUE, ",")

stulpeliai_pildymui <- c(
  "Gyventoju_tankis",
  "Gyventoju_amziaus_mediana",
  "Moterys_1000_vyru",
  "Apyvarta",
  "pareigunai",
  "Skurdo_rizika",
  "Bedarbiai",
  "Remtini_asmenys",
  "Nesimokantys_vaikai",
  "Imigrantai"
)

data <- data %>%
  arrange(Savivaldybė, Metai) %>%
  group_by(Savivaldybė) %>%
  mutate(across(
    all_of(stulpeliai_pildymui),
    ~ zoo::na.approx(., x = Metai, na.rm = FALSE, rule = 2)
  )) %>%
  ungroup()

lt_sf <- st_read("LTU.geojson")

names(lt_sf)
unique(lt_sf$name)

lt_sf <- lt_sf %>%
  mutate(
    Savivaldybė = case_when(
      name %in% c("Alytaus m. sav.", "Alytaus r. sav.") ~ "Alytaus savivaldybės",
      name == "Visagino m. sav." ~ "Visagino sav.",
      name == "Rietavo r. sav." ~ "Rietavo sav.",
      TRUE ~ name
    )
  ) %>%
  group_by(Savivaldybė) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

centrai <- read.csv("LT-savivaldybes-municipalities.csv", header = TRUE, sep = ";")

centrai <- centrai %>%
  mutate(
    SAV_PLATUM = as.numeric(gsub(",", ".", SAV_PLATUM)),
    SAV_ILGUMA = as.numeric(gsub(",", ".", SAV_ILGUMA))
  )
centrai <- centrai %>%
  mutate(
    Savivaldybė = case_when(
      SAV_PAV %in% c("Alytaus m. sav.", "Alytaus r. sav.") ~ "Alytaus savivaldybės",
      SAV_PAV == "Visagino m. sav." ~ "Visagino sav.",
      SAV_PAV == "Rietavo r. sav." ~ "Rietavo sav.",
      TRUE ~ SAV_PAV
    )
  ) %>%
  group_by(Savivaldybė) %>%
  summarise(
    SAV_PLATUM = mean(SAV_PLATUM),
    SAV_ILGUMA = mean(SAV_ILGUMA),
    .groups = "drop"
  ) %>%
  mutate(
    etikete = case_when(
      Savivaldybė == "Alytaus savivaldybės" ~ "Alytus",
      TRUE ~ gsub(" r\\. sav\\.| m\\. sav\\.| sav\\.", "", Savivaldybė)
    )
  )

ggplot() +
  geom_sf(data = lt_sf, fill = "white", color = "grey80", linewidth = 0.3) +
  geom_text(
    data = centrai,
    aes(x = SAV_ILGUMA, y = SAV_PLATUM, label = Savivaldybė),
    size = 2.5,
    check_overlap = TRUE
  ) +
  labs(title = "Lietuvos savivaldybės") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

options(scipen = 999)

metai_sarasas <- c(2004, 2007, 2010, 2013, 2016, 2019, 2022, 2025)

kintamieji <- c(
  "Nusikaltimų.skaičius.100.tūkst..gyventojų",
  "Gyventoju_tankis",
  "Gyventoju_amziaus_mediana",
  "Moterys_1000_vyru",
  "Apyvarta",
  "pareigunai",
  "Skurdo_rizika",
  "Bedarbiai",
  "Remtini_asmenys",
  "Nesimokantys_vaikai",
  "Imigrantai"
)

pavadinimai <- list(
  "Nusikaltimų.skaičius.100.tūkst..gyventojų" = "Nusikaltimų skaičius 100 tūkst. gyventojų",
  "Gyventoju_tankis" = "Gyventojų tankis (1 km²)",
  "Gyventoju_amziaus_mediana" = "Medianinis gyventojų amžius",
  "Moterys_1000_vyru" = "Moterų skaičius tenkantis 1 tūkst. vyrų",
  "Apyvarta" = "Nefinansinių įmonių apyvarta tūkst. eur.",
  "pareigunai" = "Policijos pareigūnų skaičius",
  "Skurdo_rizika" = "Skurdo rizikos lygis proc.",
  "Bedarbiai" = "Registruotų bedarbių skaičius",
  "Remtini_asmenys" = "Socialinės pašalpos gavėjų skaičius",
  "Nesimokantys_vaikai" = "Mokyklinio amžiaus vaikų nesimokančių mokykloje skaičius",
  "Imigrantai" = "Atvykusiųjų ir imigrantų skaičius per metus"
)

formatuoti_legendos_reiksmes <- function(x) {
  format(
    round(x, 0),
    big.mark = " ",
    decimal.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}

braizyti_kintamojo_zemelapius <- function(kintamasis) {
  
  reiksmes <- data %>%
    filter(Metai %in% metai_sarasas) %>%
    pull(.data[[kintamasis]])
  
  bendri_limits <- range(reiksmes, na.rm = TRUE)
  
  bendra_skale <- scale_fill_gradientn(
    colours = c("#fff5f0", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26", "#a50f15", "#67000d"),
    na.value = "white",
    trans = "sqrt",
    limits = bendri_limits,
    labels = formatuoti_legendos_reiksmes
  )
  
  braizyti_vieno_laiko_zemelapi <- function(metai) {
    
    data_metai <- data %>%
      filter(Metai == metai)
    
    map_data <- lt_sf %>%
      left_join(data_metai, by = "Savivaldybė")
    
    ggplot(map_data) +
      geom_sf(
        aes(fill = .data[[kintamasis]]),
        color = "grey35",
        linewidth = 0.2
      ) +
      bendra_skale +
      labs(
        title = as.character(metai),
        fill = str_wrap(pavadinimai[[kintamasis]], width = 25)
      ) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        legend.position = "right",
        plot.margin = margin(0, 0, 0, 0)
      )
  }
  
  zemelapiai <- lapply(metai_sarasas, braizyti_vieno_laiko_zemelapi)
  
  bendras_grafikas <- wrap_plots(zemelapiai, ncol = 4, guides = "collect") +
    plot_annotation(
      title = pavadinimai[[kintamasis]],
      theme = theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
      )
    )
  
  return(bendras_grafikas)
}

braizyti_kintamojo_zemelapius("Nusikaltimų.skaičius.100.tūkst..gyventojų")
braizyti_kintamojo_zemelapius("Gyventoju_tankis")
braizyti_kintamojo_zemelapius("Bedarbiai")
braizyti_kintamojo_zemelapius("Imigrantai")
braizyti_kintamojo_zemelapius("Apyvarta")
braizyti_kintamojo_zemelapius("Gyventoju_amziaus_mediana")
braizyti_kintamojo_zemelapius("Moterys_1000_vyru")
braizyti_kintamojo_zemelapius("pareigunai")
braizyti_kintamojo_zemelapius("Skurdo_rizika")
braizyti_kintamojo_zemelapius("Remtini_asmenys")
braizyti_kintamojo_zemelapius("Nesimokantys_vaikai")