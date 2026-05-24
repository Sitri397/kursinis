library(dplyr)
library(purrr)
library(tools)

# -----------------------------
# 1. Pagrindiniai nusikalstamumo duomenys
# -----------------------------
main_data <- list.files(pattern = "\\.csv$") %>%
  map(\(file) {
    read.csv(file, sep = ";", encoding = "UTF-8", colClasses = "character") %>%
      select(1, 2, 3, 4, 6, 8, 10)
  }) %>%
  bind_rows() %>%
  filter(
    Mėnuo == "Sausis - Gruodis",
    Savivaldybė != "Respublika"
  ) %>%
  mutate(
    Metai = as.numeric(Metai),
    `Nusikalstamų.veikų.skaičius.100.tūkstančių.gyventojų` =
      as.numeric(`Nusikalstamų.veikų.skaičius.100.tūkstančių.gyventojų`),
    `Nusikalstamos.veikos` = as.numeric(`Nusikalstamos.veikos`),
    `Nusikaltimai` = as.numeric(`Nusikaltimai`),
    `Nusižengimai` = as.numeric(`Nusižengimai`),
    Gyventojai = round(
      `Nusikalstamos.veikos` * 100000 /
        `Nusikalstamų.veikų.skaičius.100.tūkstančių.gyventojų`
    )
  )

# -----------------------------
# 2. Alytaus miesto ir rajono sujungimas
# -----------------------------
alytus_nusikaltimai <- main_data %>%
  filter(Savivaldybė %in% c("Alytaus miestas", "Alytaus rajonas")) %>%
  group_by(Metai, Mėnuo) %>%
  summarise(
    Savivaldybė = "Alytaus savivaldybės",
    Gyventojai = sum(Gyventojai, na.rm = TRUE),
    `Nusikalstamos.veikos` = sum(`Nusikalstamos.veikos`, na.rm = TRUE),
    `Nusikaltimai` = sum(`Nusikaltimai`, na.rm = TRUE),
    `Nusižengimai` = sum(`Nusižengimai`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    `Nusikalstamų.veikų.skaičius.100.tūkstančių.gyventojų` =
      round(`Nusikalstamos.veikos` / Gyventojai * 100000, 1),
    `Nusikaltimų skaičius 100 tūkst. gyventojų` =
      round(`Nusikaltimai` / Gyventojai * 100000, 1)
  )

final_dataset <- main_data %>%
  filter(!Savivaldybė %in% c("Alytaus miestas", "Alytaus rajonas")) %>%
  bind_rows(alytus_nusikaltimai) %>%
  mutate(
    `Nusikaltimų skaičius 100 tūkst. gyventojų` =
      round(`Nusikaltimai` / Gyventojai * 100000, 1)
  ) %>%
  arrange(Metai, Savivaldybė) %>%
  select(Metai, Savivaldybė, `Nusikaltimų skaičius 100 tūkst. gyventojų`)

# -----------------------------
# 3. Savivaldybių pavadinimų suvienodinimas
# -----------------------------
final_dataset <- final_dataset %>%
  mutate(
    Savivaldybė = recode(
      Savivaldybė,
      "Akmenės rajonas" = "Akmenės r. sav.",
      "Anykščių rajonas" = "Anykščių r. sav.",
      "Birštonas" = "Birštono sav.",
      "Biržų rajonas" = "Biržų r. sav.",
      "Druskininkai" = "Druskininkų sav.",
      "Elektrėnai" = "Elektrėnų sav.",
      "Ignalinos rajonas" = "Ignalinos r. sav.",
      "Jonavos rajonas" = "Jonavos r. sav.",
      "Joniškio rajonas" = "Joniškio r. sav.",
      "Jurbarko rajonas" = "Jurbarko r. sav.",
      "Kaišiadorių rajonas" = "Kaišiadorių r. sav.",
      "Kalvarijos savivaldybė" = "Kalvarijos sav.",
      "Kauno miestas" = "Kauno m. sav.",
      "Kauno rajonas" = "Kauno r. sav.",
      "Kazlų Rūdos savivaldybė" = "Kazlų Rūdos sav.",
      "Kelmės rajonas" = "Kelmės r. sav.",
      "Klaipėdos miestas" = "Klaipėdos m. sav.",
      "Klaipėdos rajonas" = "Klaipėdos r. sav.",
      "Kretingos rajonas" = "Kretingos r. sav.",
      "Kupiškio rajonas" = "Kupiškio r. sav.",
      "Kėdainių rajonas" = "Kėdainių r. sav.",
      "Lazdijų rajonas" = "Lazdijų r. sav.",
      "Marijampolės PK" = "Marijampolės sav.",
      "Marijampolės savivaldybė" = "Marijampolės sav.",
      "Mažeikių rajonas" = "Mažeikių r. sav.",
      "Molėtų rajonas" = "Molėtų r. sav.",
      "Neringa" = "Neringos sav.",
      "Pagėgių savivaldybė" = "Pagėgių sav.",
      "Pakruojo rajonas" = "Pakruojo r. sav.",
      "Palanga" = "Palangos m. sav.",
      "Panevėžio miestas" = "Panevėžio m. sav.",
      "Panevėžio rajonas" = "Panevėžio r. sav.",
      "Pasvalio rajonas" = "Pasvalio r. sav.",
      "Plungės rajonas" = "Plungės r. sav.",
      "Prienų rajonas" = "Prienų r. sav.",
      "Radviliškio rajonas" = "Radviliškio r. sav.",
      "Raseinių rajonas" = "Raseinių r. sav.",
      "Rietavo savivaldybė" = "Rietavo sav.",
      "Rokiškio rajonas" = "Rokiškio r. sav.",
      "Skuodo rajonas" = "Skuodo r. sav.",
      "Tauragės rajonas" = "Tauragės r. sav.",
      "Telšių rajonas" = "Telšių r. sav.",
      "Trakų rajonas" = "Trakų r. sav.",
      "Ukmergės rajonas" = "Ukmergės r. sav.",
      "Utenos rajonas" = "Utenos r. sav.",
      "Varėnos rajonas" = "Varėnos r. sav.",
      "Vilkaviškio rajonas" = "Vilkaviškio r. sav.",
      "Vilniaus miestas" = "Vilniaus m. sav.",
      "Vilniaus rajonas" = "Vilniaus r. sav.",
      "Visaginas" = "Visagino sav.",
      "Zarasų rajonas" = "Zarasų r. sav.",
      "Šakių rajonas" = "Šakių r. sav.",
      "Šalčininkų rajonas" = "Šalčininkų r. sav.",
      "Šiaulių miestas" = "Šiaulių m. sav.",
      "Šiaulių rajonas" = "Šiaulių r. sav.",
      "Šilalės rajonas" = "Šilalės r. sav.",
      "Šilutės rajonas" = "Šilutės r. sav.",
      "Širvintų rajonas" = "Širvintų r. sav.",
      "Švenčionių rajonas" = "Švenčionių r. sav."
    )
  )

# -----------------------------
# 4. Faktorių failų nuskaitymas
# -----------------------------
faktoriai_list <- list.files(
  "faktoriai",
  pattern = "\\.csv$",
  full.names = TRUE
) %>%
  set_names(make.names(file_path_sans_ext(basename(.)), unique = TRUE)) %>%
  map(\(file) read.csv(file, sep = ",", encoding = "UTF-8"))

# -----------------------------
# 5. Pagalbinė gyventojų lentelė
# -----------------------------
gyventojai_sujungimui <- main_data %>%
  mutate(
    Savivaldybė = case_when(
      Savivaldybė == "Alytaus miestas" ~ "Alytaus m. sav.",
      Savivaldybė == "Alytaus rajonas" ~ "Alytaus r. sav.",
      TRUE ~ Savivaldybė
    )
  ) %>%
  select(Metai, Savivaldybė, Gyventojai) %>%
  filter(!(Metai == 2004 & Savivaldybė == "Alytaus r. sav.")) %>%
  bind_rows(
    tibble(
      Metai = 2004,
      Savivaldybė = "Alytaus r. sav.",
      Gyventojai = 31578
    )
  ) %>%
  arrange(Metai, Savivaldybė)

# -----------------------------
# 6. Gyventojų tankis
# -----------------------------
Gyventoju_tankis <- faktoriai_list$Gyventoju_tankis_vienam_km2 %>%
  select(Laikotarpis, Administracinė.teritorija, Reikšmė) %>%
  transmute(
    Metai = as.numeric(Laikotarpis),
    Savivaldybė = Administracinė.teritorija,
    Gyventoju_tankis = as.numeric(Reikšmė)
  )

alytus_tankis <- Gyventoju_tankis %>%
  filter(Savivaldybė %in% c("Alytaus m. sav.", "Alytaus r. sav.")) %>%
  left_join(gyventojai_sujungimui, by = c("Metai", "Savivaldybė")) %>%
  mutate(
    Plotas = Gyventojai / Gyventoju_tankis
  ) %>%
  group_by(Metai) %>%
  summarise(
    Savivaldybė = "Alytaus savivaldybės",
    Gyventoju_tankis = round(
      sum(Gyventojai, na.rm = TRUE) / sum(Plotas, na.rm = TRUE),
      2
    ),
    .groups = "drop"
  )

Gyventoju_tankis <- Gyventoju_tankis %>%
  filter(!Savivaldybė %in% c("Alytaus m. sav.", "Alytaus r. sav.", "Marijampolės r. sav.")) %>%
  bind_rows(alytus_tankis) %>%
  arrange(Metai, Savivaldybė)

final_dataset <- final_dataset %>%
  left_join(Gyventoju_tankis, by = c("Metai", "Savivaldybė"))

# -----------------------------
# 7. Medianinis amžius
# -----------------------------
Medianinis_amzius <- faktoriai_list$medianinis_gyventoju_amzius %>%
  select(Laikotarpis, Administracinė.teritorija, Reikšmė) %>%
  transmute(
    Metai = as.numeric(Laikotarpis),
    Savivaldybė = Administracinė.teritorija,
    Gyventoju_amziaus_mediana = as.numeric(Reikšmė)
  )

alytus_amzius <- Medianinis_amzius %>%
  filter(Savivaldybė %in% c("Alytaus m. sav.", "Alytaus r. sav.")) %>%
  left_join(gyventojai_sujungimui, by = c("Metai", "Savivaldybė")) %>%
  group_by(Metai) %>%
  summarise(
    Savivaldybė = "Alytaus savivaldybės",
    Gyventoju_amziaus_mediana = round(
      sum(Gyventoju_amziaus_mediana * Gyventojai, na.rm = TRUE) /
        sum(Gyventojai, na.rm = TRUE)
    ),
    .groups = "drop"
  )

Medianinis_amzius <- Medianinis_amzius %>%
  filter(!Savivaldybė %in% c("Alytaus m. sav.", "Alytaus r. sav.", "Marijampolės r. sav.")) %>%
  bind_rows(alytus_amzius) %>%
  arrange(Metai, Savivaldybė)

final_dataset <- final_dataset %>%
  left_join(Medianinis_amzius, by = c("Metai", "Savivaldybė"))

# -----------------------------
# 8. Moterų skaičius 1000 vyrų
# -----------------------------
Moterys_vyrai <- faktoriai_list$moteru_skaicius_tenkantis_tukstanciui_vyru %>%
  select(Laikotarpis, Administracinė.teritorija, Reikšmė) %>%
  transmute(
    Metai = as.numeric(Laikotarpis),
    Savivaldybė = Administracinė.teritorija,
    Moterys_1000_vyru = as.numeric(Reikšmė)
  )

alytus_lytis <- Moterys_vyrai %>%
  filter(Savivaldybė %in% c("Alytaus m. sav.", "Alytaus r. sav.")) %>%
  left_join(gyventojai_sujungimui, by = c("Metai", "Savivaldybė")) %>%
  mutate(
    Vyrai = Gyventojai / (1 + Moterys_1000_vyru / 1000),
    Moterys = Gyventojai - Vyrai
  ) %>%
  group_by(Metai) %>%
  summarise(
    Savivaldybė = "Alytaus savivaldybės",
    Moterys_1000_vyru = round(sum(Moterys, na.rm = TRUE) / sum(Vyrai, na.rm = TRUE) * 1000),
    .groups = "drop"
  )

Moterys_vyrai <- Moterys_vyrai %>%
  filter(!Savivaldybė %in% c("Alytaus m. sav.", "Alytaus r. sav.", "Marijampolės r. sav.")) %>%
  bind_rows(alytus_lytis) %>%
  arrange(Metai, Savivaldybė)

final_dataset <- final_dataset %>%
  left_join(Moterys_vyrai, by = c("Metai", "Savivaldybė"))

# -----------------------------
# 9. Nefinansinių įmonių apyvarta
# -----------------------------
apyvarta <- faktoriai_list$nefinansiniu_imoniu_apyvarta %>%
  select(Laikotarpis, Administracinė.teritorija, Reikšmė) %>%
  transmute(
    Metai = as.numeric(Laikotarpis),
    Savivaldybė = Administracinė.teritorija,
    Apyvarta = as.numeric(Reikšmė)
  ) %>%
  mutate(
    Savivaldybė = if_else(
      Savivaldybė %in% c("Alytaus m. sav.", "Alytaus r. sav."),
      "Alytaus savivaldybės",
      Savivaldybė
    )
  ) %>%
  group_by(Metai, Savivaldybė) %>%
  summarise(
    Apyvarta = sum(Apyvarta, na.rm = TRUE),
    .groups = "drop"
  )

final_dataset <- final_dataset %>%
  left_join(apyvarta, by = c("Metai", "Savivaldybė"))

# -----------------------------
# 10. Policijos pareigūnų skaičius
# -----------------------------
pareigunai <- faktoriai_list$policijos_pareigunu_skaicius %>%
  select(Laikotarpis, Administracinė.teritorija, Reikšmė) %>%
  transmute(
    Metai = as.numeric(Laikotarpis),
    Savivaldybė = Administracinė.teritorija,
    Pareigunai = as.numeric(Reikšmė)
  ) %>%
  mutate(
    Savivaldybė = if_else(
      Savivaldybė %in% c("Alytaus m. sav.", "Alytaus r. sav."),
      "Alytaus savivaldybės",
      Savivaldybė
    )
  ) %>%
  group_by(Metai, Savivaldybė) %>%
  summarise(
    pareigunai = sum(Pareigunai, na.rm = TRUE),
    .groups = "drop"
  )

final_dataset <- final_dataset %>%
  left_join(pareigunai, by = c("Metai", "Savivaldybė"))

# -----------------------------
# 11. Skurdo rizika
# -----------------------------
skurdas <- faktoriai_list$skurdo_rizikos_lygis_nuo_2010 %>%
  select(Laikotarpis, Administracinė.teritorija, Reikšmė) %>%
  transmute(
    Metai = as.numeric(Laikotarpis),
    Savivaldybė = Administracinė.teritorija,
    Skurdo_rizika = as.numeric(Reikšmė)
  )

alytus_skurdas <- skurdas %>%
  filter(Savivaldybė %in% c("Alytaus m. sav.", "Alytaus r. sav.")) %>%
  left_join(gyventojai_sujungimui, by = c("Metai", "Savivaldybė")) %>%
  group_by(Metai) %>%
  summarise(
    Savivaldybė = "Alytaus savivaldybės",
    Skurdo_rizika = round(
      sum(Skurdo_rizika * Gyventojai, na.rm = TRUE) /
        sum(Gyventojai, na.rm = TRUE),
      1
    ),
    .groups = "drop"
  )

skurdas <- skurdas %>%
  filter(!Savivaldybė %in% c("Alytaus m. sav.", "Alytaus r. sav.")) %>%
  bind_rows(alytus_skurdas)

final_dataset <- final_dataset %>%
  left_join(skurdas, by = c("Metai", "Savivaldybė"))

# -----------------------------
# 12. Bedarbiai
# -----------------------------
bedarbiai <- faktoriai_list$bedarbiu_skaicius %>%
  select(Laikotarpis, Administracinė.teritorija, Reikšmė) %>%
  transmute(
    Metai = as.numeric(Laikotarpis),
    Savivaldybė = Administracinė.teritorija,
    Bedarbiai = as.numeric(Reikšmė)
  ) %>%
  mutate(
    Savivaldybė = if_else(
      Savivaldybė %in% c("Alytaus m. sav.", "Alytaus r. sav."),
      "Alytaus savivaldybės",
      Savivaldybė
    )
  ) %>%
  group_by(Metai, Savivaldybė) %>%
  summarise(
    Bedarbiai = sum(Bedarbiai, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!Savivaldybė %in% c("Marijampolės r. sav."))

final_dataset <- final_dataset %>%
  left_join(bedarbiai, by = c("Metai", "Savivaldybė"))

# -----------------------------
# 13. Socialinė pašalpa
# -----------------------------
pasalpos <- faktoriai_list$socialine_pasalpa_gaunanciu_asmenu_skaicius %>%
  select(Laikotarpis, Administracinė.teritorija, Reikšmė) %>%
  transmute(
    Metai = as.numeric(Laikotarpis),
    Savivaldybė = Administracinė.teritorija,
    Remtini_asmenys = as.numeric(Reikšmė)
  ) %>%
  mutate(
    Savivaldybė = if_else(
      Savivaldybė %in% c("Alytaus m. sav.", "Alytaus r. sav."),
      "Alytaus savivaldybės",
      Savivaldybė
    )
  ) %>%
  group_by(Metai, Savivaldybė) %>%
  summarise(
    Remtini_asmenys = sum(Remtini_asmenys, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!Savivaldybė %in% c("Marijampolės r. sav."))

final_dataset <- final_dataset %>%
  left_join(pasalpos, by = c("Metai", "Savivaldybė"))

# -----------------------------
# 14. Nesimokantys vaikai
# -----------------------------
nesimokantys_vaikai <- faktoriai_list$Vaikai_nesimokantys_mokykloje_nors_turetu_nuo_2009 %>%
  select(Laikotarpis, Administracinė.teritorija, Reikšmė) %>%
  transmute(
    Metai = as.numeric(substr(Laikotarpis, 1, 4)),
    Savivaldybė = Administracinė.teritorija,
    Nesimokantys_vaikai = as.numeric(Reikšmė)
  ) %>%
  mutate(
    Savivaldybė = if_else(
      Savivaldybė %in% c("Alytaus m. sav.", "Alytaus r. sav."),
      "Alytaus savivaldybės",
      Savivaldybė
    )
  ) %>%
  group_by(Metai, Savivaldybė) %>%
  summarise(
    Nesimokantys_vaikai = sum(Nesimokantys_vaikai, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!Savivaldybė %in% c("Marijampolės r. sav."))

final_dataset <- final_dataset %>%
  left_join(nesimokantys_vaikai, by = c("Metai", "Savivaldybė"))

# -----------------------------
# 15. Imigrantai
# -----------------------------
imigrantai <- faktoriai_list$imigrantu_skaicius %>%
  select(Laikotarpis, Administracinė.teritorija, Reikšmė) %>%
  transmute(
    Metai = as.numeric(Laikotarpis),
    Savivaldybė = Administracinė.teritorija,
    Imigrantai = as.numeric(Reikšmė)
  ) %>%
  mutate(
    Savivaldybė = if_else(
      Savivaldybė %in% c("Alytaus m. sav.", "Alytaus r. sav."),
      "Alytaus savivaldybės",
      Savivaldybė
    )
  ) %>%
  group_by(Metai, Savivaldybė) %>%
  summarise(
    Imigrantai = sum(Imigrantai, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!Savivaldybė %in% c("Marijampolės r. sav."))

final_dataset <- final_dataset %>%
  left_join(imigrantai, by = c("Metai", "Savivaldybė"))

write.csv(final_dataset, "duomenys.csv", row.names = FALSE)

