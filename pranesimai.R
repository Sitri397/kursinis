library(readr)
library(dplyr)
library(data.table)
library(lubridate)
library(readxl)
library(fixest)
library(AER)
library(MASS)
library(tseries)
library(sf)
library(viridis)
library(ggplot2)
library(ggspatial)
library(stringr)
library(forecast)
library(survey)
library(patchwork)
library(tsibble)
library(feasts)
library(scales)
library(ggrepel)
library(ggtime)
library(glarma)
library(spINAR)
library(parallel)
library(pbapply)
library(FinTS)
library(knitr)
library(kableExtra)
library(tscount)
theme_set(
  theme_gray(base_size = 15) +
    theme(
      plot.title = element_text(size = 20, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 14),
      
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 15, face = "bold")
    )
)
set.seed(123)

clean_first_word <- function(x) {
  x <- trimws(tolower(x))
  
  res <- rep(NA_character_, length(x))
  ok <- !is.na(x) & x != ""
  
  res[ok] <- sub(" .*", "", x[ok])
  
  abbr_idx <- ok & grepl("^[^[:space:]]+\\.\\s+", x)
  
  res[abbr_idx] <- sub("^([^[:space:]]+\\s+[^[:space:]]+).*$", "\\1", x[abbr_idx])
  
  res
}

data <- read_csv('prir_combined_crime_data.csv') |> as.data.table()
sen_2 <- fread("streets_with_seniunija.csv")
sen_3 <- fread("adr_gatves.csv", sep = "|")
sen_4 <- fread("sen_4.csv", sep = "|")
seniunijos <- read_xlsx("seniunijos.xlsx") |> as.data.table()
map_data <- st_read("adr_gra_seniunijos.json", quiet = TRUE)
map_data$sen_kodas <- as.character(map_data$SEN_KODAS)

praleistu_stebejimu_procentas_1 <- data[, .(Procentas = mean(is.na(seniunija)) * 100)]

#--------------------------------------------------------------------
#Not NA count by city

selected_vietove <- c("Vilnius", "Kaunas", "Klaipėda")

result_vietove <- data[
  vietove %in% selected_vietove,
  .(non_na_count = sum(!is.na(seniunija))),
  by = vietove
]

#--------------------------------------------------------------------
#seniunija input

seniunijos[, lower := clean_first_word(VARDAS_K)]

data[, `:=`(
  pakeista_vietove_lower = clean_first_word(vietove),
  pakeista_gatve_lower = clean_first_word(gatve)
)]

sen_full <- merge(sen_2, sen_3, by = "GAT_KODAS", all.x = TRUE) |>
  merge(sen_4, by = "GYV_KODAS", all.x = TRUE) |>
  as.data.table()

sen_full[, `:=`(
  VARDAS_K.x_lower = clean_first_word(VARDAS_K.x),
  VARDAS_K.y_lower = clean_first_word(VARDAS_K.y),
  VARDAS_lower = clean_first_word(VARDAS)
)]

sen_full[, `:=`(
  key1 = paste0(VARDAS_K.x_lower, "___", VARDAS_K.y_lower),
  key2 = paste0(VARDAS_K.x_lower, "___", VARDAS_lower)
)]

data[, key := paste0(pakeista_gatve_lower, "___", pakeista_vietove_lower)]

map1 <- unique(sen_full[, .(key = key1, sen_kodas = SEN_KODAS.x)], by = "key")
map2 <- unique(sen_full[, .(key = key2, sen_kodas = SEN_KODAS.x)], by = "key")
map3 <- unique(sen_full[, .(seniunija = SEN_PAV, sen_kodas = SEN_KODAS.x)], by = "seniunija")

setkey(map1, key)
setkey(map2, key)
setkey(map3, seniunija)

data[, sen_kodas := map1[.SD, on = .(key), sen_kodas]]
data[is.na(sen_kodas), sen_kodas := map2[.SD, on = .(key), sen_kodas]]
data[is.na(sen_kodas), sen_kodas := map3[.SD, on = .(seniunija), sen_kodas]]

praleistu_stebejimu_procentas_4 <- data[, .(Procentas = mean(is.na(sen_kodas)) * 100)]

plot_data <- data.table(
  Stage = factor(
    c("Prieš apdorojimą", "Po apdorojimo"),
    levels = c("Prieš apdorojimą", "Po apdorojimo")
  ),
  Missing = c(
    praleistu_stebejimu_procentas_1$Procentas,
    praleistu_stebejimu_procentas_4$Procentas
  )
)

ggplot(plot_data, aes(x = Stage, y = Missing, fill = Stage)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  
  geom_text(aes(label = paste0(
    format(round(Missing, 1), decimal.mark = ","),
    "%"
  )),
  vjust = -0.6,
  size = 6,
  fontface = "bold") +
  
  scale_fill_manual(values = c(
    "Prieš apdorojimą" = "#e74c3c",  
    "Po apdorojimo" = "#2ecc71"     
  )) +
  
  labs(
    title = "Praleistų seniūnijų procentas prieš ir po duomenų apdorojimo",
    x = NULL,
    y = "Praleistų reikšmių (%)"
  ) +
  
  ylim(0, max(plot_data$Missing) * 1.2) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = 15),
    panel.grid.major.x = element_blank(),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 15)
  )

ggsave("images/praleistos_reiksmes.png", width = 8, height = 5, dpi = 300)

#---------------------------------------------------------------------------
#Not NA after seniunija input

result_vietove_2 <- data[
  vietove %in% selected_vietove,
  .(non_na_count = sum(!is.na(sen_kodas))),
  by = vietove
]

df <- merge(
  result_vietove,
  result_vietove_2,
  by = "vietove",
  suffixes = c("_before", "_after")
)

df[, pct_change := 100 * (non_na_count_after - non_na_count_before) / non_na_count_before]

df[, change_type := ifelse(pct_change >= 0, "increase", "decrease")]

ggplot(df, aes(x = vietove, y = pct_change, fill = change_type)) +
  geom_col(width = 0.6) +
  scale_fill_manual(
    values = c(
      "increase" = "#1a9641",  
      "decrease" = "#d7191c"  
    ),
    guide = "none"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Pokytis (%) po duomenų apdorojimo",
    x = NULL,
    y = "Pokytis (%)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold")
  )
ggsave("images/sen_pok.png", width = 8, height = 5, dpi = 300)

#-----------------------------------------------------------------------------
#final dataset

data <- data[
  !is.na(sen_kodas) &
    !is.na(infoGavimoLaikas <- parse_date_time(
      infoGavimoLaikas,
      orders = c("Y-m-d H:M:S", "Y-m-d H:M",
                 "Y/m/d H:M:S", "d.m.Y H:M:S"),
      tz = "UTC"
    )) &
    infoGavimoLaikas >= as.POSIXct("2019-01-01", tz = "UTC")
]

extra_codes <- c("9505", "9506", "9507", "9508")

data <- data[
  startsWith(as.character(sen_kodas), "13") |
    startsWith(as.character(sen_kodas), "19") |
    as.character(sen_kodas) %in% extra_codes
]

data[seniunijos, seniunija := i.VARDAS_K, on = .(sen_kodas = SEN_KODAS)]



data[, hour_time := floor_date(infoGavimoLaikas, unit = "hour")]

all_hours <- CJ(
  sen_kodas = unique(data$sen_kodas),
  hour_time = seq(
    floor_date(min(data$infoGavimoLaikas), "hour"),
    ceiling_date(max(data$infoGavimoLaikas), "hour"),
    by = "hour"
  )
)

agg <- data[, .(N = .N), by = .(sen_kodas, hour_time)]

result <- merge(all_hours, agg, by = c("sen_kodas", "hour_time"), all.x = TRUE)
result[is.na(N), N := 0]

result[, `:=`(
  weekday = weekdays(hour_time),
  hour = factor(hour(hour_time)),
  hour_time = as_datetime(hour_time),
  sen_kodas = factor(sen_kodas),
  year = factor(year(hour_time)),
  month = factor(month(hour_time)),
  day   = factor(day(hour_time))
)]

#-------------------------------------------------
#ZEMELAPIS

result[, date := as.Date(hour_time)]


daily <- result[, .(daily_N = sum(N, na.rm = TRUE)), by = .(sen_kodas, date)]
avg_daily <- daily[, .(avg_N = mean(daily_N, na.rm = TRUE)), by = sen_kodas]

avg_13 <- avg_daily[grepl("^13", sen_kodas)]


avg_19 <- avg_daily[
  grepl("^19", sen_kodas) | sen_kodas %in% extra_codes
]

map_data <- merge(map_data, avg_daily, by = "sen_kodas", all.x = TRUE)
map_13 <- map_data[grepl("^13", map_data$sen_kodas), ]
map_19 <- map_data[
  grepl("^19", map_data$sen_kodas) | map_data$sen_kodas %in% extra_codes,
]


common_limits <- range(
  c(map_13$avg_N, map_19$avg_N),
  na.rm = TRUE
)

ggplot() +
  annotation_map_tile(type = "osm") + 
  geom_sf(data = map_13, aes(fill = avg_N), alpha = 0.3) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "",
    limits = common_limits
  ) +
  labs(title = "Vidutinis gautų pranešimų skaičius per diena Vilniuje") +
  theme_minimal()

ggsave("images/vilnius_vid.png", width = 8, height = 5, dpi = 300)

ggplot() +
  annotation_map_tile(type = "osm") + 
  geom_sf(data = map_19, aes(fill = avg_N), alpha = 0.3) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "",
    limits = common_limits
  ) +
  labs(title = "Vidutinis gautų pranešimų skaičius per diena Kaune") +
  theme_minimal()

ggsave("images/kaunas_vid.png", width = 8, height = 5, dpi = 300)


#-----------------------------------------------------------------------
#top 3

setDT(map_13)
setDT(map_19)

plot_top_bottom <- function(dt, title_text) {
  
  dt_plot <- dt[!is.na(avg_N)]

  label_col <- if ("seniunija" %in% names(dt_plot)) {
    "seniunija"
  } else if ("SEN_PAV" %in% names(dt_plot)) {
    "SEN_PAV"
  } else {
    "sen_kodas"
  }
  
  dt_plot <- unique(dt_plot[, .(
    sen_kodas,
    label = get(label_col),
    avg_N
  )])
  
  top3 <- dt_plot[order(-avg_N)][1:min(3, .N)]
  top3[, group := "Top 3"]
  
  bottom3 <- dt_plot[order(avg_N)][1:min(3, .N)]
  bottom3[, group := "Bottom 3"]
  
  chosen <- unique(rbind(top3, bottom3, fill = TRUE), by = "sen_kodas")
  
  top_part <- chosen[group == "Top 3"][order(-avg_N)]
  bottom_part <- chosen[group == "Bottom 3"][order(-avg_N)]
  
  gap_row <- data.table(
    sen_kodas = NA_character_,
    label = "...",
    avg_N = NA_real_,
    group = "Gap"
  )
  
  final_dt <- rbind(top_part, gap_row, bottom_part, fill = TRUE)
  
  final_dt[, label := factor(label, levels = rev(label))]
  
  final_dt[, fill_group := fifelse(group == "Top 3", "Top 3",
                                   fifelse(group == "Bottom 3", "Bottom 3", "Gap"))]
  
  ggplot(final_dt, aes(x = label, y = avg_N, fill = fill_group)) +
    geom_col(width = 0.7, na.rm = TRUE) +
    coord_flip() +
    scale_fill_manual(
      values = c(
        "Top 3" = "#1b9e77",
        "Bottom 3" = "#d95f02",
        "Gap" = "white"
      ),
      guide = "none"
    ) +
    labs(
      title = title_text,
      x = NULL,
      y = "Vidutinis pranešimų skaičius per dieną"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major.y = element_blank()
    )
}

p_13 <- plot_top_bottom(
  map_13,
  "Vilniaus seniūnijos pagal vidutinį pranešimų skaičių"
)

p_13

ggsave("images/vilnius_top_bottom.png", p_13, width = 9, height = 5, dpi = 300)

p_19 <- plot_top_bottom(
  map_19,
  "Kauno seniūnijos pagal vidutinį pranešimų skaičių"
)

p_19

ggsave("images/kaunas_top_bottom.png", p_19, width = 9, height = 5, dpi = 300)

#-----------------------------------------------------------------------
#Time Series Graph weekdays
daily <- result[, .(
  daily_N = sum(N, na.rm = TRUE)
), by = .(
  date = as.Date(hour_time),
  weekday
)]

weekly_avg <- daily[, .(
  avg_N = mean(daily_N, na.rm = TRUE)
), by = weekday]

weekday_order <- c(
  "Monday", "Tuesday", "Wednesday",
  "Thursday", "Friday", "Saturday", "Sunday"
)

weekly_avg[, weekday := factor(weekday, levels = weekday_order)]
setorder(weekly_avg, weekday)

ggplot(weekly_avg, aes(x = weekday, y = avg_N, group = 1)) +
  geom_line(linewidth = 1.2, color = "#2C7BB6") +
  geom_point(size = 3, color = "#D7191C") +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_discrete(labels = c(
    "Monday" = "Pirmadienis",
    "Tuesday" = "Antradienis",
    "Wednesday" = "Trečiadienis",
    "Thursday" = "Ketvirtadienis",
    "Friday" = "Penktadienis",
    "Saturday" = "Šeštadienis",
    "Sunday" = "Sekmadienis"
  )) +
  labs(
    title = "Savaitinis sezoniškumas",
    x = NULL,
    y = "Vidutinis įvykių skaičius per dieną"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("images/weekdays_vid.png", width = 8, height = 5, dpi = 300)

#-----------------------------------------------------------------------
#Time Series Graph hours

daily_hour <- result[, .(
  avg_N = mean(N, na.rm = TRUE)
), by = .(hour = (as.numeric(hour)) - 1)]

setorder(daily_hour, hour)
ggplot(daily_hour, aes(x = hour, y = avg_N)) +
  geom_line(linewidth = 1.2, color = "#2C7BB6") +
  geom_point(size = 3, color = "#D7191C") +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  scale_y_continuous(
    limits = c(0, NA),
    labels = label_number(decimal.mark = ",")
  ) +
  labs(
    title = "Paros sezoniškumas",
    x = "Valanda",
    y = "Vidutinis įvykių skaičius"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )
ggsave("images/hours_vid.png", width = 8, height = 5, dpi = 300)


#-----------------------------------------------------------------------
#Barplot

mean(result$N)
var(result$N)

plot_dt <- as.data.table(table(result$N))
setnames(plot_dt, c("N", "Count"))

plot_dt[, N := as.numeric(as.character(N))]

ggplot(plot_dt, aes(x = N, y = Count)) +
  geom_col(fill = "#2C7BB6", width = 0.8) +
  
  geom_text_repel(
    data = plot_dt[N >= 40 & Count > 0 & N != 99],
    aes(label = N),
    size = 4,
    direction = "y",
    nudge_y = max(plot_dt$Count) * 0.05,
    segment.color = "grey50",
    max.overlaps = Inf
  ) +
  

  geom_point(
    data = plot_dt[N == 99],
    color = "red",
    size = 3
  ) +
  
  geom_text_repel(
    data = plot_dt[N == 99],
    aes(label = N),
    color = "red",
    size = 5,
    fontface = "bold",
    nudge_y = max(plot_dt$Count) * 0.1,
    segment.color = "red"
  ) +
  
  scale_x_continuous(
    breaks = seq(0, max(plot_dt$N), by = 5)
  ) +
  
  scale_y_continuous(
    labels = scales::label_number(
      scale = 1e-3,
      big.mark = " ",
      decimal.mark = ","
    )
  ) +
  
  labs(
    title = "Įvykių skaičiaus pasiskirstymas",
    x = "Įvykių skaičius",
    y = "Dažnis (tūkst.)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )
ggsave("images/barplot_hist.png", width = 8, height = 5, dpi = 300)

obs_99 <- result[N == 99]
#-----------------------------------------------------------------------
#SARIMAX



# Выбираем одну seniunija
one <- copy(result[sen_kodas == "1313"])
setorder(one, hour_time)
y <- one$N
ts_y = ts(y,frequency = 24)

adf_results <- result[
  order(sen_kodas, hour_time),
  {
    y <- N
    
    # ts объект с суточной сезонностью
    ts_y <- ts(y, frequency = 24)
    
    # ADF тест
    adf_res <- tryCatch(
      adf.test(ts_y),
      error = function(e) NULL
    )
    
    # Если тест упал
    if (is.null(adf_res)) {
      .(
        p_value = NA_real_
      )
    } else {
      .(
        p_value = round(adf_res$p.value, 6)
      )
    }
    
  },
  by = sen_kodas
]

# ============================================================
# LaTeX таблица
# ============================================================

adf_latex <- kable(
  adf_results,
  format = "latex",
  booktabs = TRUE,
  caption = "ADF testo p-reikšmės kiekvienai seniūnijai",
  label = "tab:adf_all_seniunijos",
  col.names = c("Seniūnijos kodas", "p reikšmė"),
  escape = FALSE
) %>%
  kable_styling(
    latex_options = c("hold_position", "striped")
  )

# ============================================================
# Сохранение
# ============================================================

save_kable(
  adf_latex,
  file = "tables/adf_all_seniunijos.tex"
)

one_ts <- as_tsibble(
  one,
  index = hour_time
)

old_theme <- theme_get()

theme_set(
  theme_gray(base_size = 15) +
    theme(
      plot.title = element_text(size = 20, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 14)
    )
)

options(OutDec = ",")

p <- gg_tsdisplay(
  one_ts,
  N,
  plot_type = "partial",
  lag_max = 72
) +
  labs(x = "Laikas", title = "Verkių seniūnija")

p
ggsave(
  filename = "images/verkiu_seniunija_tsdisplay.png",
  plot = p,
  width = 10,
  height = 7,
  dpi = 600
)

weekday <- factor(
  wday(one$hour_time, week_start = 1),
  levels = 1:7
)

xreg <- model.matrix(~ weekday)[, -1, drop = FALSE]


# Обучаем SARIMAX через auto.arima
fit_auto <- auto.arima(
  ts_y,
  xreg = xreg,
  D = 1,
  seasonal = TRUE,
  trace = TRUE
)

summary(fit_auto)

# Диагностика остатков
res <- residuals(fit_auto)

p <- ggtsdisplay(
  res,
  lag.max = 72
) +
  labs(x = "Laikas", title = "Verkių seniūnija")

p

fit_manual <- Arima(
  ts_y,
  order = c(4, 0, 1),                    # p, d, q
  seasonal = list(
    order = c(1, 1, 1),                  # P, D, Q
    period = 24
  ),
  xreg = xreg,
  include.mean = FALSE
)

one_2 <- copy(result[sen_kodas == "9505"])
y_2 <- one_2$N
one_ts_2 <- as_tsibble(
  one_2,
  index = hour_time
)
p <- gg_tsdisplay(one_ts_2, N,
             plot_type = "partial",
             lag_max = 72 )+
  labs(x = "Laikas", title = "Žemosios Fredos seniūnija")
ggsave(
  filename = "images/zemosios_fredos_tsdisplay.png",
  plot = p,
  width = 10,
  height = 7,
  dpi = 600
)


one_3 <- copy(result[sen_kodas == "9506"])
y_3 <- one_3$N
one_ts_3 <- as_tsibble(
  one_3,
  index = hour_time
)
p <- gg_tsdisplay(one_ts_3, N,
             plot_type = "partial",
             lag_max = 72 ) +
  labs(x = "Laikas", title = "Centro-Žaliakalnio seniūnija")
ggsave(
  filename = "images/centrozaliakalnio_tsdisplay.png",
  plot = p,
  width = 10,
  height = 7,
  dpi = 600
)


fit_auto_2 <- auto.arima(
  ts_y,
  xreg = xreg,
  D = 2,
  seasonal = TRUE,
  trace = TRUE
)

png(
  filename = "images/verkiu_sar1.png",
  width = 10,
  height = 7,
  units = "in",
  res = 600
)

forecast::ggtsdisplay(
  residuals(fit_auto),
  main = "SARIMAX(4,0,0)(2,1,0)[24] modelio liekanos Verkių seniūnijoje"
)

dev.off()

png(
  filename = "images/verkiu_sar2.png",
  width = 10,
  height = 7,
  units = "in",
  res = 600
)

forecast::ggtsdisplay(
  residuals(fit_manual),
  main = "SARIMAX(4,0,1)(1,1,1)[24] modelio liekanos Verkių seniūnijoje"
)

dev.off()



resid_manual <- as.numeric(na.omit(residuals(fit_manual)))

fit_df <- tryCatch(
  forecast:::modeldf(fit_manual),
  error = function(e) 0
)

test <- Box.test(
  resid_manual,
  lag = 48,
  type = "Ljung-Box",
  fitdf = fit_df
)

format_number_comma <- function(x, digits = 3) {
  formatC(x, format = "f", digits = digits, decimal.mark = ",")
}

format_p_value <- function(p) {
  ifelse(
    p < 0.001,
    "< 0,001",
    format_number_comma(p, digits = 3)
  )
}

lb_table <- data.frame(
  Lagas = 48,
  `Testo statistika` = format_number_comma(as.numeric(test$statistic), 3),
  `Laisvės laipsniai` = as.numeric(test$parameter),
  `p-reikšmė` = format_p_value(as.numeric(test$p.value)),
  check.names = FALSE
)

dir.create("tables", showWarnings = FALSE)

table_latex <- kable(
  lb_table,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  caption = "Ljung-Box testo rezultatas modelio liekanų nepriklausomumui tikrinti"
) %>%
  kable_styling(
    latex_options = c("hold_position", "striped"),
    full_width = FALSE,
    position = "center"
  )

save_kable(
  table_latex,
  file = "tables/ljung_box_fit_manual_48.tex"
)

table_latex

#SARIMAX

setDT(result)


keep_cols <- c("sen_kodas", "hour_time", "N")
drop_cols <- setdiff(names(result), keep_cols)

if (length(drop_cols) > 0) {
  result[, (drop_cols) := NULL]
}

result[, sen_kodas := as.character(sen_kodas)]
result[, N := as.integer(N)]

setorder(result, sen_kodas, hour_time)

gc()


test_h <- 24 * 30

period <- 24

fit_mode <- "fixed"

fixed_order <- c(4, 0, 1)
fixed_seasonal <- c(1, 1, 1)

series_dir <- "sarima_series_by_sen_kodas"
model_dir <- "models_sarima"

dir.create(series_dir, showWarnings = FALSE)
dir.create(model_dir, showWarnings = FALSE)

sids <- sort(unique(result$sen_kodas))

n_physical <- detectCores(logical = FALSE)

if (is.na(n_physical) || n_physical < 2L) {
  n_physical <- 2L
}

n_cores <- min(
  max(1L, n_physical - 1L),
  length(sids)
)

safe_id <- function(x) {
  gsub("[^A-Za-z0-9_\\-]", "_", x)
}


make_weekday_xreg <- function(time_vec) {
  
  weekday <- lubridate::wday(
    time_vec,
    week_start = 1,
    label = FALSE
  )
  
  weekday <- factor(
    weekday,
    levels = 1:7,
    labels = c(
      "pirmadienis",
      "antradienis",
      "treciadienis",
      "ketvirtadienis",
      "penktadienis",
      "sestadienis",
      "sekmadienis"
    )
  )
  
  X <- model.matrix(~ weekday)[, -1, drop = FALSE]
  
  X
}



for (sid in sids) {
  
  file_i <- file.path(series_dir, paste0("series_", safe_id(sid), ".rds"))
  
  dt_i <- result[sen_kodas == sid, .(sen_kodas, hour_time, N)]
  setorder(dt_i, hour_time)
  
  saveRDS(dt_i, file_i, compress = "gzip")
}

cat("failais sukurti\n\n")

gc()


calc_metrics <- function(actual, pred, train, m = 24) {
  
  err <- actual - pred
  
  rmse <- sqrt(mean(err^2, na.rm = TRUE))
  mae <- mean(abs(err), na.rm = TRUE)
  
  smape <- mean(
    ifelse(
      abs(actual) + abs(pred) == 0,
      0,
      2 * abs(actual - pred) / (abs(actual) + abs(pred))
    ),
    na.rm = TRUE
  ) * 100
  
  if (length(train) > m) {
    
    denom <- mean(
      abs(train[(m + 1):length(train)] - train[1:(length(train) - m)]),
      na.rm = TRUE
    )
    
    mase <- ifelse(
      is.finite(denom) && denom > 0,
      mae / denom,
      NA_real_
    )
    
  } else {
    mase <- NA_real_
  }
  
  c(
    RMSE = rmse,
    MAE = mae,
    sMAPE = smape,
    MASE = mase
  )
}


fit_one_seniunija_file <- function(i) {
  
  sid <- sids[i]
  t0 <- Sys.time()
  
  cat(sprintf(
    "[%s] %d / %d | start sen_kodas = %s | pid = %s\n",
    format(Sys.time(), "%H:%M:%S"),
    i,
    length(sids),
    sid,
    Sys.getpid()
  ))
  flush.console()
  
  file_i <- file.path(series_dir, paste0("series_", safe_id(sid), ".rds"))
  dt <- readRDS(file_i)
  
  setDT(dt)
  setorder(dt, hour_time)
  
  n <- nrow(dt)
  
  
  if (n <= test_h + 24 * 14) {
    
    status_i <- "too_short"
    
    out <- list(
      metrics = data.table(
        sen_kodas = sid,
        status = status_i,
        model = NA_character_,
        n_total = n,
        n_train = NA_integer_,
        n_test = NA_integer_,
        AIC = NA_real_,
        AICc = NA_real_,
        BIC = NA_real_,
        RMSE = NA_real_,
        MAE = NA_real_,
        sMAPE = NA_real_,
        MASE = NA_real_,
        RMSE_snaive = NA_real_,
        MAE_snaive = NA_real_,
        sMAPE_snaive = NA_real_,
        MASE_snaive = NA_real_,
        error_message = NA_character_
      ),
      predictions = NULL
    )
    
    cat(sprintf(
      "[%s] done sen_kodas = %s | status = %s | time = %.2f min | pid = %s\n\n",
      format(Sys.time(), "%H:%M:%S"),
      sid,
      status_i,
      as.numeric(difftime(Sys.time(), t0, units = "mins")),
      Sys.getpid()
    ))
    flush.console()
    
    return(out)
  }
  
  # ------------------------------------------------------------
  # Train / test split
  # ------------------------------------------------------------
  
  train_n <- n - test_h
  
  y <- dt$N
  
  y_train <- y[1:train_n]
  y_test <- y[(train_n + 1):n]
  
  time_train <- dt$hour_time[1:train_n]
  time_test <- dt$hour_time[(train_n + 1):n]
  
  ts_train <- ts(y_train, frequency = period)
  
  
  xreg_train <- make_weekday_xreg(time_train)
  xreg_test <- make_weekday_xreg(time_test)
  
  xreg_test <- xreg_test[, colnames(xreg_train), drop = FALSE]
  
  
  base_fc <- forecast::snaive(ts_train, h = length(y_test))
  pred_snaive <- as.numeric(base_fc$mean)
  pred_snaive <- pmax(pred_snaive, 0)
  
  base_m <- calc_metrics(
    actual = y_test,
    pred = pred_snaive,
    train = y_train,
    m = period
  )
  
  
  if (length(unique(y_train)) <= 1L) {
    
    pred <- rep(y_train[1], length(y_test))
    pred <- pmax(pred, 0)
    
    m <- calc_metrics(
      actual = y_test,
      pred = pred,
      train = y_train,
      m = period
    )
    
    status_i <- "constant_series"
    
    out <- list(
      metrics = data.table(
        sen_kodas = sid,
        status = status_i,
        model = "constant",
        n_total = n,
        n_train = length(y_train),
        n_test = length(y_test),
        AIC = NA_real_,
        AICc = NA_real_,
        BIC = NA_real_,
        RMSE = m["RMSE"],
        MAE = m["MAE"],
        sMAPE = m["sMAPE"],
        MASE = m["MASE"],
        RMSE_snaive = base_m["RMSE"],
        MAE_snaive = base_m["MAE"],
        sMAPE_snaive = base_m["sMAPE"],
        MASE_snaive = base_m["MASE"],
        error_message = NA_character_
      ),
      predictions = data.table(
        sen_kodas = sid,
        hour_time = time_test,
        actual = y_test,
        pred = pred,
        pred_raw = pred,
        pred_snaive = pred_snaive
      )
    )
    
    cat(sprintf(
      "[%s] done sen_kodas = %s | status = %s | time = %.2f min | pid = %s\n\n",
      format(Sys.time(), "%H:%M:%S"),
      sid,
      status_i,
      as.numeric(difftime(Sys.time(), t0, units = "mins")),
      Sys.getpid()
    ))
    flush.console()
    
    return(out)
  }
  
  
  fit <- tryCatch({
    
    if (fit_mode == "fixed") {
      
      forecast::Arima(
        ts_train,
        order = fixed_order,
        seasonal = list(
          order = fixed_seasonal,
          period = period
        ),
        include.mean = FALSE,
        xreg = xreg_train
      )
      
    } else if (fit_mode == "auto") {
      
      forecast::auto.arima(
        ts_train,
        xreg = xreg_train,
        seasonal = TRUE,
        D = 1,
        max.p = 4,
        max.q = 4,
        max.P = 2,
        max.Q = 1,
        max.order = 6,
        stepwise = TRUE,
        approximation = TRUE,
        truncate = min(length(y_train), 24 * 180),
        allowdrift = FALSE,
        allowmean = FALSE,
        parallel = FALSE
      )
      
    } else {
      stop("fit_mode must be 'fixed' or 'auto'")
    }
    
  }, error = function(e) e)
  
  
  if (inherits(fit, "error")) {
    
    status_i <- "sarima_error"
    
    out <- list(
      metrics = data.table(
        sen_kodas = sid,
        status = status_i,
        model = "snaive_fallback",
        n_total = n,
        n_train = length(y_train),
        n_test = length(y_test),
        AIC = NA_real_,
        AICc = NA_real_,
        BIC = NA_real_,
        RMSE = base_m["RMSE"],
        MAE = base_m["MAE"],
        sMAPE = base_m["sMAPE"],
        MASE = base_m["MASE"],
        RMSE_snaive = base_m["RMSE"],
        MAE_snaive = base_m["MAE"],
        sMAPE_snaive = base_m["sMAPE"],
        MASE_snaive = base_m["MASE"],
        error_message = fit$message
      ),
      predictions = data.table(
        sen_kodas = sid,
        hour_time = time_test,
        actual = y_test,
        pred = pred_snaive,
        pred_raw = pred_snaive,
        pred_snaive = pred_snaive
      )
    )
    
    cat(sprintf(
      "[%s] done sen_kodas = %s | status = %s | time = %.2f min | pid = %s\n\n",
      format(Sys.time(), "%H:%M:%S"),
      sid,
      status_i,
      as.numeric(difftime(Sys.time(), t0, units = "mins")),
      Sys.getpid()
    ))
    flush.console()
    
    return(out)
  }
  
  
  fc <- forecast::forecast(
    fit,
    h = length(y_test),
    xreg = xreg_test
  )
  
  pred_raw <- as.numeric(fc$mean)
  pred <- pmax(pred_raw, 0)
  
  m <- calc_metrics(
    actual = y_test,
    pred = pred,
    train = y_train,
    m = period
  )
  
  
  if (fit_mode == "fixed") {
    
    model_name <- sprintf(
      "ARIMA(%d,%d,%d)(%d,%d,%d)[%d] + weekday dummy",
      fixed_order[1], fixed_order[2], fixed_order[3],
      fixed_seasonal[1], fixed_seasonal[2], fixed_seasonal[3],
      period
    )
    
  } else {
    
    ar <- fit$arma
    
    model_name <- sprintf(
      "ARIMA(%d,%d,%d)(%d,%d,%d)[%d] + weekday dummy",
      ar[1], ar[6], ar[2],
      ar[3], ar[7], ar[4],
      ar[5]
    )
  }
  
  
  saveRDS(
    fit,
    file = file.path(model_dir, paste0("sarima_", safe_id(sid), ".rds")),
    compress = "gzip"
  )
  
  status_i <- "ok"
  
  out <- list(
    metrics = data.table(
      sen_kodas = sid,
      status = status_i,
      model = model_name,
      n_total = n,
      n_train = length(y_train),
      n_test = length(y_test),
      AIC = AIC(fit),
      AICc = ifelse(!is.null(fit$aicc), fit$aicc, NA_real_),
      BIC = BIC(fit),
      RMSE = m["RMSE"],
      MAE = m["MAE"],
      sMAPE = m["sMAPE"],
      MASE = m["MASE"],
      RMSE_snaive = base_m["RMSE"],
      MAE_snaive = base_m["MAE"],
      sMAPE_snaive = base_m["sMAPE"],
      MASE_snaive = base_m["MASE"],
      error_message = NA_character_
    ),
    predictions = data.table(
      sen_kodas = sid,
      hour_time = time_test,
      actual = y_test,
      pred = pred,
      pred_raw = pred_raw,
      pred_snaive = pred_snaive
    )
  )
  
  cat(sprintf(
    "[%s] done sen_kodas = %s | status = %s | time = %.2f min | pid = %s\n\n",
    format(Sys.time(), "%H:%M:%S"),
    sid,
    status_i,
    as.numeric(difftime(Sys.time(), t0, units = "mins")),
    Sys.getpid()
  ))
  flush.console()
  
  rm(fit, fc, dt)
  gc()
  
  out
}


cat(sprintf(
  "islygiagretinta: %d procesai\n\n",
  n_cores
))

cl <- makeCluster(
  n_cores,
  type = "PSOCK",
  outfile = ""
)

clusterEvalQ(cl, {
  library(data.table)
  library(forecast)
  library(lubridate)
})

clusterExport(
  cl,
  varlist = c(
    "sids",
    "series_dir",
    "model_dir",
    "safe_id",
    "calc_metrics",
    "make_weekday_xreg",
    "fit_one_seniunija_file",
    "test_h",
    "period",
    "fit_mode",
    "fixed_order",
    "fixed_seasonal"
  ),
  envir = environment()
)

res_list <- tryCatch(
  {
    parallel::parLapplyLB(
      cl = cl,
      X = seq_along(sids),
      fun = fit_one_seniunija_file,
      chunk.size = 1L
    )
  },
  finally = {
    stopCluster(cl)
  }
)

cat("\nDone.\n\n")


sarima_metrics <- rbindlist(
  lapply(res_list, `[[`, "metrics"),
  fill = TRUE
)

sarima_predictions <- rbindlist(
  lapply(res_list, `[[`, "predictions"),
  fill = TRUE
)

setorder(sarima_metrics, sen_kodas)
setorder(sarima_predictions, sen_kodas, hour_time)

saveRDS(
  sarima_metrics,
  "sarima_metrics_weekday_dummy_lb.rds",
  compress = "gzip"
)

saveRDS(
  sarima_predictions,
  "sarima_predictions_weekday_dummy_lb.rds",
  compress = "gzip"
)

cat("Rezultatai išsaugoti:\n")
cat("- sarima_metrics_weekday_dummy_lb.rds\n")
cat("- sarima_predictions_weekday_dummy_lb.rds\n\n")

print(sarima_metrics)

res_dt <- data.table(
  residuals = as.numeric(residuals(sarima_fit))
)




# ============================================================
# Perrašyti vienos seniūnijos modelį naujais SARIMA parametrais
# ============================================================

target_sid <- "1311"

# Nauji parametrai šiai vienai seniūnijai
fit_mode <- "fixed"

fixed_order <- c(2, 0, 1)
fixed_seasonal <- c(1, 1, 1)

if (!target_sid %in% sids) {
  stop("Tokio sen_kodas nėra result duomenyse: ", target_sid)
}

target_i <- match(target_sid, sids)

# Apmokome vieną naują modelį
one_res <- fit_one_seniunija_file(target_i)

new_metrics <- one_res$metrics
new_predictions <- one_res$predictions

# ------------------------------------------------------------
# Jei galutinės lentelės dar neįkeltos, įkeliame jas iš failų
# ------------------------------------------------------------

if (!exists("sarima_metrics")) {
  sarima_metrics <- readRDS("sarima_metrics_weekday_dummy_lb.rds")
  setDT(sarima_metrics)
}

if (!exists("sarima_predictions")) {
  sarima_predictions <- readRDS("sarima_predictions_weekday_dummy_lb.rds")
  setDT(sarima_predictions)
}

# ------------------------------------------------------------
# Pašaliname seną šios seniūnijos rezultatą
# ------------------------------------------------------------

sarima_metrics <- sarima_metrics[sen_kodas != target_sid]

sarima_predictions <- sarima_predictions[sen_kodas != target_sid]

# ------------------------------------------------------------
# Pridedame naują rezultatą
# ------------------------------------------------------------

sarima_metrics <- rbindlist(
  list(sarima_metrics, new_metrics),
  fill = TRUE
)

sarima_predictions <- rbindlist(
  list(sarima_predictions, new_predictions),
  fill = TRUE
)
setorder(sarima_metrics, sen_kodas)
setorder(sarima_predictions, sen_kodas, hour_time)



setDT(result)

# Если объекты не загружены в память, загружаем их из файлов
if (!exists("sarima_predictions")) {
  sarima_predictions <- readRDS("sarima_predictions_weekday_dummy_lb.rds")
}

if (!exists("sarima_metrics")) {
  sarima_metrics <- readRDS("sarima_metrics_weekday_dummy_lb.rds")
}

setDT(sarima_predictions)
setDT(sarima_metrics)

if (!exists("period")) {
  period <- 24
}

# ------------------------------------------------------------
# Функция округления прогноза
# ------------------------------------------------------------

make_count_pred <- function(x) {
  x <- as.numeric(x)
  x[!is.finite(x)] <- NA_real_
  pmax(0, round(x))
}

# ------------------------------------------------------------
# Функция для метрик
# ------------------------------------------------------------

calc_metrics <- function(actual, pred, train, m = 24) {
  
  err <- actual - pred
  
  rmse <- sqrt(mean(err^2, na.rm = TRUE))
  mae <- mean(abs(err), na.rm = TRUE)
  
  smape <- mean(
    ifelse(
      abs(actual) + abs(pred) == 0,
      0,
      2 * abs(actual - pred) / (abs(actual) + abs(pred))
    ),
    na.rm = TRUE
  ) * 100
  
  if (length(train) > m) {
    
    denom <- mean(
      abs(train[(m + 1):length(train)] - train[1:(length(train) - m)]),
      na.rm = TRUE
    )
    
    mase <- ifelse(
      is.finite(denom) && denom > 0,
      mae / denom,
      NA_real_
    )
    
  } else {
    mase <- NA_real_
  }
  
  c(
    RMSE = rmse,
    MAE = mae,
    sMAPE = smape,
    MASE = mase
  )
}

# ------------------------------------------------------------
# Создаём копии, чтобы старые объекты можно было восстановить
# ------------------------------------------------------------

sarima_predictions_old <- copy(sarima_predictions)
sarima_metrics_old <- copy(sarima_metrics)

sarima_predictions_rounded <- copy(sarima_predictions)

# Если вдруг нет pred_raw, используем pred как исходный прогноз
if (!"pred_raw" %in% names(sarima_predictions_rounded)) {
  sarima_predictions_rounded[, pred_raw := pred]
}

# Если вдруг нет pred_snaive_raw, используем pred_snaive
if (!"pred_snaive_raw" %in% names(sarima_predictions_rounded)) {
  sarima_predictions_rounded[, pred_snaive_raw := pred_snaive]
}



sarima_predictions_rounded[, pred := make_count_pred(pred_raw)]

sarima_predictions_rounded[, pred_snaive := make_count_pred(pred_snaive_raw)]

recalc_one_sid <- function(sid) {
  
  pred_i <- sarima_predictions_rounded[sen_kodas == sid]
  
  if (nrow(pred_i) == 0) {
    return(NULL)
  }
  
  split_time <- min(pred_i$hour_time, na.rm = TRUE)
  
  y_train <- result[
    sen_kodas == sid & hour_time < split_time,
    N
  ]
  
  actual <- pred_i$actual
  
  m_sarima <- calc_metrics(
    actual = actual,
    pred = pred_i$pred,
    train = y_train,
    m = period
  )
  
  m_snaive <- calc_metrics(
    actual = actual,
    pred = pred_i$pred_snaive,
    train = y_train,
    m = period
  )
  
  data.table(
    sen_kodas = sid,
    RMSE = m_sarima["RMSE"],
    MAE = m_sarima["MAE"],
    sMAPE = m_sarima["sMAPE"],
    MASE = m_sarima["MASE"],
    RMSE_snaive = m_snaive["RMSE"],
    MAE_snaive = m_snaive["MAE"],
    sMAPE_snaive = m_snaive["sMAPE"],
    MASE_snaive = m_snaive["MASE"]
  )
}

metric_updates <- rbindlist(
  lapply(
    sort(unique(sarima_predictions_rounded$sen_kodas)),
    recalc_one_sid
  ),
  fill = TRUE
)

sarima_metrics_rounded <- copy(sarima_metrics)

sarima_metrics_rounded[
  metric_updates,
  on = "sen_kodas",
  `:=`(
    RMSE = i.RMSE,
    MAE = i.MAE,
    sMAPE = i.sMAPE,
    MASE = i.MASE,
    RMSE_snaive = i.RMSE_snaive,
    MAE_snaive = i.MAE_snaive,
    sMAPE_snaive = i.sMAPE_snaive,
    MASE_snaive = i.MASE_snaive
  )
]

setorder(sarima_predictions_rounded, sen_kodas, hour_time)
setorder(sarima_metrics_rounded, sen_kodas)

# ------------------------------------------------------------
# Чтобы дальнейший твой код работал без изменений:
# заменяем основные объекты в памяти
# ------------------------------------------------------------

sarima_predictions <- sarima_predictions_rounded
sarima_metrics <- sarima_metrics_rounded

# ------------------------------------------------------------
# Сохраняем новые версии
# ------------------------------------------------------------

saveRDS(
  sarima_predictions,
  "sarima_predictions_weekday_dummy_lb_rounded.rds",
  compress = "gzip"
)

saveRDS(
  sarima_metrics,
  "sarima_metrics_weekday_dummy_lb_rounded.rds",
  compress = "gzip"
)
# ============================================================
# Графики прогноза для нескольких seniūnija
# История + прогноз + доверительные интервалы
# ============================================================



# ------------------------------------------------------------
# Настройки
# ------------------------------------------------------------

chosen_sids <- c("9505", "1313", "9506")

history_days <- 3      # сколько дней train-истории показать перед test
test_plot_days <- 7    # сколько дней test выборки показать на графике

test_h <- 24 * 30      # как в твоём основном коде
period <- 24

level <- c(80, 95)

use_rounded_forecast <- TRUE
# TRUE  -> pred = max(0, round(pred_raw))
# FALSE -> дробный прогноз модели

# ------------------------------------------------------------
# Вспомогательные функции
# ------------------------------------------------------------

if (!exists("safe_id")) {
  safe_id <- function(x) {
    gsub("[^A-Za-z0-9_\\-]", "_", x)
  }
}

if (!exists("make_weekday_xreg")) {
  
  make_weekday_xreg <- function(time_vec) {
    
    weekday <- lubridate::wday(
      time_vec,
      week_start = 1,
      label = FALSE
    )
    
    weekday <- factor(
      weekday,
      levels = 1:7,
      labels = c(
        "pirmadienis",
        "antradienis",
        "treciadienis",
        "ketvirtadienis",
        "penktadienis",
        "sestadienis",
        "sekmadienis"
      )
    )
    
    X <- model.matrix(~ weekday)[, -1, drop = FALSE]
    
    X
  }
}

make_count_pred <- function(x) {
  x <- as.numeric(x)
  x[!is.finite(x)] <- NA_real_
  pmax(0, round(x))
}

# ------------------------------------------------------------
# Данные для графика одной seniunija
# ------------------------------------------------------------

make_test_forecast_plot_data <- function(sid) {
  
  model_file <- file.path(
    "models_sarima",
    paste0("sarima_", safe_id(sid), ".rds")
  )
  
  if (!file.exists(model_file)) {
    warning("Model file not found for sen_kodas = ", sid)
    return(NULL)
  }
  
  fit <- readRDS(model_file)
  
  dt <- result[
    sen_kodas == sid,
    .(sen_kodas, hour_time, N)
  ]
  
  setorder(dt, hour_time)
  
  n <- nrow(dt)
  
  if (n <= test_h) {
    warning("Too short series for sen_kodas = ", sid)
    return(NULL)
  }
  
  # ----------------------------------------------------------
  # Train / test split как в твоём основном коде
  # ----------------------------------------------------------
  
  train_n <- n - test_h
  
  train_dt <- dt[1:train_n]
  test_dt <- dt[(train_n + 1):n]
  
  split_time <- min(test_dt$hour_time)
  
  y_test <- test_dt$N
  time_test <- test_dt$hour_time
  
  # ----------------------------------------------------------
  # xreg для test периода
  # ----------------------------------------------------------
  
  time_train <- train_dt$hour_time
  
  xreg_train <- make_weekday_xreg(time_train)
  xreg_test <- make_weekday_xreg(time_test)
  
  xreg_test <- xreg_test[, colnames(xreg_train), drop = FALSE]
  
  # ----------------------------------------------------------
  # Forecast на test_h шагов
  # Модель НЕ переобучается
  # ----------------------------------------------------------
  
  fc <- forecast::forecast(
    fit,
    h = length(y_test),
    xreg = xreg_test,
    level = level
  )
  
  pred_raw <- as.numeric(fc$mean)
  
  lower_80_raw <- as.numeric(fc$lower[, "80%"])
  upper_80_raw <- as.numeric(fc$upper[, "80%"])
  
  lower_95_raw <- as.numeric(fc$lower[, "95%"])
  upper_95_raw <- as.numeric(fc$upper[, "95%"])
  
  if (use_rounded_forecast) {
    
    # Округляем только точечный прогноз
    pred <- make_count_pred(pred_raw)
    
    # Интервалы НЕ округляем
    lower_80 <- pmax(0, lower_80_raw)
    upper_80 <- pmax(0, upper_80_raw)
    
    lower_95 <- pmax(0, lower_95_raw)
    upper_95 <- pmax(0, upper_95_raw)
    
  } else {
    
    pred <- pmax(0, pred_raw)
    
    lower_80 <- pmax(0, lower_80_raw)
    upper_80 <- pmax(0, upper_80_raw)
    
    lower_95 <- pmax(0, lower_95_raw)
    upper_95 <- pmax(0, upper_95_raw)
  }
  
  # ----------------------------------------------------------
  # Ограничиваем test период для графика
  # Например, только первые 7 дней test выборки
  # ----------------------------------------------------------
  
  test_part <- data.table(
    sen_kodas = sid,
    hour_time = time_test,
    actual = as.numeric(y_test),
    pred = pred,
    pred_raw = pred_raw,
    lower_80 = lower_80,
    upper_80 = upper_80,
    lower_95 = lower_95,
    upper_95 = upper_95,
    sample_part = "Testavimo aibė"
  )
  
  test_part <- test_part[
    hour_time < split_time + days(test_plot_days)
  ]
  
  # ----------------------------------------------------------
  # История перед test периодом
  # ----------------------------------------------------------
  
  history_start <- split_time - days(history_days)
  
  history_part <- train_dt[
    hour_time >= history_start,
    .(
      sen_kodas,
      hour_time,
      actual = as.numeric(N),
      pred = NA_real_,
      pred_raw = NA_real_,
      lower_80 = NA_real_,
      upper_80 = NA_real_,
      lower_95 = NA_real_,
      upper_95 = NA_real_,
      sample_part = "Mokymo aibė"
    )
  ]
  
  out <- rbindlist(
    list(history_part, test_part),
    use.names = TRUE,
    fill = TRUE
  )
  
  out[, split_time := split_time]
  
  out
}

# ------------------------------------------------------------
# Собираем данные для всех выбранных sen_kodas
# ------------------------------------------------------------

plot_dt <- rbindlist(
  lapply(chosen_sids, make_test_forecast_plot_data),
  fill = TRUE
)

setorder(plot_dt, sen_kodas, hour_time)



plot_titles <- c(
  "9505" = "Žemosios Fredos",
  "1313" = "Verkių seniūnija",
  "9506" = "Centro-Žaliakalnio"
)


# ------------------------------------------------------------
# Построить графики
# ------------------------------------------------------------
forecast_h <- 24 * 7
history_h <- 24 * 3

lt_months <- c(
  "Saus", "Vas", "Kov", "Bal",
  "Geg", "Birž", "Liep", "Rugpjūt",
  "Rugs", "Spal", "Lapkr", "Gruod"
)

lt_date_labels <- function(x) {
  paste0(
    lt_months[as.integer(format(x, "%m"))],
    " ",
    format(x, "%d")
  )
}
plot_dt_snaive <- rbindlist(lapply(chosen_sids, function(sid) {
  
  test_i <- sarima_predictions[sen_kodas == sid]
  
  if (nrow(test_i) == 0) {
    return(NULL)
  }
  
  setorder(test_i, hour_time)
  
  test_i <- test_i[1:min(.N, forecast_h)]
  
  split_i <- min(test_i$hour_time)
  
  hist_i <- result[
    sen_kodas == sid &
      hour_time < split_i &
      hour_time >= split_i - lubridate::hours(history_h),
    .(
      sen_kodas,
      hour_time,
      actual = N,
      pred_snaive = NA_real_,
      sample_part = "Mokymo aibė",
      split_time = split_i
    )
  ]
  
  test_i <- test_i[
    ,
    .(
      sen_kodas,
      hour_time,
      actual,
      pred_snaive,
      sample_part = "Testavimo aibė",
      split_time = split_i
    )
  ]
  
  rbind(hist_i, test_i, fill = TRUE)
}), fill = TRUE)

plots_by_sid_snaive <- lapply(chosen_sids, function(sid) {
  
  dt_i <- plot_dt_snaive[sen_kodas == sid]
  
  if (nrow(dt_i) == 0) {
    return(NULL)
  }
  
  split_i <- unique(dt_i$split_time)[1]
  
  place_name <- if (sid %in% names(plot_titles)) {
    plot_titles[[sid]]
  } else {
    sid
  }
  
  graph_title <- paste0("Sezoninė naivioji prognozė ", place_name, " seniūnijoje")
  
  ggplot(dt_i, aes(x = hour_time)) +
    geom_line(
      aes(y = actual, color = "Tikrosios reikšmės"),
      linewidth = 0.8,
      na.rm = TRUE
    ) +
    geom_line(
      data = dt_i[sample_part == "Testavimo aibė"],
      aes(y = pred_snaive, color = "Sezoninė naivioji prognozė"),
      linewidth = 0.9,
      na.rm = TRUE
    ) +
    geom_vline(
      xintercept = as.numeric(split_i),
      linetype = "dashed"
    ) +
    labs(
      title = graph_title,
      subtitle = NULL,
      x = "Laikas",
      y = "Pranešimų skaičius",
      color = NULL,
      fill = NULL
    ) +
    scale_x_datetime(
      date_breaks = "2 days",
      labels = lt_date_labels
    ) +
    scale_y_continuous(
      labels = scales::label_number(
        decimal.mark = ",",
        big.mark = " "
      )
    ) +
    theme(
      legend.position = "bottom"
    )
})

names(plots_by_sid_snaive) <- chosen_sids

for (sid in names(plots_by_sid_snaive)) {
  
  p <- plots_by_sid_snaive[[sid]]
  
  if (is.null(p)) next
  
  print(p)
  
  ggsave(
    filename = paste0("images/snaive_forecast_7_days_", sid, ".png"),
    plot = p,
    width = 12,
    height = 7,
    dpi = 300
  )
}

plots_by_sid <- lapply(chosen_sids, function(sid) {
  
  dt_i <- plot_dt[sen_kodas == sid]
  
  if (nrow(dt_i) == 0) {
    return(NULL)
  }
  
  split_i <- unique(dt_i$split_time)[1]
  place_name <- if (sid %in% names(plot_titles)) {
    plot_titles[[sid]]
  } else {
    sid
  }
  
  graph_title <- paste0("SARIMAX prognozavimas ", place_name, " seniūnijoje")
  
  ggplot(dt_i, aes(x = hour_time)) +
    geom_ribbon(
      data = dt_i[sample_part == "Testavimo aibė"],
      aes(
        ymin = lower_95,
        ymax = upper_95,
        fill = "95% intervalas"
      ),
      alpha = 0.20
    ) +
    geom_ribbon(
      data = dt_i[sample_part == "Testavimo aibė"],
      aes(
        ymin = lower_80,
        ymax = upper_80,
        fill = "80% intervalas"
      ),
      alpha = 0.35
    ) +
    geom_line(
      aes(y = actual, color = "Tikrosios reikšmės"),
      linewidth = 0.8,
      na.rm = TRUE
    ) +
    geom_line(
      data = dt_i[sample_part == "Testavimo aibė"],
      aes(y = pred, color = "SARIMAX prognozė"),
      linewidth = 0.9,
      na.rm = TRUE
    ) +
    geom_vline(
      xintercept = as.numeric(split_i),
      linetype = "dashed"
    ) +
    labs(
      title = graph_title,
      subtitle = NULL,
      x = "Laikas",
      y = "Pranešimų skaičius",
      color = NULL,
      fill = NULL
    ) +
    scale_x_datetime(
      date_breaks = "2 days",
      labels = lt_date_labels
    ) +
    scale_y_continuous(
      labels = scales::label_number(
        decimal.mark = ",",
        big.mark = " "
      )
    ) +
    scale_fill_manual(
      values = c(
        "95% intervalas" = "#4B0082",
        "80% intervalas" = "#FF8C00"
      )
    ) +
    theme(
      legend.position = "bottom"
    )
})

names(plots_by_sid) <- chosen_sids

for (sid in names(plots_by_sid)) {
  
  p <- plots_by_sid[[sid]]
  
  if (is.null(p)) next
  
  print(p)
  
  ggsave(
    filename = paste0("images/sarima_forecast_", sid, ".png"),
    plot = p,
    width = 12,
    height = 7,
    dpi = 300
  )
}
# ============================================================
# 1. GLARMA
# ============================================================

one <- copy(result[sen_kodas == "1313"])
setorder(one, hour_time)

one[, N := as.integer(N)]

stopifnot(all(one$N >= 0))
stopifnot(!any(is.na(one$N)))

one[, hour := factor(
  hour(hour_time),
  levels = 0:23
)]

one[, weekday := factor(
  wday(hour_time, week_start = 1),
  levels = 1:7,
  labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
  ordered = FALSE
)]


cat("Is weekday ordered factor?\n")
print(is.ordered(one$weekday))

cat("\nWeekday contrasts:\n")
print(contrasts(one$weekday))

# ============================================================
# 3. Train / test split
# ============================================================

test_h <- 24 * 30

if (nrow(one) <= test_h + 10) {
  stop("test_h = 24 * 30.")
}

train_n <- nrow(one) - test_h

train <- one[1:train_n]
test  <- one[(train_n + 1):.N]

y_train <- train$N
y_test  <- test$N

test_time <- test$hour_time

X_train <- model.matrix(
  ~ hour + weekday,
  data = train
)

summary(ft_t)

extract_glarma_residuals <- function(fit) {
  
  resid_values <- tryCatch(
    {
      if (!is.null(fit$residuals)) {
        fit$residuals
      } else {
        residuals(fit)
      }
    },
    error = function(e) {
      NA_real_
    }
  )
  
  resid_values <- as.numeric(resid_values)
  resid_values <- resid_values[is.finite(resid_values)]
  
  resid_values
}

extract_glarma_loglik <- function(fit) {
  
  if (!is.null(fit$logLik)) {
    return(as.numeric(fit$logLik))
  }
  
  if (!is.null(fit$loglik)) {
    return(as.numeric(fit$loglik))
  }
  
  out <- tryCatch(
    as.numeric(logLik(fit)),
    error = function(e) NA_real_
  )
  
  out
}

extract_glarma_aic <- function(fit) {
  
  if (!is.null(fit$aic)) {
    return(as.numeric(fit$aic))
  }
  
  if (!is.null(fit$AIC)) {
    return(as.numeric(fit$AIC))
  }
  
  out <- tryCatch(
    as.numeric(AIC(fit)),
    error = function(e) NA_real_
  )
  
  out
}

fit_one_glarma <- function(y, X, p, q, distr) {
  
  args <- list(
    y = y,
    X = X,
    type = distr,
    method = "FS",
    residuals = "Pearson",
    maxit = 100,
    grad = 1e-6
  )
  
  if (p > 0) {
    args$phiLags <- seq_len(p)
  }
  
  if (q > 0) {
    args$thetaLags <- seq_len(q)
  }
  
  fit <- do.call(glarma, args)
  
  fit
}
t_ft <- glarma(y = y_train,X = X_train,phiLags = c(1,2,3,4),thetaLags = c(1),type= "Poi",residuals = "Pearson")


p_grid <- 0:3
q_grid <- 0:3

model_grid <- CJ(
  p = p_grid,
  q = q_grid,
  distr = c("Poi", "NegBin")
)

model_grid <- model_grid[!(p == 0 & q == 0)]

print(model_grid)


fits_glarma <- list()
summary_list <- list()

for (i in seq_len(nrow(model_grid))) {
  
  p_i <- model_grid$p[i]
  q_i <- model_grid$q[i]
  distr_i <- model_grid$distr[i]
  
  model_name <- paste0("GLARMA(", p_i, ",", q_i, ")_", distr_i)
  
  cat("\n============================================\n")
  cat("Fitting:", model_name, "\n")
  cat("============================================\n")
  
  res <- tryCatch(
    {
      fit <- fit_one_glarma(
        y = y_train,
        X = X_train,
        p = p_i,
        q = q_i,
        distr = distr_i
      )
      
      fits_glarma[[model_name]] <- fit
      
      resid_values <- extract_glarma_residuals(fit)
      
      if (length(resid_values) < 24 * 7) {
        warning(" Ljung-Box lag 168: ", model_name)
      }
      
      lb_24 <- tryCatch(
        Box.test(
          resid_values,
          lag = 24,
          type = "Ljung-Box",
          fitdf = p_i + q_i
        ),
        error = function(e) NULL
      )
      
      lb_168 <- tryCatch(
        Box.test(
          resid_values,
          lag = 24 * 7,
          type = "Ljung-Box",
          fitdf = p_i + q_i
        ),
        error = function(e) NULL
      )
      
      data.table(
        model = model_name,
        p = p_i,
        q = q_i,
        distr = distr_i,
        status = "ok",
        errCode = if (!is.null(fit$errCode)) fit$errCode else NA_integer_,
        WError = if (!is.null(fit$WError)) fit$WError else NA_integer_,
        logLik = extract_glarma_loglik(fit),
        AIC = extract_glarma_aic(fit),
        ljung_box_lag24_pvalue = if (!is.null(lb_24)) lb_24$p.value else NA_real_,
        ljung_box_lag168_pvalue = if (!is.null(lb_168)) lb_168$p.value else NA_real_,
        mean_resid = mean(resid_values, na.rm = TRUE),
        sd_resid = sd(resid_values, na.rm = TRUE),
        n_resid = length(resid_values),
        message = NA_character_
      )
    },
    error = function(e) {
      data.table(
        model = model_name,
        p = p_i,
        q = q_i,
        distr = distr_i,
        status = "error",
        errCode = NA_integer_,
        WError = NA_integer_,
        logLik = NA_real_,
        AIC = NA_real_,
        ljung_box_lag24_pvalue = NA_real_,
        ljung_box_lag168_pvalue = NA_real_,
        mean_resid = NA_real_,
        sd_resid = NA_real_,
        n_resid = NA_integer_,
        message = e$message
      )
    }
  )
  
  summary_list[[i]] <- res
}

glarma_summary <- rbindlist(summary_list)

cat("\n============================================\n")
cat("GLARMA summary:\n")
cat("============================================\n")
print(glarma_summary)


glarma_summary_ok <- glarma_summary[
  status == "ok"
][order(AIC)]

cat("\n============================================\n")
cat("AIC:\n")
cat("============================================\n")
print(glarma_summary_ok)

glarma_summary_conv <- glarma_summary[
  status == "ok" &
    (is.na(errCode) | errCode == 0) &
    (is.na(WError) | WError == 0)
][order(AIC)]

cat("\n============================================\n")
cat("woithout errror:\n")
cat("============================================\n")
print(glarma_summary_conv)


ok_models <- names(fits_glarma)

if (length(ok_models) == 0) {
  stop("f.")
}

n_models <- length(ok_models)

old_par <- par(
  mfrow = c(ceiling(n_models / 2), 2),
  mar = c(4, 4, 3, 1)
)

for (model_name in ok_models) {
  
  resid_values <- extract_glarma_residuals(fits_glarma[[model_name]])
  
  acf(
    resid_values,
    lag.max = 24 * 7,
    main = paste("ACF:", model_name),
    na.action = na.pass
  )
}

par(old_par)



if (nrow(glarma_summary_ok) > 0) {
  
  n_best <- min(4, nrow(glarma_summary_ok))
  best_models <- glarma_summary_ok[1:n_best, model]
  
  cat("\n============================================\n")
  cat("AIC:\n")
  cat("============================================\n")
  print(best_models)
  
  old_par <- par(
    mfrow = c(2, 2),
    mar = c(4, 4, 3, 1)
  )
  
  for (model_name in best_models) {
    
    resid_values <- extract_glarma_residuals(fits_glarma[[model_name]])
    
    acf(
      resid_values,
      lag.max = 24 * 7,
      main = paste("ACF:", model_name),
      na.action = na.pass
    )
  }
  
  par(old_par)
}




# ============================================================
# 1. GLARMA(3,0) Poisson ir NegBin visoms seniūnijoms
# ============================================================

setDT(result)

keep_cols <- c("sen_kodas", "hour_time", "N")
drop_cols <- setdiff(names(result), keep_cols)

if (length(drop_cols) > 0) {
  result[, (drop_cols) := NULL]
}

result[, sen_kodas := as.character(sen_kodas)]
result[, N := as.integer(N)]

setorder(result, sen_kodas, hour_time)

stopifnot(all(result$N >= 0))
stopifnot(!any(is.na(result$N)))

gc()

# ============================================================
# 2. Parametrai
# ============================================================

test_h <- 24 * 30
period <- 24

p_glarma <- 3
q_glarma <- 0

series_dir <- "glarma_series_by_sen_kodas"
model_dir <- "models_glarma"
pred_dir <- "predictions_glarma"

dir.create(series_dir, showWarnings = FALSE)
dir.create(model_dir, showWarnings = FALSE)
dir.create(pred_dir, showWarnings = FALSE)

sids <- sort(unique(result$sen_kodas))

safe_id <- function(x) {
  gsub("[^A-Za-z0-9_\\-]", "_", x)
}

# ============================================================
# 3. Pagalbinės funkcijos
# ============================================================

make_glarma_xreg <- function(dt) {
  
  dt <- copy(dt)
  
  dt[, hour := factor(
    lubridate::hour(hour_time),
    levels = 0:23
  )]
  
  dt[, weekday := factor(
    lubridate::wday(hour_time, week_start = 1),
    levels = 1:7,
    labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
    ordered = FALSE
  )]
  
  X <- model.matrix(
    ~ hour + weekday,
    data = dt
  )
  
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  
  X
}

calc_metrics <- function(actual, pred, train, m = 24) {
  
  err <- actual - pred
  
  rmse <- sqrt(mean(err^2, na.rm = TRUE))
  mae <- mean(abs(err), na.rm = TRUE)
  
  smape <- mean(
    ifelse(
      abs(actual) + abs(pred) == 0,
      0,
      2 * abs(actual - pred) / (abs(actual) + abs(pred))
    ),
    na.rm = TRUE
  ) * 100
  
  if (length(train) > m) {
    
    denom <- mean(
      abs(train[(m + 1):length(train)] - train[1:(length(train) - m)]),
      na.rm = TRUE
    )
    
    mase <- ifelse(
      is.finite(denom) && denom > 0,
      mae / denom,
      NA_real_
    )
    
  } else {
    mase <- NA_real_
  }
  
  c(
    RMSE = rmse,
    MAE = mae,
    sMAPE = smape,
    MASE = mase
  )
}

seasonal_naive_forecast <- function(y_train, h, m = 24) {
  
  if (length(y_train) < m) {
    return(rep(mean(y_train, na.rm = TRUE), h))
  }
  
  last_season <- tail(y_train, m)
  
  rep(last_season, length.out = h)
}

extract_glarma_residuals <- function(fit) {
  
  resid_values <- tryCatch(
    {
      if (!is.null(fit$residuals)) {
        fit$residuals
      } else {
        residuals(fit)
      }
    },
    error = function(e) {
      NA_real_
    }
  )
  
  resid_values <- as.numeric(resid_values)
  resid_values <- resid_values[is.finite(resid_values)]
  
  resid_values
}

extract_glarma_loglik <- function(fit) {
  
  if (!is.null(fit$logLik)) {
    return(as.numeric(fit$logLik))
  }
  
  if (!is.null(fit$loglik)) {
    return(as.numeric(fit$loglik))
  }
  
  out <- tryCatch(
    as.numeric(logLik(fit)),
    error = function(e) NA_real_
  )
  
  out
}

extract_glarma_aic <- function(fit) {
  
  if (!is.null(fit$aic)) {
    return(as.numeric(fit$aic))
  }
  
  if (!is.null(fit$AIC)) {
    return(as.numeric(fit$AIC))
  }
  
  out <- tryCatch(
    as.numeric(AIC(fit)),
    error = function(e) NA_real_
  )
  
  out
}

# ============================================================
# 4. Funkcija vienam GLARMA modeliui
# ============================================================

fit_glarma_one_model <- function(y, X, distr) {
  
  y <- as.numeric(y)
  
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  
  stopifnot(is.numeric(y))
  stopifnot(is.matrix(X))
  stopifnot(is.numeric(X))
  stopifnot(length(y) == nrow(X))
  stopifnot(!anyNA(y))
  stopifnot(!anyNA(X))
  stopifnot(all(is.finite(y)))
  stopifnot(all(is.finite(X)))
  
  args <- list(
    y = y,
    X = X,
    type = distr,
    method = "FS",
    residuals = "Pearson",
    maxit = 100,
    grad = 1e-6,
    phiLags = 1:3,
    phiInit = rep(0, 3)
  )
  
  if (distr == "NegBin") {
    args$alphaInit <- 1
  }
  
  fit <- do.call(glarma::glarma, args)
  
  fit$used_method <- "FS"
  fit$used_residuals <- "Pearson"
  
  fit
}

# ============================================================
# 5. Sukuriame atskirus failus kiekvienai seniūnijai
# ============================================================

for (sid in sids) {
  
  file_i <- file.path(
    series_dir,
    paste0("series_", safe_id(sid), ".rds")
  )
  
  dt_i <- result[sen_kodas == sid, .(sen_kodas, hour_time, N)]
  setorder(dt_i, hour_time)
  
  saveRDS(dt_i, file_i, compress = "gzip")
}

cat("Seniūnijų failai sukurti.\n\n")

gc()

# ============================================================
# 6. Užduočių lentelė: kiekvienai seniūnijai 2 modeliai
# ============================================================

tasks <- CJ(
  sen_kodas = sids,
  distr = c("Poi", "NegBin")
)

tasks[, task_id := .I]

print(tasks)

# ============================================================
# 7. Vienos užduoties funkcija
# ============================================================

fit_one_glarma_task <- function(task_i) {
  
  sid <- tasks$sen_kodas[task_i]
  distr_i <- tasks$distr[task_i]
  
  t0 <- Sys.time()
  
  model_name <- paste0("GLARMA(3,0)_", distr_i)
  
  cat(sprintf(
    "[%s] %d / %d | start sen_kodas = %s | model = %s | pid = %s\n",
    format(Sys.time(), "%H:%M:%S"),
    task_i,
    nrow(tasks),
    sid,
    model_name,
    Sys.getpid()
  ))
  flush.console()
  
  file_i <- file.path(
    series_dir,
    paste0("series_", safe_id(sid), ".rds")
  )
  
  dt <- readRDS(file_i)
  setDT(dt)
  setorder(dt, hour_time)
  
  n <- nrow(dt)
  
  if (n <= test_h + 24 * 14) {
    
    status_i <- "too_short"
    
    out <- list(
      metrics = data.table(
        sen_kodas = sid,
        model = model_name,
        distr = distr_i,
        p = 3L,
        q = 0L,
        status = status_i,
        n_total = n,
        n_train = NA_integer_,
        n_test = NA_integer_,
        method_used = NA_character_,
        residuals_used = NA_character_,
        errCode = NA_integer_,
        WError = NA_integer_,
        logLik = NA_real_,
        AIC = NA_real_,
        RMSE = NA_real_,
        MAE = NA_real_,
        sMAPE = NA_real_,
        MASE = NA_real_,
        RMSE_snaive = NA_real_,
        MAE_snaive = NA_real_,
        sMAPE_snaive = NA_real_,
        MASE_snaive = NA_real_,
        error_message = NA_character_
      ),
      predictions = NULL
    )
    
    return(out)
  }
  
  train_n <- n - test_h
  
  train <- dt[1:train_n]
  test <- dt[(train_n + 1):n]
  
  y_train <- as.numeric(train$N)
  y_test <- as.numeric(test$N)
  test_time <- test$hour_time
  
  X_train <- make_glarma_xreg(train)
  X_test <- make_glarma_xreg(test)
  
  X_test <- X_test[, colnames(X_train), drop = FALSE]
  
  pred_snaive <- seasonal_naive_forecast(
    y_train = y_train,
    h = length(y_test),
    m = period
  )
  
  pred_snaive <- pmax(pred_snaive, 0)
  
  base_m <- calc_metrics(
    actual = y_test,
    pred = pred_snaive,
    train = y_train,
    m = period
  )
  
  if (length(unique(y_train)) <= 1L) {
    
    pred_mu <- rep(y_train[1], length(y_test))
    pred_mu <- pmax(pred_mu, 0)
    
    m <- calc_metrics(
      actual = y_test,
      pred = pred_mu,
      train = y_train,
      m = period
    )
    
    predictions_i <- data.table(
      sen_kodas = sid,
      model = model_name,
      distr = distr_i,
      hour_time = test_time,
      actual = y_test,
      pred_mu = pred_mu,
      pred_sim = NA_real_,
      pred_snaive = pred_snaive
    )
    
    pred_file <- file.path(
      pred_dir,
      paste0("predictions_", safe_id(sid), "_", distr_i, "_p3_q0.rds")
    )
    
    saveRDS(predictions_i, pred_file, compress = "gzip")
    
    out <- list(
      metrics = data.table(
        sen_kodas = sid,
        model = model_name,
        distr = distr_i,
        p = 3L,
        q = 0L,
        status = "constant_series",
        n_total = n,
        n_train = length(y_train),
        n_test = length(y_test),
        method_used = NA_character_,
        residuals_used = NA_character_,
        errCode = NA_integer_,
        WError = NA_integer_,
        logLik = NA_real_,
        AIC = NA_real_,
        RMSE = m["RMSE"],
        MAE = m["MAE"],
        sMAPE = m["sMAPE"],
        MASE = m["MASE"],
        RMSE_snaive = base_m["RMSE"],
        MAE_snaive = base_m["MAE"],
        sMAPE_snaive = base_m["sMAPE"],
        MASE_snaive = base_m["MASE"],
        error_message = NA_character_
      ),
      predictions = predictions_i
    )
    
    return(out)
  }
  
  fit <- tryCatch(
    {
      fit_glarma_one_model(
        y = y_train,
        X = X_train,
        distr = distr_i
      )
    },
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    
    status_i <- "glarma_error"
    
    predictions_i <- data.table(
      sen_kodas = sid,
      model = model_name,
      distr = distr_i,
      hour_time = test_time,
      actual = y_test,
      pred_mu = pred_snaive,
      pred_sim = NA_real_,
      pred_snaive = pred_snaive
    )
    
    pred_file <- file.path(
      pred_dir,
      paste0("predictions_", safe_id(sid), "_", distr_i, "_p3_q0.rds")
    )
    
    saveRDS(predictions_i, pred_file, compress = "gzip")
    
    out <- list(
      metrics = data.table(
        sen_kodas = sid,
        model = model_name,
        distr = distr_i,
        p = 3L,
        q = 0L,
        status = status_i,
        n_total = n,
        n_train = length(y_train),
        n_test = length(y_test),
        method_used = NA_character_,
        residuals_used = NA_character_,
        errCode = NA_integer_,
        WError = NA_integer_,
        logLik = NA_real_,
        AIC = NA_real_,
        RMSE = base_m["RMSE"],
        MAE = base_m["MAE"],
        sMAPE = base_m["sMAPE"],
        MASE = base_m["MASE"],
        RMSE_snaive = base_m["RMSE"],
        MAE_snaive = base_m["MAE"],
        sMAPE_snaive = base_m["sMAPE"],
        MASE_snaive = base_m["MASE"],
        error_message = fit$message
      ),
      predictions = predictions_i
    )
    
    cat(sprintf(
      "[%s] done sen_kodas = %s | model = %s | status = %s | time = %.2f min | pid = %s\n\n",
      format(Sys.time(), "%H:%M:%S"),
      sid,
      model_name,
      status_i,
      as.numeric(difftime(Sys.time(), t0, units = "mins")),
      Sys.getpid()
    ))
    flush.console()
    
    return(out)
  }
  
  # ------------------------------------------------------------
  # Forecast
  # ------------------------------------------------------------
  
  set.seed(123 + task_i)
  
  fc <- tryCatch(
    {
      glarma::forecast(
        fit,
        n.ahead = length(y_test),
        newdata = X_test
      )
    },
    error = function(e) e
  )
  
  if (inherits(fc, "error")) {
    
    status_i <- "forecast_error"
    
    predictions_i <- data.table(
      sen_kodas = sid,
      model = model_name,
      distr = distr_i,
      hour_time = test_time,
      actual = y_test,
      pred_mu = pred_snaive,
      pred_sim = NA_real_,
      pred_snaive = pred_snaive
    )
    
    m <- base_m
    
    error_message_i <- fc$message
    
  } else {
    
    status_i <- "ok"
    
    pred_mu <- as.numeric(fc$mu)
    pred_sim <- as.numeric(fc$Y)
    
    pred_mu <- pmax(pred_mu, 0)
    pred_sim <- pmax(pred_sim, 0)
    
    predictions_i <- data.table(
      sen_kodas = sid,
      model = model_name,
      distr = distr_i,
      hour_time = test_time,
      actual = y_test,
      pred_mu = pred_mu,
      pred_sim = pred_sim,
      pred_snaive = pred_snaive
    )
    
    m <- calc_metrics(
      actual = y_test,
      pred = pred_mu,
      train = y_train,
      m = period
    )
    
    error_message_i <- NA_character_
  }
  
  # ------------------------------------------------------------
  # Save model and predictions
  # ------------------------------------------------------------
  
  model_file <- file.path(
    model_dir,
    paste0("glarma_", safe_id(sid), "_", distr_i, "_p3_q0.rds")
  )
  
  saveRDS(fit, model_file, compress = "gzip")
  
  pred_file <- file.path(
    pred_dir,
    paste0("predictions_", safe_id(sid), "_", distr_i, "_p3_q0.rds")
  )
  
  saveRDS(predictions_i, pred_file, compress = "gzip")
  
  resid_values <- extract_glarma_residuals(fit)
  
  lb_24 <- tryCatch(
    Box.test(
      resid_values,
      lag = 24,
      type = "Ljung-Box",
      fitdf = 3
    ),
    error = function(e) NULL
  )
  
  lb_168 <- tryCatch(
    Box.test(
      resid_values,
      lag = 24 * 7,
      type = "Ljung-Box",
      fitdf = 3
    ),
    error = function(e) NULL
  )
  
  metrics_i <- data.table(
    sen_kodas = sid,
    model = model_name,
    distr = distr_i,
    p = 3L,
    q = 0L,
    status = status_i,
    n_total = n,
    n_train = length(y_train),
    n_test = length(y_test),
    method_used = if (!is.null(fit$used_method)) fit$used_method else NA_character_,
    residuals_used = if (!is.null(fit$used_residuals)) fit$used_residuals else NA_character_,
    errCode = if (!is.null(fit$errCode)) fit$errCode else NA_integer_,
    WError = if (!is.null(fit$WError)) fit$WError else NA_integer_,
    logLik = extract_glarma_loglik(fit),
    AIC = extract_glarma_aic(fit),
    ljung_box_lag24_pvalue = if (!is.null(lb_24)) lb_24$p.value else NA_real_,
    ljung_box_lag168_pvalue = if (!is.null(lb_168)) lb_168$p.value else NA_real_,
    RMSE = m["RMSE"],
    MAE = m["MAE"],
    sMAPE = m["sMAPE"],
    MASE = m["MASE"],
    RMSE_snaive = base_m["RMSE"],
    MAE_snaive = base_m["MAE"],
    sMAPE_snaive = base_m["sMAPE"],
    MASE_snaive = base_m["MASE"],
    n_resid = length(resid_values),
    model_file = model_file,
    predictions_file = pred_file,
    error_message = error_message_i
  )
  
  cat(sprintf(
    "[%s] done sen_kodas = %s | model = %s | status = %s | time = %.2f min | pid = %s\n\n",
    format(Sys.time(), "%H:%M:%S"),
    sid,
    model_name,
    status_i,
    as.numeric(difftime(Sys.time(), t0, units = "mins")),
    Sys.getpid()
  ))
  flush.console()
  
  rm(fit, fc, dt)
  gc()
  
  list(
    metrics = metrics_i,
    predictions = predictions_i
  )
}

# ============================================================
# 8. Parallel setup
# ============================================================

n_physical <- parallel::detectCores(logical = FALSE)

if (is.na(n_physical) || n_physical < 2L) {
  n_physical <- 2L
}

n_cores <- min(
  max(1L, n_physical - 1L),
  nrow(tasks)
)

cat(sprintf(
  "Išlygiagretinta: %d procesai\n\n",
  n_cores
))

cl <- parallel::makeCluster(
  n_cores,
  type = "PSOCK",
  outfile = ""
)

parallel::clusterEvalQ(cl, {
  library(data.table)
  library(lubridate)
  library(glarma)
})

parallel::clusterExport(
  cl,
  varlist = c(
    "tasks",
    "series_dir",
    "model_dir",
    "pred_dir",
    "safe_id",
    "make_glarma_xreg",
    "calc_metrics",
    "seasonal_naive_forecast",
    "extract_glarma_residuals",
    "extract_glarma_loglik",
    "extract_glarma_aic",
    "fit_glarma_one_model",
    "fit_one_glarma_task",
    "test_h",
    "period"
  ),
  envir = environment()
)

parallel::clusterSetRNGStream(cl, 123)

res_list <- tryCatch(
  {
    parallel::parLapplyLB(
      cl = cl,
      X = seq_len(nrow(tasks)),
      fun = fit_one_glarma_task,
      chunk.size = 1L
    )
  },
  finally = {
    parallel::stopCluster(cl)
  }
)

cat("\nParallel GLARMA mokymas baigtas.\n\n")

# ============================================================
# 9. Bendros rezultatų lentelės
# ============================================================

glarma_metrics <- rbindlist(
  lapply(res_list, `[[`, "metrics"),
  fill = TRUE
)

glarma_predictions <- rbindlist(
  lapply(res_list, `[[`, "predictions"),
  fill = TRUE
)

setorder(glarma_metrics, sen_kodas, distr)
setorder(glarma_predictions, sen_kodas, distr, hour_time)

saveRDS(
  glarma_metrics,
  "glarma_metrics_p3_q0_all_seniunijos.rds",
  compress = "gzip"
)

saveRDS(
  glarma_predictions,
  "glarma_predictions_p3_q0_all_seniunijos.rds",
  compress = "gzip"
)

fwrite(
  glarma_metrics,
  "glarma_metrics_p3_q0_all_seniunijos.csv"
)

cat("Rezultatai išsaugoti:\n")
cat("- glarma_metrics_p3_q0_all_seniunijos.rds\n")
cat("- glarma_predictions_p3_q0_all_seniunijos.rds\n")
cat("- glarma_metrics_p3_q0_all_seniunijos.csv\n")
cat("- modeliai aplanke: ", model_dir, "\n", sep = "")
cat("- atskiros prognozės aplanke: ", pred_dir, "\n", sep = "")

# ============================================================
# 1. Pasirenkame seniūniją ir modelį
# ============================================================

target_sid <- "1313"
distr_i <- "NegBin"       # "Poi" arba "NegBin"

test_h <- 24 * 30
history_h <- 24 * 3
alpha_level <- 0.05    # 95% intervalas

# ============================================================
# 2. Duomenys
# ============================================================

one <- copy(result[sen_kodas == target_sid])
setorder(one, hour_time)

one[, N := as.integer(N)]

train_n <- nrow(one) - test_h

train <- one[1:train_n]
test  <- one[(train_n + 1):.N]

y_train <- train$N
y_test  <- test$N

# ============================================================
# 3. X matrica
# ============================================================

make_glarma_xreg <- function(dt) {
  
  dt <- copy(dt)
  
  dt[, hour := factor(
    lubridate::hour(hour_time),
    levels = 0:23
  )]
  
  dt[, weekday := factor(
    lubridate::wday(hour_time, week_start = 1),
    levels = 1:7,
    labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
    ordered = FALSE
  )]
  
  X <- model.matrix(~ hour + weekday, data = dt)
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  
  X
}

X_train <- make_glarma_xreg(train)
X_test  <- make_glarma_xreg(test)

X_test <- X_test[, colnames(X_train), drop = FALSE]

# ============================================================
# 4. Įkeliame modelį
# ============================================================

model_file <- file.path(
  "models_glarma",
  paste0("glarma_", target_sid, "_", distr_i, "_p3_q0.rds")
)

fit <- readRDS(model_file)

B <- 1000

lower_prob <- alpha_level / 2
upper_prob <- 1 - alpha_level / 2

set.seed(123)

sim_mat <- replicate(B, {
  fc_b <- glarma::forecast(
    fit,
    n.ahead = nrow(test),
    newdata = X_test
  )
  
  as.numeric(fc_b$Y)
})

if (is.vector(sim_mat)) {
  sim_mat <- matrix(sim_mat, ncol = B)
}

pred_lower <- apply(
  sim_mat,
  1,
  quantile,
  probs = lower_prob,
  na.rm = TRUE
)

pred_upper <- apply(
  sim_mat,
  1,
  quantile,
  probs = upper_prob,
  na.rm = TRUE
)

pred_mu <- rowMeans(sim_mat, na.rm = TRUE)

forecast_dt <- data.table(
  hour_time = test$hour_time,
  actual = y_test,
  pred_mu = pred_mu,
  lower = pred_lower,
  upper = pred_upper
)
random_id <- sample(seq_len(B), 1)

forecast_dt[, random_path := sim_mat[, random_id]]

# ============================================================
# 7. Istorijos dalis
# ============================================================

history_start <- max(1, train_n - history_h + 1)

history_dt <- one[history_start:train_n, .(
  hour_time,
  actual = N
)]

# ============================================================
# 8. Grafikas
# ============================================================


distr_i <- "NegBin"
B <- 1000
test_h <- 24 * 30
history_h <- 24 * 3

model_dir <- "models_glarma"
bootstrap_dir <- "bootstrap_glarma"
images_dir <- "images"

dir.create(bootstrap_dir, showWarnings = FALSE)
dir.create(images_dir, showWarnings = FALSE)

safe_id <- function(x) {
  gsub("[^A-Za-z0-9_\\-]", "_", x)
}

make_glarma_xreg <- function(dt) {
  dt <- copy(dt)
  
  dt[, hour := factor(
    lubridate::hour(hour_time),
    levels = 0:23
  )]
  
  dt[, weekday := factor(
    lubridate::wday(hour_time, week_start = 1),
    levels = 1:7,
    labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
    ordered = FALSE
  )]
  
  X <- model.matrix(~ hour + weekday, data = dt)
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  
  X
}

compute_and_save_glarma_bootstrap <- function(
    sid,
    distr_i = "NegBin",
    B = 1000,
    test_h = 24 * 30,
    model_dir = "models_glarma",
    bootstrap_dir = "bootstrap_glarma",
    overwrite = FALSE
) {
  
  out_file <- file.path(
    bootstrap_dir,
    paste0("bootstrap_", safe_id(sid), "_", distr_i, "_p3_q0_B", B, ".rds")
  )
  
  if (file.exists(out_file) && !overwrite) {
    return(readRDS(out_file))
  }
  
  one <- copy(result[sen_kodas == sid])
  setorder(one, hour_time)
  one[, N := as.integer(N)]
  
  if (nrow(one) <= test_h + 10) {
    warning(paste("Per trumpa laiko eilutė:", sid))
    return(NULL)
  }
  
  train_n <- nrow(one) - test_h
  
  train <- one[1:train_n]
  test <- one[(train_n + 1):.N]
  
  y_test <- test$N
  
  X_train <- make_glarma_xreg(train)
  X_test <- make_glarma_xreg(test)
  X_test <- X_test[, colnames(X_train), drop = FALSE]
  
  model_file <- file.path(
    model_dir,
    paste0("glarma_", safe_id(sid), "_", distr_i, "_p3_q0.rds")
  )
  
  if (!file.exists(model_file)) {
    warning(paste("Modelio failas nerastas:", model_file))
    return(NULL)
  }
  
  fit <- readRDS(model_file)
  
  seed_i <- 123 + sum(utf8ToInt(as.character(sid))) + ifelse(distr_i == "NegBin", 10000, 0)
  set.seed(seed_i)
  
  sim_mat <- replicate(B, {
    fc_b <- glarma::forecast(
      fit,
      n.ahead = nrow(test),
      newdata = X_test
    )
    
    as.numeric(fc_b$Y)
  })
  
  if (is.vector(sim_mat)) {
    sim_mat <- matrix(sim_mat, ncol = B)
  }
  
  lower_95 <- apply(sim_mat, 1, quantile, probs = 0.025, na.rm = TRUE)
  upper_95 <- apply(sim_mat, 1, quantile, probs = 0.975, na.rm = TRUE)
  
  lower_80 <- apply(sim_mat, 1, quantile, probs = 0.10, na.rm = TRUE)
  upper_80 <- apply(sim_mat, 1, quantile, probs = 0.90, na.rm = TRUE)
  
  pred_mean <- rowMeans(sim_mat, na.rm = TRUE)
  
  random_id <- sample(seq_len(B), 1)
  
  forecast_dt <- data.table(
    sen_kodas = sid,
    distr = distr_i,
    hour_time = test$hour_time,
    actual = y_test,
    pred_mean = pred_mean,
    random_path = sim_mat[, random_id],
    lower_95 = lower_95,
    upper_95 = upper_95,
    lower_80 = lower_80,
    upper_80 = upper_80
  )
  
  train_dt <- train[, .(
    sen_kodas,
    hour_time,
    actual = N
  )]
  
  test_dt <- test[, .(
    sen_kodas,
    hour_time,
    actual = N
  )]
  
  boot_data <- list(
    metadata = list(
      sen_kodas = sid,
      distr = distr_i,
      B = B,
      test_h = test_h,
      model_file = model_file,
      seed = seed_i,
      random_id = random_id,
      created_at = Sys.time()
    ),
    forecast_dt = forecast_dt,
    train_dt = train_dt,
    test_dt = test_dt,
    sim_mat = sim_mat
  )
  
  saveRDS(boot_data, out_file, compress = "gzip")
  
  boot_data
}

plot_glarma_bootstrap <- function(
    boot_data,
    history_h = 24 * 3,
    plot_titles = NULL
) {
  
  forecast_dt <- copy(boot_data$forecast_dt)
  train_dt <- copy(boot_data$train_dt)
  
  sid <- boot_data$metadata$sen_kodas
  distr_i <- boot_data$metadata$distr
  
  forecast_start <- min(forecast_dt$hour_time)
  
  forecast_dt_week <- forecast_dt[
    hour_time <= forecast_start + lubridate::days(7)
  ]
  
  history_dt <- tail(train_dt, history_h)
  
  place_name <- if (!is.null(plot_titles) && sid %in% names(plot_titles)) {
    plot_titles[[sid]]
  } else {
    sid
  }
  
  graph_title <- paste0("GLARMA prognozavimas ", place_name, " seniūnijoje")
  
  ggplot() +
    geom_ribbon(
      data = forecast_dt_week,
      aes(
        x = hour_time,
        ymin = lower_95,
        ymax = upper_95,
        fill = "95% pasikliovimo intervalas"
      ),
      alpha = 0.20
    ) +
    geom_ribbon(
      data = forecast_dt_week,
      aes(
        x = hour_time,
        ymin = lower_80,
        ymax = upper_80,
        fill = "80% pasikliovimo intervalas"
      ),
      alpha = 0.35
    ) +
    geom_line(
      data = history_dt,
      aes(x = hour_time, y = actual, color = "Tikrosios reikšmės"),
      linewidth = 1
    ) +
    geom_line(
      data = forecast_dt_week,
      aes(x = hour_time, y = actual, color = "Tikrosios reikšmės"),
      linewidth = 1
    ) +
    geom_line(
      data = forecast_dt_week,
      aes(x = hour_time, y = random_path, color = "GLARMA prognozavimas"),
      linewidth = 1.1,
      alpha = 0.8
    ) +
    geom_vline(
      xintercept = forecast_start,
      linetype = "dashed",
      linewidth = 1
    ) +
    labs(
      title = graph_title,
      x = "Laikas",
      y = "Pranešimų skaičius",
      color = NULL,
      fill = NULL
    ) +
    scale_x_datetime(
      date_breaks = "2 day",
      labels = lt_date_labels
    ) +
    scale_y_continuous(
      labels = scales::label_number(
        decimal.mark = ",",
        big.mark = " "
      )
    ) +
    scale_fill_manual(
      values = c(
        "95% pasikliovimo intervalas" = "#4B0082",
        "80% pasikliovimo intervalas" = "#FF8C00"
      )
    ) +
    theme(
      legend.position = "bottom"
    ) +
    guides(
      color = guide_legend(nrow = 1),
      fill = guide_legend(nrow = 1)
    )
}

for (sid in chosen_sids) {
  
  boot_data <- compute_and_save_glarma_bootstrap(
    sid = sid,
    distr_i = distr_i,
    B = B,
    test_h = test_h,
    model_dir = model_dir,
    bootstrap_dir = bootstrap_dir,
    overwrite = FALSE
  )
  
  if (is.null(boot_data)) next
  
  p <- plot_glarma_bootstrap(
    boot_data = boot_data,
    history_h = history_h,
    plot_titles = plot_titles
  )
  
  print(p)
  
  ggsave(
    filename = paste0("images/glarma_bootstrap_forecast_", sid, "_", distr_i, ".png"),
    plot = p,
    width = 12,
    height = 7,
    dpi = 300
  )
  
  rm(boot_data, p)
  gc()
}


target_sid <- "1313"

test_h <- 24 * 30
lag_max <- 24 * 7

# Какую GLARMA модель брать:
# "Poi" или "NegBin"
glarma_distr <- "Poi"

sarima_model_dir <- "models_sarima"
glarma_model_dir <- "models_glarma"


# ============================================================
# Train data for selected sen_kodas
# ============================================================

one <- copy(result[sen_kodas == target_sid, .(hour_time, N)])

setorder(one, hour_time)

one[, N := as.integer(N)]

if (nrow(one) <= test_h) {
  stop("Слишком короткий ряд для test_h = ", test_h)
}

train_n <- nrow(one) - test_h

train <- one[1:train_n]

train_time <- train$hour_time

# ============================================================
# Load SARIMAX model
# ============================================================

sarima_file <- file.path(
  sarima_model_dir,
  paste0("sarima_", safe_id(target_sid), ".rds")
)

if (!file.exists(sarima_file)) {
  stop("SARIMAX model file not found: ", sarima_file)
}

sarima_fit <- readRDS(sarima_file)


# ============================================================
# Load GLARMA model
# ============================================================

glarma_distr <- "NegBin"

glarma_file <- file.path(
  glarma_model_dir,
  paste0("glarma_", safe_id(target_sid), "_", glarma_distr, "_p3_q0.rds")
)

glarma_fit <- readRDS(glarma_file)
plot(
  glarma_fit,
  ask = FALSE,
  which = c(5),
  titles = list(""),
  ann = FALSE
)

title(
  main = "PIT histograma neigiamojo binominio skirstinio modeliui",
  xlab = "PIT reikšmės",
  ylab = "Santykinis dažnis"
)

pl_1 <- recordPlot()

png(
  filename = "pit_histograma.png",
  width = 2000,
  height = 1400,
  res = 300
)

replayPlot(pl_1)

dev.off()

glarma_distr <- "Poi"
glarma_file <- file.path(
  glarma_model_dir,
  paste0("glarma_", safe_id(target_sid), "_", glarma_distr, "_p3_q0.rds")
)
glarma_fit_2 <- readRDS(glarma_file)
plot(
  glarma_fit_2,
  ask = FALSE,
  which = c(5),
  titles = list(""),
  ann = FALSE
)

title(
  main = "PIT histograma Puasono skirstinio modeliui",
  xlab = "PIT reikšmės",
  ylab = "Santykinis dažnis"
)

pl_2 <- recordPlot()

png(
  filename = "pit_histograma_ps.png",
  width = 2000,
  height = 1400,
  res = 300
)

replayPlot(pl_2)

dev.off()

rs_ng <- residuals(glarma_fit)
ggAcf(rs_ng)+
labs(
  title = "Neigiamo binominio skirstinio ACF liekanų grafikas",
  x = "Ankstiniai",
  y = "ACF"
)
ggsave("images/acf_glarma_ng.png", width = 8, height = 5, dpi = 600)

rs_ps <- residuals(glarma_fit_2)
ggAcf(rs_ps) +
  labs(
    title = "Puasono skirstinio ACF liekanų grafikas",
    x = "Ankstiniai",
    y = "ACF"
  )
ggsave("images/acf_glarma_ps.png", width = 8, height = 5, dpi = 600)


if (!exists("glarma_predictions")) {
  glarma_predictions <- readRDS("glarma_predictions_p3_q0_all_seniunijos.rds")
}

if (!exists("glarma_metrics")) {
  glarma_metrics <- readRDS("glarma_metrics_p3_q0_all_seniunijos.rds")
}

if (!exists("sarima_metrics")) {
  sarima_metrics <- readRDS("sarima_metrics_weekday_dummy_lb_rounded.rds")
}

setDT(glarma_predictions)
setDT(glarma_metrics)
setDT(sarima_metrics)
setDT(result)

period <- 24

calc_metrics <- function(actual, pred, train, m = 24) {
  err <- actual - pred
  
  rmse <- sqrt(mean(err^2, na.rm = TRUE))
  mae <- mean(abs(err), na.rm = TRUE)
  
  smape <- mean(
    ifelse(
      abs(actual) + abs(pred) == 0,
      0,
      2 * abs(actual - pred) / (abs(actual) + abs(pred))
    ),
    na.rm = TRUE
  ) * 100
  
  if (length(train) > m) {
    denom <- mean(
      abs(train[(m + 1):length(train)] - train[1:(length(train) - m)]),
      na.rm = TRUE
    )
    
    mase <- ifelse(
      is.finite(denom) && denom > 0,
      mae / denom,
      NA_real_
    )
  } else {
    mase <- NA_real_
  }
  
  c(
    RMSE = rmse,
    MAE = mae,
    sMAPE = smape,
    MASE = mase
  )
}

make_count_pred <- function(x) {
  x <- as.numeric(x)
  x[!is.finite(x)] <- NA_real_
  pmax(0, round(x))
}

recalc_glarma_random_one_sid <- function(sid) {
  pred_i <- glarma_predictions[
    sen_kodas == sid & distr == "NegBin"
  ]
  
  if (nrow(pred_i) == 0) {
    return(NULL)
  }
  
  split_time <- min(pred_i$hour_time, na.rm = TRUE)
  
  y_train <- result[
    sen_kodas == sid & hour_time < split_time,
    N
  ]
  
  actual <- pred_i$actual
  pred_random <- make_count_pred(pred_i$pred_sim)
  pred_snaive <- make_count_pred(pred_i$pred_snaive)
  
  if (all(is.na(pred_random))) {
    m_glarma <- c(
      RMSE = NA_real_,
      MAE = NA_real_,
      sMAPE = NA_real_,
      MASE = NA_real_
    )
  } else {
    m_glarma <- calc_metrics(
      actual = actual,
      pred = pred_random,
      train = y_train,
      m = period
    )
  }
  
  m_snaive <- calc_metrics(
    actual = actual,
    pred = pred_snaive,
    train = y_train,
    m = period
  )
  
  data.table(
    sen_kodas = sid,
    GLARMA_RMSE = m_glarma["RMSE"],
    GLARMA_MAE = m_glarma["MAE"],
    GLARMA_sMAPE = m_glarma["sMAPE"],
    GLARMA_MASE = m_glarma["MASE"],
    SNaive_RMSE = m_snaive["RMSE"],
    SNaive_MAE = m_snaive["MAE"],
    SNaive_sMAPE = m_snaive["sMAPE"],
    SNaive_MASE = m_snaive["MASE"]
  )
}

glarma_random_metrics <- rbindlist(
  lapply(
    sort(unique(glarma_predictions[distr == "NegBin", sen_kodas])),
    recalc_glarma_random_one_sid
  ),
  fill = TRUE
)

glarma_status <- glarma_metrics[
  distr == "NegBin",
  .(
    sen_kodas,
    GLARMA_status = status
  )
]

glarma_random_metrics <- merge(
  glarma_random_metrics,
  glarma_status,
  by = "sen_kodas",
  all.x = TRUE
)

sarimax_tbl <- sarima_metrics[
  ,
  .(
    sen_kodas,
    SARIMAX_status = status,
    SARIMAX_RMSE = RMSE,
    SARIMAX_MAE = MAE,
    SARIMAX_sMAPE = sMAPE,
    SARIMAX_MASE = MASE
  )
]

compare_metrics_random <- merge(
  sarimax_tbl,
  glarma_random_metrics,
  by = "sen_kodas",
  all = TRUE
)

best_model <- function(sarimax, glarma, snaive) {
  vals <- c(
    SARIMAX = sarimax,
    GLARMA = glarma,
    `Seasonal naive` = snaive
  )
  
  if (all(is.na(vals))) {
    return(NA_character_)
  }
  
  names(vals)[which.min(vals)]
}

compare_metrics_random[
  ,
  best_RMSE := mapply(best_model, SARIMAX_RMSE, GLARMA_RMSE, SNaive_RMSE)
]

compare_metrics_random[
  ,
  best_MAE := mapply(best_model, SARIMAX_MAE, GLARMA_MAE, SNaive_MAE)
]

compare_metrics_random[
  ,
  best_sMAPE := mapply(best_model, SARIMAX_sMAPE, GLARMA_sMAPE, SNaive_sMAPE)
]

compare_metrics_random[
  ,
  best_MASE := mapply(best_model, SARIMAX_MASE, GLARMA_MASE, SNaive_MASE)
]

setorder(compare_metrics_random, sen_kodas)

saveRDS(
  compare_metrics_random,
  "compare_sarimax_glarma_random_snaive_metrics.rds",
  compress = "gzip"
)

format_num <- function(x, digits = 3) {
  ifelse(
    is.na(x),
    "",
    formatC(x, format = "f", digits = digits, decimal.mark = ",")
  )
}

highlight_min <- function(x, row_min, digits = 3) {
  value <- format_num(x, digits)
  
  ifelse(
    !is.na(x) & x == row_min,
    cell_spec(
      value,
      format = "latex",
      bold = TRUE,
      background = "gray!25"
    ),
    value
  )
}

compare_metrics_random_latex <- copy(compare_metrics_random)

compare_metrics_random_latex[
  ,
  min_RMSE := pmin(
    SARIMAX_RMSE,
    GLARMA_RMSE,
    SNaive_RMSE,
    na.rm = TRUE
  )
]

compare_metrics_random_latex[
  is.infinite(min_RMSE),
  min_RMSE := NA_real_
]

compare_metrics_random_latex <- compare_metrics_random_latex[
  ,
  .(
    `Seniūnijos kodas` = sen_kodas,
    
    `SARIMAX RMSE` = highlight_min(
      SARIMAX_RMSE,
      min_RMSE
    ),
    
    `GLARMA RMSE` = highlight_min(
      GLARMA_RMSE,
      min_RMSE
    ),
    
    `Sezoninė naivioji RMSE` = highlight_min(
      SNaive_RMSE,
      min_RMSE
    )
  )
]

table_latex_random <- kable(
  compare_metrics_random_latex,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  caption = "SARIMAX, GLARMA(3,0) neigiamojo binominio modelio atsitiktinės trajektorijos ir sezoninės naiviosios prognozės RMSE palyginimas",
  label = "tab:sarimax_glarma_random_snaive_rmse"
) %>%
  kable_styling(
    latex_options = c("hold_position", "striped"),
    full_width = FALSE,
    position = "center"
  )

save_kable(
  table_latex_random,
  file = "tables/sarimax_glarma_random_snaive_rmse.tex"
)

############TESTS

safe_id <- function(x) {
  gsub("[^A-Za-z0-9_\\-]", "_", x)
}

extract_glarma_tests <- function(fit) {
  
  lt <- tryCatch(
    {
      summary(fit, tests = TRUE)$likTests
    },
    error = function(e) {
      tryCatch(
        glarma::likTests(fit),
        error = function(e2) e2
      )
    }
  )
  
  if (inherits(lt, "error") || is.null(lt)) {
    return(data.table(
      LR_statistic = NA_real_,
      LR_p_value = NA_real_,
      Wald_statistic = NA_real_,
      Wald_p_value = NA_real_,
      test_error = if (inherits(lt, "error")) lt$message else NA_character_
    ))
  }
  
  lt <- as.matrix(lt)
  
  data.table(
    LR_statistic = as.numeric(lt["LR Test", "Statistic"]),
    LR_p_value = as.numeric(lt["LR Test", "p-value"]),
    Wald_statistic = as.numeric(lt["Wald Test", "Statistic"]),
    Wald_p_value = as.numeric(lt["Wald Test", "p-value"]),
    test_error = NA_character_
  )
}

model_index <- glarma_metrics[
  distr == "NegBin",
  .(
    sen_kodas,
    model,
    distr,
    status,
    errCode,
    WError,
    model_file = if ("model_file" %in% names(glarma_metrics)) {
      model_file
    } else {
      file.path(
        "models_glarma",
        paste0("glarma_", safe_id(sen_kodas), "_", distr, "_p3_q0.rds")
      )
    }
  )
]

glarma_tests_table <- rbindlist(
  lapply(seq_len(nrow(model_index)), function(i) {
    
    row_i <- model_index[i]
    
    if (is.na(row_i$model_file) || !file.exists(row_i$model_file)) {
      return(data.table(
        sen_kodas = row_i$sen_kodas,
        model = row_i$model,
        distr = row_i$distr,
        status = row_i$status,
        errCode = row_i$errCode,
        WError = row_i$WError,
        LR_statistic = NA_real_,
        LR_p_value = NA_real_,
        Wald_statistic = NA_real_,
        Wald_p_value = NA_real_,
        test_error = "Model file not found"
      ))
    }
    
    fit <- readRDS(row_i$model_file)
    
    tests_i <- extract_glarma_tests(fit)
    
    data.table(
      sen_kodas = row_i$sen_kodas,
      model = row_i$model,
      distr = row_i$distr,
      status = row_i$status,
      errCode = row_i$errCode,
      WError = row_i$WError
    )[, cbind(.SD, tests_i)]
  }),
  fill = TRUE
)

setorder(glarma_tests_table, sen_kodas)

format_num <- function(x, digits = 3) {
  ifelse(
    is.na(x),
    "",
    formatC(x, format = "f", digits = digits, decimal.mark = ",")
  )
}

format_p_value <- function(p) {
  ifelse(
    is.na(p),
    "",
    ifelse(
      p < 0.001,
      "< 0,001",
      formatC(p, format = "f", digits = 3, decimal.mark = ",")
    )
  )
}

glarma_tests_latex <- glarma_tests_table[
  ,
  .(
    `Seniūnijos kodas` = sen_kodas,
    `LR testo statistika` = format_num(LR_statistic),
    `LR testo p reikšmė` = format_p_value(LR_p_value),
    `Wald testo statistika` = format_num(Wald_statistic),
    `Wald testo p reikšmė` = format_p_value(Wald_p_value)
  )
]

table_latex <- kable(
  glarma_tests_latex,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  caption = "GLARMA(3,0) neigiamojo binominio modelio LR ir Wald testų rezultatai",
  label = "tab:glarma_lrt_wald_tests"
) %>%
  kable_styling(
    latex_options = c("hold_position", "striped"),
    full_width = FALSE,
    position = "center"
  )

save_kable(
  table_latex,
  file = "tables/glarma_lrt_wald_tests_negbin_p3_q0.tex"
)


#PASIKLIAUTINIAI

setDT(result)
result[, sen_kodas := as.character(sen_kodas)]

if (!exists("sarima_metrics")) {
  sarima_metrics <- readRDS("sarima_metrics_weekday_dummy_lb_rounded.rds")
}

if (!exists("glarma_metrics")) {
  glarma_metrics <- readRDS("glarma_metrics_p3_q0_all_seniunijos.rds")
}

setDT(sarima_metrics)
setDT(glarma_metrics)

test_h <- 24 * 30
period <- 24
level <- c(80, 95)

# Svarbu: kad make_test_forecast_plot_data() paimtų visą testavimo mėnesį,
# o ne tik 7 dienas kaip grafikuose
test_plot_days <- test_h / 24

use_rounded_forecast <- TRUE

sids_all <- sort(unique(result$sen_kodas))

calc_interval_coverage <- function(dt, model_name) {
  
  setDT(dt)
  
  dt <- dt[
    !is.na(actual) &
      !is.na(lower_95) &
      !is.na(upper_95)
  ]
  
  if (nrow(dt) == 0) {
    return(data.table(
      model = model_name,
      n_test = 0L,
      n_inside_95 = NA_integer_,
      coverage_95_pct = NA_real_
    ))
  }
  
  inside_95 <- dt$actual >= dt$lower_95 & dt$actual <= dt$upper_95
  
  data.table(
    model = model_name,
    n_test = nrow(dt),
    n_inside_95 = sum(inside_95, na.rm = TRUE),
    coverage_95_pct = mean(inside_95, na.rm = TRUE) * 100
  )
}

calc_sarimax_coverage_one <- function(sid) {
  
  out <- tryCatch(
    make_test_forecast_plot_data(sid),
    error = function(e) e
  )
  
  if (inherits(out, "error") || is.null(out)) {
    return(data.table(
      sen_kodas = sid,
      model = "SARIMAX",
      n_test = 0L,
      n_inside_95 = NA_integer_,
      coverage_95_pct = NA_real_,
      status = "error",
      error_message = if (inherits(out, "error")) out$message else "NULL result"
    ))
  }
  
  out <- as.data.table(out)
  
  test_dt <- out[sample_part == "Testavimo aibė"]
  
  cov_i <- calc_interval_coverage(
    dt = test_dt,
    model_name = "SARIMAX"
  )
  
  cov_i[, `:=`(
    sen_kodas = sid,
    status = "ok",
    error_message = NA_character_
  )]
  
  setcolorder(
    cov_i,
    c(
      "sen_kodas",
      "model",
      "n_test",
      "n_inside_95",
      "coverage_95_pct",
      "status",
      "error_message"
    )
  )
  
  cov_i
}

sarimax_coverage <- rbindlist(
  lapply(sids_all, calc_sarimax_coverage_one),
  fill = TRUE
)

setorder(sarimax_coverage, sen_kodas)

# ============================================================
# 2. GLARMA 95% intervalo padengimas paraleliai
# ============================================================

glarma_distrs <- c("NegBin")
# Jei nori abiejų skirstinių, naudok:
# glarma_distrs <- c("Poi", "NegBin")

B <- 1000

model_dir <- "models_glarma"
bootstrap_dir <- "bootstrap_glarma"

dir.create(bootstrap_dir, showWarnings = FALSE)

glarma_tasks <- CJ(
  sen_kodas = sids_all,
  distr = glarma_distrs
)

glarma_tasks[, task_id := .I]

calc_glarma_coverage_task <- function(task_i) {
  
  sid <- glarma_tasks$sen_kodas[task_i]
  distr_i <- glarma_tasks$distr[task_i]
  
  t0 <- Sys.time()
  
  cat(sprintf(
    "[%s] %d / %d | GLARMA intervalai | sen_kodas = %s | distr = %s | pid = %s\n",
    format(Sys.time(), "%H:%M:%S"),
    task_i,
    nrow(glarma_tasks),
    sid,
    distr_i,
    Sys.getpid()
  ))
  flush.console()
  
  boot_data <- tryCatch(
    compute_and_save_glarma_bootstrap(
      sid = sid,
      distr_i = distr_i,
      B = B,
      test_h = test_h,
      model_dir = model_dir,
      bootstrap_dir = bootstrap_dir,
      overwrite = FALSE
    ),
    error = function(e) e
  )
  
  if (inherits(boot_data, "error") || is.null(boot_data)) {
    
    status_i <- "error"
    msg_i <- if (inherits(boot_data, "error")) boot_data$message else "NULL bootstrap result"
    
    return(data.table(
      sen_kodas = sid,
      model = paste0("GLARMA_", distr_i),
      distr = distr_i,
      n_test = 0L,
      n_inside_95 = NA_integer_,
      coverage_95_pct = NA_real_,
      status = status_i,
      error_message = msg_i
    ))
  }
  
  forecast_dt <- copy(boot_data$forecast_dt)
  
  cov_i <- calc_interval_coverage(
    dt = forecast_dt,
    model_name = paste0("GLARMA_", distr_i)
  )
  
  cov_i[, `:=`(
    sen_kodas = sid,
    distr = distr_i,
    status = "ok",
    error_message = NA_character_
  )]
  
  setcolorder(
    cov_i,
    c(
      "sen_kodas",
      "model",
      "distr",
      "n_test",
      "n_inside_95",
      "coverage_95_pct",
      "status",
      "error_message"
    )
  )
  
  cat(sprintf(
    "[%s] done GLARMA intervalai | sen_kodas = %s | distr = %s | coverage = %.2f%% | time = %.2f min | pid = %s\n\n",
    format(Sys.time(), "%H:%M:%S"),
    sid,
    distr_i,
    cov_i$coverage_95_pct,
    as.numeric(difftime(Sys.time(), t0, units = "mins")),
    Sys.getpid()
  ))
  flush.console()
  
  cov_i
}

n_glarma_cores <- min(11L, nrow(glarma_tasks))

cat(sprintf(
  "GLARMA intervalų skaičiavimas išlygiagretintas: %d procesai\n\n",
  n_glarma_cores
))

cl <- parallel::makeCluster(
  n_glarma_cores,
  type = "PSOCK",
  outfile = ""
)

parallel::clusterEvalQ(cl, {
  library(data.table)
  library(lubridate)
  library(glarma)
})

parallel::clusterExport(
  cl,
  varlist = c(
    "result",
    "glarma_tasks",
    "safe_id",
    "make_glarma_xreg",
    "compute_and_save_glarma_bootstrap",
    "calc_interval_coverage",
    "calc_glarma_coverage_task",
    "test_h",
    "B",
    "model_dir",
    "bootstrap_dir"
  ),
  envir = environment()
)

parallel::clusterSetRNGStream(cl, 123)

glarma_coverage <- tryCatch(
  {
    rbindlist(
      parallel::parLapplyLB(
        cl = cl,
        X = seq_len(nrow(glarma_tasks)),
        fun = calc_glarma_coverage_task,
        chunk.size = 1L
      ),
      fill = TRUE
    )
  },
  finally = {
    parallel::stopCluster(cl)
  }
)

setorder(glarma_coverage, sen_kodas, distr)

sarimax_coverage_long <- copy(sarimax_coverage)
sarimax_coverage_long[, distr := NA_character_]

coverage_long <- rbindlist(
  list(
    sarimax_coverage_long,
    glarma_coverage
  ),
  fill = TRUE
)

setorder(coverage_long, sen_kodas, model)

coverage_pct_table <- dcast(
  coverage_long,
  sen_kodas ~ model,
  value.var = "coverage_95_pct"
)

setorder(coverage_pct_table, sen_kodas)

saveRDS(
  coverage_long,
  "interval_coverage_95_long.rds",
  compress = "gzip"
)

saveRDS(
  coverage_pct_table,
  "interval_coverage_95_pct_table.rds",
  compress = "gzip"
)

fwrite(
  coverage_long,
  "interval_coverage_95_long.csv"
)

fwrite(
  coverage_pct_table,
  "interval_coverage_95_pct_table.csv"
)


coverage_latex <- copy(coverage_pct_table)

num_cols <- setdiff(names(coverage_latex), "sen_kodas")

coverage_latex[
  ,
  (num_cols) := lapply(.SD, function(x) {
    ifelse(
      is.na(x),
      "",
      paste0(formatC(x, format = "f", digits = 1, decimal.mark = ","), "\\%")
    )
  }),
  .SDcols = num_cols
]

table_latex <- kable(
  coverage_latex,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  caption = "Tikrosios reikšmės, patenkančios į 95\\% prognozavimo intervalą",
  label = "interval_coverage_95",
  col.names = c(
    "sen\\_kodas",
    "GLARMA\\_NegBin",
    "SARIMAX"
  )
) %>%
  kable_styling(
    latex_options = c("hold_position", "striped"),
    full_width = FALSE,
    position = "center"
  )

save_kable(
  table_latex,
  file = "tables/interval_coverage_95.tex"
)

setDT(result)




