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

# Смотрим ACF исходного ряда
Acf(ts_y, lag.max = 24 * 7, main = "ACF исходного ряда")
Pacf(ts_y, lag.max = 24 * 7, main = "PACF исходного ряда")

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
checkresiduals(fit_auto)

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
setorder(one, hour_time)
y_2 <- one_2$N
one_ts_2 <- as_tsibble(
  one_2,
  index = hour_time
)
gg_tsdisplay(one_ts_2, N,
             plot_type = "partial",
             lag_max = 72 )



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


setDT(result)
setDT(sarima_predictions)

chosen_sid <- "1313"

plot_test <- sarima_predictions[sen_kodas == chosen_sid]

split_time <- min(plot_test$hour_time, na.rm = TRUE)

plot_train <- result[
  sen_kodas == chosen_sid &
    hour_time >= (split_time - days(3)) &
    hour_time < split_time,
  .(
    hour_time,
    actual = N,
    pred = NA_real_,
    pred_snaive = NA_real_
  )
]


plot_test_3d <- plot_test[
  hour_time >= split_time &
    hour_time < (split_time + days(7)),
  .(
    hour_time,
    actual,
    pred,
    pred_snaive
  )
]


plot_dt_6d <- rbindlist(
  list(plot_train, plot_test_3d),
  use.names = TRUE,
  fill = TRUE
)

plot_dt_6d[, sample_part := fifelse(
  hour_time < split_time,
  "Mokymo aibė",
  "Testavimo aibė"
)]


ggplot(plot_dt_6d, aes(x = hour_time)) +
  geom_line(
    aes(y = actual, color = "Tikrosios reikšmės"),
    linewidth = 0.8
  ) +
  geom_line(
    aes(y = pred, color = "SARIMA prognozė"),
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  geom_vline(
    xintercept = as.numeric(split_time),
    linetype = "dashed"
  ) +
  labs(
    title = paste(
      "SARIMA prognozė su savaitės dienos kintamaisiais:",
      "3 dienos prieš ir 3 dienos po prognozės pradžios, sen_kodas =",
      chosen_sid
    ),
    x = "Laikas",
    y = "Įvykių skaičius",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



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


cat("\n X_train:\n")
print(dim(X_train))

cat("\nX_train:\n")
print(head(X_train[, 1:min(10, ncol(X_train)), drop = FALSE]))

cat("\nX_train:\n")
print(head(X_train[, grep("weekday", colnames(X_train)), drop = FALSE]))


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


if (nrow(glarma_summary_ok) > 0) {
  
  best_model_name <- glarma_summary_ok[1, model]
  
  cat("\n============================================\n")
  cat("AIC:", best_model_name, "\n")
  cat("============================================\n")
  
  print(summary(fits_glarma[[best_model_name]]))
  
  best_resid <- extract_glarma_residuals(fits_glarma[[best_model_name]])
  best_resid <- na.omit(best_resid)
  
  forecast::ggAcf(
    best_resid,
  ) +
    coord_cartesian(
      ylim = c(-0.03, 0.08),
      expand = FALSE
    ) +
    labs(
      title = paste("ACF:", best_model_name),
      x = "Lag",
      y = "ACF"
    )
}

if (nrow(glarma_summary_ok) > 0) {
  
  best_model_name <- glarma_summary_ok[1, model]
  best_fit <- fits_glarma[[best_model_name]]
  
  cat("\n============================================\n")
  cat("Forecast:", best_model_name, "\n")
  cat("============================================\n")
  
  
  X_test <- model.matrix(
    ~ hour + weekday,
    data = test
  )
  
  X_test <- X_test[, colnames(X_train), drop = FALSE]
  
  cat("\n X_test:\n")
  print(dim(X_test))
  
  
  set.seed(123)
  
  fc <- glarma::forecast(
    best_fit,
    n.ahead = nrow(test),
    newdata = X_test
  )
  
  y_pred_mu <- as.numeric(fc$mu)
  
  y_pred_sim <- as.numeric(fc$Y)
  
  
  forecast_test_dt <- data.table(
    hour_time = test_time,
    actual = y_test,
    pred_mu = y_pred_mu,
    pred_sim = y_pred_sim
  )
  
  print(head(forecast_test_dt))
  
  
  rmse <- sqrt(mean((forecast_test_dt$actual - forecast_test_dt$pred_mu)^2, na.rm = TRUE))
  mae  <- mean(abs(forecast_test_dt$actual - forecast_test_dt$pred_mu), na.rm = TRUE)
  
  cat("\nRMSE:", rmse, "\n")
  cat("MAE :", mae, "\n")
  
  
  ggplot(forecast_test_dt, aes(x = hour_time)) +
    geom_line(aes(y = actual, color = "Faktinės reikšmės")) +
    geom_line(aes(y = pred_mu, color = "Prognozė"), linewidth = 1) +
    labs(
      title = paste("GLARMA prognozė testinėje imtyje:", best_model_name),
      subtitle = paste0("RMSE = ", round(rmse, 3), ", MAE = ", round(mae, 3)),
      x = "Laikas",
      y = "Pranešimų skaičius",
      color = ""
    ) +
    theme_minimal()
}


forecast_test_last_week <- forecast_test_dt[
  hour_time <= min(hour_time) + lubridate::days(3)
]

ggplot(forecast_test_last_week, aes(x = hour_time)) +
  geom_line(aes(y = actual, color = "Faktinės reikšmės")) +
  geom_line(aes(y = pred_mu, color = "Prognozė"), linewidth = 1) +
  labs(
    title = paste("GLARMA prognozė pirmaj testinės imties savaitei:", best_model_name),
    x = "Laikas",
    y = "Pranešimų skaičius",
    color = ""
  ) +
  theme_minimal()



