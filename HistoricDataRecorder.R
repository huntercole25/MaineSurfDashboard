library(httr)
library(jsonlite)
library(data.table)
library(lubridate)
library(tidyr)
library(ggplot2)

api_key <- Sys.getenv("STORMGLASS_KEY")

if (api_key == "") {
  stop("STORMGLASS_KEY not found")
}

# Hist <- fread("https://raw.githubusercontent.com/huntercole25/MaineSurfDashboard/refs/heads/main/Data/HistoricLongSands.csv")
Hist <- fread("Data/HistoricLongSands.csv")

MaxDateHist <- Hist[,with_tz(force_tz(max(DateTime), "America/New_York"), "UTC")]

FormDt <- paste0(date(MaxDateHist), "T", strrep(0, 2-nchar(hour(MaxDateHist))), hour(MaxDateHist), ":00:00+00:00")

url <- "https://api.stormglass.io/v2/historical/point"

res <- GET(
  url,
  add_headers(
    Authorization = api_key
  ),
  query = list(
    lat = 43.163338,
    lng = -70.615757,
    start = FormDt,
    params = "swellDirection,swellHeight,swellPeriod,secondarySwellDirection,secondarySwellHeight,secondarySwellPeriod,waterTemperature,waveDirection,waveHeight,wavePeriod,windWaveDirection,windWaveHeight,windWavePeriod,currentDirection,currentSpeed,windSpeed,windDirection"
  )
)

if("errors" %in% names(fromJSON(content(res, "text", encoding = "UTF-8")))) stop(print(fromJSON(content(res, "text", encoding = "UTF-8"))$errors))

Waves <- as.data.table(fromJSON(content(res, "text", encoding = "UTF-8"))$hours)

Waves <- Waves[!is.na(swellHeight)]

Waves[,DateTime := with_tz(ymd_hms(time), "America/New_York")]

Waves[,time := NULL]

Waves <- Waves[with_tz(DateTime, "UTC") > MaxDateHist]

Waves[,DateTime := as.character(DateTime)]

UpdatedHist <- rbindlist(
  list(
    Hist,
    Waves
  ),
  fill = TRUE
)

UpdatedHist <- unique(
  UpdatedHist,
  by = "DateTime"
)

setorder(UpdatedHist, DateTime)

fwrite(
  UpdatedHist,
  "Data/HistoricLongSands.csv"
)