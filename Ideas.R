library(httr)
library(jsonlite)
library(data.table)
library(lubridate)
library(tidyr)
library(ggplot2)

# api_key <- Sys.getenv("STORMGLASS_KEY")
api_key <- "bbc40e36-944b-11f0-a59f-0242ac130006-bbc40e90-944b-11f0-a59f-0242ac130006"

url <- "https://api.stormglass.io/v2/weather/point"
# url <- "https://api.stormglass.io/v2/historical/point"

res <- GET(
  url,
  add_headers(
    Authorization = api_key
  ),
  query = list(
    lat = 43.163338,
    lng = -70.615757,
#     Line below used for historical retrieval
    # end = "2026-06-03T00:00:00+00:00",
    params = "swellDirection,swellHeight,swellPeriod,secondarySwellDirection,secondarySwellHeight,secondarySwellPeriod,waterTemperature,waveDirection,waveHeight,wavePeriod,windWaveDirection,windWaveHeight,windWavePeriod,currentDirection,currentSpeed,windSpeed,windDirection"
  )
)

Waves <- as.data.table(fromJSON(content(res, "text", encoding = "UTF-8"))$hours)

Waves[,DateTime := with_tz(ymd_hms(time), "America/New_York")]

Waves[,time := NULL]

# Waves[,DateTime := as.character(DateTime)]

# fwrite(Waves, "~/Documents/MaineSurfDashboard/Data/HistoricLongSands.csv")

# Waves[,PrimaryEnergy := (swellHeight^2)*swellPeriod]

# ggplot(Waves, aes(ymd_hms(DateTime), PrimaryEnergy))+
  # geom_line()

ModWaves <- as.data.table(pivot_wider(pivot_longer(Waves, 1:(length(Waves)-1), names_pattern = "(^[[:alpha:]]+)\\.(.+$)",
                                                   names_to = c("Metric", "Model")), names_from = "Metric", values_from = "value"))

ModWaves[,swellHeight_ft := swellHeight*3.28084]
ModWaves[,waveHeight_ft := waveHeight*3.28084]
ModWaves[,windWaveHeight_ft := windWaveHeight*3.28084]
ModWaves[,secondarySwellHeight_ft := secondarySwellHeight*3.28084]

ModWaves[,waterTemperature_f := waterTemperature*(9/5)+32]

ModWaves[,PrimaryEnergyScore := (swellHeight^2)*swellPeriod]

ggplot(ModWaves, aes(DateTime, PrimaryEnergyScore))+
  geom_smooth(span = 0.05, method = "loess", se = F, linewidth = 1.5)+
  geom_line(aes(color = Model), alpha = 0.3, linewidth = 1)+
  scale_x_datetime(date_breaks = "8 hours", date_labels = "%b %d (%a) %I%p", date_minor_breaks = "2 hours")+
  labs(x = "Time", y = "Energy Score")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), text = element_text(color = "white"), panel.grid.major.x = element_line(color = "grey85"), panel.grid.minor.x = element_line(color = "grey92"),
        panel.background = element_blank(), axis.line = element_line(color = "grey85"), axis.ticks = element_line(color = "grey85"))

ggplot(ModWaves, aes(DateTime, waveHeight_ft))+
  geom_smooth(span = 0.05, method = "loess", se = F, linewidth = 1.5)+
  geom_line(aes(color = Model), alpha = 0.3, linewidth = 1)+
  scale_x_datetime(date_breaks = "8 hours", date_labels = "%b %d (%a) %I%p", date_minor_breaks = "2 hours")+
  labs(x = "Time", y = "Energy Score")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), text = element_text(color = "white"), panel.grid.major.x = element_line(color = "grey85"), panel.grid.minor.x = element_line(color = "grey92"),
        panel.background = element_blank(), axis.line = element_line(color = "grey85"), axis.ticks = element_line(color = "grey85"))

ggplot(ModWaves, aes(DateTime, swellHeight_ft))+
  geom_smooth(span = 0.05, method = "loess", se = F, linewidth = 1.5)+
  geom_line(aes(color = Model), alpha = 0.3, linewidth = 1)+
  scale_x_datetime(date_breaks = "8 hours", date_labels = "%b %d (%a) %I%p", date_minor_breaks = "2 hours")+
  labs(x = "Time", y = "Energy Score")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), text = element_text(color = "white"), panel.grid.major.x = element_line(color = "grey85"), panel.grid.minor.x = element_line(color = "grey92"),
        panel.background = element_blank(), axis.line = element_line(color = "grey85"), axis.ticks = element_line(color = "grey85"))

ggplot(ModWaves, aes(DateTime, swellPeriod))+
  geom_smooth(span = 0.05, method = "loess", se = F, linewidth = 1.5)+
  geom_line(aes(color = Model), alpha = 0.3, linewidth = 1)+
  scale_x_datetime(date_breaks = "8 hours", date_labels = "%b %d (%a) %I%p", date_minor_breaks = "2 hours")+
  labs(x = "Time", y = "Energy Score")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), text = element_text(color = "white"), panel.grid.major.x = element_line(color = "grey85"), panel.grid.minor.x = element_line(color = "grey92"),
        panel.background = element_blank(), axis.line = element_line(color = "grey85"), axis.ticks = element_line(color = "grey85"))


ggplot(ModWaves, aes(DateTime, waterTemperature_f))+
  geom_smooth(span = 0.05, method = "loess", se = F, linewidth = 1.5)+
  # geom_line(aes(color = Model), alpha = 0.3, linewidth = 1)+
  scale_x_datetime(date_breaks = "8 hours", date_labels = "%b %d (%a) %I%p", date_minor_breaks = "2 hours")+
  labs(x = "Time", y = "Energy Score")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), text = element_text(color = "white"), panel.grid.major.x = element_line(color = "grey85"), panel.grid.minor.x = element_line(color = "grey92"),
        panel.background = element_blank(), axis.line = element_line(color = "grey85"), axis.ticks = element_line(color = "grey85"))
