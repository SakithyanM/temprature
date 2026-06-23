#!/usr/bin/env Rscript

# daily_highs.R
# Download daily maximum temperature data and generate a plot image.

ensure_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cran.rstudio.com")
  }
}

ensure_package("jsonlite")
ensure_package("ggplot2")

library(jsonlite)
library(ggplot2)

args <- commandArgs(trailingOnly = TRUE)

parse_arg <- function(name, default) {
  match <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(match)) {
    sub(paste0("^--", name, "="), "", match[[1]])
  } else {
    default
  }
}

lat <- 45.52
lon <- -122.68
location_name <- "Portland, OR"
start_date <- parse_arg("start_date", "1980-01-01")
end_date <- parse_arg("end_date", format(Sys.Date(), "%Y-%m-%d"))
output_file <- parse_arg("output_file", "daily_highs.png")

message("Downloading daily high temperatures for Portland, OR...")
message("Latitude: ", lat, ", Longitude: ", lon)
message("Date range: ", start_date, " to ", end_date)

api_url <- sprintf(
  "https://archive-api.open-meteo.com/v1/archive?latitude=%s&longitude=%s&start_date=%s&end_date=%s&daily=temperature_2m_max&timezone=UTC",
  lat, lon, start_date, end_date
)

response <- fromJSON(api_url)

if (is.null(response$daily$time) || length(response$daily$time) == 0) {
  stop("No daily temperature data returned from the API. Please verify the coordinates and date range.")
}

weather <- data.frame(
  date = as.Date(response$daily$time),
  tmax = response$daily$temperature_2m_max
)

weather <- weather[order(weather$date), ]
weather$tmax_f <- weather$tmax * 9 / 5 + 32
weather$cycle_date <- as.Date(sprintf("2000-%02d-%02d", as.integer(format(weather$date, "%m")), as.integer(format(weather$date, "%d"))))

message("Data points retrieved: ", nrow(weather))

plot_title <- sprintf("Daily maximum temperatures in Portland, %s to %s", format(as.Date(start_date), "%Y"), format(as.Date(end_date), "%Y"))

p <- ggplot(weather, aes(x = cycle_date, y = tmax_f)) +
  geom_point(color = "gray40", size = 0.5, alpha = 0.12) +
  geom_smooth(method = "loess", span = 0.1, color = "firebrick", se = FALSE, linewidth = 0.9) +
  scale_x_date(
    breaks = seq(as.Date("2000-01-01"), as.Date("2000-12-01"), by = "1 month"),
    labels = format(seq(as.Date("2000-01-01"), as.Date("2000-12-01"), by = "1 month"), "%b"),
    expand = expansion(add = c(0, 0))
  ) +
  scale_y_continuous(
    name = "deg. Fahrenheit",
    breaks = seq(20, 120, by = 20),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = plot_title,
    x = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    axis.text.x = element_text(color = "black", size = 11),
    axis.text.y = element_text(color = "black", size = 11),
    axis.title.y = element_text(face = "plain", size = 12, margin = margin(r = 10)),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave(output_file, plot = p, width = 12, height = 6, dpi = 150)

message("Plot saved to: ", output_file)
