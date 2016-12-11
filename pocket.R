library(jsonlite)
library(magrittr)
library(dplyr)
library(plotly)
library(lubridate)

zeroToNA <- function(v) {
  replace(v, v == 0, NA)
}

create_history <- function(bookmarks) {
  timeAdded <- bookmarks$time_added %>% as.character %>% as.numeric %>% as.POSIXct(origin = "1970-01-01")
  timeRead <- bookmarks$time_read %>% as.character %>% as.numeric %>% zeroToNA %>% na.omit %>% as.POSIXct(origin = "1970-01-01")

  history <- data.frame(
      date = c(timeAdded, timeRead),
      action = c(rep(1, length(timeAdded)), rep(-1, length(timeRead))),
      read = c(rep(0, length(timeAdded)), rep(1, length(timeRead)))
    ) %>%
    arrange(date) %>%
    mutate(active = cumsum(action), reads = cumsum(read))
  history
}

plot_weekday_reads <- function(history) {
  week <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  history %>%
    filter(read == 1) %>%
    mutate(date_wday = weekdays(date, abbreviate = FALSE)) %>%
    mutate(date_wday = factor(date_wday, levels = week)) %>%
    group_by(date_wday) %>%
    summarise(count = n()) -> wday_reads

  plotly::plot_ly(wday_reads, x = ~date_wday, y = ~count, type = "bar")
}

day_distribution <- function(history) {
  history %>%
    filter(read == 1) %>%
    mutate(hour = hour(date)) %>%
    mutate(wday = wday(date, label = TRUE)) %>%
    mutate(workweek = ifelse(wday %in% c("Sat", "Sun"), 0, 1)) %>%
    mutate(weekend = ifelse(wday %in% c("Sat", "Sun"), 1, 0)) %>%
    group_by(hour) %>%
    summarise(workweek = sum(workweek), weekend = sum(weekend)) %>%
    plot_ly(x = ~hour, y = ~workweek, type = "bar", name = "Workweek") %>%
    add_trace(y = ~weekend, name = "Weekend")
}

plot_throughput <- function(history) {
  head(history)
  plot_ly(history, y = ~active, x = ~date, name = "pocket", type = 'scatter', mode = 'lines') %>%
    add_trace(y = ~reads, name = "reads", mode = "lines")
}

plot_laggerds <- function(bookmarks) {
  notRead <- bookmarks %>%
    filter(status == 0) %>%
    mutate(dateAdded = (time_added %>% as.character %>% as.numeric %>% as.POSIXct(origin = "1970-01-01"))) %>%
    mutate(daysDifference = ((Sys.Date() - as.Date(dateAdded)) %>% as.numeric)) %>%
    group_by(daysDifference) %>%
    summarise(count = n())

  plot_ly(notRead, x = ~daysDifference, y = ~count, type = "bar" , marker = list(color = ~daysDifference))
}

daily_adds <- function(bookmarks) {
  temp <- bookmarks %>%
    group_by(time_added) %>%
    summarise(count = n()) %>%
    ungroup
  calendar_reads <- as.list(temp$count)
  names(calendar_reads) <- temp$time_added
  calendar_reads
}

daily_reads <- function(bookmarks) {
  temp <- bookmarks %>%
    filter(time_read != 0) %>%
    group_by(time_read) %>%
    summarise(count = n()) %>%
    ungroup
  calendar_reads <- as.list(temp$count)
  names(calendar_reads) <- temp$time_read
  calendar_reads
}

year_reads_plot <- function(bookmarks) {
  calendar_reads <- daily_reads(bookmarks)
  start <- paste0(min(names(calendar_reads)), "000")
  calheatmapr::callheatmapr(data = calendar_reads, start = start, domain = "month", subDomain = "day", tooltip = T)
}

year_adds_plot <- function(bookmarks) {
  calendar_adds <- daily_adds(bookmarks)
  start <- paste0(min(names(calendar_adds)), "000")
  calheatmapr::callheatmapr(data = calendar_adds, start = start, domain = "month", subDomain = "day", tooltip = T)
}
