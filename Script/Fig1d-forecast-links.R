
library(forecast)
library(tidyverse)

domains <- c("Arts & Humanities", 
             "Social Sciences", 
             "Life Sciences & Biomedicine", 
             "Physical Sciences", 
             "Technology")


papers <- readRDS('data/papers_international_1.rds') %>% 
  filter(Date >= '2000') %>% 
  left_join(countryregion[c(2,7:8)] %>%
              `colnames<-`(c('country_from', 'country_from_region', 'country_from_direction')) %>% 
              filter(!duplicated(country_from)), 
            by = 'country_from') %>%
  left_join(countryregion[c(2,7:8)] %>%
              `colnames<-`(c('country_to', 'country_to_region', 'country_to_direction')) %>% 
              filter(!duplicated(country_to)), 
            by = 'country_to')

covid <- readRDS('index/papersCOVID.rds')

papers <- papers %>% 
  filter(!(Bibkey %in% covid$Bibkey))


coll.bymonth.original <- papers %>% 
  filter(str_detect(Research.Domains, d)) %>%
  group_by(Date) %>% 
  summarise(count = n()) %>% 
  filter(Date >= '2009.917' & Date <= '2021.000') %>% 
  slice(2:(nrow(.)-1))

coll.bymonth <- papers %>% 
  filter(str_detect(Research.Domains, d)) %>%
  group_by(Date) %>% 
  summarise(count = n()) %>% 
  filter(Date >= '2009.917' & Date <= '2021.000') %>% 
  mutate(count = c(count[1],
                   unlist(lapply(2:(nrow(.)-1), function(i){as.integer(sum(count[(i-1):(i+1)])/3)})),
                   count[nrow(.)])) %>%
  slice(2:(nrow(.)-1))

ts.df <- ts(coll.bymonth[coll.bymonth$Date <= '2020.083', ]$count, 
            frequency = 12, start = c(2010, 1))
arima.model <- auto.arima(ts.df)

ts.forecast <- forecast(arima.model, level = c(95), h = 10) %>% 
  as.data.frame() %>% 
  mutate(Date = coll.bymonth[coll.bymonth$Date > '2020.083' & coll.bymonth$Date < '2021.000', ]$Date) %>% 
  rename(forecast = `Point Forecast`, 
         low95 = `Lo 95`, 
         high95 = `Hi 95`) %>% 
  bind_rows(data.frame(forecast = coll.bymonth[coll.bymonth$Date == '2020.083', ]$count, 
                       low95 = coll.bymonth[coll.bymonth$Date == '2020.083', ]$count, 
                       high95 = coll.bymonth[coll.bymonth$Date == '2020.083', ]$count, 
                       Date = '2020.083')) %>% 
  mutate(ci95 = paste0('[', sprintf('%.02f', low95 / 1000), ',', sprintf('%.02f', high95 / 1000), ']')) %>% 
  left_join(coll.bymonth, by = 'Date') %>% 
  arrange(Date)

ggplot() + 
  geom_line(aes(as.numeric(Date), count / 1000), 
            coll.bymonth.original[coll.bymonth.original$Date <= '2020.083', ], col = 'grey65', lwd = 0.2) + 
  geom_line(aes(as.numeric(Date), count / 1000), 
            coll.bymonth[coll.bymonth$Date <= '2020.083', ], col = '#357EBD', lwd = 0.4) + 
  geom_line(aes(as.numeric(Date), count / 1000), 
            coll.bymonth[coll.bymonth$Date >= '2020.083', ], col = '#357EBD', lwd = 0.4) + 
  geom_line(aes(as.numeric(Date), forecast / 1000), 
            ts.forecast, col = '#9632B8', lwd = 0.6) + 
  geom_ribbon(aes(as.numeric(Date), forecast / 1000, ymin = low95 / 1000, ymax = high95 / 1000), 
              ts.forecast, fill = '#9632B8', alpha = 0.15) + 
  ggtitle(d) +
  scale_x_continuous('', breaks = seq(2010, 2020, 2), expand = c(0.02, 0.02)) + 
  ylab(expression('# of international collaborative papers ('*10^3*')')) + 
  theme_classic() + 
  theme(aspect.ratio = 1, 
        axis.text = element_text(size = 16), 
        axis.title = element_text(size = 16), 
        plot.title = element_text(size = 16, hjust = 0.5))

ggsave('fig/Fig1d-forecast-links.pdf', height = 5, width = 5)
  




# zoom in -----------------------------------------------------------------

ggplot() + 
  geom_line(aes(as.numeric(Date), count / 1000), 
            coll.bymonth[coll.bymonth$Date <= '2020.083', ], col = '#357EBD', lwd = 0.4) + 
  geom_line(aes(as.numeric(Date), count / 1000), 
            coll.bymonth[coll.bymonth$Date >= '2020.083', ], col = '#357EBD', lwd = 0.4) + 
  geom_line(aes(as.numeric(Date), forecast / 1000), 
            ts.forecast, col = '#9632B8', lwd = 0.6) + 
  geom_ribbon(aes(as.numeric(Date), forecast / 1000, ymin = low95 / 1000, ymax = high95 / 1000), 
              ts.forecast, fill = '#9632B8', alpha = 0.15) + 
  scale_x_continuous(breaks = seq(2010, 2018, 2), expand = c(0.02, 0.02), limits = c(2020.000, 2020.917)) + 
  scale_y_continuous(limits = c(108, 146), position = 'right') +
  ylab('') + 
  xlab('') + 
  theme_classic() + 
  theme(panel.background = element_rect(colour = 'grey70'), 
        aspect.ratio = 1, 
        axis.text = element_text(size = 16, colour = 'grey30'), 
        axis.line = element_blank(), 
        axis.ticks = element_line(colour = 'grey50'), 
        axis.title = element_text(size = 16), 
        plot.title = element_text(size = 16, hjust = 0.5), 
        legend.position = c(0.5, 0.15))

ggsave('fig/Fig1d-forecast-links-zoomin.pdf', height = 2, width = 2)
