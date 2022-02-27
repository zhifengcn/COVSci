
library(foreach)
library(tidyverse)
library(doParallel)
library(sf)
library(lfe)
library(fastDummies)
library(stargazer)

domains <- c("Arts & Humanities", 
             "Social Sciences", 
             "Life Sciences & Biomedicine", 
             "Physical Sciences", 
             "Technology")


indexes <- readRDS('index/indexes_normal_alllinks_covid.rds')

indexes <- indexes %>% filter(
  (month > '2017.667') & (month < '2021.000')
)

countryregion <- read_csv('data/00_origin/countryRegion.csv') %>% 
  mutate(Region = ifelse(Region == 'Arab States', 'Africa', Region))

indexes <- indexes %>% 
  left_join(countryregion %>% 
              dplyr::select(2,7:8) %>% 
              filter(!duplicated(country_shp)), by = c('country' = 'country_shp')) %>% 
  filter(!is.na(Region) & !is.na(Direction))

indexes <- indexes %>% 
  mutate(post_covid = ifelse(month >= 2020.167, 1, 0), 
         post_2020 = ifelse(month >= 2020, 1, 0)) %>% 
  mutate(post_2020_covid = post_covid * post_2020) %>% 
  mutate(time = as.integer(factor(month, levels = unique(month)))) %>% 
  mutate(weightedegree_log = log(weightedegree), 
         degree_log = log(degree), 
         entropy_log = log(entropy)) %>%
  filter(entropy != 0) %>% 
  mutate(month_date = gsub('.*?[.]', '', as.character(month))) %>% 
  group_by(country, month_date) %>%
  mutate(country_month_date = cur_group_id()) %>% 
  ungroup()

reg <- felm(entropy_log ~ post_2020_covid + time | 
              country_month_date | 0 | country,
            data = indexes,
            na.action = "na.omit")
stargazer(reg,type="text")

impact.main <- bind_cols(data.frame(dependant = as.numeric(reg[['beta']])), 
                                   data.frame(cse = reg[['cse']]), 
                                   data.frame(cpval = reg[['cpval']])) %>% 
  mutate(significance = '', 
         significance = ifelse(cpval < 0.1, '*', significance),
         significance = ifelse(cpval < 0.05, '**', significance),
         significance = ifelse(cpval < 0.01, '***', significance),
         ci_high = dependant + 1.96 * cse, 
         ci_low = dependant - 1.96 * cse, 
         dependant = as.numeric(sprintf('%.03f', dependant)), 
         ci_high = as.numeric(sprintf('%.03f', ci_high)), 
         ci_low = as.numeric(sprintf('%.03f', ci_low))) %>% 
  slice(1) %>% 
  select(1:2,5:6,3:4)


impact.direction <- bind_rows(foreach(i = 1:length(unique(indexes$Direction))) %do% {
  reg <- felm(entropy_log ~ post_2020_covid + time| 
                country_month_date | 0 | country,
              data = subset(indexes, Direction == unique(Direction)[i]),
              na.action = "na.omit")
  
  bind_cols(data.frame(dependant = as.numeric(reg[['beta']])), 
            data.frame(cse = reg[['cse']]), 
            data.frame(cpval = reg[['cpval']])) %>% 
    mutate(significance = '', 
           significance = ifelse(cpval < 0.1, '*', significance),
           significance = ifelse(cpval < 0.05, '**', significance),
           significance = ifelse(cpval < 0.01, '***', significance),
           ci_high = dependant + 1.96 * cse, 
           ci_low = dependant - 1.96 * cse, 
           dependant = as.numeric(sprintf('%.04f', dependant)), 
           ci_high = as.numeric(sprintf('%.04f', ci_high)), 
           ci_low = as.numeric(sprintf('%.04f', ci_low))) %>% 
    slice(1) %>% 
    select(1:2,5:6,3:4) %>% 
    mutate(direction = unique(indexes$Direction)[i])
  
})
impact.direction





indexes <- readRDS('index/indexes_normal_alllinks_5domains.rds')

indexes <- indexes %>% filter(
  (month > '2017.667') & (month < 2021.000)
)

countryregion <- read_csv('data/00_origin/countryRegion.csv') %>% 
  mutate(Region = ifelse(Region == 'Arab States', 'Africa', Region))

indexes <- indexes %>% 
  left_join(countryregion %>% 
              dplyr::select(2,7:8) %>% 
              filter(!duplicated(country_shp)), by = c('country' = 'country_shp')) %>% 
  filter(!is.na(Region) & !is.na(Direction))

indexes <- indexes %>% 
  mutate(post_covid = ifelse(month >= 2020.167, 1, 0), 
         post_2020 = ifelse(month >= 2020, 1, 0)) %>% 
  mutate(post_2020_covid = post_covid * post_2020) %>% 
  mutate(time = as.integer(factor(month, levels = unique(month)))) %>% 
  mutate(weightedegree_log = log(weightedegree), 
         degree_log = log(degree), 
         entropy_log = log(entropy)) %>%
  filter(entropy != 0) %>% 
  mutate(month_date = gsub('.*?[.]', '', as.character(month))) %>% 
  group_by(country, month_date) %>%
  mutate(country_month_date = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(domain, month_date) %>%
  mutate(domain_month_date = cur_group_id()) %>%
  ungroup()

indexes <- indexes %>%
  filter(month >= '2020.500' | month <= '2020.083')

reg <- felm(entropy_log ~ post_2020_covid + time | 
              country_month_date + domain_month_date | 0 | country,
            data = indexes,
            na.action = "na.omit")
stargazer(reg,type="text")

impact.5d.main <- bind_cols(data.frame(dependant = as.numeric(reg[['beta']])), 
                         data.frame(cse = reg[['cse']]), 
                         data.frame(cpval = reg[['cpval']])) %>% 
  mutate(significance = '', 
         significance = ifelse(cpval < 0.1, '*', significance),
         significance = ifelse(cpval < 0.05, '**', significance),
         significance = ifelse(cpval < 0.01, '***', significance),
         ci_high = dependant + 1.96 * cse, 
         ci_low = dependant - 1.96 * cse, 
         dependant = as.numeric(sprintf('%.03f', dependant)), 
         ci_high = as.numeric(sprintf('%.03f', ci_high)), 
         ci_low = as.numeric(sprintf('%.03f', ci_low))) %>% 
  slice(1) %>% 
  select(1:2,5:6,3:4)


impact.5d.direction <- bind_rows(foreach(i = 1:length(unique(indexes$Direction))) %do% {
  reg <- felm(entropy_log ~ post_2020_covid + time | 
                country_month_date + domain_month_date | 0 | country,
              data = subset(indexes, Direction == unique(Direction)[i]),
              na.action = "na.omit")
  
  stargazer(reg,type="text")
  
  bind_cols(data.frame(dependant = as.numeric(reg[['beta']])), 
            data.frame(cse = reg[['cse']]), 
            data.frame(cpval = reg[['cpval']])) %>% 
    mutate(significance = '', 
           significance = ifelse(cpval < 0.1, '*', significance),
           significance = ifelse(cpval < 0.05, '**', significance),
           significance = ifelse(cpval < 0.01, '***', significance),
           ci_high = dependant + 1.96 * cse, 
           ci_low = dependant - 1.96 * cse, 
           dependant = as.numeric(sprintf('%.04f', dependant)), 
           ci_high = as.numeric(sprintf('%.04f', ci_high)), 
           ci_low = as.numeric(sprintf('%.04f', ci_low))) %>% 
    slice(1) %>% 
    select(1:2,5:6,3:4) %>% 
    mutate(direction = unique(indexes$Direction)[i])
  
})
impact.5d.direction



impact <- bind_rows(bind_rows(impact.main, impact.direction) %>% mutate(group = 'Epidemiology'), 
                    bind_rows(impact.5d.main, impact.5d.direction) %>% mutate(group = 'Other research')) %>% 
  mutate(direction = ifelse(is.na(direction), 'Overall effect', direction), 
         direction = ifelse(direction == 'Global North', 'Developed country', direction), 
         direction = ifelse(direction == 'Global South', 'Developing country', direction)) %>% 
  mutate(y = paste0(group, direction)) %>% 
  mutate(y = factor(y, rev(c(unique(y)[1:4], unique(y)[6], unique(y)[5])))) %>% 
  mutate(dependant = dependant * 100, 
         cse = cse * 100, 
         ci_low = ci_low * 100, 
         ci_high = ci_high * 100)



ggplot(impact, aes(y, dependant, group = direction, col = group)) + 
  geom_hline(yintercept = 0, lwd = 0.2, col = 'grey') + 
  geom_bar(aes(fill = group), 
           col = NA, width = 0.6, stat = 'identity', alpha = 0.2, show.legend = F) +
  geom_linerange(aes(ymin = dependant - cse, ymax = dependant + cse), 
                 lwd = 0.5, position = position_dodge2(width = 0.4)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), lwd = 0.2, 
                position = position_dodge2(width = 0.4), width = 0.1, show.legend = F) + 
  geom_point(aes(col = group, fill = group), size = 0.8, pch = 21, 
             position = position_dodge2(width = 0.4), show.legend = F) + 
  scale_x_discrete('', labels = rev(c('Overall\neffect', 'Developing\ncountry', 'Developed\ncountry',
                              'Overall\neffect', 'Developing\ncountry', 'Developed\ncountry'))) +
  scale_y_continuous('Effect size on international\ncollaboration entropy') + 
  scale_color_manual(values = c("#357EBD", "#9632B8")) + 
  scale_fill_manual(values = c("#357EBD", "#9632B8")) + 
  coord_flip() + 
  theme_classic() + 
  theme(aspect.ratio = 0.85, 
        axis.line = element_line(size = 0.2), 
        axis.ticks = element_line(size = 0.2), 
        legend.position = c(0.55, 0.25), 
        legend.background = element_blank(), 
        text = element_text(size = 10)) + 
  guides(color = guide_legend(''))


ggsave('fig/Figure4e-entropy-did.pdf', width = 3, height = 2.5)














