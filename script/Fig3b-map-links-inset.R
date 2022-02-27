
library(foreach)
library(doParallel)
library(tidyverse)
library(lfe)
library(fastDummies)
library(stargazer)

domains <- c("Arts & Humanities", 
             "Social Sciences", 
             "Life Sciences & Biomedicine", 
             "Physical Sciences", 
             "Technology")


indexes <- readRDS('index/indexes_normal_alllinks_5domains.rds') %>% 
  filter(count >= 2)

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
  mutate(post_covid = ifelse(month >= '2020.167', 1, 0), 
         post_2020 = ifelse(month >= '2020', 1, 0)) %>% 
  mutate(post_2020_covid = post_covid * post_2020) %>% 
  mutate(time = as.integer(factor(month, levels = unique(month)))) %>% 
  mutate(weightedegree_log = log(weightedegree)) %>%
  mutate(month_date = gsub('.*?[.]', '', as.character(month))) %>% 
  mutate(majordomain = ifelse(domain %in% domains[1:2], 'Social Science', 'Natural Science')) %>%
  group_by(country, month_date) %>%
  mutate(country_month_date = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(domain, month_date) %>%
  mutate(domain_month_date = cur_group_id()) %>%
  ungroup() %>% 
  group_by(majordomain, month_date) %>%
  mutate(majordomain_month_date = cur_group_id()) %>%
  ungroup()

indexes <- indexes %>%
  filter(month >= '2020.500' | month <= '2020.083')

impact.country <- bind_rows(foreach(i = 1:length(unique(indexes$country))) %do% {
  
  reg <- tryCatch(felm(weightedegree_log ~ post_2020_covid + time| 
                         domain_month_date | 0 | month_date,
                       data = subset(indexes, country == unique(indexes$country)[i]),
                       na.action = "na.omit"), 
                  silent = T, 
                  warning = function(e) {
                    error_text <- trimws(paste0("WARNING: ", e))
                  })
  
  if (class(reg) == 'try-error') {
    return(NULL)
  }
  
  if (class(reg) == 'character') {
    if (str_detect(reg, 'WARNING')) {
      return(NULL)
    }
  }
  
  bind_cols(data.frame(dependant = as.numeric(reg[['beta']])), 
            data.frame(cse = ifelse(is_null(reg[['cse']]), NA, reg[['cse']])), 
            data.frame(cpval = ifelse(is_null(reg[['cpval']]), NA, reg[['cpval']]))) %>% 
    mutate(significance = '', 
           significance = ifelse(!is.na(cpval) & cpval < 0.1, '*', significance), 
           significance = ifelse(!is.na(cpval) & cpval < 0.05, '**', significance), 
           significance = ifelse(!is.na(cpval) & cpval < 0.01, '***', significance), 
           ci_high = dependant + 1.96 * cse, 
           ci_low = dependant - 1.96 * cse, 
           dependant = as.numeric(sprintf('%.02f', dependant * 100)),
           ci_high = as.numeric(sprintf('%.02f', ci_high * 100)),
           ci_low = as.numeric(sprintf('%.02f', ci_low * 100))) %>%
    slice(1) %>% 
    select(1:2,5:6,3:4) %>% 
    mutate(country = unique(indexes$country)[i])
}) %>% 
  mutate(dependant = ifelse(dependant > 100, 100, dependant), 
         dependant = ifelse(dependant < -100, -100, dependant))


tb.sort <- impact.country %>% 
  as.data.frame() %>% 
  arrange(dependant)

tb.max <- max(tb.sort$dependant)
tb.min <- min(tb.sort$dependant)

tb.dens <- density(tb.sort$dependant, n = (tb.max-0)/0.001+1, from = tb.min, to = tb.max, bw = 0.1)
tb.dens$y <- tb.dens$y + 0.0001

ggplot(data = cbind.data.frame(x = tb.dens$x, y = tb.dens$y*(((tb.max-tb.min)/0.001+1)/(100/0.001))), 
       aes(x = x, y = y)) +
  geom_segment(aes(xend = x, yend = 0, colour = x), show.legend = F) + 
  geom_line(data = data.frame(x = as.numeric(quantile(tb.sort$dependant, probs = 0.25, na.rm = T)), 
                              y = c(0, 0.028)), 
            aes(x = x, y = y), 
            color = 'grey40', size = 0.12) + 
  geom_line(data = data.frame(x = as.numeric(quantile(tb.sort$dependant, probs = 0.75, na.rm = T)), 
                              y = c(0, 0.028)), 
            aes(x = x, y = y), 
            color = 'grey40', size = 0.12) + 
  geom_line(color = 'grey50', size = 0.3) + 
  geom_boxplot(data = tb.sort, 
               aes(x = dependant, y = -0.003), 
               width = 0.004, 
               size = 0.2, 
               fill = 'grey97', 
               color = 'grey50', 
               outlier.shape = 20,
               outlier.size = 1, 
               show.legend = F) + 
  geom_point(aes(x = median(tb.sort$dependant, na.rm = T), y = 0), color = 'grey', shape = 18) + 
  # geom_text(aes(x = median(tb.sort$dependant, na.rm = T), y = 0.002,  
  #               label = sprintf('%.02f', median(tb.sort$dependant, na.rm = T))), 
  #           fontface = 'plain', 
  #           color = 'grey40', 
  #           inherit.aes = F,
  #           size = 2.5, 
  #           show.legend = F) + 
  # geom_text(aes(x = as.numeric(quantile(tb.sort$dependant, probs = 0.25, na.rm = T)) - 0.15, y = -0.0045,  
  #               label = sprintf('%.02f', quantile(tb.sort$dependant, probs = 0.25, na.rm = T))), 
  #           fontface = 'plain', 
  #           color = 'grey40', 
  #           inherit.aes = F,
  #           size = 2.5, 
  #           show.legend = F) + 
  # geom_text(aes(x = as.numeric(quantile(tb.sort$dependant, probs = 0.75, na.rm = T)) + 0.15, y = -0.0045,  
  #               label = sprintf('%.02f', quantile(tb.sort$dependant, probs = 0.75, na.rm = T))), 
  #           fontface = 'plain', 
  #           color = 'grey40', 
  #           inherit.aes = F,
  #           size = 2.5, 
  #           show.legend = F) + 
  scale_color_gradientn(values = rescale(c(min(impact.country$dependant), 
                                          min(impact.country$dependant)/3, 
                                          0, 
                                          max(impact.country$dependant)/3, 
                                          max(impact.country$dependant))),
                       limits = c(min(impact.country$dependant), max(impact.country$dependant)),
                       breaks = c(min(impact.country$dependant), -0.4, 0, 0.4, max(impact.country$dependant)),
                       labels = paste0(as.integer(c(min(impact.country$dependant), -0.4, 0, 
                                                    0.5, max(impact.country$dependant)) * 100), '%'),
                       colours = colorRampPalette(c("#762a83", "#9970ab", "#c2a5cf", 
                                                    "#e7d4e8", "#f7f7f7", "#d9f0d3", 
                                                    "#a6dba0", "#5aae61", "#1b7837"))(200)) +
  ylab(NULL) + 
  xlab(NULL) + 
  theme_classic() + 
  theme(aspect.ratio = 0.8, 
        axis.line.x = element_line(colour = 'white'), 
        axis.line.y = element_line(colour = 'white'), 
        axis.text.x = element_text(size = 0),  
        axis.text.y = element_text(size = 0), 
        axis.ticks.x = element_line(size = 0, color = 'white'),  
        axis.ticks.y = element_line(size = 0, color = 'white'), 
        panel.background = element_rect(color = 'transparent'), 
        plot.background = element_rect(color = 'transparent'))

ggsave('fig/Fig3b-map-links-inset.pdf', height = 2.5, width = 3)

