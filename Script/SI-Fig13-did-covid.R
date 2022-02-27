
library(foreach)
library(tidyverse)
library(doParallel)
library(sf)
library(lfe)
library(fastDummies)
library(stargazer)


# COVID-unweighted --------------------------------------------------------

indexes <- readRDS('index/indexes_normal_alllinks_paperbased_covid.rds') %>% 
  filter(papers_coll >= 2)

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
  mutate(papers_coll_ratio = papers_coll / papers, 
         papers_coll_ratio_std = as.numeric(scale(papers_coll_ratio)), 
         papers_coll_log = log(papers_coll)) %>%
  mutate(month_date = gsub('.*?[.]', '', as.character(month))) %>% 
  group_by(country, month_date) %>%
  mutate(country_month_date = cur_group_id()) %>% 
  ungroup()

reg <- felm(papers_coll_log ~ post_2020_covid + time | 
              country_month_date | 0 | country,
            data = indexes,
            na.action = "na.omit")
stargazer(reg,type="text")

impact.covid.unweighted <- bind_cols(data.frame(dependant = as.numeric(reg[['beta']])), 
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




# COVID-weighted ----------------------------------------------------------

indexes <- readRDS('index/indexes_normal_alllinks_covid.rds') %>% 
  filter(count >= 2)

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
  mutate(weightedegree_log = log(weightedegree)) %>%
  mutate(month_date = gsub('.*?[.]', '', as.character(month))) %>% 
  group_by(country, month_date) %>%
  mutate(country_month_date = cur_group_id()) %>% 
  ungroup()

reg <- felm(weightedegree_log ~ post_2020_covid + time | 
              country_month_date | 0 | country,
            data = indexes,
            na.action = "na.omit")
stargazer(reg,type="text")

impact.covid.weighted <- bind_cols(data.frame(dependant = as.numeric(reg[['beta']])), 
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



# Others - unweighted -----------------------------------------------------

indexes <- readRDS('index/indexes_normal_alllinks_5domains_paperbased.rds') %>% 
  filter(papers_coll >= 2)

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
  mutate(papers_coll_ratio = papers_coll / papers, 
         papers_coll_ratio_std = as.numeric(scale(papers_coll_ratio)), 
         papers_coll_log = log(papers_coll)) %>%
  mutate(month_date = gsub('.*?[.]', '', as.character(month))) %>% 
  mutate(majordomain = ifelse(domain %in% domains[1:2], 'Social Science', 'Natural Science')) %>%
  group_by(country, month_date) %>%
  mutate(country_month_date = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(domain, month_date) %>%
  mutate(domain_month_date = cur_group_id()) %>%
  ungroup()

indexes <- indexes %>%
  filter(month >= '2020.500' | month <= '2020.083')

reg <- felm(papers_coll_log ~ post_2020_covid + time | 
              country_month_date + domain_month_date | 0 | country,
            data = indexes,
            na.action = "na.omit")
stargazer(reg,type="text")

impact.others.unweighted <- bind_cols(data.frame(dependant = as.numeric(reg[['beta']])), 
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



# Others-weighted ----------------------------------------------------------

indexes <- readRDS('index/indexes_normal_alllinks_5domains.rds') %>% 
  filter(count >= 2)

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
  mutate(weightedegree_log = log(weightedegree)) %>%
  mutate(month_date = gsub('.*?[.]', '', as.character(month))) %>% 
  mutate(majordomain = ifelse(domain %in% domains[1:2], 'Social Science', 'Natural Science')) %>%
  group_by(country, month_date) %>%
  mutate(country_month_date = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(domain, month_date) %>%
  mutate(domain_month_date = cur_group_id()) %>%
  ungroup()

indexes <- indexes %>%
  filter(month >= '2020.500' | month <= '2020.083')

reg <- felm(weightedegree_log ~ post_2020_covid + time | 
              country_month_date + domain_month_date | 0 | country,
            data = indexes,
            na.action = "na.omit")
stargazer(reg,type="text")

impact.others.weighted <- bind_cols(data.frame(dependant = as.numeric(reg[['beta']])), 
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


impact <- bind_rows(impact.others.unweighted %>% mutate(weighted = 'Unweighted', group = 'Other subjects'), 
                    impact.others.weighted %>% mutate(weighted = 'Weighted', group = 'Other subjects'), 
                    impact.covid.unweighted %>% mutate(weighted = 'Unweighted', group = 'Epidemiological research'), 
                    impact.covid.weighted %>% mutate(weighted = 'Weighted', group = 'Epidemiological research')) %>% 
  mutate(group = factor(group, levels = c('Other subjects', 'Epidemiological research'))) %>% 
  mutate(dependant = dependant * 100, 
         cse = cse * 100, 
         ci_low = ci_low * 100, 
         ci_high = ci_high * 100)


ggplot(impact, aes(group, dependant)) + 
  geom_bar(aes(fill = group, group = weighted, alpha = weighted), 
           stat = 'identity', position = 'dodge2', width = 0.9) + 
  geom_linerange(aes(ymin = dependant - cse, ymax = dependant + cse, col = group, group = weighted), 
                 lwd = 1.2, stat = 'identity', position = position_dodge(width = 0.9)) + 
  geom_linerange(aes(ymin = ci_low, ymax = ci_high, col = group, group = weighted), 
                 lwd = 0.4, stat = 'identity', position = position_dodge(width = 0.9)) +
  geom_point(aes(col = group, group = weighted), fill = 'white', size = 1.6, pch = 21, 
             stat = 'identity', position = position_dodge(width = 0.9)) + 
  geom_hline(yintercept = 0, col = 'grey') + 
  scale_alpha_manual(values = c(1, 0.5)) + 
  scale_fill_manual(values = c("#E64B35", "#357EBD")) + 
  scale_color_manual(values = c("#B13625", "#3C5488")) + 
  xlab('') + 
  ylab('Effect size on collaborations') + 
  theme_classic() + 
  theme(aspect.ratio = 0.7,  
        text = element_text(size = 15), 
        axis.line = element_line(size = 0.2), 
        axis.ticks = element_line(size = 0.2), 
        axis.title.x = element_text(margin = margin(t = 5)), 
        axis.title.y.right = element_text(margin = margin(l = 5)), 
        legend.position = c(0.25, 0.8), 
        legend.background = element_blank()) + 
  guides(fill = guide_legend(''), 
         alpha = guide_legend(''), 
         col = guide_none())


ggsave('fig/SI-Figure13-did-covid.pdf', width = 6, height = 4)





