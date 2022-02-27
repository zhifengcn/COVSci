
library(foreach)
library(doParallel)
library(tidyverse)
library(sf)
library(lfe)
library(fastDummies)
library(stargazer)

domains <- c("Arts & Humanities", 
             "Social Sciences", 
             "Life Sciences & Biomedicine", 
             "Physical Sciences", 
             "Technology")


# -------------------------------------------------------------------------
# Unweighted
# -------------------------------------------------------------------------

indexes <- readRDS('index/indexes_normal_alllinks_5domains_paperbased_social.rds') %>% 
  filter(papers_coll >= 2) %>% 
  rename(majordomain = domain)

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
         papers_coll_log = log(papers_coll)) %>%
  mutate(month_date = gsub('.*?[.]', '', as.character(month))) %>% 
  group_by(country, month_date) %>%
  mutate(country_month_date = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(majordomain, month_date) %>%
  mutate(majordomain_month_date = cur_group_id()) %>%
  ungroup()

indexes <- indexes %>%
  filter(month >= '2020.500' | month <= '2020.083')

impact.social <- bind_rows(foreach(i = 1:length(unique(indexes$majordomain))) %do% {
  
  reg <- felm(papers_coll_log ~ post_2020_covid + time | 
                country_month_date | 0 | country,
              data = subset(indexes, majordomain == unique(majordomain)[i]), 
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
    dplyr::select(1:2,5:6,3:4) %>% 
    mutate(majordomain = unique(indexes$majordomain)[i])
})

impact.social


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
  ungroup() %>% 
  group_by(majordomain, month_date) %>%
  mutate(majordomain_month_date = cur_group_id()) %>%
  ungroup()

indexes <- indexes %>%
  filter(month >= '2020.500' | month <= '2020.083')

indexes.majordomain <- indexes %>% 
  group_by(country, month, post_2020_covid, time, majordomain) %>% 
  summarise(papers_coll = sum(papers_coll)) %>% 
  ungroup() %>% 
  mutate(papers_coll_log = log(papers_coll)) %>%
  mutate(month_date = gsub('.*?[.]', '', as.character(month))) %>% 
  group_by(country, month_date) %>%
  mutate(country_month_date = cur_group_id()) %>% 
  ungroup()


# main regression ---------------------------------------------------------

reg <- felm(papers_coll_log ~ post_2020_covid + time | 
              country_month_date + domain_month_date | 0 | country,
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
         dependant = as.numeric(sprintf('%.04f', dependant)), 
         ci_high = as.numeric(sprintf('%.04f', ci_high)), 
         ci_low = as.numeric(sprintf('%.04f', ci_low))) %>% 
  slice(1) %>% 
  select(1:2,5:6,3:4)


# regression by domain ----------------------------------------------------

impact.domains <- bind_rows(foreach(i = 1:length(domains)) %do% {
  
  reg <- felm(papers_coll_log ~ post_2020_covid + time | 
                country_month_date | 0 | country,
              data = subset(indexes, domain == domains[i]),
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
    dplyr::select(1:2,5:6,3:4) %>% 
    mutate(domain = domains[i])
})
impact.domains


# regression by majordomain ---------------------------------------------


impact.majordomains <- bind_rows(foreach(i = 1:length(unique(indexes$majordomain))) %do% {
  
  reg <- felm(papers_coll_log ~ post_2020_covid + time | 
                country_month_date | 0 | country,
              data = subset(indexes.majordomain, majordomain == unique(majordomain)[i]), 
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
    dplyr::select(1:2,5:6,3:4) %>% 
    mutate(majordomain = unique(indexes.majordomain$majordomain)[i])
})
impact.majordomains


# regression by economic status ------------------------------------------

impact.direction <- bind_rows(foreach(i = 1:length(unique(indexes$Direction))) %do% {
  reg <- felm(papers_coll_log ~ post_2020_covid + time| 
                country_month_date + domain_month_date | 0 | country,
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



impact.unweighted <- impact.main %>% 
  mutate(domain = 'Overall effect', 
         count = sum(indexes$papers_coll)) %>% 
  bind_rows(impact.majordomains %>% 
              mutate(count = unlist(lapply(1:nrow(.), function(i){
                sum(indexes[indexes$majordomain == majordomain[i], ]$papers_coll)
              }))) %>% 
              rename(domain = majordomain)) %>% 
  bind_rows(impact.domains %>% 
              mutate(count = unlist(lapply(1:nrow(.), function(i){
                sum(indexes[indexes$domain == domain[i], ]$papers_coll)
              })))) %>%
  bind_rows(impact.social %>% 
              mutate(count = unlist(lapply(impact.social$majordomain, function(dir){
                sum(readRDS('index/indexes_normal_alllinks_5domains_paperbased_social.rds') %>% 
                      left_join(countryregion %>% 
                                  dplyr::select(2,7:8) %>% 
                                  filter(!duplicated(country_shp)), by = c('country' = 'country_shp')) %>% 
                      filter(!is.na(Region) & !is.na(Direction)) %>% 
                      filter((month > '2017.667') & (month < 2021.000)) %>% 
                      filter(month >= '2020.500' | month <= '2020.083') %>% 
                      filter(domain == dir) %>% 
                      filter(papers_coll >= 2) %>% 
                      select(papers_coll), na.rm = T)
              }))) %>% 
              rename(domain = majordomain) %>% 
              mutate(domain = paste0('In ', domain))) %>% 
  bind_rows(impact.direction %>% 
              mutate(count = unlist(lapply(1:nrow(.), function(i){
                sum(indexes[indexes$Direction == direction[i], ]$papers_coll)
              }))) %>% 
              rename(domain = direction) %>% 
              mutate(domain = c('Developed country', 'Developing country'))) %>% 
  mutate(category = c('Overall effect', 'Social Science', 'Natural Science', rep('Social Science', 2),
                      rep('Natural Science', 3), rep('Social experiment', 2), rep('Economic status', 2))) %>%
  mutate(cse = as.numeric(sprintf('%.04f', cse))) %>% 
  mutate(ci95 = paste0('[', sprintf('%.04f', ci_low), ',', sprintf('%.04f', ci_high), ']')) %>% 
  mutate(dependant = sprintf('%.01f', dependant * 100), 
         ci_low = sprintf('%.01f', ci_low * 100), 
         ci_high = sprintf('%.01f', ci_high * 100), 
         dependant = ifelse(nchar(dependant) == 5, dependant, 
                            ifelse(nchar(dependant) == 4, paste0('  ', dependant), 
                                   paste0('    ', dependant))), 
         ci_low = ifelse(nchar(ci_low) == 5, ci_low, 
                         ifelse(nchar(ci_low) == 4, paste0('  ', ci_low), 
                                paste0('    ', ci_low))), 
         ci_high = ifelse(nchar(ci_high) == 5, ci_high, 
                          ifelse(nchar(ci_high) == 4, paste0('  ', ci_high), 
                                 paste0('    ', ci_high))), 
         ci95 = paste0(dependant, ' [', ci_low, ',', ci_high, ']')) %>% 
  mutate(count = format(count, big.mark = ",", scientific = FALSE))



# -------------------------------------------------------------------------
# Weighted
# -------------------------------------------------------------------------

indexes <- readRDS('index/indexes_normal_alllinks_5domains_social.rds') %>% 
  filter(weightedegree >= 2)

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
  ungroup() %>% 
  group_by(majordomain, month_date) %>%
  mutate(majordomain_month_date = cur_group_id()) %>%
  ungroup()

indexes <- indexes %>%
  filter(month >= '2020.500' | month <= '2020.083')


indexes.majordomain <- indexes %>% 
  group_by(country, month, post_2020_covid, time, majordomain) %>% 
  summarise(weightedegree = sum(weightedegree)) %>% 
  ungroup() %>% 
  mutate(weightedegree_log = log(weightedegree)) %>%
  mutate(month_date = gsub('.*?[.]', '', as.character(month))) %>% 
  group_by(country, month_date) %>%
  mutate(country_month_date = cur_group_id()) %>% 
  ungroup()

impact.social <- bind_rows(foreach(i = 1:length(unique(indexes.majordomain$majordomain))) %do% {
  
  reg <- felm(weightedegree_log ~ post_2020_covid + time | 
                country_month_date | 0 | country,
              data = subset(indexes.majordomain, majordomain == unique(majordomain)[i]), 
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
    dplyr::select(1:2,5:6,3:4) %>% 
    mutate(majordomain = unique(indexes.majordomain$majordomain)[i])
})

impact.social


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
  mutate(post_covid = ifelse(month >= 2020.167, 1, 0), 
         post_2020 = ifelse(month >= 2020, 1, 0)) %>% 
  mutate(post_2020_covid = post_covid * post_2020) %>% 
  mutate(time = as.integer(factor(month, levels = unique(month)))) %>% 
  mutate(weightedegree_log = log(weightedegree), 
         weightedegree_std = as.numeric(scale(weightedegree)), 
         entropy_std = as.numeric(scale(entropy)), 
         equitability_std = as.numeric(scale(equitability))) %>% 
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

indexes.majordomain <- indexes %>% 
  group_by(country, month, post_2020_covid, time, majordomain) %>% 
  summarise(weightedegree = sum(weightedegree)) %>% 
  ungroup() %>% 
  mutate(weightedegree_log = log(weightedegree)) %>%
  mutate(month_date = gsub('.*?[.]', '', as.character(month))) %>% 
  group_by(country, month_date) %>%
  mutate(country_month_date = cur_group_id()) %>% 
  ungroup()


# main regression ---------------------------------------------------------

reg <- felm(weightedegree_log ~ post_2020_covid + time | 
              country_month_date + domain_month_date | 0 | country,
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
         dependant = as.numeric(sprintf('%.04f', dependant)), 
         ci_high = as.numeric(sprintf('%.04f', ci_high)), 
         ci_low = as.numeric(sprintf('%.04f', ci_low))) %>% 
  slice(1) %>% 
  select(1:2,5:6,3:4)


# regression by domain ----------------------------------------------------

impact.domains <- bind_rows(foreach(i = 1:length(domains)) %do% {
  
  reg <- felm(weightedegree_log ~ post_2020_covid + time | 
                country_month_date | 0 | country,
              data = subset(indexes, domain == domains[i]),
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
    dplyr::select(1:2,5:6,3:4) %>% 
    mutate(domain = domains[i])
})
impact.domains


# regression by major domain ----------------------------------------------------


impact.majordomains <- bind_rows(foreach(i = 1:length(unique(indexes$majordomain))) %do% {
  
  reg <- felm(weightedegree_log ~ post_2020_covid + time | 
                country_month_date | 0 | country,
              data = subset(indexes.majordomain, majordomain == unique(majordomain)[i]), 
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
    dplyr::select(1:2,5:6,3:4) %>% 
    mutate(majordomain = unique(indexes.majordomain$majordomain)[i])
})
impact.majordomains


# regression by direction -------------------------------------------------

impact.direction <- bind_rows(foreach(i = 1:length(unique(indexes$Direction))) %do% {
  reg <- felm(weightedegree_log ~ post_2020_covid + time| 
                country_month_date + domain_month_date | 0 | country,
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



impact.weighted <- impact.main %>% 
  mutate(domain = 'Overall effect', 
         count = sum(indexes$count)) %>% 
  bind_rows(impact.majordomains %>% 
              mutate(count = unlist(lapply(1:nrow(.), function(i){
                sum(indexes[indexes$majordomain == majordomain[i], ]$count)
              }))) %>% 
              rename(domain = majordomain)) %>% 
  bind_rows(impact.domains %>% 
              mutate(count = unlist(lapply(1:nrow(.), function(i){
                sum(indexes[indexes$domain == domain[i], ]$count)
              })))) %>%
  bind_rows(impact.social %>% 
              mutate(count = unlist(lapply(impact.social$majordomain, function(dir){
                sum(readRDS('index/indexes_normal_alllinks_5domains_social.rds') %>% 
                      left_join(countryregion %>% 
                                  dplyr::select(2,7:8) %>% 
                                  filter(!duplicated(country_shp)), by = c('country' = 'country_shp')) %>% 
                      filter(!is.na(Region) & !is.na(Direction)) %>% 
                      filter((month > '2017.667') & (month < 2021.000)) %>% 
                      filter(month >= '2020.500' | month <= '2020.083') %>% 
                      mutate(majordomain = ifelse(domain %in% domains[1:2], 
                                                  'Social Science', 'Natural Science')) %>% 
                      filter(majordomain == dir) %>% 
                      filter(weightedegree >= 2) %>% 
                      select(weightedegree), na.rm = T)
              }))) %>% 
              rename(domain = majordomain) %>% 
              mutate(domain = paste0('In ', domain))) %>% 
  bind_rows(impact.direction %>% 
              mutate(count = unlist(lapply(1:nrow(.), function(i){
                sum(indexes[indexes$Direction == direction[i], ]$count)
              }))) %>% 
              rename(domain = direction) %>% 
              mutate(domain = c('Developed country', 'Developing country'))) %>% 
  mutate(category = c('Overall effect', 'Social Science', 'Natural Science', rep('Social Science', 2),
                      rep('Natural Science', 3), rep('Social Experiment', 2), rep('Economic status', 2))) %>%
  mutate(cse = as.numeric(sprintf('%.04f', cse))) %>% 
  mutate(ci95 = paste0('[', sprintf('%.04f', ci_low), ',', sprintf('%.04f', ci_high), ']')) %>% 
  mutate(dependant = sprintf('%.01f', dependant * 100), 
         ci_low = sprintf('%.01f', ci_low * 100), 
         ci_high = sprintf('%.01f', ci_high * 100), 
         dependant = ifelse(nchar(dependant) == 5, dependant, 
                            ifelse(nchar(dependant) == 4, paste0('  ', dependant), 
                                   paste0('    ', dependant))), 
         ci_low = ifelse(nchar(ci_low) == 5, ci_low, 
                         ifelse(nchar(ci_low) == 4, paste0('  ', ci_low), 
                                paste0('    ', ci_low))), 
         ci_high = ifelse(nchar(ci_high) == 5, ci_high, 
                          ifelse(nchar(ci_high) == 4, paste0('  ', ci_high), 
                                 paste0('    ', ci_high))), 
         ci95 = paste0(dependant, ' [', ci_low, ',', ci_high, ']')) %>% 
  mutate(count = format(count, big.mark = ",", scientific = FALSE))



# -------------------------------------------------------------------------
# Combine
# -------------------------------------------------------------------------


impact.weighted <- impact.weighted %>% 
  mutate(category = c('Overall effect', rep('Discipline', 7), rep('Social Experiment', 2), rep('Economic status', 2))) %>% 
  mutate(domain = as.character(domain),
         domain = ifelse(domain == 'Social Sciences', 'Other Social Science', domain), 
         domain = ifelse(category == 'Discipline' & !(domain %in% c('Social Science', 'Natural Science')), paste0('      ', domain), domain)) %>%
  mutate(domain = factor(domain, levels = c('Overall effect', 'Social Science', '      Arts & Humanities', '      Other Social Science',
                                            'Natural Science', '      Physical Sciences', '      Technology', '      Life Sciences & Biomedicine', 'In Social Science', 'In Natural Science', 
                                            'Developed country', 'Developing country'))) %>%
  arrange(domain)

impact.weighted[c(2:4, 6:8, 10, 12), ]$category <- NA

impact.unweighted <- impact.unweighted %>% 
  mutate(category = c('Overall effect', rep('Discipline', 7), rep('Social Experiment', 2), rep('Economic status', 2))) %>% 
  mutate(domain = as.character(domain),
         domain = ifelse(domain == 'Social Sciences', 'Other Social Science', domain), 
         domain = ifelse(category == 'Discipline' & !(domain %in% c('Social Science', 'Natural Science')), paste0('      ', domain), domain)) %>%
  mutate(domain = factor(domain, levels = c('Overall effect', 'Social Science', '      Arts & Humanities', '      Other Social Science',
                                            'Natural Science', '      Physical Sciences', '      Technology', '      Life Sciences & Biomedicine', 'In Social Science', 'In Natural Science', 
                                            'Developed country', 'Developing country'))) %>%
  arrange(domain)


mean = as.matrix(rbind(c(NA, NA), cbind(as.numeric(impact.weighted$dependant), as.numeric(impact.unweighted$dependant))))
lower = as.matrix(rbind(c(NA, NA), cbind(as.numeric(impact.weighted$ci_low), as.numeric(impact.unweighted$ci_low))))
upper = as.matrix(rbind(c(NA, NA), cbind(as.numeric(impact.weighted$ci_high), as.numeric(impact.unweighted$ci_high))))


text <- cbind(impact.weighted[c('category', 'domain')], 
              impact.weighted[c('count')], 
              impact.unweighted[c('count')] %>% `colnames<-`('count2'), 
              impact.weighted[c('ci95')], 
              impact.unweighted[c('ci95')] %>% `colnames<-`('ci95_2')) %>% 
  mutate(domain = as.character(domain))
text <- rbind(colnames(text), text)
text[1, ] <- c('Category', 'Group', 'Links count', 'Papers count', 
               'Effect size in links\ncount [95% CI]', 
               'Effect size in papers\ncount [95% CI]')


forestplot(labeltext = as.matrix(text),
           mean = mean, 
           lower = lower, 
           upper = upper, 
           txt_gp = fpTxtGp(label = gpar(cex = 0.8), 
                            ticks = gpar(cex = 0.8), 
                            xlab = gpar(cex = 0.8), 
                            legend = gpar(cex = 0.7)),  
           xlab = "Effect size of the pandemic on the number of\ninternational collaborations",
           zero = 0, 
           lwd.zero = 0.5,
           align = c('c', 'l', rep('r', 4)), 
           boxsize = c(NA, 
                       rep(c(0.23), 28)),
           line.margin = .25,
           colgap = unit(5, 'mm'), 
           graph.pos = 3,
           xticks = c(-50, -40, -30, -20, -10, 0, 10),
           graphwidth = unit(55, 'mm'), 
           is.summary = c(T, rep(F, nrow(text)-1)), 
           fn.ci_norm = c(c('fpDrawCircleCI', 'fpDrawCircleCI')), 
           shapes_gp = fpShapesGp(
             default = gpar(lineend = "square", linejoin = "mitre"), 
             vertices = gpar(lwd = 5, col = "red"), 
             box = list(
               gpar(fill = NA),
               gpar(fill = NA),
               gpar(fill = "#DD5221", col = NA),
               gpar(col = "#DD5221", fill = 'white', lwd = 1),
               gpar(fill = "#0FADC7", col = NA),
               gpar(col = "#0FADC7", fill = 'white', lwd = 1),
               gpar(fill = "#0FADC7", col = NA),
               gpar(col = "#0FADC7", fill = 'white', lwd = 1),
               gpar(fill = "#0FADC7", col = NA),
               gpar(col = "#0FADC7", fill = 'white', lwd = 1),
               gpar(fill = "#357EBD", col = NA),
               gpar(col = "#357EBD", fill = 'white', lwd = 1),
               gpar(fill = "#357EBD", col = NA),
               gpar(col = "#357EBD", fill = 'white', lwd = 1),
               gpar(fill = "#357EBD", col = NA),
               gpar(col = "#357EBD", fill = 'white', lwd = 1),
               gpar(fill = "#357EBD", col = NA),
               gpar(col = "#357EBD", fill = 'white', lwd = 1),
               gpar(fill = "#9759CB", col = NA),
               gpar(col = "#9759CB", fill = 'white', lwd = 1),
               gpar(fill = "#9759CB", col = NA),
               gpar(col = "#9759CB", fill = 'white', lwd = 1),
               gpar(fill = "#4D8D74", col = NA),
               gpar(col = "#4D8D74", fill = 'white', lwd = 1),
               gpar(fill = "#4D8D74", col = NA),
               gpar(col = "#4D8D74", fill = 'white', lwd = 1)
             ), 
             line = list(
               gpar(col = NA),
               gpar(col = NA),
               gpar(col = "#DD5221", lineend = 'round', lwd = 1.1),
               gpar(col = "#DD5221", lineend = 'round', lwd = 1.1),
               gpar(col = "#0FADC7", lineend = 'round', lwd = 1.1),
               gpar(col = "#0FADC7", lineend = 'round', lwd = 1.1),
               gpar(col = "#0FADC7", lineend = 'round', lwd = 1.1),
               gpar(col = "#0FADC7", lineend = 'round', lwd = 1.1),
               gpar(col = "#0FADC7", lineend = 'round', lwd = 1.1),
               gpar(col = "#0FADC7", lineend = 'round', lwd = 1.1),
               gpar(col = "#357EBD", lineend = 'round', lwd = 1.1),
               gpar(col = "#357EBD", lineend = 'round', lwd = 1.1),
               gpar(col = "#357EBD", lineend = 'round', lwd = 1.1),
               gpar(col = "#357EBD", lineend = 'round', lwd = 1.1),
               gpar(col = "#357EBD", lineend = 'round', lwd = 1.1),
               gpar(col = "#357EBD", lineend = 'round', lwd = 1.1),
               gpar(col = "#357EBD", lineend = 'round', lwd = 1.1),
               gpar(col = "#357EBD", lineend = 'round', lwd = 1.1),
               gpar(col = "#9759CB", lineend = 'round', lwd = 1.1),
               gpar(col = "#9759CB", lineend = 'round', lwd = 1.1),
               gpar(col = "#9759CB", lineend = 'round', lwd = 1.1),
               gpar(col = "#9759CB", lineend = 'round', lwd = 1.1),
               gpar(col = "#4D8D74", lineend = 'round', lwd = 1.1), 
               gpar(col = "#4D8D74", lineend = 'round', lwd = 1.1), 
               gpar(col = "#4D8D74", lineend = 'round', lwd = 1.1), 
               gpar(col = "#4D8D74", lineend = 'round', lwd = 1.1)
             )), 
           hrzl_lines = 
             list("2" = gpar(lty=1, lwd = 0.8, columns=c(1:7), col = "black"),
                  "3" = gpar(lty=1, lwd = 0.5, columns=c(1:7), col = "grey50"),
                  "6" = gpar(lty=1, lwd = 0.5, columns=c(2:7), col = "grey50"),
                  "10" = gpar(lty=1, lwd = 0.5, columns=c(1:7), col = "grey50"),
                  "12" = gpar(lty=1, lwd = 0.5, columns=c(1:7), col = "grey50")))
