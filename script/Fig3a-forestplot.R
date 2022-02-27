
library(foreach)
library(tidyverse)
library(doParallel)
library(sf)
library(lfe)
library(fastDummies)
library(stargazer)
library(forestplot)

domains <- c("Arts & Humanities", 
             "Social Sciences", 
             "Life Sciences & Biomedicine", 
             "Physical Sciences", 
             "Technology")


# linkage type ---------------------------------------------------------

impact.linkage <- bind_rows(foreach(dir = c('NN', 'SS', 'NS')) %do% {
  
  indexes <- readRDS(paste0('index/indexes_normal_alllinks_5domains_', dir, '.rds')) %>% 
    filter(weightedegree >= 2)
  
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
    mutate(direct = dir)
})

impact.linkage




# team size -----------------------------------------------------------

impact.team <- bind_rows(foreach(dir = c('2-5', '6-10', 'gt10')) %do% {
  
  indexes <- readRDS(paste0('index/indexes_normal_alllinks_5domains_team_', dir, '.rds')) %>% 
    filter(weightedegree >= 2)
  
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
    mutate(direct = dir)
})

impact.team




# social experiment ---------------------------------------------------------

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




# main regression - discipline & economic status -----------------------

indexes <- readRDS('index/indexes_normal_alllinks_5domains.rds') %>% 
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




# combine together ----------------------------------------------------

impact <- impact.main %>% 
  mutate(domain = 'Overall effect', 
         count = sum(indexes$weightedegree)) %>% 
  bind_rows(impact.majordomains %>% 
              mutate(count = unlist(lapply(1:nrow(.), function(i){
                sum(indexes[indexes$majordomain == majordomain[i], ]$weightedegree)
              }))) %>% 
              rename(domain = majordomain)) %>% 
  bind_rows(impact.domains %>% 
              mutate(count = unlist(lapply(1:nrow(.), function(i){
                sum(indexes[indexes$domain == domain[i], ]$weightedegree)
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
                sum(indexes[indexes$Direction == direction[i], ]$weightedegree)
              }))) %>% 
              rename(domain = direction) %>% 
              mutate(domain = c('Developed country', 'Developing country'))) %>% 
  bind_rows(impact.linkage %>% 
              mutate(count = unlist(lapply(c('NN', 'SS', 'NS'), function(dir){
                sum(readRDS(paste0('index/indexes_normal_alllinks_5domains_', dir, '.rds')) %>% 
                      left_join(countryregion %>% 
                                  dplyr::select(2,7:8) %>% 
                                  filter(!duplicated(country_shp)), by = c('country' = 'country_shp')) %>% 
                      filter(!is.na(Region) & !is.na(Direction)) %>% 
                      filter((month > '2017.667') & (month < 2021.000)) %>% 
                      filter(month >= '2020.500' | month <= '2020.083') %>% 
                      filter(weightedegree >= 2) %>% 
                      select(weightedegree), na.rm = T)
              }))) %>% 
              rename(domain = direct) %>% 
              mutate(domain = c('Between developed country', 'Between developing country', 
                                'Between developed &\ndeveloping country'))) %>% 
  bind_rows(impact.team %>% 
              mutate(count = unlist(lapply(c('2-5', '6-10', 'gt10'), function(dir){
                sum(readRDS(paste0('index/indexes_normal_alllinks_5domains_team_', dir, '.rds')) %>% 
                      left_join(countryregion %>% 
                                  dplyr::select(2,7:8) %>% 
                                  filter(!duplicated(country_shp)), by = c('country' = 'country_shp')) %>% 
                      filter(!is.na(Region) & !is.na(Direction)) %>% 
                      filter((month > '2017.667') & (month < 2021.000)) %>% 
                      filter(month >= '2020.500' | month <= '2020.083') %>% 
                      filter(weightedegree >= 2) %>% 
                      select(weightedegree), na.rm = T)
              }))) %>% 
              rename(domain = direct) %>% 
              mutate(domain = c('2-5 countries', '6-10 countries', 
                                '>10 countries'))) %>% 
  mutate(category = c('Overall effect', 'Social Science', 'Natural Science', rep('Social Science', 2),
                      rep('Natural Science', 3), rep('Social Experiment', 2), rep('Economic status', 2), 
                      rep('Linkage type', 3), rep('Team size', 3))) %>%
  mutate(cse = as.numeric(sprintf('%.04f', cse))) %>% 
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



# forestplot --------------------------------------------------------------

impact <- impact %>% 
  mutate(category = c('Overall effect', rep('Discipline', 7), rep('Social Experiment', 2), 
                      rep('Economic status', 2), rep('Linkage type', 3), rep('Team size', 3))) %>% 
  mutate(domain = as.character(domain),
         domain = ifelse(domain == 'Social Sciences', 'Other Social Science', domain), 
         domain = ifelse(category == 'Discipline' & !(domain %in% c('Social Science', 'Natural Science')), 
                         paste0('      ', domain), domain)) %>%
  mutate(domain = factor(domain, 
                         levels = c('Overall effect', 'Social Science', 
                                    '      Arts & Humanities', '      Other Social Science',
                                    'Natural Science', '      Physical Sciences', 
                                    '      Technology', '      Life Sciences & Biomedicine',
                                    'In Social Science', 'In Natural Science', 
                                    'Developed country', 'Developing country',
                                    'Between developed country', 'Between developing country',
                                    'Between developed &\ndeveloping country', 
                                    '2-5 countries', '6-10 countries', '>10 countries'))) %>%
  arrange(domain)


mean = as.matrix(rbind(c(NA, NA), cbind(impact$dependant)))
lower = as.matrix(rbind(c(NA, NA), cbind(impact$ci_low)))
upper = as.matrix(rbind(c(NA, NA), cbind(impact$ci_high)))


text <- impact[c('category', 'domain', 'count', 'ci95')] %>% 
  mutate(domain = as.character(domain))
text <- rbind(colnames(text), text)
text[c(3:5, 7:9, 11, 13:14, 16:17, 19), 1] <- NA
text[1, ] <- c('Category', 'Group', 
               'Links count', 
               'Effect size')


forestplot(labeltext = as.matrix(text),
           mean = as.numeric(mean), 
           lower = as.numeric(lower), 
           upper = as.numeric(upper), 
           txt_gp = fpTxtGp(label = gpar(cex = 0.8), 
                            ticks = gpar(cex = 0.8), 
                            xlab = gpar(cex = 0.8), 
                            legend = gpar(cex = 0.7)),  
           xlab = "Effect size of the pandemic on international collaboration links",
           zero = 0, 
           lwd.zero = 0.5,
           align = c('c', 'l', rep('r', 2)), 
           boxsize = c(NA, 
                       rep(c(0.23), 14), 
                       rep(c(0.26), 14)),
           colgap = unit(8, 'mm'),
           # xticks = c(-0.5 , -0.4, -0.3, -0.2, -0.1, 0),
           xticks = c(-50 , -40, -30, -20, -10, 0, 10),
           # xticks = c(0, 0.02, 0.04, 0.06, 0.08, 0.1),
           graph.pos = 3,
           graphwidth = unit(55, 'mm'), 
           is.summary = c(T, rep(F, nrow(text)-1)), 
           fn.ci_norm = c(c('fpDrawNormalCI')), 
           shapes_gp = fpShapesGp(
             default = gpar(lineend = "square", linejoin = "mitre"), 
             vertices = gpar(lwd = 5, col = "red"), 
             box = list(
               gpar(fill = NA),
               gpar(fill = "#DD5221", col = NA),
               gpar(fill = "#0FADC7", col = NA),
               gpar(fill = "#0FADC7", col = NA),
               gpar(fill = "#0FADC7", col = NA),
               gpar(fill = "#357EBD", col = NA),
               gpar(fill = "#357EBD", col = NA),
               gpar(fill = "#357EBD", col = NA),
               gpar(fill = "#357EBD", col = NA),
               gpar(fill = "#9759CB", col = NA),
               gpar(fill = "#9759CB", col = NA),
               gpar(fill = "#4D8D74", col = NA),
               gpar(fill = "#4D8D74", col = NA),
               gpar(fill = "#89B9A3", col = NA),
               gpar(fill = "#89B9A3", col = NA),
               gpar(fill = "#89B9A3", col = NA),
               gpar(fill = "#E18726", col = NA),
               gpar(fill = "#E18726", col = NA),
               gpar(fill = "#E18726", col = NA)
             ), 
             line = list(
               gpar(col = NA),
               gpar(col = "#DD5221", lineend = 'round', lwd = 1.1),
               gpar(col = "#0FADC7", lineend = 'round', lwd = 1.1),
               gpar(col = "#0FADC7", lineend = 'round', lwd = 1.1),
               gpar(col = "#0FADC7", lineend = 'round', lwd = 1.1),
               gpar(col = "#357EBD", lineend = 'round', lwd = 1.1),
               gpar(col = "#357EBD", lineend = 'round', lwd = 1.1),
               gpar(col = "#357EBD", lineend = 'round', lwd = 1.1),
               gpar(col = "#357EBD", lineend = 'round', lwd = 1.1),
               gpar(col = "#9759CB", lineend = 'round', lwd = 1.1),
               gpar(col = "#9759CB", lineend = 'round', lwd = 1.1),
               gpar(col = "#4D8D74", lineend = 'round', lwd = 1.1),
               gpar(col = "#4D8D74", lineend = 'round', lwd = 1.1),
               gpar(col = "#89B9A3", lineend = 'round', lwd = 1.1), 
               gpar(col = "#89B9A3", lineend = 'round', lwd = 1.1), 
               gpar(col = "#89B9A3", lineend = 'round', lwd = 1.1),
               gpar(col = "#E18726", lineend = 'round', lwd = 1.1), 
               gpar(col = "#E18726", lineend = 'round', lwd = 1.1), 
               gpar(col = "#E18726", lineend = 'round', lwd = 1.1)
             )), 
           hrzl_lines = 
             list("2" = gpar(lty=1, lwd = 0.8, columns=c(1:5), col = "black"),
                  "3" = gpar(lty=1, lwd = 0.5, columns=c(1:5), col = "grey50"),
                  "6" = gpar(lty=1, lwd = 0.5, columns=c(2:5), col = "grey50"),
                  "10" = gpar(lty=1, lwd = 0.5, columns=c(1:5), col = "grey50"),
                  "12" = gpar(lty=1, lwd = 0.5, columns=c(1:5), col = "grey50"), 
                  "14" = gpar(lty=1, lwd = 0.5, columns=c(1:5), col = "grey50"), 
                  "17" = gpar(lty=1, lwd = 0.5, columns=c(1:5), col = "grey50")))



