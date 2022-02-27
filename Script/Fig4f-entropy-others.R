
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


# Number of collaborations ------------------------------------------------

indexes <- readRDS('index/indexes_normal_alllinks_5domains.rds')

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
  group_by(country, month) %>%
  mutate(country_date = cur_group_id()) %>%
  ungroup() %>%
  group_by(domain, month) %>%
  mutate(domain_date = cur_group_id()) %>%
  ungroup() %>%
  mutate(time = as.integer(factor(month, levels = unique(month)))) %>%
  mutate(weightedegree_log = log(weightedegree), 
         weightedegree_std = as.numeric(scale(weightedegree)), 
         degree_log = log(degree), 
         betweenness_std = as.numeric(scale(betweenness)), 
         eigendegree_std = as.numeric(scale(eigendegree)), 
         entropy_std = as.numeric(scale(entropy)), 
         entropy_log = log(entropy), 
         equitability_std = as.numeric(scale(equitability))) %>% 
  mutate(month_date = gsub('.*?[.]', '', as.character(month))) %>%
  group_by(domain, month_date) %>%
  mutate(domain_month_date = cur_group_id()) %>%
  ungroup() %>%
  group_by(country, month_date) %>%
  mutate(country_month_date = cur_group_id()) %>%
  ungroup()

indexes <- fastDummies::dummy_cols(indexes, select_columns = "month")

var_nam <- paste0("month_", unique(indexes$month)[25:length(unique(indexes$month))])
var_nam <- var_nam[var_nam != 'month_2020.083']


reg_dta <- bind_rows(foreach(i = c('entropy', 'degree_log')) %do% {
  
  if (i == 'entropy') {
    fmla1 <- as.formula(paste0("entropy_log ~ ", paste(var_nam, collapse= " + "),
                               " + time | country_month_date | 0 | country"))
    reg <- felm(formula(fmla1), 
                data = indexes[indexes$entropy != 0, ],
                na.action = "na.omit")
  } else if (i == 'degree_log') {
    fmla1 <- as.formula(paste0("degree_log ~ ", paste(var_nam, collapse= " + "),
                               " + time | country_month_date | 0 | country"))
    reg <- felm(formula(fmla1), 
                data = indexes,
                na.action = "na.omit")
  }
  
  stargazer(reg,type="text")
  
  data.frame(coef = reg$beta[1:(length(reg$beta))], 
             se = reg$cse[1:(length(reg$beta))], 
             cpval = reg$cpval[1:(length(reg$beta))]) %>% 
    bind_rows(data.frame(coef = 0, se = 0) %>% `rownames<-`('month_2020.083')) %>% 
    mutate(significance = '', 
           significance = ifelse(cpval < 0.1, '*', significance),
           significance = ifelse(cpval < 0.05, '**', significance),
           significance = ifelse(cpval < 0.01, '***', significance),
           ci_low = coef - 1.96 * se, 
           ci_high = coef + 1.96 * se) %>% 
    mutate(month = gsub('month_', '', row.names(.))) %>% 
    mutate(year = gsub('[.].*', '', month)) %>% 
    mutate(time = c(-(sum(month<'2020.083'):1), 1:sum(month>'2020.083'), 0)) %>% 
    arrange(time) %>% 
    mutate(shape = ifelse((ci_low > 0) | (ci_high < 0), 'solid', 'hollow')) %>% 
    filter(year != '2021') %>% 
    mutate(group = i)
}) %>% 
  mutate(group = ifelse(group == 'degree_log', 'Ln(Degree)', group), 
         group = ifelse(group == 'entropy', 'Shannon entropy', group)) %>% 
  mutate(coef = coef * 100, 
         se = se * 100, 
         ci_low = ci_low * 100, 
         ci_high = ci_high * 100) %>% 
  filter(!str_detect(rownames(.), 'time'))



ggplot(reg_dta[reg_dta$group == 'Shannon entropy', ], aes(x = time, y = coef)) + 
  geom_rect(aes(xmin = -4.5, xmax = -1.5, ymin = -Inf, ymax = Inf), fill = 'grey97') +
  geom_rect(aes(xmin = 1.5, xmax = 4.5, ymin = -Inf, ymax = Inf), fill = 'grey97') +
  geom_rect(aes(xmin = 7.5, xmax = 10.5, ymin = -Inf, ymax = Inf), fill = 'grey97') +
  geom_hline(yintercept = 0, color = "grey50", lwd = 0.3, linetype = "dashed") +
  geom_linerange(aes(x = 0.5, ymin = -Inf, ymax = Inf), color = "grey50", lwd = 0.1) +
  geom_linerange(aes(ymin = coef - se, ymax = coef + se, group = group, col = group), 
                 lwd = 0.7, stat = 'identity', position = position_dodge(width = 0.7), show.legend = F) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, group = group, col = group), 
                lwd = 0.2, width = 0.2, stat = 'identity', position = position_dodge(width = 0.7), show.legend = F) +
  geom_point(aes(group = group, col = group, fill = group),  
             size = 1.2, shape = 21, show.legend = F, 
             stat = 'identity', position = position_dodge(width = 0.7)) + 
  scale_color_manual(values = c('black')) + 
  scale_fill_manual(values = c('black')) + 
  scale_x_continuous('', breaks = seq(-3, 9, 3), expand = c(0, 0), 
                     labels = c('Nov 2019', 'Feb 2020', 'May 2020', 'Aug 2020', 'Nov 2020')) + 
  scale_y_continuous('Change in collaborations in\nepidemiological research (%)', expand = c(0.05, 0.05), position = 'right') + 
  xlab("Month") +
  theme_classic() + 
  theme(aspect.ratio = 0.5,  
        text = element_text(size = 10), 
        axis.line = element_line(size = 0.2), 
        axis.ticks = element_line(size = 0.2), 
        axis.title.x = element_text(margin = margin(t = 5)), 
        axis.title.y.right = element_text(margin = margin(l = 5)), 
        legend.position = c(0.65, 0.25), 
        legend.background = element_blank()) + 
  guides(color = guide_legend(''))


ggsave('fig/Figure4f-entropy-others.pdf', width = 4, height = 3)





