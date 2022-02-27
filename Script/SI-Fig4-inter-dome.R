
library(foreach)
library(tidyverse)
library(lfe)
library(fastDummies)
library(stargazer)

domains <- c("Arts & Humanities", 
             "Social Sciences", 
             "Life Sciences & Biomedicine", 
             "Physical Sciences", 
             "Technology")


indexes <- readRDS('index/indexes_normal_alllinks_5domains_paperbased.rds')

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
  mutate(papers_coll_ratio = papers_coll / papers, 
         papers_in_ratio = papers_in / papers, 
         papers_log = log(papers), 
         papers_coll_log = log(papers_coll), 
         papers_in_log = log(papers_in)) %>%
  mutate(month_date = gsub('.*?[.]', '', as.character(month))) %>%
  group_by(country, month_date) %>%
  mutate(country_month_date = cur_group_id()) %>%
  ungroup() %>%
  group_by(domain, month_date) %>%
  mutate(domain_month_date = cur_group_id()) %>%
  ungroup()


indexes <- fastDummies::dummy_cols(indexes, select_columns = "month")

var_nam <- paste0("month_", unique(indexes$month)[25:length(unique(indexes$month))])
var_nam <- var_nam[var_nam != 'month_2020.083']



formula <- data.frame(title = c('Total papers', 
                                'Total international papers', 
                                'Total domestic papers',
                                'Ratio of international papers', 
                                'Ratio of domestic papers'), 
                      label = c('Total papers', 
                                'International papers', 
                                'Domestic papers',
                                'International papers', 
                                'Domestic papers'), 
                      formula = c(paste0("papers_log ~ ", paste(var_nam, collapse= " + "),
                                         " + time | country_month_date + domain_month_date | 0 | country"), 
                                  paste0("papers_coll_log ~ ", paste(var_nam, collapse= " + "),
                                         " + time | country_month_date + domain_month_date | 0 | country"), 
                                  paste0("papers_in_log ~ ", paste(var_nam, collapse= " + "),
                                         " + time | country_month_date + domain_month_date | 0 | country"), 
                                  paste0("papers_coll_ratio ~ ", paste(var_nam, collapse= " + "),
                                         " + time | country_month_date + domain_month_date | 0 | country"), 
                                  paste0("papers_in_ratio ~ ", paste(var_nam, collapse= " + "),
                                         " + time | country_month_date + domain_month_date | 0 | country")))

reg_dta <- bind_rows(foreach(i = c(2:3)) %do% {
  
  if (i %in% c(3,5)) {
    reg <- felm(as.formula(formula$formula[i]), data = indexes %>% filter(papers_in >= 2), na.action = "na.omit")
  } else if (i %in% c(2,4)) {
    reg <- felm(as.formula(formula$formula[i]), data = indexes %>% filter(papers_coll >= 2), na.action = "na.omit")
  } else {
    reg <- felm(as.formula(formula$formula[i]), data = indexes %>% filter(papers >= 2), na.action = "na.omit")
  }
  
  stargazer(reg,type="text")
  
  data.frame(coef = reg$beta[1:(length(reg$beta)-1)], 
                        se = reg$cse[1:(length(reg$beta)-1)]) %>% 
    bind_rows(data.frame(coef = 0, se = 0) %>% `rownames<-`('month_2020.083')) %>% 
    mutate(ci_low = coef - 1.96 * se, 
           ci_high = coef + 1.96 * se) %>% 
    mutate(month = gsub('month_', '', row.names(.))) %>% 
    mutate(year = gsub('[.].*', '', month)) %>% 
    mutate(time = c(-(sum(month<'2020.083'):1), 1:sum(month>'2020.083'), 0)) %>% 
    arrange(time) %>% 
    mutate(shape = ifelse((ci_low > 0) | (ci_high < 0), 'solid', 'hollow')) %>% 
    filter(year != '2021') %>% 
    mutate(group = formula$label[i])
})


p1 <- ggplot(reg_dta, aes(x = time,y = coef)) + 
  geom_rect(aes(xmin = 0.5, xmax = 4.5, ymin = -Inf, ymax = max(ci_high)*1.1), fill = 'grey97') + 
  geom_hline(yintercept = 0, color = "grey50", lwd = 0.3, linetype = "dashed") +
  geom_linerange(aes(x = 0.5, ymin = -Inf, ymax = max(ci_high)*1.1), color = "grey50", lwd = 0.1) +
  geom_linerange(aes(ymin = coef - se, ymax = coef + se, col = group, group = group), 
                 lwd = 1.2, stat = 'identity', position = position_dodge(width = 0.6), show.legend = F) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, col = group, group = group), width = 0.2, 
                 stat = 'identity', position = position_dodge(width = 0.6), show.legend = F) +
  geom_point(aes(col = group, fill = group, group = group), 
             size = 2.5, shape = 21, show.legend = F, 
             stat = 'identity', position = position_dodge(width = 0.6)) + 
  geom_text(aes(x, y, label = label), size = 4.5,
            data.frame(x = c(0.5),
                       y = max(reg_dta$ci_high)*1.4,
                       label = c('Outbreak'))) +
  scale_color_manual(values = c('#B13625', '#4D8D74')) +
  scale_fill_manual(values = c('#B13625', '#4D8D74')) +
  scale_x_continuous('', breaks = seq(-5, 10, 3), expand = c(0.03, 0.03),
                     labels = c('Sep 2019', 'Dec 2019', 'Mar 2020', 'Jun 2020', 'Sep 2020', 'Dec 2020')) +
  scale_y_continuous('Change in number (%)', breaks = seq(-0.3, 0.1, 0.1), labels = c(-0.3, -0.2, -0.1, 0, 0.1) * 100) + 
  xlab("Month") +
  ylab('Relative changes') +
  theme_classic() + 
  theme(aspect.ratio = 0.6,  
        text = element_text(size = 18), 
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.title.y.left = element_text(margin = margin(r = 5)), 
        legend.position = c(0.17, 0.2), 
        legend.background = element_blank(), 
        plot.margin = margin(0,0,0,0)) +
  guides(col = guide_legend(''))



reg_dta <- bind_rows(foreach(i = c(4,5)) %do% {
  
  if (i %in% c(3,5)) {
    reg <- felm(as.formula(formula$formula[i]), data = indexes %>% filter(papers_in >= 2), na.action = "na.omit")
  } else if (i %in% c(2,4)) {
    reg <- felm(as.formula(formula$formula[i]), data = indexes %>% filter(papers_coll >= 2), na.action = "na.omit")
  } else {
    reg <- felm(as.formula(formula$formula[i]), data = indexes %>% filter(papers >= 2), na.action = "na.omit")
  }
  
  stargazer(reg,type="text")
  
  data.frame(coef = reg$beta[1:(length(reg$beta)-1)], 
             se = reg$cse[1:(length(reg$beta)-1)]) %>% 
    bind_rows(data.frame(coef = 0, se = 0) %>% `rownames<-`('month_2020.083')) %>% 
    mutate(ci_low = coef - 1.96 * se, 
           ci_high = coef + 1.96 * se) %>% 
    mutate(month = gsub('month_', '', row.names(.))) %>% 
    mutate(year = gsub('[.].*', '', month)) %>% 
    mutate(time = c(-(sum(month<'2020.083'):1), 1:sum(month>'2020.083'), 0)) %>% 
    arrange(time) %>% 
    mutate(shape = ifelse((ci_low > 0) | (ci_high < 0), 'solid', 'hollow')) %>% 
    filter(year != '2021') %>% 
    mutate(group = formula$label[i])
})


p2 <- ggplot(reg_dta, aes(x = time,y = coef)) + 
  geom_rect(aes(xmin = 0.5, xmax = 4.5, ymin = -Inf, ymax = Inf), fill = 'grey97') + 
  geom_hline(yintercept = 0, color = "grey50", lwd = 0.3, linetype = "dashed") +
  geom_linerange(aes(x = 0.5, ymin = -Inf, ymax = Inf), color = "grey50", lwd = 0.1) +
  geom_linerange(aes(ymin = coef - se, ymax = coef + se, col = group, group = group), 
                 lwd = 1.2, stat = 'identity', position = position_dodge(width = 0.6), ) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, col = group, group = group), width = 0.2, 
                 stat = 'identity', position = position_dodge(width = 0.6), show.legend = F) +
  geom_point(aes(col = group, fill = group, group = group), 
             size = 2.5, shape = 21, show.legend = F, 
             stat = 'identity', position = position_dodge(width = 0.6)) + 
  scale_color_manual(values = c('#B13625', '#4D8D74')) +
  scale_fill_manual(values = c('#B13625', '#4D8D74')) +
  scale_x_continuous('', breaks = seq(-5, 10, 3), expand = c(0.03, 0.03),
                     labels = c('Sep 2019', 'Dec 2019', 'Mar 2020', 'Jun 2020', 'Sep 2020', 'Dec 2020')) +
  scale_y_continuous('Change in proportion (percentage point)', breaks = seq(-0.04, 0.04, 0.02), labels = c(-0.04, -0.02, 0, 0.02, 0.04) * 100, position = 'right') + 
  xlab("Month") +
  ylab('Relative changes') +
  theme_classic() + 
  theme(aspect.ratio = 0.6,  
        text = element_text(size = 18), 
        axis.title.x = element_text(margin = margin(t = 5)), 
        axis.title.y.right = element_text(margin = margin(l = 5)), 
        legend.position = c(0.15, 0.2), 
        legend.background = element_blank(), 
        plot.margin = margin(0,0,0,0)) +
  guides(col = guide_legend(''))


p1 / p2

ggsave('fig/SI-Figure4-inter-dome.pdf', width = 10, height = 8)

