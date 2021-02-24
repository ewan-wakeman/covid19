# dataurls
urls <-
  list(
    cases = "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv",
    deaths = "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-deaths_latest.csv"
  )

# ons population estimates
ltla_xl <- 'https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2019april2020localauthoritydistrictcodes/ukmidyearestimates20192020ladcodes.xls'
la_to_region <- 'https://opendata.arcgis.com/datasets/3ba3daf9278f47daba0f561889c3521a_0.csv'
date_range <- list(start = as.Date('2020-03-10'),
                   end = Sys.Date() - 1)

# load packages
require(tidyverse)
require(slider)
require(readxl)

# set area (Local Authority District)
area_select <- 'Westminster'

# read latest case and deaths data
d <-
  map2_dfr(names(urls), urls, function(name, url){
    df <-
      read_csv(url) %>%
      mutate(measure = name) %>%
      rename_all(~str_replace_all(., '\\s|\\-', '_') %>% str_to_lower() %>% str_remove_all('_cases|_deaths|reporting_|specimen_'))
    return(df)
  })

# download and tidy population data
download.file(ltla_xl, t <- tempfile(), mode = 'wb')
ltla_pop <- read_excel(t, sheet = "MYE2 - Persons", skip = 4) %>% transmute(code = Code, pop = `All ages`)
ltla_to_region <- 
  read_csv(la_to_region) %>%
  rename_all(~str_replace_all(., '[0-9]{2}', '_') %>% str_to_lower) %>%
  select(-fid, -lad_nm)

#
ltla_d <- 
  d %>%
  filter(area_type == 'Lower tier local authority') %>%
  left_join(ltla_pop, by = c('area_code' = 'code')) %>%
  left_join(ltla_to_region, by = c('area_code' = 'lad_cd')) %>%
  transmute(area_code, area_name, date, measure, 
            region_code = rgn_cd, region = rgn_nm,
            daily_count = daily_lab_confirmed, pop, 
            pop_adj_count = daily_count / (pop/1e5)
  ) %>%
  group_by(area_code) %>%
  mutate(
    daily_count = if_else(is.na(daily_count), 0, daily_count),
    mean_7 = slide_dbl(daily_count, mean, na.rm = T, .before = 7),
    pop_adj_mean_7 = slide_dbl(pop_adj_count, mean, na.rm = T, .before = 7),
    mean_14 = slide_dbl(daily_count, mean, na.rm = T, .before = 14),
    pop_adj_mean_14 = slide_dbl(pop_adj_count, mean, na.rm = T, .before = 14),
    cumulative_count = slide_dbl(daily_count, sum, na.rm = T, .before = Inf)
  )

region_select <- ltla_d[ltla_d$area_name == area_select,]$region[[1]]

ltla_d <-
  ltla_d %>%
  mutate(
    highlight = case_when(area_name == area_select ~ area_name,
                          region == region_select ~ paste0(region, ' (excl. ', area_select, ')'),
                          T ~ paste0('England (excl. ', region_select, ')'))
  )

mean7_p <-
  ltla_d %>%
  filter(date >= date_range$start,
         date <= date_range$end) %>%
  ggplot(aes(date, pop_adj_mean_7, colour = highlight, group = area_code,
             alpha = area_name == area_select)) +
  geom_line() +
  geom_line(stat = 'summary', fun = 'mean', aes(group = highlight), size = .5, linetype = 2, alpha  = 1, show.legend = F) +
  scale_y_continuous(limits = c(0,25)) +
  scale_x_date(date_labels = '%d-%b') +
  labs(title = 'New Coronavirus Cases by date (7-day rolling average)',
       subtitle = paste0(area_select, 'highlghted'),
       x = 'Date tested',
       y = 'Number of new cases per 100,000 people',
       colour = 'Area Type',
       alpha = 'Area Type') +
  guides(
    alpha = F
  ) +
  theme_dark() +
  theme(legend.position = 'bottom')


mean7_p
  

df %>%
  group_by(area_name) %>%
  arrange(specimen_date) %>%
  mutate('rolling_avg' = slide_dbl(new_cases_adj, mean, .before = 7)) %>%
  ungroup() %>%
  ggplot(aes(x = specimen_date, y = rolling_avg, group = area_name, colour = area_group, alpha = area_group)) +
  geom_line() +
  labs(title = 'New Coronavirus Cases by date (7-day rolling average)',
       subtitle = 'Newham highlghted',
       x = 'Date tested',
       y = 'Number of new cases per 100,000 people',
       colour = 'Area Type',
       alpha = 'Area Type') +
  scale_alpha_manual(values = c(area_select = 1, 'London' = .1, 'Outside of London' = .05)) +
  scale_y_continuous(limits = c(0, 25)) +
  theme_dark() +
  theme(legend.position = 'bottom')

total <- 
  df %>%
  group_by(area_group, area_name) %>%
  filter(specimen_date == max(specimen_date)) %>%
  ungroup() %>%
  mutate(cases_per_100000 = cumulative_lab_confirmed_cases/pop*1e5) %>%
  select(area_group, area_name, cumulative_lab_confirmed_cases, cases_per_100000, pop)

total_mean_cases_per_100k <- mean(total$cases_per_100000, na.rm = T)
total_mean_cases <- mean(total$cumulative_lab_confirmed_cases)
london_mean_per_100k <- total %>% filter(area_group != 'Outside of London') %>% .$cases_per_100000 %>% mean(na.rm = T)
  
ggplot(total, aes(x = pop, cases_per_100000, colour = area_group, fill = area_group)) +
  geom_hline(yintercept = total_mean_cases_per_100k, colour = colours$light, linetype = 2, alpha = .7, size = .2) +
  geom_hline(yintercept = london_mean_per_100k, colour = colours$blue, linetype = 2, alpha = .7, size = .2) +
  #geom_smooth(method = 'gam', se = T, alpha =.2, fullrange = T, size = .5) +
  geom_point() +
  stat_summary(geom = 'line', fun = 'mean', aes(x= NA)) +
  scale_x_continuous(trans = 'sqrt') +
  scale_y_continuous(trans = 'sqrt') +
  scale_colour_manual(aesthetics = c('colour', 'fill'), values = c('Newham' = colours$leaf, 'London' = colours$blue, 'Outside of London' = paste0(colours$fresh, 20))) +
  theme_dark()

total_2 <- 
  total %>%
  mutate(area_group = if_else(area_group == 'Newham', 'London', area_group),
         highlight = if_else(area_name == 'Newham', cases_per_100000, NA_real_),
         highlight_label = if_else(!is.na(highlight), area_name, NA_character_))
  
 test_plot <- 
   ggplot(total_2, aes(x = cases_per_100000, y = area_group, label = area_name, 
                    fill = area_group)) + 
  geom_violin(width = .6, alpha = .3) +
  stat_boxplot(geom = 'errorbar', width = .1, colour = colours$light, show.legend = F) +
  geom_boxplot(width = .1, outlier.shape = NA, colour = colours$light, alpha = .2, show.legend = F) + 
  geom_point(aes(x = highlight), colour = colours$leaf, size = 3, ) +
  #geom_jitter(data = filter(total_2, area_name != 'Newham'), alpha = .5, height = .1, show.legend = F) +
  scale_colour_manual(aesthetics = c('colour', 'fill'), values = c('London' = colours$blue, 'Outside of London' = colours$fuschia)) +
  theme_dark()

cum <-
  

