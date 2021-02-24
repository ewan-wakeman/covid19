# setup.r

#' Includes installation and loading of required packages.

packages <- c('tidyverse',
              'nCov2019',
              'ggrepel',
              'scales',
              'gridSVG',
              'paletteer')

unpack <- function(p){
  if(p %in% installed.packages()[,'Package']){
    result <-if(require(p, character.only = T)){
      paste('loaded package', p)
    } else { 
      paste('unable to load package', p)
      }
  } else {
    install.packages(p, character.only = T)
    result <- if(require(p, character.only = T)){
      paste('installed and loaded', p)
    } else {
      paste('unable to load package', p)
    }
  }
  return(result)
}

sapply(packages, unpack)
source('~/Documents/Analysis/R/base/theme.r')
theme_set(theme_dark(base_size = 12))
colours <- as.list(colours)
pref_colours <- function(n){as.character(colours[3:(2+n)])}


data <- load_nCov2019(lang = 'en')

t10 <- 
  data$global %>%
  filter(time == max(time)) %>%
  arrange(-cum_confirm) %>%
  head(8) %>%
  pluck('country')

df <- 
  data$global %>%
  filter(country %in% t10) %>%
  group_by(country) %>%
  mutate(day100 = min(time[which(cum_confirm >= 100)]),
         duration = time - day100,
         cd_ratio = cum_dead / cum_confirm
         ) %>%
  filter(duration >= 0) %>%
  pivot_longer(c(cum_confirm, cum_dead, cum_heal, cd_ratio), names_to = 'type') %>%
  mutate(type = case_when(type == 'cum_confirm' ~ 'confirmed',
                          type == 'cum_dead' ~ 'deaths',
                          type == 'cum_heal' ~ 'recoveries',
                          type == 'cd_ratio' ~ 'deaths/confirmed ratio',
                          T ~ 'other'),
         latest_point = duration == max(duration),
         latest_lab = case_when(#duration == 45 ~ paste(country,
                                duration != max(duration) ~ NA_character_,
                                str_detect(country, '\\s') ~ paste(str_remove_all(country, '[a-z\\s]*'), format(value, big.mark = ',', scientific = F, digits = 0)),
                                T ~ paste(country, format(value, big.mark = ',', scientific = F, digits = 0)))
        )

bright_pal <- paste0(as.character(colours[c(4,7:13)]), 80)

cdr_pd <-
  df %>%
  filter(type %in% c('confirmed', 'deaths', 'recoveries')) %>%
  group_by(type) %>%
  nest()
  
cdr_pl <- 
  cdr_pd %>%
  unname() %>%
  pmap(function(t, d){
    p <-
      ggplot(d, aes(x = duration, y = value, colour = country, fill = country, label = latest_lab)) +
      geom_line(size = .5) +
      geom_point(aes(size = latest_point), show.legend = F) +
      scale_size_manual(values = c(0,2)) +
      #geom_label_repel(size = 2.5, colour = colours$light, label.size = 0, alpha = .5) +
      # geom_text_repel(box.padding = .7, segment.colour = colours$light, segment.size = .1, family = 'Helvetica Neue Thin', size = 2.5, fill = paste0(colours$light, 10), label.size = 0) +
      scale_x_continuous(labels = scales::unit_format(suffix = ' days')) +
      #scale_y_continuous(trans = 'log', breaks = c(1e-1,1e0,1e1,1e2,1e3,1e4,1e5,1e6,1e7), limits = c(NA, NA), labels = scales::comma_format(accuracy = 1)) +
      # scale_colour_paletteer_d("ggsci::hallmarks_light_cosmic", direction = -1) +
      scale_colour_manual(values = bright_pal) +
      # #facet_grid(str_to_title(type)~., scales = 'free_y') +
      labs(
        title = str_glue('COVID-19 {type} Rate', type = case_when(t == 'confirmed' ~ 'Confirmed Case',
                                                                       t == 'deaths' ~ 'Death',
                                                                       t == 'recoveries' ~ 'Recovery',
                                                                       T ~ 'Other')),
        subtitle = str_glue("Data last updated: {format(td, '%A %d %b %Y')}", td = data$time),
        x = '…since 100th case',
        colour = 'Country'
      ) +
      theme(
        legend.position = 'bottom',
        panel.grid.major.y = element_line(colour = paste0(colours$light, 80), size = .05, ),
        axis.title.x = element_text(size = rel(.9)),
        axis.title.y = element_blank()
      )
   out <- list(data = d, plot = p)
  })
  
  
names(cdr_pl) <- cdr_pd$type
  
cdr_pl$deaths$plot


grid::grid.force()
pts <- grid::grid.ls(print = F)$name
points <- grep('geom_point', pts, value = T)

grid.garnish(points, dur = paste(cdr_pl$confirmed$data$duration), country = paste(cdr_pl$confirmed$data$country), value = paste(cdr_pl$confirmed$data$value), group = F)
svg_text <- grid.export(strict = F, addClasses = T)$svg
grid.export('test.svg', addClasses = T)

if(!dir.exists('./output')){dir.create('output')}


ggsave(plot = cdr_p, path = 'output', filename = 'covid_rates.png', scale = 2, height = 5, width = 5)

together <-
  ggplot(df, aes(x = duration, y = value, colour = country, fill = country, label = latest_lab)) +
  geom_line(size = .5) +
  geom_point(aes(size = latest_point), show.legend = F) +
  scale_size_manual(values = c(NA,2)) +
  scale_x_continuous(labels = scales::unit_format(suffix = ' days')) +
  scale_colour_manual(values = bright_pal) +
  facet_wrap(~type, scales = 'free_y') +
  labs(
    title = 'COVID-19',
    subtitle = str_glue("Data last updated: {format(td, '%A %d %b %Y')}", td = data$time),
    x = '…since 100th case',
    colour = 'Country'
  ) +
  theme(
    strip.text = element_text(colour = colours$light),
    legend.position = 'bottom',
    panel.grid.major.y = element_line(colour = paste0(colours$light, 80), size = .05, ),
    axis.title.x = element_text(size = rel(.9)),
    axis.title.y = element_blank()
  )

together




