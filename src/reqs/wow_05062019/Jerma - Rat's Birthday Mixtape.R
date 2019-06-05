source("src/lib/read_osu.R")
chart <- f.chart.parse.osu(
  readLines("src/reqs/wow_05062019/Jerma - Rat's Birthday Mixtape (FULL VERSION) (AngeLItchysick) [Rats, rats, we are the rats].osu"))

require(ggplot2)
require(ggdark)
require(scales)
require(reshape2)

ho <- chart$ho

ho.new <- ho %>% 
  filter(types != 'lnotel') %>% 
  mutate(bins = (offsets %/% 1000) * 1000) %>% 
  group_by(bins, keys) %>% 
  summarize(n = n())

ggplot(ho.new) + 
  aes(bins,
      n,
      fill = factor(keys), color = factor(keys),
      group = keys) +
  geom_smooth(se = F, span = 0.1, size = 0.6, color = 'white') + 
  geom_point(size = 0.3, alpha = 0.5) +
  geom_area(alpha = 0.5) +

  facet_wrap(. ~ keys, ncol = 1) +
  ggtitle("Jerma - Rat's Birthday Mixtape (FULL VERSION)",
          "Density Analysis") +
  dark_theme_gray()

ggsave("src/reqs/wow_05062019/RPlot_nps.png", width = 13, height = 6)

ho.all <- ho %>% 
  filter(types != 'lnotel') %>% 
  mutate(bins = (offsets %/% 1000) * 1000) %>% 
  group_by(bins) %>% 
  summarize(n = n())

ggplot(ho.all) + 
  aes(bins,
      n) +
  geom_smooth(se = F, span = 0.3, size = 1, color = 'white') + 
  geom_point(size = 0.3, alpha = 0.5) +
  geom_area(alpha = 0.8) +
  ggtitle("Jerma - Rat's Birthday Mixtape (FULL VERSION)",
          "Density Analysis All") +
  dark_theme_gray()

ggsave("src/reqs/wow_05062019/RPlot_npsall.png", width = 13, height = 6)

