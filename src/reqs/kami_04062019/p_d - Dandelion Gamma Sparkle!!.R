source("src/lib/read_osu.R")
chart <- f.chart.parse.osu(
  readLines("src/reqs/kami_04062019/p_d - Dandelion Gamma Sparkle!! (Kamikaze) [Rush].osu")
)
require(ggplot2)
require(ggdark)
require(scales)
require(reshape2)

ho <- chart$ho
ho %<>%
  mutate(bins = (offsets %/% 1000) * 1000) %>% 
  group_by(bins, types, keys) %>% 
  summarise(n = n())

ggplot(ho) +
  aes(x = bins, y = n,
      group = keys, fill = keys) + 
  geom_bar(size = 0.3, stat = 'identity') + 
  facet_wrap(. ~ keys, ncol = 1) +
  scale_x_continuous(labels = comma) + 
  ggtitle("p_d - Dandelion Gamma Sparkle!!",
          "Density Analysis 1s Binning")

ggsave("src/reqs/kami_04062019/Rplot.png")

ho <- chart$ho
ho %<>%
  mutate(bins = (offsets %/% 1000) * 1000) %>% 
  group_by(bins, types) %>% 
  summarise(n = n())

ggplot(ho) +
  aes(x = bins, y = n) + 
  geom_bar(size = 0.3, stat = 'identity') + 
  facet_wrap(. ~ types, ncol = 1) +
  scale_x_continuous(labels = comma) + 
  ggtitle("p_d - Dandelion Gamma Sparkle!!",
          "Density Analysis 1s Binning")

ggsave("src/reqs/kami_04062019/Rplot_solo.png")
