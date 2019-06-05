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
      fill = keys,
      group = keys) +
  geom_point(size = 0.3) +
  geom_area() +
  facet_wrap(. ~ keys, ncol = 1) +
  ggtitle("Jerma - Rat's Birthday Mixtape (FULL VERSION)",
          "Density Analysis")

ggsave("src/reqs/wow_05062019/RPlot.png", width = 13, height = 6)

