source("src/lib/read_osu.R")
chart <- f.chart.parse.osu(
  readLines("src/reqs/fletch_04062019/Hyper Potions - Jungle Cruise (Theresa May) [Stage 2 - Excitement].osu")
)
require(ggplot2)
require(ggdark)
require(scales)
require(reshape2)

ho <- chart$ho
ho %<>%
  mutate(bins = (offsets %/% 1000) * 1000) %>% 
  group_by(bins, types) %>% 
  summarise(n = n())

ho.sum <-
  ho %>% 
  filter(types != "lnotel") %>% 
  mutate(types = "all") %>% 
  group_by(bins, types) %>% 
  summarise(n = sum(n))


ho %<>% rbind(ho.sum)

ggplot(ho) +
  aes(x = bins, y = n,
      group = types, color = types) + 
  geom_bar(size = 0.3, stat = 'identity') + 
  facet_wrap(. ~ types, ncol = 1) +
  scale_x_continuous(labels = comma) + 
  ggtitle("Hyper Potions - Jungle Cruise (Theresa May)",
          "Density Analysis 1s Binning")

ggsave("src/reqs/fletch_04062019/Rplot.png")
