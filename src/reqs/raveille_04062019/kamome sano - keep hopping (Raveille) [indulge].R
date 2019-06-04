source("src/lib/read_osu.R")
chart <- f.chart.parse.osu(
  readLines("src/reqs/raveille_04062019/kamome sano - keep hopping (Raveille) [indulge].osu"))

# oh uh, idk whatever graph tbh LOL, density?
require(ggplot2)
require(ggdark)
require(scales)

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
  scale_x_continuous(labels = comma)

ggsave("src/reqs/raveille_04062019/Rplot.png")
