source("src/lib/read_osu.R")
chart <- f.chart.parse.osu(
  readLines("src/reqs/uj_05062019/KOTOKO - Princess Bride! (_underjoy) [DOKIDOKI].osu"))

# Average velocity of sv in every 5 seconds
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
  aes(x = bins,
      y = n,
      group = keys,
      color = factor(keys),
      fill = factor(keys)) +
  geom_area(alpha = 0.3) +
  geom_line(linetype = "dotted") + 
  facet_wrap(. ~ keys, ncol = 1) +
  ggtitle("KOTOKO - Princess Bride!",
          "Density Analysis") +
  dark_theme_gray()

ggsave("src/reqs/uj_05062019/Rplot_density.png", width = 13)
  