source("src/lib/read_osu.R")
chart <- f.chart.parse.osu(
  readLines("src/reqs/icd_05062019/HyuN - Grin (IceDynamix) [Klang des Fluegels].osu"))

require(ggplot2)
require(ggdark)
require(scales)
require(reshape2)

ho <- chart$ho

ho.cast <- ho %>% 
  mutate(t = 1) %>% 
  dcast(offsets ~ keys,
        value.var = "t")

ho.cast[is.na(ho.cast)] <- 0
ho.cast$sum <- rowSums(ho.cast[2:5], na.rm = F) 
ggplot(ho.cast) +
  aes(offsets, sum,
      color = factor(sum),
      group = sum) +
  geom_point() +
  dark_theme_gray() +
  ggtitle("HyuN - Grin",
          "Occurences of Chord Sizes")

ggsave("src/reqs/icd_05062019/RPlot_ung.png", width = 13, height = 4)

ho.cast.av <- ho.cast %>% 
  mutate(bins = (offsets %/% 3000) * 3000) %>% 
  group_by(bins) %>% 
  summarize(av.size = mean(sum)) 

ggplot(ho.cast.av) +
  aes(bins, av.size) + 
  geom_point(alpha = 0.4) +
  geom_line(alpha = 0.4, linetype = 'dotted') +
  geom_smooth(se = F, span = 0.2)  +
  dark_theme_gray() +
  ggtitle("HyuN - Grin",
          "Rolling Average Occurences of Chord Sizes")

ggsave("src/reqs/icd_05062019/RPlot_gr.png", width = 13, height = 6)
