source("src/lib/read_osu.R")
chart <- f.chart.parse.osu(
  readLines("src/reqs/popegadg_040602019/Car Bomb - Constant Sleep (Pope Gadget) [Midnight Black].osu"))

# Average velocity of sv in every 5 seconds
require(ggplot2)
require(ggdark)
require(scales)
require(reshape2)

# BPM VS OFFSET
tp <- chart$tp

tp.bpm <- tp %>% 
  filter(is.bpm == 1)

ggplot(tp.bpm) +
  aes(offset, value) +
  geom_line(size = 0.5) +
  ggtitle("Car Bomb - Constant Sleep (Pope Gadget) [Midnight Black]",
          "BPM Plot")

ggsave("src/reqs/popegadg_040602019/RPlot_bpm.png",
       width = 13,
       height = 5)

# Combination
tp.cast <- tp %>% 
  dcast(offset ~ is.bpm) %>% 
  rename(bpm = `1`, sv = `0`) %>% 
  fill(bpm) %>% 
  mutate(sv = ifelse(is.na(sv), 1, sv)) %>% 
  mutate(mult = bpm * sv / 217.604)
  
ggplot(tp.cast) +
  aes(offset, mult) +
  geom_line(size = 0.5) +
  ggtitle("Car Bomb - Constant Sleep (Pope Gadget) [Midnight Black]",
          "Multiplied Plot")

ggsave("src/reqs/popegadg_040602019/RPlot_mult.png",
       width = 13,
       height = 5)

# bin it

tp.cast.bin <- tp.cast %>%
  mutate(bins = (offset %/% 1000) * 1000) %>% 
  group_by(bins) %>% 
  summarize(mn = mean(mult))

ggplot(tp.cast.bin) +
  aes(bins, mn) +
  geom_line(size = 0.5) +
  ggtitle("Car Bomb - Constant Sleep (Pope Gadget) [Midnight Black]",
          "Multiplied Plot (Binned per second)")

ggsave("src/reqs/popegadg_040602019/RPlot_mult_bin.png",
       width = 13,
       height = 5)
