source("src/lib/read_osu.R")
chart <- f.chart.parse.osu(
  readLines("src/reqs/htpln_ 040602019/Ice - Floor of Lava (HowToPlayLN) [Volcanic Eruption].osu"))

# Average velocity of sv in every 5 seconds
require(ggplot2)
require(ggdark)
require(scales)

tp <- chart$tp
ggplot(tp[tp$is.bpm == 0,]) +
  aes(offset, value) +
  geom_line() +
  geom_point() +
  ylim(c(0,2.5)) + 
  scale_x_continuous(labels = comma) + 
  ggtitle("Ice - Floor of Lava (HowToPlayLN) [Volcanic Eruption]",
          "No Binning")

ggsave("src/reqs/htpln_ 040602019/Rplotraw.png")

tp %<>%
  filter(is.bpm == 0) %>% 
  mutate(bins = (offset %/% 5000) * 5000) %>% 
  group_by(bins) %>% 
  summarize(mn = mean(value))

ggplot(tp) +
  aes(bins, mn) +
  geom_line() +
  geom_point() +
  ylim(c(0,2)) + 
  scale_x_continuous(labels = comma) + 
  ggtitle("Ice - Floor of Lava (HowToPlayLN) [Volcanic Eruption]",
          "5s Binning")

ggsave("src/reqs/htpln_ 040602019/Rplot.png")
