source("src/lib/read_osu.R")
chart <- f.chart.parse.osu(
  readLines("src/reqs/htpln_ 040602019/Camellia - New Challenger Approaching (HowToPlayLN) [RuleNote's Professional].osu"))

# Average velocity of sv in every 5 seconds
require(ggplot2)
require(ggdark)
require(scales)

tp <- chart$tp
ggplot(tp[tp$is.bpm == 0,]) +
  aes(offset, value) +
  geom_line(size=0.2) +
  geom_point(size=0.2) +
  ylim(c(0,2.5)) + 
  scale_x_continuous(labels = comma) + 
  ggtitle("Camellia - New Challenger Approaching (HowToPlayLN) [RuleNote's Professional]",
          "No Binning")

ggsave("src/reqs/htpln_ 040602019/Rplotraw_cam.png")

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
  ggtitle("Camellia - New Challenger Approaching (HowToPlayLN) [RuleNote's Professional]",
          "5s Bin")

ggsave("src/reqs/htpln_ 040602019/Rplot_cam.png")
