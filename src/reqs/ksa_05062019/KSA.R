source("src/lib/read_osu.R")
chart.1 <- f.chart.parse.osu(
  readLines("src/reqs/ksa_05062019/Au5 - Eden (feat. Danyka Nadeau) (Ksardas) [Crystal].osu"))
chart.2 <- f.chart.parse.osu(
  readLines("src/reqs/ksa_05062019/Neuromonakh Feofan - Tak i znay (Ksardas) [Afflatus].osu"))
chart.3 <- f.chart.parse.osu(
  readLines("src/reqs/ksa_05062019/Sasha Zames - Chetkiy dram v tachku (Ksardas) [Midnight ~ Time of Fairy Tales].osu"))

# Note that these are standard maps

# CHART 1 & 3
# SV GRAPH

require(ggplot2)
require(ggdark)
require(scales)
require(reshape2)

tp.1 <- chart.1$tp

tp.1.new <- tp.1 %>% 
  filter(!is.bpm) 

ggplot(tp.1.new) +
  aes(offset, value) +
  geom_line(color = 'green') + 
  dark_theme_gray() +
  scale_x_continuous(labels = comma) +
  ggtitle("Au5 - Eden (feat. Danyka Nadeau)",
          "SV Graph")

ggsave("src/reqs/ksa_05062019/RPlot_eden.png", height = 4, width = 15)

tp.3 <- chart.3$tp

tp.3.new <- tp.3 %>% 
  filter(!is.bpm) 

ggplot(tp.3.new) +
  aes(offset, value) +
  geom_line(color = 'green') + 
  dark_theme_gray() +
  scale_x_continuous(labels = comma) +
  ggtitle("Sasha Zames - Chetkiy dram v tachku",
          "SV Graph")

ggsave("src/reqs/ksa_05062019/RPlot_chetkiy.png", height = 4, width = 15)

# --------------

# and there an average number of red-dots on one slider per 5 second (?)
tp.2 <- chart.2$tp 
