source("src/lib/read_osu.R")
chart <- f.chart.parse.osu(
  readLines("src/reqs/lofus_04062019/xi - Last Resort (loafusofbread) [HEAVENLY BREAD].osu"))

# Average velocity of sv in every 5 seconds
require(ggplot2)
require(ggdark)
require(scales)
require(reshape2)

ho <- chart$ho

ho.bin <- ho %>%
  mutate(bins = (offsets %/% 1000) * 1000) %>% 
  group_by(bins, types) %>% 
  summarise(n = n())

ho.sum <-
  ho.bin %>% 
  filter(types != "lnotel") %>% 
  mutate(types = "all") %>% 
  group_by(bins, types) %>% 
  summarise(n = sum(n))

ho.bin %<>% rbind(ho.sum)

ggplot(ho.bin) +
  aes(x = bins, y = n,
      group = types, color = types) + 
  geom_bar(size = 0.3, stat = 'identity') + 
  facet_wrap(. ~ types, ncol = 1) +
  scale_x_continuous(labels = comma) +
  ggtitle("xi - Last Resort",
          "Density Analysis 1s Binning")


ggsave("src/reqs/lofus_04062019/Rplot_dens.png")

# Jack Analysis

ho.jack <- chart$ho

ho.jack.bcast <- ho.jack %>% 
  filter(types != 'lnotel') %>% 
  dcast(offsets ~ keys) %>% 
  fill(2:5)

ho.jack.bcast.up <- ho.jack.bcast[2:nrow(ho.jack.bcast),]
ho.jack.bcast.up[nrow(ho.jack.bcast.up) + 1,] <- NA
ho.jack.bcast <- cbind((ho.jack.bcast.up - ho.jack.bcast)[2:5],
                       ho.jack.bcast$offsets)
colnames(ho.jack.bcast) <- c('1', '2', '3', '4', 'offsets')

ho.jack.bcast %<>% 
  melt(value.name = "diffs",
       variable.name = "keys",
       id.vars = 'offsets')

ho.jack.bcast %<>% 
  filter(diffs > 0, !is.na(diffs)) %>% 
  mutate(jack.inverse = 1/diffs)

ggplot(ho.jack.bcast) +
  aes(offsets, jack.inverse,
      color = keys) +
  geom_line(stat="identity") +
  geom_smooth(span = 0.07, se = F) + 
  facet_wrap(. ~ keys, ncol = 1) +
  ggtitle("xi - Last Resort",
          "Jack difficulty Analysis")

ggsave("src/reqs/lofus_04062019/Rplot_jack.png",
       width = 13,
       height = 7)
