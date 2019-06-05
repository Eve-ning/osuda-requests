source("src/lib/read_osu.R")
chart <- f.chart.parse.osu(
  readLines("src/reqs/guil_05062019/Kobaryo vs. t+pazolite - Galaxy Friends (tpz Overheat Remix) (Guilhermeziat) [Redemption].osu"))

require(ggplot2)
require(ggdark)
require(scales)
require(reshape2)
require(directlabels)

tp <- chart$tp
ho <- chart$ho

# BPM Over Time

tp.bpm <- tp %>% 
  filter(is.bpm == 1)

ggplot(tp.bpm) + 
  aes(offset, value) + 
  geom_line(color = 'red') +
  dark_theme_gray() +
  ggtitle("Kobaryo vs. t+pazolite - Galaxy Friends (tpz Overheat Remix)",
          "BPM Plot")

ggsave("src/reqs/guil_05062019/Rplot_bpm.png")

# Mult Over Time

tp.cast <- tp %>% 
  dcast(offset ~ is.bpm, value.var = "value", na.rm = F) %>% 
  rename(offsets = offset, sv = `0`, bpm = `1`) %>% 
  mutate(mult = sv * bpm)

ggplot(tp.cast) +
  aes(offsets, mult) +
  geom_line(color = 'yellow') +
  geom_line(aes(y=bpm), color ='red') +
  dark_theme_gray() +
  ylim(0, 300) +
  ggtitle("Kobaryo vs. t+pazolite - Galaxy Friends (tpz Overheat Remix)",
          "BPM & Mult Plot")

ggsave("src/reqs/guil_05062019/Rplot_mult.png")

# Cumulative
ho <- chart$ho

morph <- function(x) {
  return(ifelse(is.na(x), 0, 1))
}

ho %<>% 
  filter(types != "lnotel") %>% 
  dcast(offsets ~ keys) %>% 
  mutate_at(.vars = 2:5, morph)

ho.cs <- apply(ho[2:5], 2, cumsum)

ho[2:5] <- (ho.cs)

ho %<>%
  melt(value.name = "cumsum",
       id.vars = 1,
       variable.name = "keys") %>% 
  mutate(keys = as.numeric(keys))

ggplot(ho) +
  aes(offsets, cumsum,
      group = keys,
      color = factor(keys)) +
  geom_point(size=0.2) +
  scale_x_continuous(labels = comma) +
  dark_theme_gray() +
  geom_dl(aes(label = keys), method = list(dl.trans(x = x + 0.2),
                                           'last.bumpup',
                                           dl.combine("last.points"), cex = 0.7)) +
  ggtitle("Kobaryo vs. t+pazolite - Galaxy Friends (tpz Overheat Remix)",
          "Cumulative Sum") 

ggsave("src/reqs/guil_05062019/Rplot_cumsum.png")

# NPS
ho <- chart$ho

ho.nps <- ho %>% 
  filter(types != 'lnotel') %>% 
  mutate(bins = (offsets %/% 1000) * 1000) %>% 
  group_by(bins, keys) %>% 
  summarise(nps = n()) %>% 
  mutate(keys = as.character(keys))

ggplot(ho.nps) +
  aes(x = bins,
      y = nps, color = keys, group = keys) +
  geom_smooth(se = F, span = 0.1) +
  dark_theme_gray() +
  ggtitle("Kobaryo vs. t+pazolite - Galaxy Friends (tpz Overheat Remix)",
          "NPS Plot")

ggsave("src/reqs/guil_05062019/Rplot_nps.png")

ho.all <- ho %>% 
  filter(types != 'lnotel') %>% 
  mutate(keys = 'all') %>% 
  mutate(bins = (offsets %/% 1000) * 1000) %>% 
  group_by(bins, keys) %>% 
  summarise(nps = n())

ggplot(ho.all) +
  aes(x = bins,
      y = nps, color = keys) +
  geom_smooth(se = F, span = 0.1) +
  dark_theme_gray() +
  ggtitle("Kobaryo vs. t+pazolite - Galaxy Friends (tpz Overheat Remix)",
          "NPS Plot All")

ggsave("src/reqs/guil_05062019/Rplot_npsall.png")