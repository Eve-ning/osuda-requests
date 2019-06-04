source("src/lib/read_osu.R")
chart <- f.chart.parse.osu(
  readLines("src/reqs/l51_04062019/xi - Double Helix (Level 51) [Nucleic].osu"))

# Average velocity of sv in every 5 seconds
require(ggplot2)
require(ggdark)
require(scales)
require(reshape2)
require(directlabels)

ho <- chart$ho

morph <- function(x) {
  return(ifelse(is.na(x), 0, 1))
}

ho %<>% 
  filter(types != "lnotel") %>% 
  dcast(offsets ~ keys) %>% 
  mutate_at(.vars = 2:8, morph)

ho.cs <- apply(ho[2:8], 2, cumsum)

ho[2:8] <- (ho.cs)

ho %<>%
  melt(value.name = "cumsum",
       id.vars = 1,
       variable.name = "keys") %>% 
  mutate(keys = as.numeric(keys))

ggplot(ho) +
  aes(offsets, cumsum,
      group = keys,
      color = keys) +
  geom_point(size=0.2) +
  scale_x_continuous(labels = comma) +
  geom_dl(aes(label = keys), method = list(dl.trans(x = x + 0.2),
                                           'last.bumpup',
                                           dl.combine("last.points"), cex = 0.7)) +
  ylim(0, 1100) + 
  ggtitle("xi - Double Helix",
          "Cumulative Sum")

ggsave("src/reqs/l51_04062019/Rplot.png",
       width = 7,
       height = 4)
