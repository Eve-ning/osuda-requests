source("src/lib/read_osu.R")
chart.1 <- f.chart.parse.osu(
  readLines("src/reqs/unkn_06062019/KARUT a.k.a Triplebullets - Anata dake kiku koto ga dekiru chiisana boryoumu de (Unknown_player) [Renai].osu"))
chart.2 <- f.chart.parse.osu(
  readLines("src/reqs/unkn_06062019/xi - over the top (Extended) (Unknown_player) [Ascended].osu",)
)


require(ggplot2)
require(ggdark)
require(scales)
require(reshape2)
require(directlabels)

ho.1 <- chart.1$ho
ho.2 <- chart.2$ho

ho.1.nps <- ho.1 %>% 
  filter(types != 'lnotel') %>% 
  mutate(bins = (offsets %/% 1000) * 1000) %>% 
  group_by(bins) %>% 
  summarize(nps = n())

ggplot(ho.1.nps) +
  aes(bins, nps
      ) +
  geom_point(alpha = 0.3, size = 1)+
  geom_line(alpha = 0.8) +
  geom_area(alpha = 0.3) +
  scale_x_continuous(labels = comma) + 
  ggtitle("KARUT a.k.a Triplebullets - Anata dake kiku koto ga dekiru chiisana boryoumu de")

ggsave("src/reqs/unkn_06062019/Rplot_KARUT.png")

ho.2.nps <- ho.2 %>% 
  filter(types != 'lnotel') %>% 
  mutate(bins = (offsets %/% 1000) * 1000) %>% 
  group_by(bins) %>% 
  summarize(nps = n())

ggplot(ho.2.nps) +
  aes(bins, nps
  ) +
  geom_point(alpha = 0.3, size = 1)+
  geom_line(alpha = 0.8) +
  geom_area(alpha = 0.3) +
  scale_x_continuous(labels = comma) + 
  ggtitle("xi - over the top (Extended)")

ggsave("src/reqs/unkn_06062019/Rplot_ott.png")
