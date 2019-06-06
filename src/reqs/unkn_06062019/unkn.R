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

tp <- chart$tp
ho <- chart$ho

