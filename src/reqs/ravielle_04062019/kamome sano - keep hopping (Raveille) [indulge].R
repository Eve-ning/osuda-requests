source("src/lib/read_osu.R")
chart <- f.chart.parse.osu(
  readLines("src/reqs/ravielle_04062019/kamome sano - keep hopping (Raveille) [indulge].osu"))
chart
