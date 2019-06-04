osu <- read.csv("src/uj_23052019/main.osu", header = F)
osu$metadata <- paste(osu$V3,osu$V4,osu$V5,osu$V6,osu$V7,osu$V8,sep = ",")
osu$is.bpm <- osu$V7 == 1
osu <- osu[c(1,2,9,10)]
colnames(osu) <- c('offset', 'code', 'metadata', 'is.bpm')
osu$value[osu$is.bpm] <- 60000/osu$code[osu$is.bpm]
osu$value[!osu$is.bpm] <- -100/osu$code[!osu$is.bpm]

brks <- c(0, 910, 20878, 35423 ,51880 ,65594 ,78394 ,
          93394 ,112594,136008,148008,
          162230,189658,216458,229712,246063,287691)

mults <- c(1.0,
           1.16666,
           1.06060,
           1.06666,
           1.0,
           0.93333,
           1.09375,
           0.7,
           0.68292,
           1.64705,
           1.03703,
           1.0,
           0.93333,
           0.24163,
           0.5,
           0.28)

osu$brks <- cut(osu$offset, breaks = brks,)

osu.s <- split(osu,f = osu$brks)
osu.l <- c()
for (brk in 1:16){
  osu.l.i <- osu.s[[brk]]
  osu.l.i$value.n[osu.l.i$is.bpm == F] <-
    osu.l.i$value[osu.l.i$is.bpm == F] * mults[brk]
  osu.l <- append(osu.l, list(osu.l.i))
}

require(dplyr)

osu <- bind_rows(osu.l)

osu$value.n[is.na(osu$value.n)] <- osu$value[is.na(osu$value.n)]
osu$code.n[osu$is.bpm] <- 60000/osu$value.n[osu$is.bpm]
osu$code.n[!osu$is.bpm] <- -100/osu$value.n[!osu$is.bpm]

final <- paste(osu$offset, osu$code.n, osu$metadata, sep = ",")

write.table(final, file='export.txt', quote = F,row.names = F)
