f.chart.parse.osu <- function(chart) {
  #' Parses the osu chart into a data.frame
  #' 
  #' @param chart The chart to be parsed, in a vector of characters.
  #' This can be provided via readLines function.
  #' 
  require(dplyr)
  require(tidyr)
  require(magrittr)
  require(stringr)
  require(reshape2)
  require(docstring)
  
  f.extract <- function(chart) {
    cs.i <- pmatch('CircleSize:', chart)
    keys <- as.integer(substr(chart[cs.i],
                              start = 12,
                              stop = nchar(chart[cs.i])))
    tp.i <- pmatch('[TimingPoints]', chart)
    ho.i <- pmatch('[HitObjects]', chart)
    
    tp <- chart[(tp.i+1):(ho.i-1)]
    ho <- chart[ho.i+1:length(chart)]
    
    return(list("tp" = tp, "ho" = ho, "keys" = keys))
  }
  
  extract <- f.extract(chart)
  {
    ho <- extract$ho
    keys <- extract$keys
    
    ho <- data.frame(ho, stringsAsFactors = F)
    colnames(ho) <- "txt"
    
    ho$is.ln <- str_count(string = ho$txt,
                             pattern = ":") == 5
    
    ho %<>% separate(col=txt,
                        sep=":",
                        into=c("txt","_"),
                        extra="drop") %>%
      separate(col=txt, sep=",",
               into=c("axis",".0","note",".1",".2","lnotel"))
    
    ho$keys = round((as.integer(ho$axis) * keys - 256) / 512) + 1
    ho %<>% na.omit() 
    ho$lnotel[ho$is.ln == F] <- NA
    ho %<>% mutate_if(is.character, as.numeric)
    
    ho <- ho[c('note', 'lnotel', 'keys', 'is.ln')]
    ho %<>% 
      mutate(lng = ifelse(is.ln, lnotel - note, 0))
    ho$lnoteh[ho$is.ln] <- ho$note[ho$is.ln]
    ho$note[ho$is.ln] <- NA
    ho <- melt(ho, id.vars = c('keys', 'lng'),
                  na.rm = T, variable.name = 'types',
                  value.name = 'offsets')
    ho <- subset(ho, types != 'is.ln')
  }
  {
    tp <- data.frame(extract$tp)
    colnames(tp) <- "txt"
    tp %<>% separate(col=txt,
                     sep=",",
                     into=c("offset","code","_","_","_","_","is.bpm"),
                     extra="drop",
                     fill="right")
    tp %<>% na.omit()
    tp <- tp[c('offset', 'code', 'is.bpm')]
    tp %<>%
      mutate(value = ifelse(is.bpm == 1,
                            60000.0/as.numeric(code),
                            -100.0/as.numeric(code)))
  }
  return(list("ho"=ho, "tp"=tp))
}

f <- readLines("src/reqs/fletch_04062019/Hyper Potions - Jungle Cruise (Theresa May) [Stage 2 - Excitement].osu")
t <- f.chart.parse.osu(f)
t$ho
