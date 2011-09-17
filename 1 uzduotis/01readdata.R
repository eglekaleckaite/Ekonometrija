library(plm)
library(reshape)
library(foreach)
library(ggplot2)

dt <- read.csv("imones5_8_2011_imones.csv")

dt.m <- subset(dt, grupe %in% 3)

melt.dt <- melt(dt.m, id.vars = c("nr", "grupe", "veikla", "ter",
                        "nace1", "nace2"))

time <- as.factor(aaply(as.matrix(melt.dt$variable), 1,
              function(ll) {
                date <- strsplit(as.character(ll), split = "_")[[1]][2]
                year.ll <- paste("20", gsub("[a-z]", "",
              date), sep = "")
                q.ll <- gsub("d", ".75",
                                        gsub("c", ".50",
                                             gsub("b", ".25",
                                                  gsub("a", ".00",
                                                       gsub("0[1-9]", "",
                                                            date)))))
                return(paste(year.ll, q.ll, sep = ""))
              }))

melt.dt$time <- time
melt.dt$variable <- aaply(as.matrix(melt.dt$variable), 1,
              function(ll) {
                date <- strsplit(as.character(ll), split = "_")[[1]][1]
              })

dt.fixed <- foreach(cc = unique(melt.dt$variable), .combine = cbind) %do% {
  rr <- data.frame(melt.dt[melt.dt$variable == cc, c("nr",
                             "time", "veikla", "grupe", "value")])
  names(rr) <- c("nr", "time", "veikla", "grupe",  cc)
  return(rr)
}

dt.old <- dt.fixed[, c("nr", "time", "veikla", "grupe")]


dt.new <- dt.fixed[, c("paj", "dsk", "val", "atlyg", "ter", "nace1", "nace2")]

add.dt <- cbind(dt.old, dt.new)

write.csv(add.dt, "reshaped.csv", row.names = F, na = "")

rm(list = ls())
#### Read fixed data ####
dat <- read.csv("reshaped.csv")
length(dat[is.na(dat)])
summary(dat)
prob <- problematic(dat, not = c("nr", "time", "veikla", "grupe",
                           "nice1", "nace2", "ter"))

problematic <- function(data, not){
   # Check data for negative, zero and NA values.
   # Arguments:
   # data : data.frame where first columns is Country, second -
   # Year and the rest colnames are variables.
   # Return:
   # res: data.frame with Country, Indicator, Problem and Year.
   # Problem contains "Empty", "Has na", "Has 0" and "Has < 0"
   #
  res <- foreach(cc = colnames(data)[!(colnames(data) %in% not)], .combine = rbind) %do%{
    indicator <- data[, c("nr", "time", cc)]
    prob <- foreach(C = unique(indicator$nr), .combine = rbind) %do% {
      gg <- subset(indicator, nr %in% C)

      if(all(is.na(gg[,ncol(gg)]))) {
        empty <- data.frame(nr = C, Indicator = cc,
                            Problem = "Empty", time = NA)
      } else {
        empty <- data.frame(nr = NULL, Indicator = NULL,
                              Problem = NULL, time = NULL)
      }

      has.zero <- data.frame(nr = NULL, Indicator = NULL,
                               Problem = NULL, time = NULL)
      has.na <- data.frame(nr = NULL, Indicator = NULL,
                             Problem = NULL, time = NULL)
      has.negative <- data.frame(nr = NULL, Indicator = NULL,
                                 Problem = NULL, time = NULL)

      if (nrow(empty) < 1){
        gg1 <- na.omit(gg)
        if (any(gg1[,ncol(gg1)] == 0))
          has.zero <- data.frame(nr = C, Indicator = cc,
                                 Problem = "Has 0", time =
                                 gg1[gg1[,ncol(gg1)] == 0,]$time)

        if (any(is.na(gg[,ncol(gg)])))
          has.na <- data.frame(nr = C, Indicator = cc,
                               Problem = "Has na", time =
                               gg[is.na(gg[,ncol(gg)]),]$time)

        if (any(gg1[,ncol(gg1)] < 0)&&cc!="Inflation")
          has.negative <- data.frame(nr = C, Indicator = cc,
                                     Problem = "Has < 0", time =
                                     gg1[gg1[,ncol(gg1)] < 0,]$time)

      }
      return(rbind(empty, has.zero, has.na, has.negative))
    }
  }
  return(res)
}

m.dat <- melt(dat, id=c(1:4,9:11))


dt.plot <- foreach(var = c("paj", "dsk", "val", "atlyg")) %do% {
  dd <- subset(m.dat, variable == var)
  foreach(C = unique(dd$nr)) %do% {
    dt.c <- subset(dd, nr == C)
    qplot(time, value, data = dt.c, geom = "line", colour = "blac",
  main = paste(var, C))
  }
}

pdf("all_ind.pdf", onefile = TRUE, height = 5, width = 10)
print(dt.plot)
dev.off()
