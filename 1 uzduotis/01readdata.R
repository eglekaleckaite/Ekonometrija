library(plm)
library(reshape)
library(foreach)

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

#### Read fixed data ####
