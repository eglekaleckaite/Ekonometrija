# This file contains all the functions that are used in first level of quaids project
#
# List of functions:
#   forecastNAs(data, cnt, n, type = c("regression", "ar"), trend = TRUE, ...)
#   adddummiesn(data, cnlist)
#   balance(x)
#   bestIVprice(data, write = FALSE)
#   change.levels(f, levt)
#   choosedt
#   combineAF(act, forc)
#   cont2discrete(x, points)
#   convertDataSpecific(data)
#   dumvar(x, region)
#   elast(model, elast.names, data)
#   elast.direct(mod, var, data, id)
#   elast.indirect(mod, var, data, id)
#   fillNAsINivs(data, names, which)
#   getdata(...)
#   graphs(obj)
#   fitted.pgmm(object, output=c("pseries","pdata.frame"), ...)
#   makeTable(lvls, able)
#   mape(o, res)
#   MAPE(model)
#   mase(o, res, naive)
#   MASE(model)
#   my.summary(formula, data, elast.names, name1 = NULL, name2 = NULL,
#              plot = TRUE,...)
#   resCorr(resi, var, data)
#   scenario(obj, data, variab, country, tt, change)
#   statsNeeded(data, vars)

################################################################################
##' Forecast countrie's NA's data using arima or regression
##'
##' @title
##' @param data data.frame, containing countries cnt
##' @param cnt character vector of countries
##' @param n number of regressors to include in the model
##' @param type method to obtain forecast
##' @param trend logical. FALSE fo not inculding trend in regression
##' @param Country Character indicating country, which data should be forecasted
##' @param ...
##' @return list with graph, forecasted data and model object.
##' @author
forecastNAs <- function(data, cnt = NULL, n = NULL,
                        type = c("regression", "arima"), trend = TRUE,
                        Country, ...){

    dt <- cast(data, Year ~ Country)
      if (all(is.na(dt[,Country]))) stop("Selected country has all NA data")
    # revert data
    dt.rev <- revert(dt[,c(2:dim(dt)[2])])
    dt[,c(2:dim(dt)[2])] <- dt.rev
    dt$t <- seq(1:dim(dt)[1])
    if (type == "regression"){
    # find best lm model(according to mean square error)
        obj <- findbestfit(Country, cnt, dt, n, trend = trend)
        predicted <- predict(obj, newdata = dt)
    } else {
        dt.arima <- log(na.omit(as.ts(dt[,Country])))
        obj <- auto.arima(dt.arima, allowdrift = TRUE)
        frct <- forecast(obj)$mean
        predicted <- exp(union(dt.arima, frct))
    }
    frct.length <- length(dt$Year[which(is.na(dt[,Country]) == TRUE)])
    frct.start <- dt$Year[1]+frct.length
    dt[,Country][which(is.na(dt[,Country]) == TRUE)] <- predicted[which(is.na(dt[,Country]) == TRUE)]
    dt <- dt[,-which(colnames(dt)=="t")]
    # revert data with prediction back
    revback <- revert(dt[,c(2:dim(dt)[2])])
    dt[,c(2:dim(dt)[2])] <- revback
    ## dt.split <- data.frame(start = c(1998, frct.start),
    ##                        end = c(frct.start, 2010),
    ##                        data = c("forecast", "original"))
    ## graph <- qplot(Year, dt[,which(colnames(dt) == Country)], data = dt,
    ##                geom="line") + labs(x = "Year", y = paste(Country)) +
    ## geom_rect(aes(NULL, NULL, xmin = start, xmax = end,
    ##               fill = data), ymin = range(dt[,Country])[1],
    ##           ymax = range(dt[,Country])[2], data = dt.split) +
    ## scale_fill_manual(values = alpha(c("red", "grey"), 0.2))
return(list(data = dt[,c("Year", Country)], model = obj))
}

##############################################################################

addMeanVar <- function(data){
  # Adds columns with values of averaged by time variables;
  # Args:
  # data - a data frame for which addtional variables should be added.
  names <- colnames(data)
  id <- c(grep("inst", names), grep("rtecereals", names))
  data.mean <- data[, -id]
  data.mean <- ddply(data.mean, .(Country),
                     function(x) apply(x[, -c(1,2)], 2,
                                       function(l) mean(l, na.rm=TRUE)))
  colnames(data.mean)[-1] <- paste(colnames(data.mean)[-1], ".mean",
                                   sep="")
  res <- merge(data, data.mean ,by = "Country")
  res
}
################################################################################
adddummies <- function(data, cnlist){
   dummycollumns= foreach(Reg = names(cnlist), .combine=cbind) %do%{
        region <- cnlist[[Reg]]
        added.d <- ddply(data, .(Country), dumvar, region)[,3]

  }
   colnames(dummycollumns) <- names(cnlist)
   cbind(data[,c("Country", "Year")], dummycollumns)
}

################################################################################
balance <- function(x){
  # Makes balanced panel by removing countries that have problems (ex: NA values)
  # from the dataset. Raises a warning about removed countries.
  #
  # Args:
  #   x: data.frame with dataset to balance;
  #
  # Returns:
  #   x: data.frame with balanced panel.
  #
  problems <- problematic(x)
  countries <-  drop.levels(unique(problems$Country))
  cycle <- foreach(i = countries) %do% {
    x <- x[x$Country != as.character(drop.levels(i)), ]
  }

  warning(paste("In order to make panel balanced such countries were removed",
                "from the dataset:", paste(countries, collapse = ", "), "."))
  return(x)
}

################################################################################
bestIVprice <- function(data, write = FALSE){
  dt <- data[, c("Country", "rtecereals.p")]

  price <- foreach(C = unique(dt$Country), .combine = cbind) %do% {
  country <- dt[dt$Country == C, ]
  series <- ts(country[, "rtecereals.p"], start = 1998, end = 2010, frequency = 1)
  return(series)
}
colnames(price) <- unique(dt$Country)
  inst <- round(cor(price, use = "pairwise.complete.obs"), 2)
  # Rank best instruments
  best.inst <- adply(inst, 2, function(cc) {
  r <- rank(cc)
  ord <- sort(r, decreasing = TRUE)
  return(names(ord))
  })
  colnames(best.inst) <- c("Country", paste(0:(nrow(best.inst)-1)))

  best.inst <- t(best.inst)
  colnames(best.inst) <- best.inst[1, ]
  bestinst <- best.inst[-1:-2, ]
  if (write == TRUE) {
    write.csv(bestinst, "output/Best instruments.csv")
  }
  list(best = bestinst, inst = inst)
}

################################################################################
change.levels <- function(f,levt) {
    ##Change the the names of the factor f levels from
    ##substitution table levt.
    ## In the first column there are the original levels, in
    ## the second column -- the substitutes
    lv <- levels(f)
    if(sum(sort(lv)!=sort(levt[,1]))>0)
      stop ("The names from substitution table does not match given level names")
    res <- rep(NA,length(f))

    for(i in lv) {
        res[f==i] <- as.character(levt[levt[,1]==i,2])
    }
    res
}

################################################################################
choosedt <- function(data, cap, case, xrate = "exchange",
                     quant = c("rtecereals.q", "hes", "fruits.q",
                     "vegetables.q", "eggs.q", "meat.q", "fish.q"),
                     price = c("rtecereals.p", "bread.p", "milk.p", "butter.p",
                     "cheese.p", "hot.cereals.p", "yoghurt.p"),
                     income = c("adi", "consum", "gdp", "gdp.ppp",
                     "consum.bread.cereal", "consum.food"),
                     indexclass = c("kof.econ.glob", "kof.act.flows",
                     "kof.restr", "kof.soc.glob", "kof.pers.cont",
                     "kof.info.flows", "kof.cult.prox", "kof.pol.glob",
                     "kof.overall.glob", "mpa", "gini", "hdi", "op",
                     "daily.cal", "smoking", "life.exp",
                     "expen.health"),
                     exception = "Argentina") {
   # Function to choose data.frame
   # Args: data: data.frame. (units should be like in data.input)
   #       cap: logical. TRUE if per capita is required. FALSE otherwise.
   #       case: character. One of: "cpi", "food", "specific", "ppp"
   # quant, price, income, indexclas are vectors of variables, which will be
   # colnames of choosed data.frame.
   # Return: data.frame
   # example : choosedt(data.input, cap=FALSE, case = "food")

   case <- match.arg(case, c("cpi", "food", "specific", "ppp"))
   # deal with Health expenditure (it's in US$ per capita)
   # form exchange rates of USA
   exch.USA <- rep(data[data$Country %in% "USA",]$exchange,
                  length(unique(data$Country)))
   # in EUR
   data$expen.health <- data$expen.health/exch.USA
   # divided by GDP per capita
   data$expen.health <- data$expen.health * 100 * data$pop/data$gdp

   cpi.dt <- data[,c("Country", "Year", "cpi")]
   new.cpi <- ddply(cpi.dt, .(Country), function(ll)  {
       ll$cpi <- ll$cpi/ll[ll$Year==2004,]$cpi
       ll})
   data$cpi <- new.cpi$cpi

   # Form vector with exchange rates in 2004
   exchange <- data[, c("Country", "Year", "exchange")]
   exchange.2004 <- foreach(r = rownames(exchange), .combine = cbind) %do% {
     country <- drop.levels(exchange[r, 1])
     row <- exchange[exchange$Country %in% country & exchange$Year %in% 2004,
                     "exchange"]
   }
   exchange.2004 <- as.vector(exchange.2004)

   # Form ppp of USA
   cpi.USA <- rep(data[data$Country %in% "USA",]$cpi,
                  length(unique(data$Country)))
   # Form nominal prices in national currency
   prices.nominal <- foreach(cc = price, .combine=cbind) %do%{
       data.frame(data[, cc] * data$exchange)
   }

   prices.real <- foreach(cc = price, .combine=cbind) %do%{
     if (case == "food"){
       division(data, cc, exception = exception,
                other = "cpi", main = "index.food")
     } else if ((case == "cpi") & (xrate == "exchange")){
       data.frame(cc = data[,cc] * data$exchange /
                  (data$cpi * exchange.2004))
     } else if ((case == "cpi") & (xrate == "ppp")){
       data.frame(cc = data[,cc] * data$exchange /
                  (cpi.USA * data$ppp))
     } else if (case == "ppp"){
       data.frame(cc = (data[,cc] * data$exchange)/data$ppp)
     } else if (cc == "Bread.p"){
       division(data, cc, exception = exception,
                other = "cpi", main = "index.bread")
     } else if (cc == "Milk.p" | cc == "Cheese.p"){
       division(data, cc, exception = exception,
                other = "cpi", main = "index.milk.cheese.eggs")
     } else {
       division(data, cc, exception = exception,
                other = "cpi", main = "index.food")
     }
   }

   prices=cbind(prices.real, prices.nominal)
   colnames(prices) <- c(price, paste(price, ".nominal", sep = ""))

   if (cap){
      dt.df <- data.frame(Country = data$Country, Year = data$Year)

      quants <- foreach(cc = quant, .combine=cbind) %do%{
                    data.frame(cc = data[,cc]/data$pop)
                }
      colnames(quants) <- quant
        #gsub(".q", ".q.cap", quant)

      incomes <- foreach(cc = income, .combine=cbind) %do%{
                 data.frame(cc = data[,cc]/(data$cpi * data$pop))
                 }
      colnames(incomes) <- income

      indexcla <- foreach(cc = indexclass, .combine=cbind) %do%{
                  data.frame(cc = data[,cc])
                  }
      colnames(indexcla) <- indexclass

      cbind(dt.df, quants, prices, incomes, indexcla)
   } else {
       # If not per capita is required
             dt.df <- data.frame(Country = data$Country, Year = data$Year)

      quants <- foreach(cc = quant, .combine=cbind) %do%{
                    data.frame(cc = data[,cc])
                }
      colnames(quants) <- quant
        #gsub(".q.abs", ".q", gsub("$", ".abs", quant))

      incomes <- foreach(cc = income, .combine=cbind) %do%{
                 data.frame(cc = data[,cc]/data$cpi)
                 }
      colnames(incomes) <- income
        #gsub("$", ".abs" , income)

      indexcla <- foreach(cc = indexclass, .combine=cbind) %do%{
                  data.frame(cc = data[,cc])
                  }
      colnames(indexcla) <- indexclass

      cbind(dt.df, quants, prices, incomes, indexcla)
  }
}

#############################################################################
combineAF <- function(act, forc) {
  # Combines forecasts with actual data.
  #
  # Args:
  #   act: matrix with time series;
  #   forc: matrix with time series;
  #
  #
  # Returns:
  #  act.forc: matrix with time series.
  #
  act.forc <- foreach(C = colnames(forc), .combine = cbind) %do% {
    both <- ts.union(na.omit(act[, C]), forc[, C])
    pmax(both[, 1], both[, 2], na.rm = TRUE)
  }
  colnames(act.forc) <- colnames(forc)
  return(act.forc)
}

################################################################################
cont2discrete <- function(x, points){
  new.x <- foreach(pp = 1:length(points), .final=function(x) x[[length(x)]]) %do%{
    value <- points[pp]
    if (pp == 1){
      x[x <= value] <- value
    } else{
      valueprev <- points[pp-1]
      x[x <= value & x > valueprev] <- value
    }
    return(x)
  }
    new.x[new.x > points[length(points)]] <- (1 + points[length(points)]) / 2
    return(new.x)
}

################################################################################
convertDataSpecific <- function(data){
   #Converts all prices into real terms using a specific index for
   #particular food type.
   #
   # Args:
   #   data: data frame where changes should be made.
   #
   # Returns:
   #   Relevant parts of the data frame are replaced. Adjustments were made for
   #   all countries except Argentina.

  data[data$Country!="Argentina",]$Bread.p <-
    ((d[d$Country!="Argentina",]$bread.rsp /
      d[d$Country!="Argentina",]$index.bread) * 100)
  data[data$Country!="Argentina",]$Bread.p <-
    ((d[d$Country!="Argentina",]$bread.rsp /
      d[d$Country!="Argentina",]$index.bread) * 100)
  data[data$Country!="Argentina",]$Milk.p <-
    ((d[d$Country!="Argentina",]$drinking.milk.products.rsp /
      d[d$Country!="Argentina",]$index.food) * 100)
  data[data$Country!="Argentina",]$Butter.p <-
    ((d[d$Country!="Argentina",]$butter.rsp /
      d[d$Country!="Argentina",]$index.food) * 100)
  data[data$Country!="Argentina",]$Cheese.p <-
    ((d[d$Country!="Argentina",]$cheese.rsp /
      d[d$Country!="Argentina",]$index.food) * 100)
  data[data$Country!="Argentina",]$Yoghurt.p <-
    (((d[d$Country!="Argentina",]$plain.spoonable.yoghurt.rsp +
    d[d$Country!="Argentina",]$fruited.spoonable.yoghurt.rsp) /
      d[d$Country!="Argentina",]$index.food) * 100)
                   # a price index of available yoghurt prices

  return(data)
}

##############################################################################
division <- function(data, cc, exception, other, main){
    # Performs division of data.frames. Main denominator is 'main'.
    # For countries 'exception', denominator is other.
    # cc is variable name, which should be divided.
    dt.other <- data[,c("Country", other)]
    denom <- data[,c("Country", main)]
    denom[denom$Country %in% exception,] <- dt.other[dt.other$Country %in%
                                                          exception,]
    data.frame(data[,cc]/denom[,2])
}

#################################################################################
dumvar <- function(x, region){
    withdummy <- cbind(x[,c("Country", "Year")], data.frame(NA))
 if (unique(x$Country)%in%region){
     withdummy[,3]=1
 } else withdummy[,3]=0
 withdummy
}

################################################################################
EMgetSummary <- function(obj,...){
  s <- summary(obj)
  coef <- if(class(obj)[1] == "plm") s$coef else s$CoefTable
  colnames(coef) <- c("est", "se", "stat", "p")
  #sumstat <- c(s$r.squared["rsq"], s$r.squared["adjrsq"],
  #             s$fstatistic$statistic, s$fstatistic$p.value)
  #names(sumstat) <- c("r.squared", "adj.r.squared", "F", "p")
  call <- s$call
  res <- list("coef" = coef, "sumstat" = NULL, contrasts = NULL,
              "xlevles" = list(), "call" = call)
  res
}
################################################################################

##' Computes R squared
##'
##' Computes R squared for the pgmm object
##' @title
##' @param obj an object of the class pgmm
##' @return
##' @author
EM_R2 <- function(obj){
    fit <- melt(obj$fitted.values)
    colnames(fit)[c(1, 2)] <- c("Year", "Country")
    act <- sapply(obj$model, function(x) cbind(x[, 1]))
    rownames(act) <- rownames(obj$model[[1]])
    act <- melt(act)
    colnames(act)[c(1, 2)] <- c("Year", "Country")
    R2 <- cor(act$value, fit$value)^2
    R2
}

################################################################################
EMmtable <- function (..., coef.style = getOption("coef.style"), summary.stats = TRUE,
    factor.style = getOption("factor.style"), getSummary = function(obj,
        ...) UseMethod("getSummary"), float.style = getOption("float.style"),
    digits = min(3, getOption("digits")), drop = TRUE)
{
    args <- list(...)
    if (length(args) == 1 && inherits(args[[1]], "by"))
        args <- args[[1]]
    argnames <- names(args)
    if (!length(argnames)) {
        m <- match.call(expand.dots = FALSE)
        argnames <- sapply(m$..., paste)
    }
    n.args <- length(args)
    arg.classes <- lapply(args, class)
    if (any(sapply(arg.classes, length)) == 0)
        stop("don't know how to handle these arguments")
    summaries <- lapply(args, getSummary)
    calls <- lapply(summaries, function(x) x$call)
    names(calls) <- argnames
    ctemplate <- as.matrix(memisc:::getCoefTemplate(coef.style))
    ctdims <- dim(ctemplate)
    lctdims <- length(ctdims)
    if (lctdims > 2)
        stop("can't handle templates with dim>2")
    getCoef1 <- function(coef, contrasts, xlevels) {
        dimnames(coef)[[1]] <- memisc:::prettyNames(dimnames(coef)[[1]],
            contrast = contrasts, xlevels = xlevels, factor.style = factor.style)
        adims <- if (length(dim(coef)) == 2)
            1
        else c(1, 3)
        ans <- apply(coef, adims, function(x) memisc:::applyTemplate(x,
            template = ctemplate, float.style = float.style,
            digits = digits))
        if (length(dim(ctemplate))) {
            newdims <- c(dim(ctemplate), dim(ans)[-1])
            newdimnames <- c(dimnames(ctemplate), dimnames(ans)[-1])
            newdimnames <- lapply(1:length(newdims), function(i) {
                if (length(newdimnames[[i]]))
                  return(newdimnames[[i]])
                else return(as.character(1:newdims[i]))
            })
            dim(ans) <- newdims
            dimnames(ans) <- newdimnames
        }
        else rownames(ans) <- names(ctemplate)
        ans[ans == "()"] <- ""
        return(ans)
    }
    getCoef <- function(i) {
        coef.i <- summaries[[i]]$coef
        contrasts.i <- summaries[[i]]$contrasts
        xlevels.i <- summaries[[i]]$xlevels
        if (is.list(coef.i))
            lapply(coef.i, getCoef1, contrasts = contrasts.i,
                xlevels = xlevels.i)
        else getCoef1(coef.i, contrasts = contrasts.i, xlevels = xlevels.i)
    }
    coefs <- lapply(seq(n.args), getCoef)
    isList <- sapply(coefs, is.list)
    if (any(isList)) {
        all.names <- unique(unlist(lapply(coefs, names)))
        coefs <- lapply(coefs, function(x) {
            if (is.list(x))
                x
            else structure(list(x), names = all.names[1])
        })
        coefs.tmp <- vector(length = length(all.names), mode = "list")
        names(coefs.tmp) <- all.names
        for (n in all.names) {
            tmp <- lapply(coefs, .subset2, n)
            isNULL <- sapply(tmp, is.null)
            if (any(isNULL)) {
                firstNonNULL <- tmp[[min(which(!isNULL))]]
                dummy <- array(NA, dim = dim(firstNonNULL),
                               dimnames = dimnames(firstNonNULL))
                tmp[isNULL] <- list(dummy)
            }
            tmp <- memisc:::clct.arrays(tmp)
            n.dims <- length(dim(tmp))
            dimnames(tmp)[[n.dims]] <- argnames
            tmp[is.na(tmp)] <- ""
            coefs.tmp[[n]] <- tmp
        }
        coefs <- bind_arrays(coefs.tmp, 3)
    }
    else {
        coefs <- memisc:::clct.arrays(coefs)
        n.dims <- length(dim(coefs))
        dimnames(coefs)[[n.dims]] <- argnames
        coefs[is.na(coefs)] <- ""
    }
    groups <- attr(coefs, "groups")
    n.dims <- length(dim(coefs))
    dimnames(coefs)[[n.dims]] <- argnames
    if (drop && length(dim(coefs)) > 3) {
        cdims <- dim(coefs)
        ckeep <- cdims > 1 | 1:length(dim(coefs)) <= 3
        dn <- dimnames(coefs)
        dim(coefs) <- dim(coefs)[ckeep]
        dimnames(coefs) <- dn[ckeep]
        dims <- sum(ckeep)
    }
    as.row <- c(1, 3)
    as.col <- which(!(seq(length(dim(coefs))) %in% as.row))
    kill.col <- 2
    kill.header <- length(as.col)
    coef.dim <- 3
    if (isTRUE(summary.stats) || is.character(summary.stats) &&
        length(summary.stats)) {
        stemplates <- lapply(args, getSummaryTemplate)
        sumstats <- lapply(seq(n.args), function(i) {
            drop(memisc:::applyTemplate(summaries[[i]]$sumstat,
                                        template = stemplates[[i]],
                digits = digits))
        })
        sumstats <- clct.vectors(sumstats)
        colnames(sumstats) <- argnames
        if (is.character(summary.stats) && !all(summary.stats %in%
            rownames(sumstats))) {
            undefnd <- summary.stats[!(summary.stats %in% rownames(sumstats))]
            undefnd <- paste(sQuote(undefnd), sep = ", ")
            if (length(undefnd) == 1)
                stop("summary statistic ", undefnd, " is undefined")
            else stop("summary statistics ", undefnd, " are undefined")
        }
        sumstats <- sumstats[summary.stats, , drop = FALSE]
        sumstats[is.na(sumstats)] <- ""
        substats <- as.table(sumstats)
    }
    else sumstats <- NULL
    structure(list(coefficients = as.table(coefs), groups = groups,
        summaries = sumstats, calls = calls, as.row = as.row,
        as.col = as.col, kill.col = kill.col, kill.header = kill.header,
        coef.dim = coef.dim), class = "mtable")
}

################################################################################
elast <- function(model, elast.names, data){

  ELAST.direct <- foreach(var = elast.names, .errorhandling="pass") %do% {
    elast.direct(model, var, data, id = c("Country", "Year"))
  }
  names(ELAST.direct) <- elast.names
  ELAST.direct <- foreach(var = names(ELAST.direct),
                          .combine = function(l1, l2)
                          merge(l1, l2, by=c("Country", "Year")),
                          .errorhandling="pass") %do% {
                            ELAST.direct[[var]]
                          }
  id.dir <- which(apply(ELAST.direct,2,function(l)all(l == 0)))
  ELAST.direct <- if(length(id.dir) != 0) ELAST.direct[,-id.dir]
                  else ELAST.direct

  ELAST.indirect <- foreach(var = elast.names) %do% {
    elast.indirect(model, var, data, id = c("Country", "Year"))
  }
  names(ELAST.indirect) <- elast.names
  ELAST.indirect <- foreach(var = names(ELAST.indirect),
                            .combine = function(l1, l2)
                            merge(l1, l2), .errorhandling="remove") %do% {
                              ELAST.indirect[[var]]
                            }
  id.indir <- which(apply(ELAST.indirect,2,function(l)all(l == 0)))
  ELAST.indirect <- if(length(id.indir) != 0) ELAST.indirect[,-id.indir]
                    else ELAST.indirect

  list(direct = ELAST.direct, indirect = ELAST.indirect)
}

################################################################################
elast.direct <- function(mod, var, data, id){
  # Computes direct elasticities of a given variable.
  #
  # Args:
  #   mod - a model;
  #   var - names of variables for which elasticities are required;
  #   data - a data.frame used in estimating the given model;
  #   id - a character consisting of two variables:
  #        an individual and a time indexes(colnames of the data);
  #
  # Returns:
  #   elast: elasticities of a given variable.
  #
  formula <- mod$level.form
  left.side <- as.character(splitFormula(formula, sep = "+"))
  coef.names <- gsub(var, "",
                    left.side[grep(var, left.side,fixed = TRUE)],                              fixed = TRUE)
  coef.names.all <- gsub(var, "",
                         names(coef(mod))[grep(var, names(coef(mod)),
                                                    fixed = TRUE)],
                         fixed = TRUE)
  coef.names <- gsub("~", "", coef.names)
  indirect <- regexpr(":", coef.names)
  coef.names <- coef.names[which(indirect == -1 |
                indirect == str_length(coef.names))]
  coef.names <- gsub(":", "", coef.names)
  coef.names.all <- gsub(":", "", coef.names.all)
  coef.names.all[coef.names.all == ""] <- "(Intercept)"
  if(any(coef.names == "")) {
    coef.names <- coef.names[coef.names != ""]
    edfrm <- formula(paste("~", paste(c(coef.names, 1), collapse = "+")))
  } else
      edfrm <- formula(paste("~", paste(c(coef.names, -1), collapse = "+")))
  DATA  <- model.matrix(terms(edfrm), data)
  extra <- setdiff(coef.names.all, colnames(DATA))
  coef <- coef(mod)[grep(var, names(coef(mod)), fixed=TRUE)]
  if(length(extra) != 0)
     for(i in 1:length(extra)) {
       coef <- coef[-grep(extra[i], names(coef), fixed=TRUE)]
     }
  elast <- DATA%*%coef
  colnames(elast) <- var
  elast <- cbind(data[, id], elast)
  elast
}

################################################################################
elast.indirect <- function(mod, var, data, id){
  # Computes indirect elasticities of a given variable.
  #
  # Args:
  #   mod - a model;
  #   var - names of variables for which elasticities are required;
  #   data - a data.frame used in estimating the given model;
  #   id - a character consisting of two variables:
  #        an individual and a time indexes(colnames of the data);
  #
  # Returns:
  #   elast: elasticities of a given variable.
  #
  coef.names <- gsub(var, "", names(coef(mod))[grep(var, names(coef(mod)),
                                                    fixed = TRUE)], fixed = TRUE)
  coef.names <- gsub(":", "", coef.names)
  if(any(coef.names == "")) {
    coef.names <- coef.names[coef.names != ""]
    edfrm <- formula(paste("~", paste(c(coef.names, 1), collapse = "+")))
  } else
      edfrm <- formula(paste("~", paste(c(coef.names, -1), collapse = "+")))
  DATA  <- model.matrix(terms(edfrm), data)
  elast <- DATA%*%coef(mod)[grep(var, names(coef(mod)), fixed = TRUE)]
  colnames(elast) <- var
  elast <- cbind(data[, id],elast)
  elast
}

#############################################################################

fillNAsINivs <- function(data, names, inst = "avg_price_tot"){
  # Fills NA values for countries in 'names' by the most immediate value in 'inst'
  #
  # Args
  #   data : a data frame with at least two columns: 'Country' and 'inst' (by defaul   #          t the name of it is "avg_price_tot")
  #   names: names of countries for which NA values have to be filled
  #   inst: column in which NA values have to be filled
  # Returns:
  #   a data frame in which NA values are filled in
  filled <- foreach(C = unique(data$Country), .combine = rbind) %do% {
    if (C %in% names){
      dt <- subset(data, Country %in% C)
      dt[, inst][which(is.na(dt[, inst]) == TRUE)] <- na.omit(dt[, inst])[1]
    } else dt <- subset(data, Country %in% C)
   dt
  }
}

#############################################################################

##' Replace NA with previous or following (if the first value is NA) value
##' @title
##' @param x vector
##' @return vector
##' @author
fillNA <- function(x){
    for(i in (1:length(x))) {
        if (is.na(x[i])){
            x[i] <- if (i != 1) x[i-1] else if (all(is.na(x))) x[i]
            else x[!is.na(x)][1]
        } else{
            x[i]
        }
    }
    x
}

#############################################################################

##' Fill NAs with previous values.
##' If all values for particular country and variable are NA, then they are
##' left.
##' @title
##' @param data data.frame. Panel data.frame structure.
##' @param id numeric vector. Numbers of columns such as Country, Year, Region.
##' @return data with filled NAs
##' @author
fillAllNAs <- function(data, id = 1:3){

    all <- foreach(nm = colnames(data[, -id]), .combine = cbind) %do% {
        column <- data[, c("Country", nm)]

        onecol <- foreach(cnt = unique(column$Country), .combine = c) %do% {
            vect <- column[column$Country==cnt,][, nm]
            fillNA(vect)
        }

        onecol
    }

    colnames(all) <- colnames(data[, -id])
    dt.frame <- data.frame(data[, id], all)
    dt.frame
}

#############################################################################
##' Find lm model with smallest residuals
##' Best model is selected from all combinations with n regressors.
##' @title
##' @param Country Character indicating country, which data should be forecasted
##' @param cnt character vector of potential regressors
##' @param dt data.frame
##' @param n number of regressors
##' @param trend logical. Should trend be considered.
##' @return lm object
##' @author
findbestfit <- function(Country, cnt, dt, n, trend = TRUE){
    if (n > (length(cnt) -1)) stop("n is too big")
    if (trend == TRUE){
    comb <- combn(c(cnt[cnt!=Country], "t"), n)
    } else comb <- combn(cnt[cnt!=Country], n)
    result <- foreach(i = 1:dim(comb)[2], .combine=rbind) %do%{
        formula <- paste(Country,"~", paste(comb[,i], collapse = "+"))
        obj <- lm(data = dt, formula)
        mm <- mean(sum(residuals(obj)^2))
        mm
    }
    bestcomb <- comb[,which(result == min(result))]
    b.form <- paste(Country,"~", paste(bestcomb, collapse = "+"))
    bestobj <- lm(data = dt, b.form)
    bestobj
}

################################################################################
finalIVs <- function(data, name.by, write = FALSE){
  best.list <- bestIVprice(data)
  best.price <- best.list$best
  inst <- best.list$inst
  var.df <- data[, c("Country", name.by)]
  var <- foreach(C=unique(var.df$Country), .combine = cbind) %do% {
    country <- var.df[var.df$Country == C, ]
    series <- ts(country[, name.by], start = 1998, end = 2010, frequency = 1)
    return(series)
  }
  colnames(var) <- unique(var.df$Country)

  Form.best.inst <- foreach(C = colnames(best.price), .combine = rbind) %do% {
    best7 <- var[,best.price[1:7, C]]
    var.c <- var[, C]
    if (!all(is.na(var.c))) {
      diff.var <- apply(best7 - var.c, 2, function(cc) mean(abs(cc),
                                                            na.rm = TRUE))
      names(diff.var) <- colnames(best7)
      r <- rank(diff.var)
      ord <- sort(r)
      best.var <- names(ord)[1:5]
    } else {
      best.var <- colnames(best7)[1:5]
    }
    best.cor <- inst[which(rownames(inst) %in% C),
                     which(colnames(inst) %in% best.var)][best.var]
    best.df <- data.frame(Country = C, Instrument = best.var,
                          Correlation = best.cor)
  }
  if (write == TRUE) {
    write.csv(Form.best.inst,
              paste("output/Best price instruments by ", name.by,
                    ".csv", sep = ""),
              row.names = FALSE)
  }
  Form.best.inst
}

##############################################################################
forgraph <- function(countries, cols, types){
    # Creates data frame with number reflecting colours and line types
    # countries: vector of countries
    # cols: numeric, number of different colours
    # types: numeric, number of different line types
    N <- length(countries)
    a1 <- rep(1:cols, each = types, length.out = N)
    a2 <- rep(1:types, length.out = N)
    data.frame(col = a1, type = a2)
}
################################################################################
formInstruments <- function(file.name, data, inst.name) {
  # Function for instruments formation.
  # Args:
  #   file.name: name of file with instruments from
  #              20initial_analysis.R script;
  #   data: data frame with all data;
  #   inst.name: colnames of a new data. If ncol > 1, then 1, 2,
  #   ... are added accordingly.
  #
  # Returns:
  #  p.inst: data frame with instruments.
  #
  instruments <- read.csv(file.name)
  p.inst <- foreach(C = unique(instruments$Country), .combine = rbind) %do% {
    inst.C <- subset(instruments, Country %in% C)$Instrument
    gg <- foreach(cc = inst.C, .combine = cbind) %do% {
      ff <- drop.levels(subset(data[, c("Country", "Year", "rtecereals.p")], Country
                               %in% cc))
      return(ff$p)
    }
    colnames(gg) <- paste(inst.name, 1:length(inst.C), sep = "")
    gg <- data.frame(Country = C, Year = unique(data$Year), gg)
  }
}

################################################################################
formInstruments2 <- function(data, name.by) {
  # Function for instruments formation.
  # Args:
  #   file.name: name of file with instruments from
  #              20initial_analysis.R script;
  #   data: data frame with all data;
  #   inst.name: colnames of a new data. If ncol > 1, then 1, 2,
  #   ... are added accordingly.
  #
  # Returns:
  #  p.inst: data frame with instruments.
  #
  instruments <- finalIVs(data, name.by)
  p.inst <- foreach(C = unique(instruments$Country), .combine = rbind) %do% {
    inst.C <- subset(instruments, Country %in% C)$Instrument
    gg <- foreach(cc = inst.C, .combine = cbind) %do% {
      ff <- drop.levels(subset(data[, c("Country", "Year", "rtecereals.p")], Country
                               %in% cc))
      return(ff$rtecereals.p)
    }
    colnames(gg) <- paste( paste(name.by, "inst", sep="."), 1:length(inst.C), sep = "")
    gg <- data.frame(Country = C, Year = unique(data$Year), gg)
  }
  merge(data, p.inst)
}

################################################################################
getdata <- function(data.input, cap, case, xrate = "exchange",
                    balanced = FALSE,
                    quant = c("rtecereals.q", "hes", "fruits.q",
                    "vegetables.q", "eggs.q", "meat.q", "fish.q"),
                    price = c("rtecereals.p", "bread.p", "milk.p", "butter.p",
                    "cheese.p", "hot.cereals.p", "yoghurt.p"),
                    income = c("adi", "consum", "gdp", "gdp.ppp",
                    "consum.bread.cereal", "consum.food"),
                    indexclass = c("kof.econ.glob", "kof.act.flows",
                    "kof.restr", "kof.soc.glob", "kof.pers.cont",
                    "kof.info.flows", "kof.cult.prox", "kof.pol.glob",
                    "kof.overall.glob", "mpa", "gini", "hdi", "op",
                    "daily.cal", "smoking", "life.exp",
                    "expen.health", "female.emp.rate"),
                    exception = "Argentina",
                    regions = NULL,
                    name_inst = "../instruments/output/allivs.csv",
                    fillNA = FALSE) {
  # Function for final data frame formation.
  # Args:
  #   data.input: input data frame with data from 001readdata.R;
  #   cap: logical variable for data to be counted per capita or not;
  #   case: posible "cpi", "food", "specific" or "ppp" - variable by
  #         which prices are divided;
  #   xrate: character. echange of ppp
  #   balanced: logical, if TRUE, balanced data is returned;
  #   quant: names of quantity variables;
  #   price: names of price variables;
  #   income: names of income variables;
  #   indexclass: names of index variables;
  #   exception: names of countries which have cpi only.
  #   regions: a list with custom region names and country vectors for each
  #   region (to form custom dummy region variables)
  #   name_inst: character. Path of Instruments' file
  #
  # Returns:
  #   result: data frame with data and instruments.
  #
  chosen <- choosedt(data.input, cap = cap, case = case, xrate = xrate,
                     price = price, income = income,
                     indexclass = indexclass, exception = exception)
  # add instruments
  allivs <- read.csv(name_inst)
  with.inst <- merge(chosen, allivs, all.x = T)
  if (fillNA) with.inst <- fillAllNAs(with.inst)
  with.inst$trend <- with.inst$Year - unique(with.inst$Year)[1] + 1

  result <- with.inst

  if (!is.null(regions)) {
    dummies <-  adddummies(with.inst, regions)
    result <- merge(with.inst, dummies)
  }

  return(result)
}

################################################################################
graphs <- function(obj, data, inverse = function(x) x,
                   fit.method = c("insample", "dynamic")){

  # Plots graphs of pgmm residuals and fitted values
  #
  # Args:
  #   obj: object of pgmm.
  #
  # Returns:
  #  list of 3 elements, which are graphs.
  #
  ## res <- obj$residuals
  ## fit <- obj$fitted.values

  ## m.res <- do.call("cbind", res)
  ## actual <- fit + m.res
  ## melted.res <- melt(m.res)
  ## melted.fit <- melt(fit)
  ## melted.act <- melt(actual)
  ## act.fit <- merge(melted.act, melted.fit, by = c("X1", "X2"))
  ## names(act.fit) <- c("Year", "Country", "Actual", "Fitted")
  ## names(melted.res) <- c("Year", "Country", "Residuals")
  actual <- obj$data
  actual <- ldply(actual,
                  function(l)data.frame(Year =
                                        as.numeric(rownames(l)),
                                        Actual = inverse(l[,1])))
  colnames(actual)[1] <- "Country"
  if(fit.method == "insample") fit <- fitted(obj, output="pdata.frame",
                                        inverse = inverse)
  if(fit.method == "dynamic") {
       index <- obj$index
       horizon <- as.character(unique(index[, 2])[-c(1, 2)])
       index <- index[index[, 2] %in% horizon, ]
       fit <- forecast(obj, data = data, output = "pdata.frame",
                       inverse = inverse, horizon = horizon,
                       index=colnames(index))
  }
  colnames(fit)[3] <- "Fitted"
  act.fit <- merge(actual, fit, by = c("Country", "Year"))
  res <- act.fit[, "Actual"] - act.fit[, "Fitted"]
  melted.res <- cbind(act.fit[,c(1,2)], Residuals = res)
  # Prepare for qplot actual and fitted values
  prep.af <- melt(act.fit, id = 1 : 2)
  # Dividing into groups
  all <- groups(unique(prep.af$Country))
  graph.af <- foreach(part = names(all)) %do%{
                group <- all[[part]]
                af <- subset(prep.af, prep.af$Country %in% group)
                qplot(x = Year, y = value, data = af,
                      geom = "line",
                      group = variable, colour = variable) +
                      scale_colour_manual(value = c("black", "red")) +
                      facet_wrap( ~ Country, ncol = 4, scales = "free_y")
              }
  # Plot residuals
  graph.res <- foreach(part = names(all)) %do%{
                group <- all[[part]]
                res <- subset(melted.res, melted.res$Country %in% group)
                qplot(x = Year, y = Residuals, data = res,
                      geom = "line", colour = "residuals")+
                      scale_y_continuous("Value") +
                      scale_colour_manual("Legend", c("residuals" = "red"))                      + facet_wrap( ~ Country, ncol = 4, scales = "free_y")
               }
 return(list(act.fit = graph.af,
              residuals = graph.res))
}

################################################################################
graphsforElast <- function(elast){
  # Function for graphs of elasticities.
  # Args:
  #   elast - a data.frame with elasticities.
  VAR <- colnames(elast)[-c(1,2)]
  CN <- as.character(unique(elast$Country))
  for.sort <- foreach(cn = CN, .combine = rbind) %do% {
    data.cn <- elast[elast$Country == cn, ]
    mean.elast <- apply(data.cn[, -c(1, 2)], 2, mean)
    mean.elast
  }
  rownames(for.sort) <- NULL
  for.sort <- data.frame(CN, for.sort)
  colnames(for.sort) <- c("Country", VAR)
  elast <- melt(elast, id = c("Country", "Year"))
  foreach(var = VAR) %do% {
    data.graph <- elast[elast$variable == var, ]
    CN.sorted <- as.character(data.frame(sort_df(for.sort, vars = var))$Country)
    all <- groups(CN.sorted[length(CN.sorted):1])
    foreach(part = names(all)) %do% {
      group <- all[[part]]
      data.plot <- data.graph[data.graph$Country %in% group, ]
      data.plot$Country <- factor(data.plot$Country)
      cn <- levels(data.plot$Country)
      sub <- for.sort[for.sort$Country %in% group, c(1, 2)]
      table <- data.frame(cn, paste(as.character(sub$Country),
                                    round(sub[, 2], 3), sep = "-"))
      data.plot$Country <- change.levels(data.plot$Country, table)
      qplot(Year, value, data = data.plot, geom = "line", colour = Country,main = var )
    }
  }
}

################################################################################
groups <- function(x) {
  # Divides x into groups
  # Args:
  #    x - a vector to be divided.
  # Returns:
  #      a list consisting of groups.
  len.whole <- length(x)
  len.part <- len.whole/4
  if(len.part < 8)len.part <- len.whole/3
  if(len.part < 8)len.part <- len.whole/2
  if(len.part < 8) len.part <- len.whole
  len.part <- ceil(len.part)
  if(len.part == 12)len.part <- 13
  len.group <- ceil(len.whole/len.part)
  all <- list()
  for(i in 1:len.group) {
    all[[i]] <- if(i*len.part <= len.whole)
                  x[((i - 1)*len.part + 1) : (i*len.part)]
                else x[((i-1)*len.part + 1) : len.whole]
  }
  names(all) <- c(1:len.group)
  all
}

################################################################################
fitted.pgmm <- function(object, output=c("pseries", "pdata.frame"),
                        inverse = function(x) x,...) {
    # Extracts fitted values based on dynamic panel regression object;
    #
    #   Args:
    # object: an object of the class 'pgmm';
    # output: how the result is presented.
    #         If output="pseries" the fitted values are returned as
    #         a pseries object with index of data supplied to pgmm.
    #         If output="pdata.frame" then pdata.frame is returned,
    #         with first two columns representing the index of data
    #         supplied to pgmm, and the third column the fitted values.



    output <- match.arg(output)

    if(object$args$model == "twosteps") coeffs <- object$coefficients[[2]]
    else coeffs <- object$coefficients

    if(object$args$effect == "twoways") {
        notd <- length(object$arg$namest)
        ncoeff <- length(coeffs)
        td <- diag(1, notd)
        rownames(td) <- object$arg$namest

        prodXc <- mapply(function(x) {
            X <- cbind(x[, -1], matrix(NA, nrow=nrow(x), ncol=notd))
            tdX <- intersect(rownames(x), rownames(td))
            X[tdX, ncoeff-notd:1 + 1] <- td[tdX, ]
            X <- diff(X)
            crossprod(t(X), coeffs)
        }, object$data)
    }
    else {
        prodXc <- mapply(function(x)crossprod(t(diff(x[, -1])),
            coeffs), object$data, SIMPLIFY = FALSE)
    }

    fit <- mapply(function(x, y){
        yy <- y[rownames(x), 1, drop=FALSE]
        yy <- rbind(NA, yy[-nrow(yy), 1, drop=FALSE])
        r <- matrix(NA, nrow=nrow(y), ncol=1)
        rownames(r) <- rownames(y)
        r[rownames(x), 1] <- x + yy
        r
    },prodXc, object$data, SIMPLIFY = FALSE)

    result <- ldply(fit, function(l)data.frame(time=rownames(l), value=l[, 1]))

    index <- colnames(object$index)
    colnames(result)[1:2] <- index
    result <- pdata.frame(result)
    result[, 3] <- inverse(result[, 3])

    if(output == "pseries") result <- result[, 3]

    result
}

################################################################################
fittedEffect.pgmm <- function(object, output=c("pseries", "pdata.frame"),
                        inverse = function(x) x,...) {
    # Extracts fitted values based on dynamic panel regression object;
    #
    #   Args:
    # object: an object of the class 'pgmm';
    # output: how the result is presented.
    #         If output="pseries" the fitted values are returned as
    #         a pseries object with index of data supplied to pgmm.
    #         If output="pdata.frame" then pdata.frame is returned,
    #         with first two columns representing the index of data
    #         supplied to pgmm, and the third column the fitted values.
    # details: the main difference between fitted.pgmm and
    #          fittedEffect.pgmm is that the fit is calculated for levels
    #          directly, not for differences, as fitted.pgmm does.
    #          The unobserved effect is estimated using the formula:
    #          \hat{c}_i = \bar{y} - \hat{\beta}\bar{x}.


    output <- match.arg(output)

    if(object$args$model == "twosteps") coeffs <- object$coefficients[[2]]
    else coeffs <- object$coefficients

    if(object$args$effect == "twoways") {
        notd <- length(object$arg$namest)
        ncoeff <- length(coeffs)
        td <- diag(1, notd)
        rownames(td) <- object$arg$namest

        prodXc <- mapply(function(x) {
            X <- cbind(x[, -1], matrix(NA, nrow=nrow(x), ncol=notd))
            tdX <- intersect(rownames(x), rownames(td))
            X[tdX, ncoeff-notd:1 + 1] <- td[tdX, ]
            #X <- diff(X)
            crossprod(t(X), coeffs)
        }, object$data)
    }
    else {
        prodXc <- mapply(function(x)crossprod(t(x[, -1]), coeffs),
                         object$data, SIMPLIFY = FALSE)
    }

     if(object$args$effect == "twoways") {
      c_i<- mapply(function(x){
         X <- cbind(x, matrix(NA, nrow=nrow(x), ncol=notd))
         tdX <- intersect(rownames(x), rownames(td))
         X[tdX, ncoeff-notd:1 + 2] <- td[tdX, ]
         data.mean <- apply(X, 2, function(l) mean(l, na.rm = TRUE))
         y.mean <- data.mean[1]
         prodX <- crossprod(data.mean[-1], coeffs)
         y.mean - prodX
       }, object$data, SIMPLIFY = FALSE)
    }
    else {
      c_i<- mapply(function(x){
         data.mean <- apply(x, 2, function(l) mean(l, na.rm = TRUE))
         y.mean <- data.mean[1]
         prodX <- crossprod(data.mean[-1], coeffs)
         y.mean - prodX
       }, object$data, SIMPLIFY = FALSE)
    }
    fit <- mapply(function(x, y){
            x + as.numeric(y)
           },prodXc, c_i, SIMPLIFY = FALSE)


    result <- ldply(fit, function(l)data.frame(time=rownames(l), value=l[, 1]))

    index <- colnames(object$index)
    colnames(result)[1:2] <- index
    result <- pdata.frame(result)
    result[, 3] <- inverse(result[, 3])

    if(output == "pseries") result <- result[, 3]

    result
}

################################################################################
makeTable <- function(lvls, table){
  # Makes a table for changing country names according to conventions (table is used     by function change.levels())
  #
  # Args:
  #   lvls : levels of the object for which names need to be changed
  #   table: table with two columns: 'badnames' and 'goodnames', according to which n             ames will be changed
  #
  table1 <- subset(table, badname %in% lvls)
  left <- setdiff(lvls, table1[, "badname"])
  table2 <- data.frame(badname = c(as.character(table1[, "badname"]), left),
                       goodname = c(as.character(table1[, "goodname"]), left))
  table2
  }

################################################################################

mape <- function(o, res){
  # Calculates MAPE.
  #
  # Args:
  #   o - an observed data;
  #   res - residuals.
  #
  o <- as.numeric(as.character(o))
  temp <- data.frame(100 * abs((res) / o))
  if (all(is.na(temp))) {
    MAPE <- NA
  } else {
    if (any(na.omit(temp) == Inf | na.omit(temp) == -Inf)) {
      warning("Division by zero!!!")
      temp[which(temp == Inf | temp == -Inf), ] <- NA
    }
    MAPE <- mean(temp, na.rm = TRUE)
  }
  names(MAPE) <- "MAPE"
  MAPE
}

################################################################################
MAPE <- function(model, inverse = function(x) x, data,
                 fit.method = c("insample", "dynamic")){
  # Calculates MAPE for a given model.
  #
  # Args:
  #   model - an object of the class "pgmm".
  #
  index <- model$index
    if(fit.method == "insample") fit <- fitted(model, output="pdata.frame",
                                        inverse = inverse)
    if(fit.method == "dynamic") {
       horizon <- as.character(unique(index[, 2])[-c(1, 2)])
       index <- index[index[, 2] %in% horizon, ]
       fit <- forecast(model, data = data, output = "pdata.frame",
                       inverse = inverse, horizon = horizon,
                       index=colnames(index))
    }
  data.model <- model$data
  fit.split <- split(as.data.frame(fit), index[[1]])
  time.split <- split(index[[2]], index[[1]])
  fit.split <- mapply(
                 function(x, y){
                   rownames(x) <- y
                   x
                 }
                 , fit.split, time.split, SIMPLIFY = FALSE)
  res <- mapply(
          function(x, y){
             if(fit.method == "dynamic") y <- y[rownames(y) %in% horizon, ]
             o <- inverse(y[,1])
             fit <- x[, 3]
             res <- o - fit
             mape(o, res)
           }, fit.split, data.model)
  res <- data.frame(Country = unique(index[, 1]), mape = res)
  rownames(res) <- NULL
  res
}

################################################################################
mase <- function(o, res, naive){
  # Calculates MASE.
  #
  # Args:
  #   o - an observed data;
  #   res - residuals.
  #

  #p predicted data
  #nn <- naive
  #nn[which(is.na(p))] <- NA
  #insampleMAE<-mean(abs(o-nn), na.rm=TRUE)
  #d<-cbind(o,p,nn)

  insampleMAE <- mean(abs(o - naive), na.rm = TRUE)
  if (insampleMAE == 0)
    stop("Division by zero!!!")
  d <- cbind(res, naive)
  MASE <- mean(abs((d[, "res"]) / insampleMAE), na.rm = TRUE)
  names(MASE) <- "MASE"
  MASE
}

################################################################################
MASE <- function(model, inverse = function(x) x, data,
                 fit.method = c("insample", "dynamic")) {
  # Calculates MASE for a given model.
  #
  # Args:
  #   model - an object of the class "pgmm".
  #
  index <- model$index
    if(fit.method == "insample") fit <- fitted(model, output="pdata.frame",
                                        inverse = inverse)
    if(fit.method == "dynamic") {
       horizon <- as.character(unique(index[, 2])[-c(1, 2)])
       index <- index[index[, 2] %in% horizon, ]
       fit <- forecast(model, data = data, output = "pdata.frame",
                       inverse = inverse, horizon = horizon,
                       index=colnames(index))
    }
  data.model <- model$data
  fit.split <- split(as.data.frame(fit), index[[1]])
  time.split <- split(index[[2]], index[[1]])
  fit.split <- mapply(
                 function(x, y){
                   rownames(x) <- y
                   x
                 }
                 , fit.split, time.split, SIMPLIFY = FALSE)
  res <- mapply(
           function(x, y){
             if(fit.method == "dynamic") y <- y[rownames(y) %in% horizon, ]
             o <- inverse(y[,1])
             fit <- x[, 3]
             res <- o - fit
             naive <- o
             naive[1] <- NA
             naive[2 : length(o)] <- o[1 : (length(o) - 1)]
             mase(o, res, naive)
           }, fit.split, data.model)
  res <- data.frame(Country = unique(index[, 1]), mase = res)
  rownames(res) <- NULL
  res
}

################################################################################
my.summary <- function(formula, data, elast.names, name1 = NULL,
                       name2 = NULL, plot = TRUE, fit.method = "insample",
                       cross.mean,...){
  # Returns a list consisting of some information about the model:
  # 1) summary; 2) elasticities; 3) insample forecast accuracy measures mape and mase;
  #
  # Args:
  #   formula - formula to be passed to the EMpgmm function;
  #   data - the data;
  #   elast.names - a vector of characters that represent names of variables;
  #                 for which elasticities are required;
  #   name1, name2 - character. Name of the graph of model actual and fitted;
  #   plot - logical. False if you do not want to plot graphs of fitted;
  #   values and model residuals respectively;
  #   fit.method - fit method to be applied("insample" or "dynamic");
  #   corss.mean - logical indicating if the model should be estimated with
  #   interactions of averaged variables.
  #   ... - further arguments to be passed to the EMpgmm function;
  #
  if(cross.mean) data <- addMeanVar(data)
  model <- EMpgmm(formula, data = data, ...)
  E <- elast(model, elast.names, data)
  mape <- MAPE(model, inverse=exp, data, fit.method)
  mase <- MASE(model, inverse=exp, data, fit.method)
  res.summary <- summary(model, ...)
  res.summary$CoefTable <- apply(res.summary$CoefTable, 2, function(l) round(l, 4))
  RES <- list(summary = res.summary,
              elasticities.direct = E$direct,
              elasticities.indirect = E$indirect,
              forecast.accur = merge(mape, mase, by = "Country"),
              model = model)
  if (plot){
    graphObj <- graphs(model, data, inverse = exp, fit.method)
    pdf(name1, onefile = TRUE, height = 5, width = 10)
    print(graphObj$act.fit)
    dev.off()
    pdf(name2, onefile = TRUE, height = 5, width = 10)
    print(graphObj$residuals)
    dev.off()

  }
  RES
}

################################################################################

##' Computes partial R squared
##'
##' Computes partial R squared for plm and pgmm object
##' @title
##' @param mod.full an object of class plm or pgmm
##' @return
##' @author
partialR2 <- function (object, ...)
UseMethod("partialR2")

##' Computes partial R squared
##'
##' Computes partial R squared for the plm object
##' @title
##' @param mod.full an object of the class plm
##' @param vars variable names for which partial R squared are required
##' @return
##' @author
partialR2.plm <- function(mod.full,...){
    R2 <- summary(mod.full,...)$r.squared["rsq"]
    call <- mod.full$call
    form <- call$formula
    class(form) <- "formula"
    form.split <- splitFormula(form, sep = "|")
    inst <- if(length(form.split) != 1) form.split[[2]] else NULL
    form.split <- splitFormula(form.split[[1]], sep="+")
    rhs.vars <- sapply(form.split, function(x) as.character(x)[2])
    rhs.without <- sapply(rhs.vars, function(x) rhs.vars[-match(x, rhs.vars)],
                          simplify=FALSE)
    form.endo <- as.character(form[[2]])
    form.endo <- paste(form.endo, collapse="(")
    form.endo <- paste(form.endo,")", sep="")
    rhs.form <- if(!is.null(inst)) sapply(rhs.without, function(x){
                        first <- paste("~", paste(x, collapse="+"))
                        second <- paste("|", paste(inst)[2])
                        as.formula(paste(form.endo, first, second, sep=""))
                       })
                 else sapply(rhs.without, function(x){
                        first <- paste("~", paste(x, collapse="+"))
                        as.formula(paste(form.endo, first, sep=""))
                       })
    R2.vars <- lapply(rhs.form, function(x){
                       call$formula <- x
                       summary(eval(call, parent.frame()))$r.squared["rsq"]
                       })
    partialR2 <- sapply(R2.vars, function(R2_i) (R2 - R2_i)/(1 - R2_i))
    names(partialR2) <- gsub(".rsq", "", names(partialR2))

    partialR2
}

##' Computes partial R squared
##'
##' Computes partial R squared for plm and pgmm object
##' @title
##' @param mod.full an object of the class pgmm
##' @param vars variable names for which partial R squared are required
##' @return
##' @author
partialR2.pgmm <- function(mod.full,...){
    call <- mod.full$call
    R2 <- EM_R2(mod.full)
    form <- formula(mod.full)
    form.rhs <- attributes(Formula(form))$rhs
    form.split <- splitFormula(form.rhs[[1]],
                               sep = "+")
    form.inst <- as.character(splitFormula(form, sep="|"))[-1]
    form.inst <- sapply(form.inst, function(x) gsub("~", "", x))
    form.inst <- paste(form.inst, collapse="|")

    form.endo <- as.character(attributes(Formula(form))$lhs[[1]])
    form.endo <- paste(form.endo, collapse="(")
    form.endo <- paste(form.endo,")", sep="")

    rhs.vars <- sapply(form.split, function(x) as.character(x)[2])
    rhs.without <- sapply(rhs.vars, function(x) rhs.vars[-match(x, rhs.vars)],
                          simplify=FALSE)
    rhs.form <- lapply(rhs.without, function(x){
                exo <- paste("~", paste(x, collapse="+"))
                exo.inst <- paste(exo, form.inst, sep="|")
                as.formula(paste(form.endo, exo.inst, sep=""))
               })

    R2.vars <- lapply(rhs.form, function(x){
                           call$formula <- x
                           model <- try(eval(call, parent.frame()))
                           if(class(model)[1] != "try-error") EM_R2(model)
                           else NA
                      })

    partialR2 <- sapply(R2.vars, function(R2_i) (R2 - R2_i)/(1 - R2_i))

    partialR2
}

################################################################################
problematic <- function(data){
   # Check data for negative, zero and NA values.
   # Arguments:
   #  data : data.frame where first columns is Country, second -
   #                    Year and the rest colnames are variables.
   # Return:
   #  res: data.frame with Country, Indicator, Problem and Year.
   #       Problem contains "Empty", "Has na", "Has 0" and "Has < 0"
   #
  res <- foreach(cc = colnames(data)[!(colnames(data) %in% c("Country",
                   "Region", "Year"))], .combine = rbind) %do%{
    indicator <- data[, c("Country", "Year", cc)]
    prob <- foreach(C = unique(indicator$Country), .combine = rbind) %do% {
      gg <- subset(indicator, Country %in% C)

      if(all(is.na(gg[,ncol(gg)]))) {
        empty <- data.frame(Country = C, Indicator = cc,
                            Problem = "Empty", Year = NA)
      } else {
        empty <- data.frame(Country = NULL, Indicator = NULL,
                              Problem = NULL, Year = NULL)
      }

      has.zero <- data.frame(Country = NULL, Indicator = NULL,
                               Problem = NULL, Year = NULL)
      has.na <- data.frame(Country = NULL, Indicator = NULL,
                             Problem = NULL, Year = NULL)
      has.negative <- data.frame(Country = NULL, Indicator = NULL,
                                 Problem = NULL, Year = NULL)

      if (nrow(empty) < 1){
        gg1 <- na.omit(gg)
        if (any(gg1[,ncol(gg1)] == 0))
          has.zero <- data.frame(Country = C, Indicator = cc,
                                 Problem = "Has 0", Year =
                                 gg1[gg1[,ncol(gg1)] == 0,]$Year)

        if (any(is.na(gg[,ncol(gg)])))
          has.na <- data.frame(Country = C, Indicator = cc,
                               Problem = "Has na", Year =
                               gg[is.na(gg[,ncol(gg)]),]$Year)

        if (any(gg1[,ncol(gg1)] < 0)&&cc!="Inflation")
          has.negative <- data.frame(Country = C, Indicator = cc,
                                     Problem = "Has < 0", Year =
                                     gg1[gg1[,ncol(gg1)] < 0,]$Year)

      }
      return(rbind(empty, has.zero, has.na, has.negative))
    }
  }
  return(res)
}

################################################################################
res <- function(obj, data, inverse = function(x) x,
                fit.method = c("insample", "dynamic"), diff.res = FALSE) {
  # Returns residuals of a given pgmm object;
  #
  # Args:
  #  obj: an objec generated by EMpgmm function;
  #  inverse: transformation to be done;
  actual <- obj$data
  actual <- ldply(actual, function(l)data.frame(Year =
                                                as.numeric(rownames(l)),
                                                Actual =
                                                inverse(l[,1])))

  colnames(actual)[1] <- "Country"
  if(fit.method == "insample") fit <- fitted(obj, output = "pdata.frame",
                                        inverse = inverse)
  if(fit.method == "dynamic") {
       index <- obj$index
       horizon <- as.character(unique(index[, 2])[-c(1, 2)])
       index <- index[index[, 2] %in% horizon, ]
       fit <- forecast(obj, data = data, output = "pdata.frame",
                       inverse = inverse, horizon = horizon,
                       index = colnames(index))
  }
  colnames(fit)[3] <- "Fitted"
  act.fit <- merge(actual, fit, by = c("Country", "Year"))
  res <- act.fit[, "Actual"] - act.fit[, "Fitted"]
  resid <- data.frame(Country = act.fit[, "Country"], Year = act.fit[, "Year"],
             res = res)
  if (diff.res) {
    res.diff <- foreach(C = unique(resid$Country), .combine = rbind.fill) %do%{
      x.ts <- ts(subset(resid, Country == C)[,"res"],
                         start = unique(resid$Year)[1], freq = 1)
      x.diff <- diff(x.ts)
      x.df <- data.frame(Country = C, Year = time(x.diff),
                         res = x.diff)
      return(x.df)
    }
    resid <- res.diff
  }
  return(resid)
}

################################################################################
resCorr <- function(resi, var, data){
  # Calculates correlations between residuals and certain variables
  #
  # Args:
  #   resi: a vector of residuals
  #   var : names of the variables for which correlations are to be
  #        computed
  #   data: name of the data.frame from wich variables in 'var' should
  #         be taken
  #
  # Returns:
  #   a data.frame with correlations
  #
  cor <- foreach(n = var, .combine = cbind) %do% {
    if (grepl(":", n)) {
      sep <- unlist(strsplit(n, split = ":"))
      dt <- foreach (i = 1:2, .combine = cbind) %do% {
        if (grepl("log", sep[i], fixed = TRUE)) {
          name <- unlist(strsplit(unlist(strsplit(sep[i],
                                                  split =
                                                  "log\\("))[2],
                                  split = "\\)"))[1]
          dt <- log(data[, name])
          if (grepl("lag", sep[i], fixed = TRUE)) {
            dt = lag(dt, as.numeric(strsplit(unlist(strsplit(sep[i],
                                                           split=","))[2],
              split = "\\)")))
          } else {
            dt
          }
        } else {
          name <- sep[i]
          dt <- data[, name]
          if (grepl("lag", sep[i], fixed = TRUE)) {
            dt =
              lag(dt, as.numeric(strsplit(unlist(strsplit(sep[i],
                                                          split=","))[2],
                                          split = "\\)")))
          } else {
            dt
          }
        }
      }
      new <- dt[, 1] * dt[, 2]
      corr <- cor(resi, new, use = "pairwise.complete.obs")
    } else {
      dt <- NULL
      if (grepl("log", n, fixed = TRUE)) {
        name <- unlist(strsplit(unlist(strsplit(n, split = "log\\("))[2],
                                split = "\\)"))[1]
        dt <- log(data[, name])
        if (grepl("lag", n, fixed = TRUE)) {
          dt = lag(dt, as.numeric(strsplit(unlist(strsplit(n,
            split=","))[2], split = "\\)")))
        } else {
          dt
        }
      } else {
        name <- n
        dt <- data[, name]
        if (grepl("lag", n, fixed = TRUE)) {
          dt = lag(dt, as.numeric(strsplit(unlist(strsplit(n,
            split=","))[2], split = "\\)")))
        } else {
          dt
        }
      }
      corr <- cor(resi, dt, use = "pairwise.complete.obs")
    }
  }
  cor <- as.data.frame(cor)
  colnames(cor) <- var
  rownames(cor) <- "residuals"
  return(cor)
}

##############################################################################
revert <- function(data.f) {
    if (length(colnames(data.f))==0) {
        reverted <- rev(data.f)
    } else {
    reverted <- foreach(cc = colnames(data.f), .combine = cbind) %do% {
        rev(data.f[,cc])
    }
    }
}
################################################################################
scenario <- function(obj, data1, data2){
    # Plot graphs of two data frames
    # Args:
    # obj: pgmm output.
    # data1, data2 - data.frames. data1 is original dataframe. data2 is changed
    # (new) dataframe.
    # Return: graph.
  frct.old <- forecast(obj, data=data1, horizon=2000:2010, inverse=exp,
              output="pdata.frame")
  frct.new <- forecast(obj, data=data2, horizon=2000:2010, inverse=exp,
              output="pdata.frame")
  colnames(frct.old)[3] <- "oldfit"
  colnames(frct.new)[3] <- "newfit"
  both <- merge(frct.new, frct.old)
  m.both <- melt(both, id = 1:2)
  all <- groups(unique(m.both$Country))

  graph <- foreach(part = names(all)) %do%{
              group <- all[[part]]
              dt <- subset(m.both, m.both$Country %in% group)
              qplot(x = Year, y = value, data = dt,
                    geom = "line",
                    group = variable, colour = variable) +
                    scale_colour_manual(name = "Legend",
                                        value = c("red", "black"),
                                        breaks = c("newfit", "oldfit"),
                                        labels = c("New data", "Original")) +
                    facet_wrap( ~ Country, ncol = 4, scales = "free_y")
              }
  return(graph)
}

################################################################################
sort.cn <- function(data, by = "u"){
    # sort countries of data (first and second columns: Country and Year)
    # by by column values.
    for.sort <- foreach(cn = unique(data$Country), .combine = rbind) %do% {
    dt.cn <- data[data$Country == cn, ]
    mean.cn <- apply(dt.cn[, -c(1, 2)], 2, mean)
    mean.cn
    }
    rownames(for.sort) <- NULL
    for.sort <- data.frame(unique(data$Country), for.sort)
    colnames(for.sort) <- c("Country", colnames(data)[c(3,4)])
    sorted.cn <- rev(sort_df(for.sort, vars = by)$Country)
    all <- groups(sorted.cn)
}

################################################################################
stand <- function(x){
 # Divide column value of data frame x by its first value
    if (!(all(is.na(x$value)))){
     x$value <- x$value/na.omit(x$value)[1]
 } else x$value <- x$value
     x
}

################################################################################
statsNeeded <- function(data, vars){
# Returns 5 number summary and also a sd and a number of NAs of every variable in var
#
# Args:
#   data: data frame
#   vars: names of variables in a data frame for which summary is needed
summaries <- foreach(i = 1:length(vars), .combine = rbind) %do% {
  name <- vars[i]
  stat <- round(summary(na.omit(data[, name])), 3)
  sd <- round(sd(na.omit(data[, name])), 3)
  nas <- sum(is.na(data[, name]))
  final <- c(stat, sd = sd, NAs = nas)
  }
rownames(summaries) <- vars
summaries
}

################################################################################
##' Calculate total, within and between variations of variables.
##' Within and between shares are obtained by division of sum of squares
##' Variance is not estimated.
##' @param dt - data.frame. Panel data frame with columns: Country, Year,
##' variables....
##' @param x - vector. Variable data, taken from dt type data.frame.
##' @return data.frame with composition of variation.
##' @author
varcomposition <- function(dt, x){

total <- sum((x - rep(mean(x, na.rm=TRUE), length.out=length(dt$Year)))^2,
             na.rm=TRUE)

within <- sum((x-rep(tapply(x, dt[,"Country"],
                              function(ll) mean(ll, na.rm=TRUE)),
                       each=length(unique(dt$Year))))^2, na.rm=TRUE)

each <- as.vector(tapply(x, dt[,"Country"], function(ll) length(na.omit(ll))))
between <- sum((rep(tapply(x, dt[,"Country"],
                           function(ll) mean(ll, na.rm=TRUE)),
                    times = each)-
                rep(mean(x, na.rm=TRUE), length.out=sum(each)))^2,
               na.rm=TRUE)

data.frame(total = total, within = 100*within/total,
           between = 100*between/total)
}


