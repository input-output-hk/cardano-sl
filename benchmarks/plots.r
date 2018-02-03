#
# call: Rscript ../plots.r 2018-02-03_053826
# (curr. workdir is where the run-???.csv is)
#
# packages to install: dplyr, ggplot2, svglite, gplots, anytime
# (e.g. install.packages('svglite'))
#

# if run in RStudio one can select the csv in a file dialogue
INTERACTIVE <- FALSE

library(dplyr)
library(ggplot2)
library(gplots)
library(anytime)

if (INTERACTIVE) {
  #library('ggedit')    # only if interactive editing
  fnames <- file.choose()
  fname <- fnames[1]
  RUN <- sub('.*run-([-_0-9]+).csv', '\\1', fname)
  bp <- dirname(fname)
  fname2 <- paste(bp, '/report_', RUN, '.txt', sep='')
  fname3 <- paste(bp, '/bench-settings', sep='')
} else {
  args = commandArgs(trailingOnly=TRUE)
  RUN <- args[1]
  fname <- paste('run-', RUN, '.csv', sep='')
  fname2 <- paste('report_', RUN, '.txt', sep='')
  fname3 <- 'bench-settings'
}

DESC=''                    # Add custom text to titles
k <- 6                     # Protocol parameters determining
SLOTLENGTH <- 20           # the length of an epoch
EPOCHLENGTH <- 10*k*SLOTLENGTH

# names of relay nodes
relays <- c('r-a-1', 'r-a-2', 'r-a-3'
          , 'r-b-1', 'r-b-2'
          , 'r-c-1', 'r-c-2'
          , 'u-a-1', 'u-b-1', 'u-c-1'
            )

# prep and read in benchmark parameters
system(paste("sed -e 's/, /\\\n/g' ", fname3, " > ", fname3, "2", sep=''))
params <- read.csv(paste(fname3, "2", sep=''), header=FALSE, 
                   col.names = c("parameter","value"), sep="=",
                   stringsAsFactor=FALSE)

maxparam <- length(params[,2])
if (params[maxparam,1]=="systemstart") {
  params[maxparam+1,1] <- "time UTC"
  params[maxparam+1,2] <- paste("", anytime(as.numeric(params[maxparam,2]), tz="UTC"), sep='')
}

# read the report of the benchmark run
readReport <- function(filename, run=RUN) {
  print(paste('reading data from', filename, sep=' '))
  report <- read.table(filename, sep=':', nrows=3, skip=1, col.names = c("desc", "count"))
  return (report);
}

# read and pre-process data from a benchmark run
readData <- function(filename, run=RUN) {
    #filename <- paste('run-', run, '.csv', sep='')
    print(paste('reading data from', filename, sep=' '))
    data <- read.csv(filename)
    t0 = min(data$time)
    # time after start of experiment, in seconds
    data$t <- (data$time - t0) %/% 1e6
    data$clustersize <- as.factor(data$clustersize)
    data$concF <- as.factor(data$conc)
    data$delayF <- as.factor(data$delay)
    data <- data[!(data$txType %in% c('failed')),]
    data$endT <- ave(data$t, data$run, FUN=max)
    data$isRelay <- data$node %in% relays
    data$isRelay <- as.factor(data$isRelay)
    levels(data$isRelay) <- c('Core Nodes', 'Relay Nodes')

    # make the labels shorter
    levels(data$txType)[levels(data$txType)=="mp_ProcessTransaction_Modify"] <- "hold tx"
    levels(data$txType)[levels(data$txType)=="mp_ProcessTransaction_Wait"] <- "wait tx"
    levels(data$txType)[levels(data$txType)=="mp_ApplyBlock_Modify"] <- "hold block"
    levels(data$txType)[levels(data$txType)=="mp_ApplyBlock_Wait"] <- "wait block"
    levels(data$txType)[levels(data$txType)=="mp_ApplyBlockWithRollback_Modify"] <- "hold rollback"
    levels(data$txType)[levels(data$txType)=="mp_ApplyBlockWithRollback_Wait"] <- "wait rollback"
    levels(data$txType)[levels(data$txType)=="mp_ApplyBlock_SizeAfter"] <- "size after block"
    levels(data$txType)[levels(data$txType)=="mp_ApplyBlockWithRollback_SizeAfter"] <- "size after rollback"
    levels(data$txType)[levels(data$txType)=="mp_ProcessTransaction_SizeAfter"] <- "size after tx"

    return(data)
}

# dashed vertical lines at the epoch boundaries
epochs <- function(d) {
    epochs <- data.frame(start=seq(from=0, to=max(d$t), by=EPOCHLENGTH))
    geom_vline(data=epochs,
               aes(xintercept = start, alpha=0.65)
             , colour='red'
             , linetype='dashed'
               )
    }

# dotted vertical lines every k slots
kslots <- function(d) {
    kslots <- data.frame(start=seq(from=0, to=max(d$t), by=k*SLOTLENGTH))
    geom_vline(data=kslots,
               aes(xintercept = start, alpha=0.40)
             , colour='red'
             , linetype='dotted'
               )
    }


# histograms the rate of sent and written transactions
histTxs <- function(d, run=RUN, desc=DESC) {
  dd <- d %>%
    filter(txType %in% c("submitted", "written"))
  maxtx <- max(dd$txCount)
  crit <- "submitted"
  dd <- d %>%
    filter(txType %in% c(crit))
#  ggplot(dd, aes(txCount)) + stat_ecdf(geom="step") +
#      ggtitle(paste(crit, 'transactions', desc, sep = ' ')) +
#      ylab("") + xlab(paste("tx", crit, sep=" "))

#  qqplot(x=1:length(dd$txCount), y=dd$txCount, xlab="", ylab="tx/slot", main=paste(crit, 'transactions', desc, sep = ' '))

  hist(dd$txCount, main=paste('transaction count per slot', desc, sep = ' ')
       , breaks=seq(0,maxtx, by=1)
       , col=gray.colors(maxtx/2)
       , xlab = crit )
  crit <- "written"
  dd <- d %>%
    filter(txType %in% c(crit))
#  ggplot(dd, aes(txCount)) + stat_ecdf(geom="step") +
#      ggtitle(paste(crit, 'transactions', desc, sep = ' ')) +
#      ylab("") + xlab(paste("tx", crit, sep=" "))

#  qqplot(x=1:length(dd$txCount), y=dd$txCount, xlab="", ylab="tx/slot", main=paste(crit, 'transactions', desc, sep = ' '))

  hist(dd$txCount, main=paste('transaction count per slot', desc, sep = ' ')
       , breaks=seq(0,maxtx, by=1)
       , col=gray.colors(maxtx/2)
       , xlab = crit )
}

# plot the rate of sent and written transactions
plotTxs <- function(d, run=RUN, desc=DESC) {
    dd <- d %>%
        filter(txType %in% c("submitted", "written"))
    ggplot(dd, aes(t, txCount/slotDuration)) +
        geom_point(aes(colour=node)) +
        geom_smooth() +
        ggtitle(paste(
            'Transaction frequency for core and relay nodes, run at '
          , run, desc, sep = ' ')) +
        xlab("t [s]") +
        ylab("transaction rate [Hz]") +
        facet_grid(txType ~ run) +
        epochs(d) + kslots(d) +
        guides(size = "none", colour = "legend", alpha = "none")
    }

# plot the mempool residency for core and relay nodes
plotMempools <- function(d, run=RUN, desc=DESC) {
    dd <- d %>%
        filter(txType %in% c("size after block"
                           , "size after rollback"
                           , "size after tx"))
    ggplot(dd, aes(t, txCount)) +
        geom_point(aes(colour=node)) +
        ggtitle(paste(
            'Mempool sizes for core and relay nodes, run at '
          , run, desc, sep = ' ')) +
        xlab("t [s]") +
        ylab("Mempool size [# of transactions]") +
        facet_grid(txType ~ isRelay, scales= "free", space = "free") +
        epochs(d) +
        guides(size = "none", colour = "legend", alpha = "none")
}

# plot the wait and hold times for the local state lock
plotTimes <- function(d, run=RUN, desc=DESC) {
    dd <- d %>%
            filter(txType %in% c("hold tx"
                               , "wait tx"
                               , "hold block"
                               , "wait block"
                               , "hold rollback"
                               , "wait rollback"
                                 ))
        #dd$t <- dd$t %/% 1000
    ggplot(dd, aes(t, txCount)) +
        geom_point(aes(colour=node)) +
        ggtitle(paste(
            'Wait and work times for core and relay nodes, run at '
          , run, desc, sep = ' ')) +
        xlab("t [s]") +
        ylab("Times waiting for/holding the lock [microseconds]") +
        scale_y_log10() +
        facet_grid(txType ~ isRelay) +
        epochs(d) +
        guides(size = "none", colour = "legend", alpha = "none")
}

plotOverview <- function(d, report, run=RUN, desc=DESC) {
  def.par <- par(no.readonly = TRUE)
  layout(mat = matrix(c(1,1,1,1,2,3,4,4,5,5,6,6), 3, 4, byrow = TRUE), heights=c(2,3,4))
  
  textplot(paste("\nBenchmark of ", run), cex = 2, valign = "top")
  bp <- barplot(as.matrix(report[,2]), col=c("green", "blue", "red"))
  textplot(report, show.rownames = FALSE)
  textplot(params, show.rownames = FALSE)
  
  histTxs(data)

  par(def.par)
  #layout(matrix(c(1,1), 1, 1, byrow = TRUE))
}

report <- readReport(fname2)
data <- readData(fname)

png(filename=paste('overview-', RUN, '.png', sep=''))
plotOverview(data, report)
dev.off()
plotOverview(data, report)

plotTxs(data)
ggsave(paste('txs-', RUN, '.svg', sep=''))
ggsave(paste('txs-', RUN, '.png', sep=''))

plotMempools(data)
ggsave(paste('mempools-', RUN, '.svg', sep=''))
ggsave(paste('mempools-', RUN, '.png', sep=''))

plotTimes(data)
ggsave(paste('times-', RUN, '.svg', sep=''))
ggsave(paste('times-', RUN, '.png', sep=''))

# example: observe only core nodes:
plotMempools(data %>% filter(!(node %in% relays)))
plotTimes(data %>% filter(!(node %in% relays)))
