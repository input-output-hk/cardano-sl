#
# call: Rscript ../plots.r 2018-02-03_053826
# (curr. workdir is where the run-???.csv is)
#
# packages to install: dplyr, ggplot2, svglite, gplots
# (e.g. install.packages('svglite'))
#

# if run in RStudio one can select the csv in a file dialogue
INTERACTIVE <- FALSE

library(dplyr)
library(ggplot2)
library(gplots)

if (INTERACTIVE) {
  library('ggedit')    # only if interactive editing
  fnames <- file.choose()
  fname <- fnames[1]
  RUN <- sub('.*run-([-_0-9]+).csv', '\\1', fname)
  bp <- dirname(fname)
  fname2 <- paste(bp, '/report_', RUN, '.txt', sep='')
} else {
  args = commandArgs(trailingOnly=TRUE)
  RUN <- args[1]
  fname <- paste('run-', RUN, '.csv', sep='')
  fname2 <- paste('report_', RUN, '.txt', sep='')
}
#RUN <- '2018-02-03_053826' # Select run to plot

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
    geom_vline( data=epochs
              , aes(xintercept = start)
              , color="red"
              , linetype='dashed'
              )
    }

# dotted vertical lines every k slots
kslots <- function(d) {
    kslots <- data.frame(start=seq(from=0, to=max(d$t), by=k*SLOTLENGTH))
    geom_vline(data=kslots
              , aes(xintercept = start)
              , color="green"
              , linetype='dotted'
              )
    }


# histograms the rate of sent and written transactions
histTxs <- function(d, run=RUN, desc=DESC) {
#  dd <- d %>%
#    filter(txType %in% c("submitted", "written"))
#  ggplot(dd, aes(txCount)) +
#    geom_histogram(breaks=seq(0,max(dd$txCount), by=1)) +
#    ggtitle(paste(
#      'Histogram of transaction count per slot, run at '
#      , run, desc, sep = ' ')) +
#    ylab("count") +
#    xlab("tx per slot") +
#    facet_grid(txType ~ run) +
#    guides(size = "none", colour = "legend", alpha = "none")
  dd <- d %>%
    filter(txType %in% c("submitted", "written"))
  maxtx <- max(dd$txCount)
  crit <- "submitted"
  dd <- d %>%
    filter(txType %in% c(crit))
  hist(dd$txCount, main=paste('transaction count per slot', desc, sep = ' ')
       , breaks=seq(0,maxtx, by=1)
       , col=gray.colors(maxtx/2)
       , xlab = crit )
  crit <- "written"
  dd <- d %>%
  filter(txType %in% c(crit))
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

layout(matrix(c(1,3,2,4), 2, 2, byrow = FALSE))

report <- readReport(fname2)
bp <- barplot(as.matrix(report[,2]), main=paste("Benchmark result of ", RUN), col=c("green", "blue", "red"))
textplot(report, show.rownames = FALSE)
data <- readData(fname)

histTxs(data)

#ggsave(paste('hist-', RUN, '.svg', sep=''))
#ggsave(paste('hist-', RUN, '.png', sep=''))

layout(matrix(c(1,1), 1, 1, byrow = TRUE))

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
