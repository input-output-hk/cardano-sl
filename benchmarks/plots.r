library('dplyr')
library('ggplot2')
RUN <- '2018-02-03_053826' # Select run to plot
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

# read and pre-process data from a benchmark run
readData <- function(run=RUN) {
    filename <- paste('run-', run, '.csv', sep='')
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
               aes(xintercept = start, colour='red')
             , linetype='dashed'
               )
    }

# dotted vertical lines every k slots
kslots <- function(d) {
    epochs <- data.frame(start=seq(from=0, to=max(d$t), by=k*SLOTLENGTH))
    geom_vline(data=epochs,
               aes(xintercept = start, colour='red')
             , linetype='dotted'
               )
    }


# plot the rate of sent and written transactions
plotTxs <- function(d, run=RUN, desc=DESC) {
    dd <- d %>%
        filter(txType %in% c("submitted", "written"))
    ggplot(dd, aes(t, txCount/slotDuration)) +
        geom_point(aes(colour=node)) +
        geom_smooth() +
        ggtitle(paste(
            'Transaction frequency for the 7 core cluster, run at '
          , run, desc, sep = ' ')) +
        xlab("t [s]") +
        ylab("transaction rate [Hz]") +
        facet_grid(txType ~ run) +
        epochs(d) + kslots(d)
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
            'Mempool sizes for the 7 core cluster, run at '
          , run, desc, sep = ' ')) +
        xlab("t [s]") +
        ylab("Mempool size [# of transactions]") +
        facet_grid(txType ~ isRelay, scales= "free", space = "free") +
        epochs(d) # + kslots(d)
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
            'Wait and work times for the 7 core cluster, run at '
          , run, desc, sep = ' ')) +
        xlab("t [s]") +
        ylab("Times waiting for/holding the lock [microseconds]") +
        scale_y_log10() +
        facet_grid(txType ~ isRelay) +
        epochs(d) # + kslots(d)
    }



data <- readData()
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
