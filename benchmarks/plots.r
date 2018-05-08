#
# call: Rscript ../plots.r 2018-02-03_053826
# (curr. workdir is where the run-???.csv is)
#
# packages to install: dplyr, ggplot2, svglite, gplots, anytime, gridExtra
# (e.g. install.packages('svglite'))
#

# if run in RStudio one can select the csv ("run-2018???.csv") in a file dialogue
INTERACTIVE <- FALSE

library(dplyr)
library(ggplot2)
library(gplots)
library(anytime)
library(grid)
library(gridExtra)

if (INTERACTIVE) {
  #library('ggedit')    # only if interactive editing
  #pdf(NULL)   # prevent PDF output
  fnames <- file.choose()
  fname <- fnames[1]
  RUN <- sub('.*run-([-_0-9]+).csv', '\\1', fname)
  bp <- dirname(fname)
} else {
  args = commandArgs(trailingOnly=TRUE)
  RUN <- args[1]
  bp <- "."
  fname <- paste('run-', RUN, '.csv', sep='')
  pdf(paste("BM-",RUN,".pdf",sep=''), onefile=TRUE, compress=TRUE, paper="a4")   # PDF output to file
}
fname2 <- paste(bp, '/report_', RUN, '.txt', sep='')
fname3 <- paste(bp, '/bench-settings', sep='')
fname4 <- paste(bp, '/times.csv', sep='')

DESC=''                    # Add custom text to titles
k <- 6 #12 #24             # Protocol parameters determining
SLOTLENGTH <- 20           # the length of an epoch
EPOCHLENGTH <- 10*k*SLOTLENGTH

# general constants which should correspond to the recording script
# these are defaults to which we fall back if they cannot be extracted from the log
#recDelay0 <- 5   # seconds
recCpuTicks0 <- 100  # `getconf CLK_TCK`
recPageSize0 <- 4096 # memory is indicated in pages

# have trx times?
hasTrxTimes <- FALSE
if (file.exists(fname4)) {
  hasTrxTimes <- TRUE
}

# names of relay nodes
relays <- c('r-a-1', 'r-a-2', 'r-a-3'
          , 'r-b-1', 'r-b-2'
          , 'r-c-1', 'r-c-2'
          , 'u-a-1', 'u-b-1', 'u-c-1'
            )

# names of unprivileged relay nodes
uRelays <- c('u-a-1', 'u-b-1', 'u-c-1')

coreNodes <- c('c-a-1', 'c-a-2', 'c-a-3', 'c-b-1', 'c-b-2', 'c-c-1', 'c-c-2')

# read in transaction times
if (hasTrxTimes) {
  times <- read.csv(fname4, header=FALSE, col.names=c("time", "cumm"))
}

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
    t0 <- min(data$time)

    # find first block created
    txWritten <- data %>% filter(txType %in% c("written")) %>% filter(txCount > 0)
    tFirstBlock <- min(txWritten$time)
    tFirstBlock <- trunc((tFirstBlock - t0) / 1000 / EPOCHLENGTH) * EPOCHLENGTH * 1000 + t0
    data <- filter(data, time >= tFirstBlock)

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

# reads os metrics from a file per node and returns dataframe with (io, statm, stat)
readOSmetrics <- function(nodename) {
  fn <- paste(bp, '/', nodename, '-ts.log', sep='')
  metrics <- {}
  if (file.exists(fn)) {
    isNewFile <- TRUE
    # add to read.csv?   stringsAsFactors=FALSE
    if (isNewFile) {
      metrics_csv <- read.csv(fn, as.is = c(TRUE), header=FALSE, skip=7, sep=" ")
    } else {
      metrics_csv <- read.csv(fn, as.is = c(TRUE), header=FALSE, skip=5, sep=" ")
    }
    mlen <- dim(metrics_csv)[1]
    colnames(metrics_csv)[1:2] <- c("time", "stats")
    metrics_csv <- cbind(nodename, metrics_csv[1:(mlen-2),])  # skip last 2 lines
    metrics_csv$stats <- as.factor(metrics_csv$stats)
    metrics_csv$time <- as.numeric(metrics_csv$time)

    metrics$io <- select(metrics_csv %>% filter(stats %in% c("io")), nodename : V9)
    colnames(metrics$io) <- c("nodename", "time", "stats", "rchar", "wchar", "syscr", "syscw", "readbytes", "writebytes", "cxwbytes")
    # convert columns from char to numeric
    metrics$io$rchar <- as.numeric(metrics$io$rchar)
    metrics$io$wchar <- as.numeric(metrics$io$wchar)
    metrics$io$syscr <- as.numeric(metrics$io$syscr)

    metrics$mstat <- select(metrics_csv %>% filter(stats %in% c("statm")), nodename : V9)
    colnames(metrics$mstat) <- c("nodename", "time", "stats", "size", "resident", "shared", "text", "lib", "data", "dt")
    # convert columns from char to numeric
    metrics$mstat$size <- as.numeric(metrics$mstat$size)
    metrics$mstat$resident <- as.numeric(metrics$mstat$resident)
    metrics$mstat$shared <- as.numeric(metrics$mstat$shared)
    #plot(metrics$statm$size)

    metrics$stat <- metrics_csv %>% filter(stats %in% c("stat"))
    colnames(metrics$stat) <- c("nodename", "time", "stats", "pid", "comm", "state", "ppid", "pgrp", "session", "tty", "tpgid", "flags", "minflt","cminflt","majflt","cmajflt", "utime", "stime", "cutime", "cstime", "priority", "nice", "nthr", "itrv", "starttime", "vsize", "rss", "rsslim", "startcode", "endcode", "startstack", "kstkesp", "kstkeip", "signal",
                                "blocked", "sigignore", "sigcatch", "wchan", "nswap", "cnswap", "exitsig", "processor", "rtprio", "policy", "delayio", "guesttime", "cguesttime", "startdata", "enddata", "startbrk", "argstart", "argend", "envstart", "envend", "exitcode")
    metrics$stat$pid <- as.factor(metrics$stat$pid)
    metrics$stat$comm <- as.factor(metrics$stat$comm)
    metrics$stat$state <- as.factor(metrics$stat$state)
    metrics$stat$ppid <- as.factor(metrics$stat$ppid)
    metrics$stat$pgrp <- as.factor(metrics$stat$pgrp)
    metrics$stat$session <- as.factor(metrics$stat$session)
    metrics$stat$tty <- as.factor(metrics$stat$tty)
    metrics$stat$tpgid <- as.factor(metrics$stat$tpgid)
    metrics$stat$priority <- as.factor(metrics$stat$priority)
    metrics$stat$starttime <- as.factor(metrics$stat$starttime)
    metrics$stat$policy <- as.factor(metrics$stat$policy)

    # kernel constants per node; read from log file once they are there
    if (isNewFile) {
      metrics_params <- read.csv(fn, as.is = c(TRUE), header=FALSE, skip=5, nrows=1, sep=" ")
      metrics_params <- strsplit(t(metrics_params), "=")
      for (q in metrics_params) {
        if (q[1] == "PAGESIZE") { metrics$pagesize[nodename] <- as.numeric(q[2]); }
        if (q[1] == "CLKTCK") { metrics$clktck[nodename] <- as.numeric(q[2]); }
      }
      if (is.na(metrics$pagesize[nodename])) {
        metrics$pagesize[nodename] <- recPageSize0;
      }
      if (is.na(metrics$clktck[nodename])) { metrics$clktck[nodename] <- recCpuTicks0; }
    } else {
      metrics$pagesize[nodename] <- recPageSize0
      #metrics$delay[nodename] <- recDelay0
      metrics$clktck[nodename] <- recCpuTicks0
    }

    # adjust
    recPageSize <- recPageSize0
    if (! is.null(metrics$pagesize[nodename])) { recPageSize <- metrics$pagesize[nodename] }
    recCpuTicks <- recCpuTicks0
    if (! is.null(metrics$clktck[nodename])) { recCpuTicks <- metrics$clktck[nodename] }
    metrics$stat$rss <- metrics$stat$rss * recPageSize
    metrics$stat$utime <- metrics$stat$utime / recCpuTicks * 100
    metrics$stat$stime <- metrics$stat$stime / recCpuTicks * 100
    #plot(metrics$stat$nthr)
    #plot(metrics$stat$vsize)
  }
  return(metrics)
}

# dashed vertical lines at the epoch boundaries
epochs <- function(d) {
    tmin <- min(d$t)
    tmin <- trunc(tmin / EPOCHLENGTH) * EPOCHLENGTH
    epochs <- data.frame(start=seq(from=tmin, to=max(d$t), by=EPOCHLENGTH))
    geom_vline(data=epochs,
               aes(xintercept = start, alpha=0.60)
             , colour='red'
             , linetype='dashed'
               )
    }

# dotted vertical lines every k slots
kslots <- function(d) {
  tmin <- min(d$t)
  tmin <- trunc(tmin / EPOCHLENGTH) * EPOCHLENGTH
  kslots <- data.frame(start=seq(from=tmin, to=max(d$t), by=k*SLOTLENGTH))
    geom_vline(data=kslots,
               aes(xintercept = start, alpha=0.60)
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
    filter(txType %in% c(crit)) %>%
    filter(txCount > 0)   ### only blocks with transactions

  hist(dd$txCount, main=paste('generated (>0)', desc, sep = ' ')
       , breaks=seq(0,maxtx, by=1)
       , col=gray.colors(maxtx/2)
       , xlab = "transactions/slot" )

  crit <- "written"
  dd <- d %>%
    filter(txType %in% c(crit)) %>%
    filter(txCount > 0)   ### only blocks with transactions

  hist(dd$txCount, main=paste('integrated in blocks (>0)', desc, sep = ' ')
       , breaks=seq(0,maxtx, by=1)
       , col=gray.colors(maxtx/2)
       , xlab = "transactions/slot" )
}

# plot the rate of sent and written transactions
plotTxs <- function(d, run=RUN, desc=DESC) {
    dd <- d %>%
        filter(txType %in% c("submitted", "written")) %>%
            filter(txCount > 0)   ### only blocks with transactions
    ggplot(dd, aes(t, txCount/slotDuration)) +
        epochs(d) + kslots(d) +
        geom_point(aes(colour=node)) +
        geom_smooth() +
        ggtitle(paste(
            'Transaction frequency for core and relay nodes, run at '
          , run, desc, sep = ' ')) +
        xlab("t [s] after start of experiment") +
        ylab("transaction rate [Hz]") +
        facet_grid(txType ~ run) +
        guides(size = "none", colour = "legend", alpha = "none")
    }

# plot the mempool residency for core and relay nodes
plotMempools <- function(d, str='core and relay', run=RUN, desc=DESC) {
    dd <- d %>%
        filter(txType %in% c("size after block"
                           , "size after rollback"
                           , "size after tx"))
    ggplot(dd, aes(t, txCount)) +
      epochs(d) +
      geom_point(aes(colour=node)) +
        ggtitle(paste(
            'Mempool sizes for', str, 'nodes, run at '
          , run, desc, sep = ' ')) +
        xlab("t [s] after start of experiment") +
        ylab("Mempool size [# of transactions]") +
        facet_grid(txType ~ isRelay, scales= "free", space = "free") +
        guides(size = "none", colour = "legend", alpha = "none")
}

# plot the wait and hold times for the local state lock
plotTimes <- function(d, str='core and relay', run=RUN, desc=DESC, lin=TRUE, minfilter = 0) {
    if (lin) {
      desc <- paste("\n(", desc, "linear scale,", "min =", minfilter, ")", sep=" ")
    } else {
      desc <- paste("\n(", desc, "log scale,", "min =", minfilter, ")", sep=" ")
    }
    dd <- d %>%
            filter(txType %in% c("hold tx"
                               , "wait tx"
                               , "hold block"
                               , "wait block"
                               , "hold rollback"
                               , "wait rollback"
                                 ))      %>% filter(txCount > minfilter)    ###  <<<<<<
    ggplot(dd, aes(t, txCount/1000)) +
      epochs(d) +
      geom_point(aes(colour=node)) +
#      geom_smooth() +
      ggtitle(paste(
            'Wait and work times for', str, 'nodes, run at '
          , run, desc, sep = ' ')) +
      xlab("t [s] after start of experiment") +
      ylab("Times waiting for/holding the lock [milliseconds]") +
      (if (lin) { scale_y_continuous(); } else { scale_y_log10(); }) +     ###  <<<<<<
      facet_grid(txType ~ isRelay) +
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

plotDuration <- function(run=RUN, desc=DESC) {

  formatylabels <- function(x) { lab <- sprintf("%1.2f", x) }

  # 1
  t1 <- grid.text(paste("\nTransaction times ", run), draw = FALSE)
  # 2
  bxstats <- boxplot.stats(times$time)
  t2 <- grid.text(paste("median time", sprintf("%1.1f", bxstats$stats[3]), "seconds", sep=" "), draw = FALSE)
  # 3
  g1 <- ggplot(times, aes(x=time, y=cumm)) +
          geom_point(col="blue") +
          geom_vline(xintercept = bxstats$stats, col=c("grey","grey60","darkgreen","grey60","grey")) +
          xlab("time (s)") + ylab("") +
          scale_y_continuous(label=formatylabels)
  # 4
  g2 <- ggplot(times, aes(y=time, x=1)) +
          geom_boxplot(fill = "lightblue", outlier.color = "darkred", outlier.size = 3, outlier.shape = 6) +
          coord_flip() +
          xlab("") + ylab("") +
          scale_x_continuous(label=formatylabels) +
          theme(axis.text.y = element_text(color="white"), axis.ticks.y = element_blank())

  grid.arrange(t1, t2, g1, g2, ncol = 1, heights = c(1,0.5,4,1))
}

plotMessages <- function(d, run=RUN, desc=DESC) {
  messages_by_type <- aggregate(txCount ~ txType, data, sum)
  submitted_by_node <- aggregate(txCount ~ node, data %>% filter(txType %in% c("submitted")), sum)
  colnames(submitted_by_node) <- c("node", "submitted")
  written_by_node <- aggregate(txCount ~ node, data %>% filter(txType %in% c("written")), sum)  %>% filter(txCount > 0)
  core_nodes <- select(written_by_node %>% filter(txCount > 0), node)
  colnames(written_by_node) <- c("node", "written")
  colnames(core_nodes) <- c("core")
  rollbackwait_by_node <- {}
  try({
      rollbackwait_by_node <- aggregate(txCount ~ node, data %>% filter(txType %in% c("wait rollback")), sum)
      colnames(rollbackwait_by_node) <- c("node", "wait rollback") },
    TRUE
  )
  rollbacksize_by_node <- {}
  try({
      rollbacksize_by_node <- aggregate(txCount ~ node, data %>% filter(txType %in% c("size after rollback")), sum)
      colnames(rollbacksize_by_node) <- c("node", "size rollback") },
    TRUE
  )
  maxparam <- length(submitted_by_node[,2])
  summsg <- sum(submitted_by_node[,2])
  #submitted_by_node[maxparam+1,1] <- "sum"
  submitted_by_node[maxparam+1,2] <- summsg
  maxparam <- length(written_by_node[,2])
  summsg <- sum(written_by_node[,2])
  #written_by_node[maxparam+1,1] <- "sum"
  written_by_node[maxparam+1,2] <- summsg

  def.par <- par(no.readonly = TRUE)
  layout(mat = matrix(c(1,1,1,1,2,2,3,4,5,6,7,0), 3, 4, byrow = TRUE), heights=c(2,4,3))

  layout(matrix(c(1,1,1,2,3,4,5,6,7), 3, 3, byrow = TRUE))
  textplot(paste("\nMessage counts of ", run), cex = 2, valign = "top")

  defborder <- c(1,0,1,1)
  textplot(messages_by_type, show.rownames = FALSE, mar=defborder, cex=1, valign="top")
  textplot(submitted_by_node, show.rownames = FALSE, mar=defborder, cex=1.1, valign="top")
  textplot(written_by_node, show.rownames = FALSE, mar=defborder, cex=1.1, valign="top")

  if (! is.null(rollbackwait_by_node)) {
    textplot(rollbackwait_by_node, show.rownames = FALSE, mar=defborder, cex=1.1, valign="top")
  }
  if (! is.null(rollbacksize_by_node)) {
    textplot(rollbacksize_by_node, show.rownames = FALSE, mar=defborder, cex=1.1, valign="top")
  }
  #textplot(core_nodes, show.rownames = FALSE, mar=defborder, cex=1.1, valign="top")

  par(def.par)
}

report <- readReport(fname2)
data <- readData(fname)


if (! INTERACTIVE) {
  png(filename=paste('overview-', RUN, '.png', sep=''))
  plotOverview(data, report)
  dev.off()
  p1 <- plotOverview(data, report)

  png(filename=paste('msgcount-', RUN, '.png', sep=''))
  plotMessages(data)
  dev.off()
  p2 <- plotMessages(data)

  p3 <- if (hasTrxTimes) {
    png(filename=paste('duration-', RUN, '.png', sep=''))
    plotDuration()
    dev.off()
    plotDuration()
  }

  p4 <- plotTxs(data)
  #ggsave(paste('txs-', RUN, '.svg', sep=''))
  ggsave(paste('txs-', RUN, '.png', sep=''))

  p5 <- plotMempools(data)
  #ggsave(paste('mempools-', RUN, '.svg', sep=''))
  ggsave(paste('mempools-', RUN, '.png', sep=''))

  p6 <- plotTimes(data, lin=FALSE)
  #ggsave(paste('times-', RUN, '.svg', sep=''))
  ggsave(paste('times-', RUN, '.png', sep=''))

  p7 <- plotTimes(data, lin=TRUE, minfilter=1e+03)
  #ggsave(paste('times-', RUN, '.svg', sep=''))
  ggsave(paste('times-', RUN, '-linear_scale.png', sep=''))

  #observe only core nodes:
  #plotMempools(data %>% filter(!(node %in% relays)), 'core')

  #observe only unprivileged relays:
  #plotMempools(data %>% filter(node %in% uRelays), 'unprivileged relay')

  #observe only privileged relays:
  #plotMempools(data %>% filter((node %in% relays) & (!(node %in% uRelays))), 'privileged relay')

  #plotTimes(data %>% filter(!(node %in% relays)), 'core')
  list(p1,p2,p3,p4,p5,p6,p7)
}

# plot the rate of sent and written transactions
plotOSMetrics <- function(d, run=RUN, labx="", laby="", desc=DESC) {
     ggplot(d, aes(x=(time), y=metrics)) +
        epochs(data) + kslots(data) +
        geom_point(colour = "blue", size=0.1) +
        ggtitle(paste(
            desc
          , run, sep = ' ')) +
        xlab(if (labx == "") {"t [s] since start of experiment"} else labx) +
        ylab(if (laby == "") {"[??]"} else laby) +
#        scale_x_date(date_labels = "%H:%M:%S") +
#        scale_x_datetime() +
        facet_grid(node ~ category, scales = "fixed") +
        guides(size = "none", colour = "legend", alpha = "none")
    }

# first block was written
#t1stBlock <- min(trunc(data$time/1000/1000))
#tlastBlock <- max(trunc(data$time/1000/1000))

prepareOSMetrics4nodes <- function (nodes, target, title) {
  # read in OS metrics from all core nodes
  osmetrics = {}
  for (n in nodes) {
      m <- readOSmetrics(n)
      osmetrics$io <- rbind(osmetrics$io, m$io)
      osmetrics$mstat <- rbind(osmetrics$mstat, m$mstat)
      osmetrics$stat <- rbind(osmetrics$stat, m$stat)
  }

  # we only run this if there are some ts data
  # older runs that lack them can still be analyzed
  if (! is.null(osmetrics$io)) {
    # skip first 20 datapoins (start up of process)
    tmin <- min(osmetrics$io$time)
    tmax <- max(osmetrics$io$time)
    tstartup <- 20
    osmetrics$io <- osmetrics$io %>% filter(time >= tmin + tstartup)
    osmetrics$mstat <- osmetrics$mstat %>% filter(time >= tmin + tstartup)
    osmetrics$stat <- osmetrics$stat %>% filter(time >= tmin + tstartup)

    # epoch start time, aligned to times in `data`
    t0 <- round(data$time[1] / 1e6) - data$t[1]

    osmetrics$io$time0 <- osmetrics$io$time
    osmetrics$mstat$time0 <- osmetrics$mstat$time
    osmetrics$stat$time0 <- osmetrics$stat$time
    osmetrics$io$time <- osmetrics$io$time - t0
    osmetrics$mstat$time <- osmetrics$mstat$time - t0
    osmetrics$stat$time <- osmetrics$stat$time - t0

    osmetrics$io <- osmetrics$io %>% filter(time >= 0)
    osmetrics$mstat <- osmetrics$mstat %>% filter(time >= 0)
    osmetrics$stat <- osmetrics$stat %>% filter(time >= 0)

  # prepare plot data (I/O)
    plotdata <- {}
    for (n in nodes) {
      # extract node's data and sort it by the 'time' column:
      d <- osmetrics$io %>% filter(nodename == n) %>% arrange_all(c(time))
      if (dim(d)[1] > 50) {
        print(c(dim(d), n))
        td <- d$time[2:length(d$time)] - d$time[1:length(d$time)-1]
        plotdata <- rbind(plotdata, data.frame(node=d$nodename, time=d$time, metrics=append(0,diff(d$rchar) / td), category='read'))
        plotdata <- rbind(plotdata, data.frame(node=d$nodename, time=d$time, metrics=append(0,diff(d$wchar) / td), category='written'))
      }
    }

    p1 <- plotOSMetrics(plotdata %>% filter(metrics > 0), laby="bytes per s", desc=paste("I/O metrics", title, sep=" "))
    ggsave(paste('os-io-', target, "-", RUN, '.png', sep=''))

  # prepare plot data (CPU)
    plotdata <- {}
    for (n in nodes) {
      #recDelay <- recDelay0
      #if (! is.null(osmetrics$delay[n])) { recDelay <- osmetrics$delay[n] }
      # extract node's data and sort it by the 'time' column:
      d <- osmetrics$stat %>% filter(nodename == n) %>% arrange_all(c(time))
      if (dim(d)[1] > 50) {
        print(c(dim(d), n))
        td <- d$time[2:length(d$time)] - d$time[1:length(d$time)-1]
        plotdata <- rbind(plotdata, data.frame(node=d$nodename, time=d$time, metrics=append(0,diff(d$utime) / td), category='CPU (user time)'))
        plotdata <- rbind(plotdata, data.frame(node=d$nodename, time=d$time, metrics=append(0,diff(d$stime) / td), category='CPU (kernel time)'))
      }
    }

    p2 <- plotOSMetrics(plotdata %>% filter(metrics > 0), laby="single CPU %", desc=paste("CPU metrics", title, sep=" "))
    ggsave(paste('os-cpu-', target, "-", RUN, '.png', sep=''))

  # prepare plot data (memory)
    plotdata <- {}
    for (n in nodes) {
      # extract node's data and sort it by the 'time' column:
      d <- osmetrics$stat %>% filter(nodename == n) %>% arrange_all(c(time))
      if (dim(d)[1] > 50) {
        print(c(dim(d), n))
        plotdata <- rbind(plotdata, data.frame(node=d$nodename, time=d$time, metrics=(d$rss / 1024), category='memory (RSS)'))
      }
    }

    p3 <- plotOSMetrics(plotdata %>% filter(metrics > 0), laby="kilobytes", desc=paste("Memory usage", title, sep=" "))
    ggsave(paste('os-mem-', target, "-", RUN, '.png', sep=''))

  # prepare plot data (read bytes / sys. calls, wbytes / sys. calls)
    plotdata <- {}
    for (n in nodes) {
      d <- osmetrics$io %>% filter(nodename == n) %>% arrange_all(c(time))
      if (dim(d)[1] > 50) {
        print(c(dim(d), n))
        td <- d$time[2:length(d$time)] - d$time[1:length(d$time)-1]
        tr <- append(0,diff(d$syscr) / td) #d$rchar / d$syscr
        tw <- append(0,diff(d$syscw) / td) #d$wchar / d$syscw
        plotdata <- rbind(plotdata, data.frame(node=d$nodename, time=d$time, metrics=tr, category='read syscalls'))
        plotdata <- rbind(plotdata, data.frame(node=d$nodename, time=d$time, metrics=tw, category='write syscalls'))
      }
    }

    p4 <- plotOSMetrics(plotdata %>% filter(metrics > 0), laby="syscalls per second", desc=paste("I/O work", title, sep=" "))
    ggsave(paste('os-io-work-', target, "-", RUN, '.png', sep=''))

    list(p1,p2,p3,p4);
  }
}

prepareOSMetrics4nodes(coreNodes, "core", "for core nodes")

prepareOSMetrics4nodes(relays, "relays", "for relay nodes")

print("all done.")
dev.off()

