library("data.table")
##fread("example_desktop_network_traffic.pcapng")#binary! but wireshark can't read it either...
activity.dt <- fread("example_network_activity_desktop")
activity.dt[1]
activity.dt[2]

activity.dt[, floor.Time := floor(Time)]
packets.dt <- activity.dt[, list(
  packets=.N
), by=list(floor.Time)]
range(packets.dt$packets)
activity.dt[, box := paste(floor.Time, Protocol)]

packets.Prot.dt <- activity.dt[, list(
  packets=.N
), by=list(floor.Time, Protocol, box)]
library(animint2)
ggplot()+
    geom_tile(aes(
      floor.Time, Protocol, fill=log10(packets)),
      data=packets.Prot.dt)+
    scale_fill_gradient(low="grey95", high="red")+
  theme_bw()

data.table(
  div=c(50, 500),
  x=c(1, 1))

getDT <- function(div){
  activity.dt[, floor.Length := floor(Length/div)]
  count.wide <- dcast(
    activity.dt,
    floor.Time ~ floor.Length + Protocol,
    fun.aggregate=length,
    value.var="floor.Length")
  M <- as.matrix(count.wide[, -1])
  colnames(M) <- paste0(div, "_", colnames(M))
  M
}

floor.factor <- 10
activity.dt[, floor.Length := floor(Length/floor.factor)]
packets.Length.hist.dt <- activity.dt[, list(
  packets=.N
), by=list(floor.Time, Protocol, box, floor.Length)]
packets.Length.hist.dt[, min.Length := floor.Length*floor.factor]
packets.Length.hist.dt[, max.Length := (floor.Length+1)*floor.factor]

viz <- animint(
  title="Histogram features for network packet data",
  first=list(box="31 TLSv1.2"),
  heatmap=ggplot()+
    ggtitle("Packet Protocol count features over time")+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    theme_animint(width=600)+
    geom_tile(aes(
      floor.Time, Protocol, fill=log10(packets)),
      color="black",
      clickSelects="box",
      data=packets.Prot.dt)+
    scale_fill_gradient(low="grey95", high="red")+
    xlab("Time window"),
  scatter=ggplot()+
    ggtitle("Packet length features in one time window")+
    theme_bw()+
    xlab("Relative time in window")+
    ylab("Packet length")+
    scale_fill_gradient(low="grey95", high="blue")+
    geom_rect(aes(
      xmin=1, xmax=1.1,
      ymin=min.Length, ymax=max.Length,
      fill=log10(packets)),
      color=NA,
      showSelected="box",
      data=packets.Length.hist.dt)+
    geom_point(aes(
      Time-floor.Time, Length),
      data=activity.dt,
      fill=NA,
      color="black",
      showSelected="box"))
animint2dir(viz, "figure-histogram-features")
##animint2gist(viz)
