library("data.table")
activity.dt <- fread("example_network_activity_desktop.csv")
activity.dt[1]
activity.dt[2]
activity.dt[, user := ifelse(Time < 27.5, "Alice", "Bob")]
activity.dt[, floor.Time := floor(Time)]
packets.dt <- activity.dt[, list(
  packets=.N
), by=list(floor.Time)]
range(packets.dt$packets)
activity.dt[, box := paste(floor.Time, Protocol)]
activity.dt[, Protocol.fac := factor(Protocol)]
activity.dt[, Protocol.int := as.integer(Protocol.fac)]
packet.name <- "Packet protocol count features"
ffac <- function(x){
  factor(x, c("User", packet.name))
}
packets.Prot.dt <- activity.dt[, list(
  packets=.N,
  facet=ffac(packet.name)
), by=list(floor.Time, Protocol.int, Protocol, box)]
change.after <- 27
user.dt <- data.table(
  facet=ffac("User"), rbind(
    data.table(user="Alice", Time=0:change.after),
    data.table(user="Bob", Time=(change.after+1):41)))
change.dt <- data.table(change=change.after+0.5)
library(ggplot2)
set.seed(100)
box.dt <- packets.Prot.dt[floor.Time==31 & Protocol=="TLSv1.2"]
gg <- ggplot()+
  ggtitle("User labels and packet protocol count features over time")+
  ylab("")+
  scale_color_manual(values=c("purple", "deepskyblue"))+
  geom_tile(aes(
    floor.Time, Protocol, fill=log10(packets)),
    data=packets.Prot.dt[Protocol.int %% 2 == 0])+
  geom_text(aes(
    change, "PIMv2", label=" Change-point"),
    data=data.table(facet=ffac(packet.name), change.dt),
    hjust=0,
    color="grey50")+
  geom_vline(aes(
    xintercept=change),
    color="grey50",
    data=change.dt)+
  ## geom_tile(aes(
  ##   floor.Time, Protocol),
  ##   fill=NA,
  ##   size=1,
  ##   color="black",
  ##   data=box.dt)+
  geom_text(aes(
    Time, "Unobserved/Truth", color=user, label=substring(user, 1, 1)),
    data=user.dt)+
  geom_text(aes(
    Time, "Observed/Labels", color=user, label=substring(user, 1, 1)),
    data=user.dt[sample(1:.N, 8)])+
  guides(color="none")+
  scale_fill_gradient(low="grey95", high="red")+
  theme_bw()+
  xlab("Time window (seconds)")+
  theme(
    panel.grid=element_blank(),
    panel.margin=grid::unit(0, "lines"))+
  facet_grid(facet ~ ., scales="free", space="free")
png("figure-histogram-features-labels-heatmap.png", 7, 3, units="in", res=300)
print(gg)
dev.off()

length.facet <- "Length of individual TLSv1.2 packets in time window=31"
select.dt <- data.table(box.dt)
select.dt[, facet := NULL]
floor.vec <- c(10, 40, 160)
histfacet <- function(x){
  paste(
    "Histogram",
    "features",
    paste0("size=", x),
    sep="\n")
}
ffac <- function(x){
  factor(x, c(length.facet, histfacet(floor.vec)))
}
rect.dt.list <- list()
for(floor.factor in floor.vec){
  activity.dt[, floor.Length := floor(Length/floor.factor)]
  packets.Length.hist.dt <- activity.dt[select.dt, on=list(
    Protocol, floor.Time)][, list(
      packets=.N
    ), by=list(floor.Length)]
  packets.Length.hist.dt[, norm.packets := (packets-min(packets))/(max(packets)-min(packets))]
  packets.Length.hist.dt[, min.Length := floor.Length*floor.factor]
  packets.Length.hist.dt[, max.Length := (floor.Length+1)*floor.factor]
  rect.dt.list[[paste(floor.factor)]] <- data.table(
    facet=ffac(histfacet(floor.factor)),
    packets.Length.hist.dt)
}
rect.dt <- do.call(rbind, rect.dt.list)[norm.packets>0]
point.dt <- data.table(
  facet=ffac(length.facet),
  activity.dt[select.dt, on=list(Protocol, floor.Time)])
gg <- ggplot()+
  ##ggtitle("Packet length histogram features")+
  theme_bw()+
  facet_grid(. ~ facet, scales="free", space="free")+
  theme(panel.margin=grid::unit(0, "lines"))+
  scale_x_continuous(
    "Relative time in window (seconds)",
    breaks=seq(0, 1, by=0.5)
  )+
  ylab("Packet length")+
  scale_fill_gradient("Relative
number of
packets", low="grey95", high="blue")+
  geom_rect(aes(
    xmin=2, xmax=2.2,
    ymin=min.Length, ymax=max.Length,
    fill=norm.packets),
    color=NA,
    data=rect.dt)+
  geom_point(aes(
    Time-floor.Time, Length),
    data=point.dt,
    fill=NA,
    shape=1,
    color="black")
png("figure-histogram-features-labels.png", 7, 2.5, units="in", res=300)
print(gg)
dev.off()
