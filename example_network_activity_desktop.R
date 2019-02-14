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

##animint2gist(viz)

hist(activity.dt$Length)
range(activity.dt$Length)
plot(packets ~ floor.Time, packets.dt)
plot(Time ~ No., activity.dt)
plot(Length ~ Time, activity.dt)

getMat <- function(div){
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

str(getMat(10))
str(getMat(20))
str(getMat(40))
str(getMat(100))
str(getMat(1000))
str(getMat(10000))
str(getMat(100000))

feature.mat.list <- list()
for(i in 10^(1:4)){
  feature.mat.list[[paste(i)]] <- getMat(i)
}
feature.mat <- do.call(cbind, feature.mat.list)
str(feature.mat)

dcast(activity.dt, floor.Time ~ Protocol)## same as getMat(x) for x>=10000

activity.dt[, sort(table(Protocol))]

int.pattern <- list("[0-9]+", as.numeric)
int.pattern <- list("[0-9]+")
colon.int <- function(name){
  L <- list(
    ", ",
    name,
    ": ",
    int.pattern)
  names(L) <- c("", "", "", name)
  L
}
namedCapture::df_match_variable(
  activity.dt[Protocol=="QUIC"],
  Info=list(
    first=".*?",
    colon.int("PKN"),
    list(colon.int("CID")), "?"))

namedCapture::df_match_variable(
  activity.dt[Protocol=="DNS"],
  Info=list(
    "Standard query ",
    response=list("response "), "?",
    hex="0x.*?",
    " A ",
    domain="[^ ]+"))


TLS12 <- activity.dt[Protocol=="TLSv1.2"]
TLS12.list <- strsplit(TLS12$Info, split=", ")
TLS12.levels <- unique(unlist(TLS12.list))
TLS12.mat <- t(sapply(TLS12.list, function(values){
  table(factor(values, TLS12.levels))
}))
str(TLS12.mat)
TLS12.wide <- melt(data.table(No.=TLS12$No., TLS12.mat), id.vars="No.")
some.na <- TLS12.wide[activity.dt, on=list(No.)]

ARP <- namedCapture::df_match_variable(
  activity.dt[Protocol=="ARP"],
  Info=list(
    "Who has ",
    WhoHas=".*?", #"169.254.169.254",
    "[?] Tell ",
    Tell=".*",
    "|",
    "Gratuitous ARP for ",
    Gratuitous="[^ ]+"))
ARP[Info.WhoHas==""]
ARP[, list(
  count=.N
), by=list(Info.WhoHas, Info.Tell, Info.Gratuitous)][order(Info.WhoHas)]

int.equals <- function(name){
  L <- list(
    " ",
    paste0(name),
    "=",
    name=int.pattern)
  names(L) <- c("", "", "", name)
  list(L, "?")
}
TCP <- namedCapture::df_match_variable(
  activity.dt[Protocol=="TCP"],
  Info=list(
    #[TCP ACKed unseen segment] 443  >  51491 [ACK] Seq=1 Ack=2 Win=114 Len=0 TSval=65548819 TSecr=632537039
    #443  >  51949 [FIN, ACK] Seq=2076074 Ack=1610 Win=32512 Len=0 TSval=1536698726 TSecr=632616997
    prefix=list(
      "\\[",
      "[^]]+",
      "\\] "),
    "?",
    from=int.pattern,
    "  >  ",
    to=int.pattern,
    " \\[",
    inside=".*?",
    "\\]",
    int.equals("Seq"),
    int.equals("Ack"),
    int.equals("Win"),
    int.equals("Len"),
    int.equals("TSval"),
    int.equals("TSecr")))
TCP[is.na(Info.Len)]
