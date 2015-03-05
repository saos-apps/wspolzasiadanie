library(igraph)
library(sqldf)
library(ggplot2)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#1.funkcja color.graph koloruje wierzchołki i krawędzie grafu.
#	bierzemy wierzchołki grupami, zaczynając od najbardziej licznej. Pierwsze 8 kolorów z palety Set2 jest używane do pokolorowania największych grup. Następnie używamy wybranych z Paired i Greys. Kolory razem z kształtami moga stworzyc max. 8+5x4=28 kombinacji, które nie będą się dublowały dla różnych grup. W tej sieci jest 25 grup.
color.graph<-function(g){
V(g)$color<-NA;
units<-unique(V(g)$label)
temp<-sapply(1:length(units),function(x) {length(V(g)[V(g)$label==units[x]])})
colors<-c(brewer.pal(8,"Set2"),rep(c(brewer.pal(12,"Paired")[c(2,6,10,12)],brewer.pal(9,"Greys")[8]),each=ceiling((length(units)-8)/5))) # jeśli byśmy kolory z Set2 powtarzali dla wszystkich kształtów to możemy stworzyć max. 13x4 kombinacji.
ord<-order(temp,decreasing=T) 
units.ord<-units[ord] #tablica z lvl2 jednostek zaczynając od najbardziej licznych. użyta ponizej do kolorowania wierzchołków
sapply(1:length(units.ord),function(x) V(g)$color[V(g)$label==units.ord[x]]<<-colors[x])
V(g)$color[V(g1b)$label==3999]<-"black" # Inne jednostki kolorujemy chwilowo na czarno aby taki sam był kolor krawędzi
ord<-order(temp,decreasing=F) #tutaj odwracamy kolejność kolorowania dla krawędzi tak aby możliwie jak najwięcej było krawędzi w kolorze wierzchołków z których wychodzą jednocześnie dając priorytet tym najbardziej licznym jednostkom.
units.ord<-units[ord]
sapply(1:length(units.ord),function(x) {col<-V(g)$color[V(g)$label==units.ord[x]][1];e<-E(g)[from(V(g)[V(g)$label==units.ord[x]])];E(g)$color[e]<<-rep(col,length(e));})
V(g)$color[V(g1b)$label==3999]<-"white" #wypełnienie V innych jednostek białe
g
}

#2.funkcja shape.graph która przyporządkowuje 4 kształty (zdefiniowane poniżej) dla kolejnych jednostek. 
shape.graph<-function(g){
V(g)$shape<-NA;
units<-unique(V(g)$label)
temp<-sapply(1:length(units),function(x) {length(V(g)[V(g)$label==units[x]])})
shapes<-rep(c("fsquare","fcircle","ftriangle","fstar"),ceiling(length(units)/4))
ord<-order(temp,decreasing=T)
units.ord<-units[ord]
sapply(1:length(units.ord),function(x) V(g)$shape[V(g)$label==units.ord[x]]<<-shapes[x])
g
}

#3.funkcja rysująca legendę. w plot rysujemy tylko wierzchołki. 
plogleg<-function(g,layout1=layout.auto){
plot.igraph(g,vertex.size=5,vertex.label=NA,vertex.label.cex=3,vertex.label.degree=0,vertex.shape=V(g)$shape,vertex.frame.width=1,
vertex.frame.color=V(g)$fcolor,edge.color=NA,edge.width=0.7,edge.curved=FALSE,layout=layout1,asp=.35)#,rescale=F,ylim=c(6,8))
text(x=-.9,y=seq(1,-1,length.out=9),labels=V(g)$label,pos=4,adj=c(.5,.5),cex=2.5); #ylim=c(-.5,.5)
}

#4.funkcja rysująca sieć współpracy. Ustawiona stała wlk. wierzchołków, grubość krawędzi zależy od wagi.
plog.old<-function(g,layout1=layout.auto){
plot.igraph(g,vertex.size=2,vertex.label=NA,vertex.frame.color="black",edge.color=E(g)$color,edge.width=3*log(E(g)$weight,10),edge.curved=TRUE,layout=layout1)
}

plog<-function(g,layout1=layout.auto){
  plot.igraph(g,vertex.size=2,vertex.label=NA,edge.color="grey45",edge.width=1,edge.curved=TRUE,layout=layout1)
}
