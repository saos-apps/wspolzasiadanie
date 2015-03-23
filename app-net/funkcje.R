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

# addalpha()
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}
# colorRampPaletteAlpha()
colorRampPaletteAlpha <- function(colors, n=32, interpolate='linear') {
  # Create the color ramp normally
  cr <- colorRampPalette(colors, interpolate=interpolate)(n)
  # Find the alpha channel
  a <- col2rgb(colors, alpha=T)[4,]
  # Interpolate
  if (interpolate=='linear') {
    l <- approx(a, n=n)
  } else {
    l <- spline(a, n=n)
  }
  l$y[l$y > 255] <- 255 # Clamp if spline is > 255
  cr <- addalpha(cr, l$y/255.0)
  return(cr)
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
plogleg.old<-function(g,layout1=layout.auto){
plot.igraph(g,vertex.size=5,vertex.label=NA,vertex.label.cex=3,vertex.label.degree=0,vertex.shape=V(g)$shape,vertex.frame.width=1,
vertex.frame.color=V(g)$fcolor,edge.color=NA,edge.width=0.7,edge.curved=FALSE,layout=layout1,asp=.35)#,rescale=F,ylim=c(6,8))
text(x=-.9,y=seq(1,-1,length.out=9),labels=V(g)$label,pos=4,adj=c(.5,.5),cex=2.5); #ylim=c(-.5,.5)
}

plog.legend<-function(list){
  vc<-length(list)-1
  g.leg<-graph.empty(vc,F)
  V(g.leg)$label=list$labels
  V(g.leg)$color=names(list[-length(list)])
  lay.leg<-matrix(c(seq(1,vc),rep(0,vc)),byrow = F,nrow = vc)
  plot.igraph(g.leg,vertex.label.dist=1,layout=lay.leg)
}
#4.funkcja rysująca sieć współpracy. Ustawiona stała wlk. wierzchołków, grubość krawędzi zależy od wagi.
plog.old<-function(g,layout1=layout.auto){
plot.igraph(g,vertex.size=2,vertex.label=NA,vertex.frame.color="black",edge.color=E(g)$color,edge.width=3*log(E(g)$weight,10),edge.curved=TRUE,layout=layout1)
}

plog<-function(g,layout1=layout.auto,list=NULL){
  plot.igraph(g,vertex.size=3,vertex.label=NA,vertex.shape=V(g)$vertex.shape,edge.color="grey45",edge.width=ifelse(E(g)$type=="real",1,0),
              edge.curved=TRUE,layout=layout1,mark.groups=list,mark.col=names(list),mark.shape=1,mark.expand=8)#,mark.border=NA)
}

plog.pie<-function(g,layout1=layout.auto){
  plot.igraph(g,vertex.size=4,vertex.label=NA,vertex.shape="pie",vertex.pie=V(g)$pie.values,vertex.pie.color=V(g)$colour,edge.color="grey45",edge.width=ifelse(E(g)$type=="real",1,0),
              edge.curved=TRUE,layout=layout1)
}

mycircle <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size  <- 1/200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  vertex.frame.color <- params("vertex", "frame.color")
  if (length(vertex.frame.color) != 1 && !is.null(v)) {
    vertex.frame.color <- vertex.frame.color[v]
  }
  vertex.frame.width <- params("vertex", "frame.width")
  if (length(vertex.frame.width) != 1 && !is.null(v)) {
    vertex.frame.width <- vertex.frame.width[v]
  }
  
  mapply(coords[,1], coords[,2], vertex.color, vertex.frame.color,
         vertex.size, vertex.frame.width,
         FUN=function(x, y, bg, fg, size, lwd) {
           symbols(x=x, y=y, bg=bg, fg=fg, lwd=lwd,
                   circles=size, add=TRUE, inches=FALSE)
         })
}



mytriangle <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1/200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  vertex.frame.color <- params("vertex", "frame.color")
  if (length(vertex.frame.color) != 1 && !is.null(v)) {
    vertex.frame.color <- vertex.frame.color[v]
  }
  vertex.frame.width <- params("vertex", "frame.width")
  if (length(vertex.frame.width) != 1 && !is.null(v)) {
    vertex.frame.width <- vertex.frame.width[v]
  }
  mapply(coords[,1], coords[,2], vertex.color, vertex.frame.color,
         vertex.size, vertex.frame.width,
         FUN=function(x, y, bg, fg, size, lwd) {
           symbols(x=x, y=y, bg=bg, fg=fg, lwd=lwd,
                   stars=cbind(1.5*vertex.size, 1.5*vertex.size, 1.5*vertex.size), add=TRUE, inches=FALSE)
         })
}

mystar <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size  <- 1/200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  norays <- params("vertex", "norays")
  if (length(norays) != 1 && !is.null(v)) {
    norays <- norays[v]
  }
  vertex.frame.color <- params("vertex", "frame.color")
  if (length(vertex.frame.color) != 1 && !is.null(v)) {
    vertex.frame.color <- vertex.frame.color[v]
  }
  vertex.frame.width <- params("vertex", "frame.width")
  if (length(vertex.frame.width) != 1 && !is.null(v)) {
    vertex.frame.width <- vertex.frame.width[v]
  }
  mapply(coords[,1], coords[,2], vertex.color, vertex.frame.color,
         vertex.size, vertex.frame.width, norays,
         FUN=function(x, y, bg, fg, size,lwd, nor) {
           symbols(x=x, y=y, bg=bg, fg=fg,lwd=lwd,
                   stars=matrix(c(1.5*size,1.5*size/2), nrow=1, ncol=nor*2), add=TRUE, inches=FALSE)
         })
}

#6.dodanie zdefiniowanych powyżej kształtów wierzchołków do opcji wyboru przy rysowaniu sieci.
add.vertex.shape("fcircle", clip=igraph.shape.noclip,
                 plot=mycircle, parameters=list(vertex.frame.color=1,
                                                vertex.frame.width=1))

add.vertex.shape("ftriangle", clip=igraph.shape.noclip,
                 plot=mytriangle,parameters=list(vertex.frame.color=1,
                                                 vertex.frame.width=1))

add.vertex.shape("fstar", clip=igraph.shape.noclip,
                 plot=mystar, parameters=list(vertex.norays=5,vertex.frame.color=1,
                                              vertex.frame.width=1))

max.unique.links<-function(njudges,array){
  array<-sort(array,T)
  un.links<-choose(njudges,2)

  ordered<-1
  nnext<-2
  { if(length(array)<=1000) {
    #ordered<-fun1(array,njudges,ordered,nnext)
    ret<-fun1(array,njudges,ordered,nnext)
    nnext<-ret$l2
    ifelse(nnext>x*1000 | nnext>length(array),ordered<-ret$l1,ordered<-c(ret$l1,ret$l2))
  }
    else
  {
    for(x in 1:ceiling(length(array)/1000))
    {
      ret<-fun1(array[1:min(x*1000,length(array))],njudges,ordered,nnext)
      #nnext<-ordered[length(ordered)]
      #ordered<-ordered[-length(ordered)]
      nnext<-ret$l2
      ifelse(nnext>x*1000 | nnext>length(array),ordered<-ret$l1,ordered<-c(ret$l1,ret$l2))
      if(sum(array[ordered])==njudges) break
    }
  }
  }
  
  exist.links<-sum(choose(array[ordered],2),na.rm = T)
  pos.links<-sum(choose(array[-ordered],2)-choose(ceiling(array[-ordered]/2),2)-choose(floor(array[-ordered]/2),2))

  ifelse((exist.links+pos.links)<un.links,
         c<-exist.links+pos.links,
         c<-un.links
         )
  c
}

fun1<-function(array,njudges,order,nnext){
  {if(length(array)<nnext)
         list(l1=order,l2=nnext)
   else {if(sum(array[c(order,nnext)])<njudges)
         return(fun1(array,njudges,c(order,nnext),nnext+1))
   else
         { if(sum(array[c(order,nnext)])>njudges)
                return(fun1(array,njudges,order,nnext+2))
           else
                list(l1=order,l2=nnext)
         }
}
   }
}
#{ if(nnext>length(array)) list(l1=order,l2=nnext) else list(l1=c(order,nnext),l2=nnext)}
#  list(l1=order,l2=nnext)
