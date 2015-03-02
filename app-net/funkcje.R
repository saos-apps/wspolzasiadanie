library(igraph)
library(sqldf)
library(ggplot2)

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
plog<-function(g,layout1=layout.auto){
plot.igraph(g,vertex.size=2,vertex.label=NA,vertex.frame.color="black",edge.color=E(g)$color,edge.width=3*log(E(g)$weight,10),edge.curved=TRUE,layout=layout1)
}

plog2<-function(g,layout1=layout.auto){
  plot.igraph(g,vertex.size=2,vertex.label=NA,vertex.frame.color="black",edge.color="grey45",edge.width=1,edge.curved=TRUE,layout=layout1)
}

#5.poniżej są 4 funkcje definiujące symbole wierzchołków: koło,trójkąt,gwiazda,kwadrat.
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
				 
mysquare <- function(coords, v=NULL, params) {
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
                   squares=2*vertex.size, add=TRUE, inches=FALSE)
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
								  
add.vertex.shape("fsquare", clip=igraph.shape.noclip,
                 plot=mysquare, parameters=list(vertex.frame.color=1,
                                  vertex.frame.width=1))	