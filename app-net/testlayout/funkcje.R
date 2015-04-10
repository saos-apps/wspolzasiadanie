library(igraph)
library(sqldf)
library(ggplot2)

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

addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

plog.legend<-function(list){
  vc<-length(list)-1
  g.leg<-graph.empty(vc,F)
  V(g.leg)$label=list$labels
  V(g.leg)$color=names(list[-length(list)])
  lay.leg<-matrix(c(rep(0,vc),seq(1,vc)),byrow = F,nrow = vc)
  #lay.leg<-layout.auto(g.leg)
  #par(mar = c(0,0,0,0))
  greeks<-{}
  for( x in 1:length(list$labels)){
    y=strsplit(list$labels[x]," ")
    z=as.character(y[[1]][1])
    numb<-as.integer(which(as.roman(z)==as.roman(1:50)))
    greeks<-rbind(greeks,numb)
  }
  lay.leg<-lay.leg[order(greeks),]
  list$labels<-list$labels[order(greeks)]
  plot(g.leg,vertex.label.dist=4,layout=lay.leg,vertex.label.degree=0,vertex.label=NA,vertex.size=3)
  label<-ifelse(nchar(list$labels)>55,paste(substr(list$labels,1,55),"...",sep=""),list$labels)
  text(x=-.9,y=seq(1,-1,length.out=length(list$labels)),labels=label,pos=4,adj=c(1,1),cex=1.25,ylim=c(-1,1)); #ylim=c(-.5,.5)
}

plog.legend2<-function(list){
  vc<-length(list)-1
  g.leg<-graph.empty(vc,F)
  V(g.leg)$label=list$labels
  V(g.leg)$color=names(list[-length(list)])
  lay.leg<-matrix(c(rep(0,vc),seq(1,vc)),byrow = F,nrow = vc)
  plot(g.leg,vertex.label.dist=4,layout=lay.leg,vertex.label.degree=0,vertex.label=NA,vertex.size=3)
  label<-ifelse(nchar(list$labels)>55,paste(substr(list$labels,1,55),"...",sep=""),list$labels)
  text(x=-.9,y=seq(1,-1,length.out=length(list$labels)),labels=label,pos=4,adj=c(1,1),cex=1.25,ylim=c(-1,1.5),xlim=c(-1.3,1)); #ylim=c(-.5,.5)
  text(x=-1.1,y=1.3,labels="Divisions:",cex=1.75,pos=4,adj=c(1,1)) #,,
}

plog<-function(g,layout1=layout.auto,list=NULL){
  plot.igraph(g,vertex.size=3,vertex.label=NA,vertex.shape=V(g)$vertex.shape,edge.color="grey45",edge.width=ifelse(E(g)$type=="real",1,0),
              edge.curved=TRUE,layout=layout1,mark.groups=list,mark.col=names(list),mark.shape=1,mark.expand=8)#,mark.border=NA)
}

plog.pie<-function(g,layout1=layout.auto){
  plot.igraph(g,vertex.size=4,vertex.label=NA,vertex.shape=V(g)$vertex.shape,vertex.pie=V(g)$pie.values,vertex.pie.color=V(g)$colour,edge.color="grey45",edge.width=1,
              edge.curved=TRUE,layout=layout1,vertex.pie.lty=1,vertex.color=V(g)$colour,
              vertex.frame.color=ifelse(V(g)$JudgeSex=="M","black",ifelse(V(g)$JudgeSex=="F","red","green"))
              )
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

# funkcja licząca różnorodność składów poprzez liczenie ile maksymalnie mogłoby być w danej konfig (l.sędziów, l.osób w składach sędziowskich) unikalnych krawędzi
max.unique.links<-function(njudges,array){
  array<-sort(array,T)
  un.links<-choose(njudges,2)

  ordered<-1
  nnext<-2
  { if(length(array)<=1000) {
    #ordered<-fun1(array,njudges,ordered,nnext)
    ret<-fun1(array,njudges,ordered,nnext)
    nnext<-ret$l2
    ifelse(nnext>1000 | nnext>length(array),ordered<-ret$l1,ordered<-c(ret$l1,ret$l2))
  }
    else
  {
    for(x in 1:ceiling(length(array)/1000))
    {
      ret<-fun1(array[1:min(x*1000,length(array))],njudges,ordered,nnext)
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