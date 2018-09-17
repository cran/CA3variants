plot.CA3variants <-
function(x, firstaxis = 1, lastaxis = 2, cex = 0.8, plottype = "biplot",biptype="column-tube", prop = 1, 
scaleplot = 1, arrow = T,pos=1,size=2,size2=3,...){
#library(ggplot2)
#library(ggrepel)
#library(gridExtra)
         if ((plottype == "Biplot")|(plottype == "biplot")&(biptype=="column-tube")){
        plottype <- "biplot"
        cord1 <- x$fiStandard*scaleplot
I<- nrow(cord1)
        cord2 <- x$gjk/scaleplot
J<- nrow(cord2)
        rowlabels <- x$flabels
        collabels <- x$glabels
        dimnames(cord1)[[1]] <- rowlabels
inertiapc<-round(x$inertiacoltub,1)
        main <- "Interactive row Biplot"
        arrow <- T
plotitle<-"column-tube biplot"   
 }
#---------------------------------------------------second biplot
 if ((plottype == "Biplot")|(plottype == "biplot")&(biptype=="row")){
        plottype <- "biplot"
        cord1 <- x$gjkStandard*scaleplot
        cord2 <- x$fi/scaleplot
I<- nrow(cord1)
J<- nrow(cord2)     
   rowlabels <- x$flabels
        collabels <- x$glabels
        dimnames(cord1)[[1]] <- collabels
        dimnames(cord2)[[1]] <- rowlabels
inertiapc<-round(x$inertiarow,1)    
    main <- "Interactive column-tube Biplot"
        arrow <- T
plotitle<-"row- biplot"   
    }
frows <- data.frame(coord=cord1, labels=dimnames(cord1)[[1]], categ=rep("rows", I)) # build a dataframe to be used as input for plotting via ggplot2
  gcols <- data.frame(coord=cord2, labels=dimnames(cord2)[[1]], categ=rep("col-tubes", J)) # build a dataframe to be used as input for plotting via ggplot2
  FGcord <- rbind(frows, gcols)                                       # build a dataframe to be used as input for plotting via library(ggplot2)
#---------------------------------------------------------------------

if (x$ca3type=="OCA3"){
 if ((plottype == "Biplot")|(plottype == "biplot")&(biptype=="row")){
cord1<-x$fi/scaleplot
cord2<-x$gjkStandard*scaleplot
      }
else{
cord1<-x$fiStandard*scaleplot
cord2<-x$gjk/scaleplot
}
I<- nrow(cord1)
J<- nrow(cord2)     
categ<-NULL
graph2poly(cord1,cord2,ni=x$rows,nj=x$cols,nk=x$tubes,
a1=firstaxis,a2=lastaxis,cex=cex,pos=pos,prop=prop,cols=c(1, 4))
frows <- data.frame(coord=cord1, labels=dimnames(cord1)[[1]],categ=rep("rows", I)) # build a dataframe to be used as input for plotting via ggplot2
gcols <- data.frame(coord=cord2, labels=dimnames(cord2)[[1]],categ=rep("col-tubes", J)) # build a dataframe to be used as input for plotting via ggplot2
#categ1=rep("rows", I)
#categ2=rep("col-tubes", J)
#categ=as.matrix(c(categ1,categ2))
  FGcord <- rbind(frows,gcols)     # build a dataframe to be used as input for plotting via library(ggplot2)
}
#-------------------------------------------------------------
   xmin <- min(FGcord[,firstaxis],FGcord[,lastaxis])
  xmax <- max(FGcord[,firstaxis],FGcord[,lastaxis])
 ymin <- min(FGcord[,lastaxis],FGcord[,firstaxis])
 ymax <- max(FGcord[,lastaxis],FGcord[,firstaxis])
 nv <- rep(0, nrow(cord2))
    vec <-cord2[, c(firstaxis, lastaxis)]
  #---------------------------------
dev.new()
  CA3plot <- ggplot(FGcord, aes(x=FGcord[,firstaxis], y=FGcord[,lastaxis])) + 
    geom_point(aes(colour=categ, shape=categ), size=size) +
geom_vline(xintercept = 0, linetype=2, color="gray") + 
    geom_hline(yintercept = 0, linetype=2, color="gray") + 
    labs(x=paste0("Principal Axis", firstaxis, " ", inertiapc[firstaxis],"%"),  y=paste0("Principal Axis", lastaxis," ",inertiapc[lastaxis], "%" ))  +  
    scale_x_continuous(limits = c(xmin, xmax)) +
    scale_y_continuous(limits = c(ymin, ymax)) + 
    theme(panel.background = element_rect(fill="white", colour="black")) + 
    scale_color_manual(values=c("black", "red")) + 
    coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE) + 
    geom_text_repel(data=FGcord, aes(colour=categ, label = labels), size = size2) +
#geom_segment(aes(x=nv[1],y=nv[1],xend=gcols[, firstaxis],yend=gcols[, lastaxis]), data = gcols,color="red")+
theme(legend.position="none")+
    ggtitle(plotitle) 
  grid.arrange(CA3plot, ncol=1)
 
 #--------------------------------------------------

}