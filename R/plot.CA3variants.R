plot.CA3variants<-
function(x, firstaxis = 1, lastaxis = 2, thirdaxis=3, cex = 0.8,biptype="column-tube",  
scaleplot = NULL, plot3d=FALSE, pos=1, size1=1, size2=2, addlines=TRUE,...){
#library(ggplot2)
#library(ggrepel)
#library(gridExtra)
#library(plotly)
ndim<-dim(x$Data)
ndimg<-dim(x$g)
lab<-dimnames(x$Data)
categ<-NULL
categtub<-NULL
catc<-NULL
#------------------------------------------------------
    if ((biptype == "row")||(biptype == "col-tube")||(biptype == "column-tube")||(biptype == "col-tube")) {
for (i in 1:ndim[[3]]){
categtub<-c(categtub, paste(lab[[2]],ndim[[3]],sep=""))
#for (i in 1:ndim[[2]]){
#categtub<-c(categtub, paste(lab[[3]],ndim[[2]],sep=""))
}
}#end if biptype

#--------------------------------------------------------------------------------------row and column-tube
      if ((biptype == "column-tube")||(biptype == "col-tube")) {
  if (is.null(scaleplot)==TRUE) {scaleplot<-x$gammajk}
        cord1 <- x$fiStandard * scaleplot
        I <- nrow(cord1)
        cord2 <- x$gjk/scaleplot
        J <- nrow(cord2)
        rowlabels <- x$labelfi
        collabels <- x$labelgjk
        dimnames(cord1)[[1]] <- rowlabels
dimnames(cord2)[[1]] <- collabels
        inertiapc <- round(x$inertiacoltub)
         plotitle <- "column-tube biplot"
catc=c(1,0)
if (I<2) {stop("number of axis for plotting must be at least 2\n\n")}

    }
    if (biptype == "row") {
if (is.null(scaleplot)==TRUE) {scaleplot<-x$gammai}
         cord1 <- x$fi/scaleplot
         cord2 <- x$gjkStandard * scaleplot
        I <- nrow(cord1)
        J <- nrow(cord2)
        rowlabels <- x$labelfi
        collabels <- x$labelgjk
        dimnames(cord2)[[1]] <- collabels
        dimnames(cord1)[[1]] <- rowlabels
        inertiapc <- round(x$inertiarow)
        plotitle <- "row- biplot"
catc=c(0,1)
if (J<2) {stop("number of axis for graphing must be at least 2\n\n")}

    }
   #----------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------col and row-tub
       if ((biptype == "row-tube")) {
if (is.null(scaleplot)==TRUE) {scaleplot<-x$gammaik}
        cord1 <- x$fjStandard * scaleplot
        I <- nrow(cord1)
        cord2 <- x$gik/scaleplot
        J <- nrow(cord2)
        rowlabels <- x$labelfj
        collabels <- x$labelgik
        dimnames(cord1)[[1]] <- rowlabels
dimnames(cord2)[[1]] <- collabels
        inertiapc <- round(x$inertiarowtub)
        plotitle <- "row-tube biplot"
catc=c(1,0)
if (I<2) {stop("number of axis for graphing must be at least 2\n\n")}

    }

    if ((biptype == "col")||(biptype == "column")) {
if (is.null(scaleplot)==TRUE) {scaleplot<-x$gammaj}
        cord1 <- x$fj/scaleplot
        cord2 <- x$gikStandard * scaleplot
        I <- nrow(cord1)
        J <- nrow(cord2)
        rowlabels <- x$labelfj
        collabels <- x$labelgik
        dimnames(cord2)[[1]] <- collabels
        dimnames(cord1)[[1]] <- rowlabels
        inertiapc <- round(x$inertiacol)
        plotitle <- "col biplot"
catc=c(0,1)
if (J<2) {stop("number of axis for graphing must be at least 2\n\n")}
    }
       if (((biptype == "column")||(biptype == "row-tube")||(biptype == "col")||(biptype == "tube-row"))) {
#catall=NULL
for (i in 1:ndim[[3]]){
categtub<-c(categtub, paste(lab[[1]],ndim[[3]],sep=""))
#categtub<-c(categtub, paste(rep("rowtub", ndim[[1]]),lab[[1]][i],sep=""))
}
catall <- rep("solid",ndim[[2]])
}#end if biptype
#----------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------tube and row-col
       if ((biptype == "row-column")||(biptype == "row-col")||(biptype == "col-row")||(biptype == "column-row")) {
if (is.null(scaleplot)==TRUE) {scaleplot<-x$gammaij}
        cord1 <- x$fkStandard * scaleplot
        I <- nrow(cord1)
        cord2 <- x$gij/scaleplot
        J <- nrow(cord2)
        rowlabels <- x$labelfk
        collabels <- x$labelgij
        dimnames(cord1)[[1]] <- rowlabels
dimnames(cord2)[[1]] <- collabels
        inertiapc <- round(x$inertiarowcol)
        plotitle <- "row-col biplot"
catc=c(1,0) #solid for standard coord and blank for principal ones
if (I<2) {stop("number of axis for graphing must be at least 2\n\n")}
  }

    if (biptype == "tube") {
if (is.null(scaleplot)==TRUE) {scaleplot<-x$gammak}
        cord1 <- x$fk/scaleplot
        cord2 <- x$gijStandard * scaleplot
        I <- nrow(cord1)
        J <- nrow(cord2)
        rowlabels <- x$labelfk
        collabels <- x$labelgij
        dimnames(cord2)[[1]] <- collabels
        dimnames(cord1)[[1]] <- rowlabels
        inertiapc <- round(x$inertiatube)
        plotitle <- "tube biplot"
catc=c(0,1) #solid for standard coord and blank for principal ones
if (J<2) {stop("number of axis for graphing must be at least 2\n\n")}
    }
       if (( (biptype == "tube")||(biptype == "row-col")||(biptype == "row-column")||(biptype == "col-row")||(biptype == "column-row"))) {
for (i in 1:ndim[[2]]){
#categtub<-c(categtub, paste(rep("rowcol", ndim[[2]]),lab[[1]][i],sep=""))
#categtub<-c(categtub, paste(rep("rowtub", ndim[[1]]),lab[[1]][i],sep=""))
categtub<-c(categtub, paste(lab[[1]],ndim[[2]],sep=""))
}
}#end if biptype
#----------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------
if ((x$ca3type=="CA3")||(x$ca3type=="NSCA3")) {
frows <- data.frame(cord1, labels=dimnames(cord1)[[1]], categ=rep("rows", I)) # build a dataframe to be used as input for plotting via ggplot2
gcols <- data.frame(cord2, labels=dimnames(cord2)[[1]], categ=categtub) # build a dataframe to be used as input for plotting via ggplot2
ca3plot(frows=frows,gcols=gcols,firstaxis=firstaxis,lastaxis=lastaxis,inertiapc=inertiapc,size1=size1,size2=size2,biptype=biptype,addlines=addlines)
}#end catype
linet<-NULL
#---------------------------------------------------------------------
  if ((x$ca3type == "OCA3")||(x$ca3type == "ONSCA3"))    {
frows <- data.frame(coord = cord1, labels = dimnames(cord1)[[1]], categ = rep("rows", I), linet=rep("blank",I) )
gcols <- data.frame(coord = cord2, labels = dimnames(cord2)[[1]], categ = categtub, linet=rep("solid",J))
FGcord <- rbind(frows, gcols)    # build a dataframe to be used as input for plotting via library(ggplot2)
    xmin <- min(FGcord[, firstaxis], FGcord[, lastaxis])
    xmax <- max(FGcord[, firstaxis], FGcord[, lastaxis])
    ymin <- min(FGcord[, lastaxis], FGcord[, firstaxis])
    ymax <- max(FGcord[, lastaxis], FGcord[, firstaxis])
if (addlines==TRUE){ 
 #  CA3plot <- ggplot(FGcord, aes(x =  FGcord[, firstaxis], y =  FGcord[, lastaxis]),group=categ) + 
   CA3plot <- ggplot(FGcord, aes(x =  FGcord[, firstaxis], y =  FGcord[, lastaxis])) + 
geom_point(aes(colour = categ, shape = categ), size = size1) + 
geom_vline(xintercept = 0, linetype = 2, color = "gray") + 
geom_hline(yintercept = 0, linetype = 2, color = "gray") + 
labs(x = paste0("Axis", firstaxis, " ", inertiapc[firstaxis], "%"), y = paste0("Axis", 
        lastaxis, " ", inertiapc[lastaxis], "%")) + 
scale_x_continuous(limits = c(xmin, xmax)) + 
scale_y_continuous(limits = c(ymin, ymax)) + 
theme(panel.background = element_rect(fill = "white", colour = "black")) + 
coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE) + 
geom_text_repel(data = FGcord, aes(colour = categ, label = labels), size = size2) + 
#geom_line(aes(color=categ,linetype=linet),lwd=.5)+
geom_path(aes(color=categ,linetype=linet),lwd=.5)+
scale_linetype_manual(values=catc) + 
#scale_color_manual(values=c('red',rep('#E69F00',6)))+
expand_limits(y = 0) + 
        theme(legend.position = "none") + ggtitle(plotitle)
  grid.arrange(CA3plot, ncol = 1)
}#end if addlines
else{
   CA3plot <- ggplot(FGcord, aes(x =  FGcord[, firstaxis], y =  FGcord[, lastaxis])) + 
geom_point(aes(colour = categ, shape = categ), size = size1) + 
geom_vline(xintercept = 0, linetype = 2, color = "gray") + 
geom_hline(yintercept = 0, linetype = 2, color = "gray") + 
labs(x = paste0("Axis", firstaxis, " ", inertiapc[firstaxis], "%"), y = paste0("Axis", 
        lastaxis, " ", inertiapc[lastaxis], "%")) + 
scale_x_continuous(limits = c(xmin, xmax)) + 
scale_y_continuous(limits = c(ymin, ymax)) + 
theme(panel.background = element_rect(fill = "white", colour = "black")) + 
coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE) + geom_text_repel(data = FGcord, aes(colour = categ, 
        label = labels), size = size2) + 
expand_limits(y = 0) + 
        theme(legend.position = "none") + ggtitle(plotitle)
  grid.arrange(CA3plot, ncol = 1)
}#end else
}#end if oca3 or onsca3
#--------------------------------------------------
  #------------------------------------------------
 if (plot3d==TRUE) {
   coordR<-cord1
   coordC<-cord2
   inertiaper=x$inertias[,2]
   caplot3d(coordR=cord1,coordC =cord2,inertiaper=inertiapc)
 }
}