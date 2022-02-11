plot.CA3variants<-
function(x, firstaxis = 1, lastaxis = 2, thirdaxis=3, cex = 0.8,biptype="column-tube",  
scaleplot = NULL, plot3d=FALSE, pos=1, size1=1, size2=3, addlines=TRUE,...){
#library(ggplot2)
#library(ggrepel)
#library(gridExtra)
#library(plotly)
ndim<-dim(x$Data)
ni<-ndim[1]
nj<-ndim[2]
nk<-ndim[3]
ndimg<-dim(x$g)
lab<-dimnames(x$Data)
nomi<-lab[[1]]
nomj<-lab[[2]]
nomk<-lab[[3]]
#-----------------------------------------------------------------------------scaleplot
VV1<-sum(diag(t(x$gjkStandard)%*%x$gjkStandard)) #for row biplot
VV2<-sum(diag(t(x$fi)%*%x$fi))
gammai<-1/((ni/nj*nk)*VV1/VV2)^{1/4}
V1<-sum(diag(t(x$fiStandard)%*%x$fiStandard)) #for column-tube biplot
V2<-sum(diag(t(x$gjk)%*%x$gjk))
gammajk<-1/((nj*nk/ni)*V1/V2)^{1/4}
V1<-sum(diag(t(x$fjStandard)%*%x$fjStandard)) #for row-tube biplot
V2<-sum(diag(t(x$gik)%*%x$gik))
gammaik<-1/((ni*nk/nj)*V1/V2)^{1/4}
VV1<-sum(diag(t(x$gikStandard)%*%x$gikStandard)) #for col biplot
VV2<-sum(diag(t(x$fj)%*%x$fj))
gammaj<-1/((ni/nj*nk)*VV1/VV2)^{1/4}
V1<-sum(diag(t(x$fkStandard)%*%x$fkStandard)) #for row-tube biplot
V2<-sum(diag(t(x$gij)%*%x$gij))
gammaij<-1/((ni*nj/nk)*V1/V2)^{1/4}
VV1<-sum(diag(t(x$gijStandard)%*%x$gijStandard)) #for col biplot
VV2<-sum(diag(t(x$fk)%*%x$fk))
gammak<-1/((nk/nj*ni)*VV1/VV2)^{1/4}

#----------------------------------------------------------------------------------
categ<-NULL
categtub<-NULL
catc<-NULL
#------------------------------------------------------
    if ((biptype == "row")||(biptype == "col-tube")||(biptype == "column-tube")||(biptype == "pred")||(biptype == "resp")) {
for (i in 1:ndim[[3]]){
categtub<-c(categtub, paste(lab[[2]],ndim[[3]],sep=""))
#for (i in 1:ndim[[2]]){
#categtub<-c(categtub, paste(lab[[3]],ndim[[2]],sep=""))
 if ((x$ca3type == "OCA3")||(x$ca3type == "ONSCA3"))   {for (i in 1:ndim[[3]]){
categtub<-c(categtub, paste(rep("coltub", ndim[[2]]),lab[[3]][i],sep=""))
}
}
}
}#end if biptype
#--------------------------------biplot choice only for symmetrical variants is limited to biptype="pred" and biptype="resp"-----------
  if (biptype == "pred") {
  if (is.null(scaleplot)==TRUE) {scaleplot<-gammajk}
        cord1 <- x$fiStandard * scaleplot
        I <- nrow(cord1)
        cord2 <- x$gjk/scaleplot
        J <- nrow(cord2)
  rowlabels <- dimnames(x$fi)[[1]]
        collabels <-  dimnames(x$gjk)[[1]]
cord1<-as.matrix(cord1)
cord2<-as.matrix(cord2)
        dimnames(cord1)[[1]] <- rowlabels
dimnames(cord2)[[1]] <- collabels
        inertiapc <- round(x$inertiacoltub)
         plotitle <- "column-tube biplot"
catc=c(1,0)
if (I<2) {stop("number of axis for plotting must be at least 2\n\n")}
if ((x$ca3type=="CA3")||(x$ca3type=="OCA3")) {stop("WARNING: For symmetrical variants biptype should be not equal to resp or pred \n\n")}
    }
    if (biptype == "resp") {
if (is.null(scaleplot)==TRUE) {scaleplot<-gammai}
         cord1 <- x$fi/scaleplot
         cord2 <- x$gjkStandard * scaleplot
        I <- nrow(cord1)
        J <- nrow(cord2)
  rowlabels <- dimnames(x$fi)[[1]]
        collabels <-  dimnames(x$gjk)[[1]]
        dimnames(cord2)[[1]] <- collabels
        dimnames(cord1)[[1]] <- rowlabels
        inertiapc <- round(x$inertiarow)
        plotitle <- "row- biplot"
catc=c(0,1)
if (J<2) {stop("number of axis for graphing must be at least 2\n\n")}
if ((x$ca3type=="CA3")||(x$ca3type=="OCA3")) {stop("WARNING: For symmetrical variants biptype should be equal to row or column-tube, or column or tube etc \n\n")}
    }
#------------------------------------------------------------biplots for symmetrical variants
#--------------------------------------------------------------------------------------row and column-tube
      if ((biptype == "column-tube")||(biptype == "col-tube")) {
  if (is.null(scaleplot)==TRUE) {scaleplot<-gammajk}
        cord1 <- x$fiStandard * scaleplot
        I <- nrow(cord1)
        cord2 <- x$gjk/scaleplot
        J <- nrow(cord2)
  rowlabels <- dimnames(x$fi)[[1]]
        collabels <-  dimnames(x$gjk)[[1]]
cord1<-as.matrix(cord1)
cord2<-as.matrix(cord2)
        dimnames(cord1)[[1]] <- rowlabels
dimnames(cord2)[[1]] <- collabels
        inertiapc <- round(x$inertiacoltub)
         plotitle <- "column-tube biplot"
catc=c(1,0)

if (I<2) {stop("number of axis for plotting must be at least 2\n\n")}
if ((x$ca3type=="NSCA3")||(x$ca3type=="ONSCA3")) {stop("WARNING: For symmetrical variants biptype should  be equal to resp or pred \n\n")}
    }
    if (biptype == "row") {
if (is.null(scaleplot)==TRUE) {scaleplot<-gammai}
         cord1 <- x$fi/scaleplot
         cord2 <- x$gjkStandard * scaleplot
        I <- nrow(cord1)
        J <- nrow(cord2)
  rowlabels <- dimnames(x$fi)[[1]]
        collabels <-  dimnames(x$gjk)[[1]]
        dimnames(cord2)[[1]] <- collabels
        dimnames(cord1)[[1]] <- rowlabels
        inertiapc <- round(x$inertiarow)
        plotitle <- "row- biplot"
catc=c(0,1)
if (J<2) {stop("number of axis for graphing must be at least 2\n\n")}
if ((x$ca3type=="NSCA3")||(x$ca3type=="ONSCA3")) {stop("WARNING: For non-symmetrical variants biptype should be equal to resp or pred \n\n")}
    }
   #----------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------col and row-tub
       if ((biptype == "row-tube")) {
if (is.null(scaleplot)==TRUE) {scaleplot<-gammaik}
        cord1 <- x$fjStandard * scaleplot
        I <- nrow(cord1)
        cord2 <- x$gik/scaleplot
        J <- nrow(cord2)
  rowlabels <- dimnames(x$fj)[[1]]
        collabels <-  dimnames(x$gik)[[1]]
        dimnames(cord1)[[1]] <- rowlabels
dimnames(cord2)[[1]] <- collabels
        inertiapc <- round(x$inertiarowtub)
        plotitle <- "row-tube biplot"
catc=c(1,0)
if (I<2) {stop("number of axis for graphing must be at least 2\n\n")}
if ((x$ca3type=="NSCA3")||(x$ca3type=="ONSCA3")) {stop("WARNING: For non-symmetrical variants biptype should be equal to resp or pred \n\n")}
    }#end biptype
    if ((biptype == "col")||(biptype == "column")) {
if (is.null(scaleplot)==TRUE) {scaleplot<-gammaj}
        cord1 <- x$fj/scaleplot
        cord2 <- x$gikStandard * scaleplot
        I <- nrow(cord1)
        J <- nrow(cord2)
         rowlabels <- dimnames(x$fj)[[1]]
        collabels <-  dimnames(x$gik)[[1]]
        dimnames(cord2)[[1]] <- collabels
        dimnames(cord1)[[1]] <- rowlabels
        inertiapc <- round(x$inertiacol)
        plotitle <- "col biplot"
catc=c(0,1)
if (J<2) {stop("number of axis for graphing must be at least 2\n\n")}
if ((x$ca3type=="NSCA3")||(x$ca3type=="ONSCA3")) {stop("WARNING: For non-symmetrical variants biptype should be equal to resp or pred \n\n")}
   }
       if (((biptype == "column")||(biptype == "row-tube")||(biptype == "col")||(biptype == "tube-row"))) {
#catall=NULL
for (i in 1:ndim[[3]]){
categtub<-c(categtub, paste(lab[[1]],ndim[[3]],sep=""))
}
 
catall <- rep("solid",ndim[[2]])
}#end if biptype
#----------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------tube and row-col
       if ((biptype == "row-column")||(biptype == "row-col")||(biptype == "col-row")||(biptype == "column-row")) {
if (is.null(scaleplot)==TRUE) {scaleplot<-gammaij}
        cord1 <- x$fkStandard * scaleplot
        I <- nrow(cord1)
        cord2 <- x$gij/scaleplot
        J <- nrow(cord2)
         rowlabels <- dimnames(x$fk)[[1]]
        collabels <-  dimnames(x$gij)[[1]]
        dimnames(cord1)[[1]] <- rowlabels
dimnames(cord2)[[1]] <- collabels
        inertiapc <- round(x$inertiarowcol)
        plotitle <- "row-col biplot"
catc=c(1,0) #solid for standard coord and blank for principal ones
if (I<2) {stop("number of axis for graphing must be at least 2\n\n")}
if ((x$ca3type=="NSCA3")||(x$ca3type=="ONSCA3")) {stop("WARNING: For non-symmetrical variants biptype should be equal to resp or pred \n\n")}
  }

    if (biptype == "tube") {
if (is.null(scaleplot)==TRUE) {scaleplot<-gammak}
        cord1 <- x$fk/scaleplot
        cord2 <- x$gijStandard * scaleplot
        I <- nrow(cord1)
        J <- nrow(cord2)
             rowlabels <- dimnames(x$fk)[[1]]
        collabels <-  dimnames(x$gij)[[1]]
        dimnames(cord2)[[1]] <- collabels
        dimnames(cord1)[[1]] <- rowlabels
        inertiapc <- round(x$inertiatube)
        plotitle <- "tube biplot"
catc=c(0,1) #solid for standard coord and blank for principal ones
if (J<2) {stop("number of axis for graphing must be at least 2\n\n")}
if ((x$ca3type=="NSCA3")||(x$ca3type=="ONSCA3")) {stop("WARNING: For non-symmetrical variants biptype should be equal to resp or pred \n\n")}
    }
       if (( (biptype == "tube")||(biptype == "row-col")||(biptype == "row-column")||(biptype == "col-row")||(biptype == "column-row"))) {
for (i in 1:ndim[[2]]){
#categtub<-c(categtub, paste(rep("rowcol", ndim[[2]]),lab[[1]][i],sep=""))
#categtub<-c(categtub, paste(rep("rowtub", ndim[[1]]),lab[[1]][i],sep=""))
#categtub<-c(categtub, paste(lab[[1]],ndim[[2]],sep=""))
categtub<-c(categtub, paste(rep("rowcol", ndim[[1]]),lab[[2]][i],sep=""))
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
categtub<-NULL
####
catall <- rep("solid",ndim[[3]])
for (i in 1:ndim[[3]]){
categtub<-c(categtub, paste(rep("coltub", ndim[[2]]),lab[[3]][i],sep=""))
}
####
###
frows <- data.frame(coord = cord1, labels = dimnames(cord1)[[1]], categ = rep("rows", I), linet=rep("blank",I) )
gcols <- data.frame(coord = cord2, labels = dimnames(cord2)[[1]], categ = categtub, linet=rep("solid",J))
FGcord <- rbind(frows, gcols)    # build a dataframe to be used as input for plotting via library(ggplot2)
  xmin <- min(FGcord[, firstaxis], FGcord[, lastaxis])
    xmax <- max(FGcord[, firstaxis], FGcord[, lastaxis])
    ymin <- min(FGcord[, lastaxis], FGcord[, firstaxis])
    ymax <- max(FGcord[, lastaxis], FGcord[, firstaxis])
if (addlines==TRUE){ 
 # CA3plot <- ggplot(FGcord, aes(x =  FGcord[, firstaxis], y =  FGcord[, lastaxis]),group=categ) + 
   CA3plot <- ggplot(FGcord, aes(x =  FGcord[, firstaxis], y =  FGcord[, lastaxis])) + 
geom_point(aes(colour = categ, shape = categ), size = size1) + 
geom_vline(xintercept = 0, linetype = 2, color = "gray") + 
geom_hline(yintercept = 0, linetype = 2, color = "gray") + 
labs(x = paste0("Dimension", firstaxis, " ", inertiapc[firstaxis], "%"), y = paste0("Dimension", 
        lastaxis, " ", inertiapc[lastaxis], "%")) + 
scale_x_continuous(limits = c(xmin, xmax)) + 
scale_y_continuous(limits = c(ymin, ymax)) + 
theme(panel.background = element_rect(fill = "white", colour = "black")) + 
coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE) + 
geom_text_repel(data = FGcord, aes(colour = categ, label = labels), size = size2, max.overlaps =Inf) + 
geom_path(aes(color=categ,linetype=linet),lwd=.5)+
scale_linetype_manual(values=c("blank",catall)) + 
#scale_linetype_manual(values=catc) + 
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
labs(x = paste0("Dimension", firstaxis, " ", inertiapc[firstaxis], "%"), y = paste0("Dimension", 
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