plot.CA3variants<-function (x, firstaxis=x$firstaxis,lastaxis=x$lastaxis, cex = 0.8,  
    biptype = "column-tube", prop = 1, scaleplot = 1, pos = 1, 
    size = 2, size2 = 3, ...) 
{
ndim<-dim(x$DataMatrix)
lab<-dimnames(x$DataMatrix)
    if (biptype == "column-tube") {
        cord1 <- x$fiStandard * scaleplot
        I <- nrow(cord1)
        cord2 <- x$gjk/scaleplot
        J <- nrow(cord2)
        rowlabels <- x$flabels
        collabels <- x$glabels
        dimnames(cord1)[[1]] <- rowlabels
        inertiapc <- round(x$inertiacoltub)
        main <- "Interactive row Biplot"
        plotitle <- "column-tube biplot"
    }
    if (biptype == "row") {
        cord2 <- x$gjkStandard * scaleplot
        cord1 <- x$fi/scaleplot
        I <- nrow(cord1)
        J <- nrow(cord2)
        rowlabels <- x$flabels
        collabels <- x$glabels
        dimnames(cord2)[[1]] <- collabels
        dimnames(cord1)[[1]] <- rowlabels
        inertiapc <- round(x$inertiarow)
        main <- "Interactive column-tube Biplot"
        plotitle <- "row- biplot"
    }
categ<-NULL
categtub<-NULL
catall <- rep("solid",ndim[[3]])
for (i in 1:ndim[[3]]){
categtub<-c(categtub, paste(rep("coltub", ndim[[2]]),lab[[3]][i],sep=""))
}
        frows <- data.frame(coord = cord1, labels = dimnames(cord1)[[1]], 
            categ = rep("rows", I) )
        gcols <- data.frame(coord = cord2, labels = dimnames(cord2)[[1]], 
            categ = categtub)
        FGcord <- rbind(frows, gcols)

    if ((x$ca3type == "OCA3")||(x$ca3type == "ONSCA3"))    {
	if  (biptype == "row") {
            cord1 <- x$fi/scaleplot
            cord2 <- x$gjkStandard * scaleplot
               I <- nrow(cord1)
        J <- nrow(cord2)
dimnames(cord1)[[1]] <- rowlabels
        dimnames(cord2)[[1]] <- collabels
        inertiapc <- round(x$inertiarow)
                }
    if (biptype == "column-tube") {
              cord1 <- x$fiStandard * scaleplot
            cord2 <- x$gjk/scaleplot
               I <- nrow(cord1)
        J <- nrow(cord2)
dimnames(cord1)[[1]] <- rowlabels
        dimnames(cord2)[[1]] <- collabels
        inertiapc <- round(x$inertiacoltub)
                }

##--------------------------------------------------------
categtub<-NULL
catall <- rep("solid",ndim[[3]])
for (i in 1:ndim[[3]]){
categtub<-c(categtub, paste(rep("coltub", ndim[[2]]),lab[[3]][i],sep=""))
}
        frows <- data.frame(coord = cord1, labels = dimnames(cord1)[[1]], 
            categ = rep("rows", I) )
        gcols <- data.frame(coord = cord2, labels = dimnames(cord2)[[1]], 
            categ = categtub)
        FGcord <- rbind(frows, gcols)
    xmin <- min(FGcord[, firstaxis], FGcord[, lastaxis])
    xmax <- max(FGcord[, firstaxis], FGcord[, lastaxis])
    ymin <- min(FGcord[, lastaxis], FGcord[, firstaxis])
    ymax <- max(FGcord[, lastaxis], FGcord[, firstaxis])
    nv <- rep(0, nrow(cord2))
    vec <- cord2[, c(firstaxis, lastaxis)]
    CA3plot <- ggplot(FGcord, aes(x = FGcord[, firstaxis], y = FGcord[, 
        lastaxis])) + geom_point(aes(colour = categ, shape = categ), 
        size = size) + geom_vline(xintercept = 0, linetype = 2, 
        color = "gray") + geom_hline(yintercept = 0, linetype = 2, 
        color = "gray") + labs(x = paste0("Axis", firstaxis, 
        " ", inertiapc[firstaxis], "%"), y = paste0("Axis", 
        lastaxis, " ", inertiapc[lastaxis], "%")) + scale_x_continuous(limits = c(xmin, 
        xmax)) + scale_y_continuous(limits = c(ymin, ymax)) + 
        theme(panel.background = element_rect(fill = "white", 
            colour = "black")) + 
coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, 
        expand = TRUE) + geom_text_repel(data = FGcord, aes(colour = categ, 
        label = labels), size = size2) + 
#geom_segment(data=gcols,aes(x = nv[1],  y = nv[1], xend = gcols[, firstaxis], yend = gcols[,lastaxis],group=categtub,color=categtub),lwd=.5)+
geom_line(aes(group=categ,color=categ,linetype=categ),lwd=.5)+
scale_linetype_manual(values=c("blank",catall)) + 
expand_limits(y = 0) + 
        theme(legend.position = "none") + ggtitle(plotitle)
    grid.arrange(CA3plot, ncol = 1)
}#end if oca3 or onsca3
else{
    if (biptype == "column-tube") {freccia<-gcols}
    if (biptype == "row") {freccia<-frows}
   # xmin <- min(FGcord[, firstaxis], FGcord[, lastaxis])
  #  xmax <- max(FGcord[, firstaxis], FGcord[, lastaxis])
    #ymin <- min(FGcord[, lastaxis], FGcord[, firstaxis])
    #ymax <- max(FGcord[, lastaxis], FGcord[, firstaxis])
  xmin <- min(FGcord[, firstaxis])
  xmax <- max(FGcord[, firstaxis])
  
   ymin <- min(FGcord[, lastaxis])
    ymax <- max(FGcord[, lastaxis])
        nv <- rep(0, nrow(cord2))
    vec <- cord2[, c(firstaxis, lastaxis)]
CA3plot <- ggplot(FGcord, aes(x = FGcord[, firstaxis], y = FGcord[,lastaxis])) + 
geom_point(aes(colour = categ, shape = categ), size = size) + 
geom_vline(xintercept = 0, linetype = 2,color = "gray") + 
geom_hline(yintercept = 0, linetype = 2,  color = "gray") + 
labs(x = paste0("Axis", firstaxis, " ", inertiapc[firstaxis], "%"), y = paste0("Axis", 
        lastaxis, " ", inertiapc[lastaxis], "%")) + 
scale_x_continuous(limits = c(xmin, xmax)) + scale_y_continuous(limits = c(ymin, ymax)) + 
        theme(panel.background = element_rect(fill = "white", colour = "black")) + 
#scale_color_manual(values = c("black",  catall)) + 
coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE) + 
geom_text_repel(data = FGcord, aes(colour = categ,label = labels), size = size2) + 
#geom_segment(aes(x = nv[1],  y = nv[1], xend = freccia[, firstaxis], yend = freccia[,lastaxis]), data = freccia, color = "black") + 
expand_limits(y = 0) + 
        theme(legend.position = "none") + 
ggtitle(plotitle)
    grid.arrange(CA3plot, ncol = 1)
}#endif
#browser()
}