ca3plot<-function(frows,gcols,firstaxis,lastaxis,inertiapc,size1,size2,biptype,addlines){
#biplots for nominal variables
categ<-NULL
attnam<-NULL
slp<-NULL
if ((biptype == "resp")||(biptype == "row")||(biptype == "col")||(biptype == "column")||(biptype == "tube")) {
#rglines=data.frame(d1=gcols$d1,d2=gcols$d2,attnam=gcols$categ)
dimcord<-dim(gcols)[[2]]
if (dimcord<4) {stop("number of axis for graphing must be at least 2\n\n")}
if ((dimcord-2)<lastaxis) {stop("unsuitable number of axis for graphing\n\n")}

rglines=data.frame(d1=gcols[,firstaxis],d2=gcols[,lastaxis],attnam=gcols$categ)
rglines$slp=rglines$d2/rglines$d1
}
if ((biptype == "pred")||(biptype == "col-tube")||(biptype == "column-tube")||(biptype == "row-tube")||(biptype == "row-column")||(biptype == "row-col")) {
#rglines=data.frame(d1=frows$d1,d2=frows$d2,attnam=frows$categ)
dimcord<-dim(frows)[[2]]
if (dimcord<4) {stop("number of axis for graphing must be at least 2\n\n")}
if ((dimcord-2)<lastaxis) {stop("unsuitable number of axis for graphing\n\n")}
rglines=data.frame(d1=frows[,firstaxis],d2=frows[,lastaxis],attnam=frows$categ)
rglines$slp=rglines$d2/rglines$d1
}
FGcord <- rbind(frows, gcols)   
xmin <-min(FGcord[,firstaxis],FGcord[,lastaxis])
xmax <- max(FGcord[,firstaxis],FGcord[,lastaxis])
ymin <- min(FGcord[,lastaxis],FGcord[,firstaxis])
ymax <- max(FGcord[,lastaxis],FGcord[,firstaxis])
 CAplot <- ggplot(FGcord, aes(x=FGcord[,firstaxis], y=FGcord[,lastaxis]),type="b") + 
 #   geom_point(aes(color=categ, shape=categ), size=size1) +
   geom_point(aes(color=categ), size=size1) +
scale_shape_manual(values=categ) +
    geom_vline(xintercept = 0, linetype=2, color="gray") + 
    geom_hline(yintercept = 0, linetype=2, color="gray") + 
    labs(x=paste0("Dimension",firstaxis,sep=" (", round(inertiapc[firstaxis],1), "%)"),y=paste0("Dimension",lastaxis,sep= " (", round(inertiapc[lastaxis],1),"%)"))  +  
    scale_x_continuous(limits = c(xmin, xmax)) +
    scale_y_continuous(limits = c(ymin, ymax)) + 
theme(panel.background = element_rect(fill="white", colour="black")) + 
  #  scale_colour_manual(values=c("red", "blue")) + 
# scale_colour_manual(values=c(col1, col2)) +   
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE) + 
    geom_text_repel(data=FGcord, aes(colour=categ, label = labels), size = size2, max.overlaps =Inf) +
theme(legend.position="none")+
#   ggtitle(" ") 
 # grid.arrange(CAplot, ncol=1)
if (addlines==TRUE){
geom_abline(data=rglines,aes(intercept=0,slope=slp,colour=attnam),alpha=.5)
}#endif
grid.arrange(CAplot, ncol=1)
 }
