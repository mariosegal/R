plotTree <- function(x) {
  #Developed by Mario Segal - No warranties provided - Use at your own risk
  require(ggdendro)
  require(odfWeave)
  require(tree)
  require(ggplot2)
  require(plyr)
  
  #get tree data in a form suitable for ggdendro;
  tree_data <- dendro_data(x)
  #get labels and splits and modify labels to incluse splits
  labels <- as.matrix(as.character(tree_data$labels$label),nrow=1)
  splits <- as.matrix(x$frame$splits[,1][as.numeric(row.names(tree_data$labels))],nrow=1)
  splits <- prettyNum(splits,big.mark=",",scientific=F,digits=2,format="f")
  ns <- as.matrix(x$frame$n[as.numeric(row.names(tree_data$leaf_labels))],nrow=1)
  new_labels <- matrixPaste(as.character(tree_data$labels$label),splits,sep="\n")
  tree_data$labels$label <- new_labels
  new_ends <- paste(as.character(tree_data$leaf_labels$label)," (N=",as.character(ns),")",sep="")
  tree_data$leaf_label$labels <- new_ends
  
  range <- max(tree_data$segments$y)-min(tree_data$segments$y)
  my_limits = c(min(tree_data$segments$y)-0.25*range,max(tree_data$segments$y)+0.25*range)
  my_limits[1] = round_any(my_limits[1],10,f=floor)
  my_limits[2] = round_any(my_limits[2],10,f=ceiling)
  #create the ggplot chart (parts of code copied from ggdendro documentation);
  plot_x <- plot_x <- ggplot(segment(tree_data)) +geom_segment(aes(x=x, y=y, xend=xend, yend=yend),colour="blue", alpha=0.5) +theme_dendro()
  plot_x <- plot_x +geom_text(data=label(tree_data),aes(x=x, y=y, label=label), vjust=-0.5, size=3) 
  plot_x <- plot_x +geom_text(data=tree_data$leaf_label,aes(x=x, y=y, label=labels,color=label), vjust=0.5, hjust=1, size=3,angle=90)
  plot_x <- plot_x + scale_y_continuous(limits=my_limits)+theme(legend.position="none")
  #colors for terminal node labels are optional but very nice, they can be custome defined as below;
  #or if the next line is ignored they will be ggplot default colors;
  plot_x <- plot_x + scale_color_manual(values=c("#FFB300","#007856","#C3E76F","#86499D","#003359","#AFAAA3"))
  print(plot_x)
  return(plot_x)
}
