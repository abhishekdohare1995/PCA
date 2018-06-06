#' Make a random plot
#' 
#' This function creates a random histogram plot.
#' 
#' @export
#' @param n numer of random values 
#' @param dist one of "normal" or "uniform".
randomplot <- function(n, dist=c("normal", "uniform")){
  # #input validation
  # dist <- match.arg(dist)
  # stopifnot(n < 1e6)
  
  # if(dist == "normal"){
  #   hist(rnorm(n))
  # }
  
  # if(dist == "uniform"){
  #   hist(runif(n))
  # }
  
  # #return nothing
  # invisible();  

    if(!grepl(".csv$", gene_data_file)){
    stop("Uploaded file must be a .csv file!")
  }

  if(!grepl(".csv$", meta_data_file)){
    stop("Uploaded file must be a .csv file!")
  }
  
  gene_data<-read.csv(gene_data_file,sep = ',',stringsAsFactors = FALSE)
  meta_data<-read.csv(meta_data_file, sep=',',stringsAsFactors = FALSE)
  
  rownames(gene_data)<-make.names(gene_data$symbol,unique = TRUE)
  gene_data<-gene_data[,-c(1,2)]
  sample_names<-colnames(gene_data)
  gene_data_t<-as.data.frame(t(gene_data),stringsAsFactors = FALSE )
  gene_data_final<-as.data.frame(lapply(gene_data_t, as.numeric))
  row.names(gene_data_final)<-sample_names
  gene_data_final[is.na(gene_data_final)] <- 0
  PC<-prcomp(gene_data_final)
  PCi<-data.frame(PC$x,time_point=paste(meta_data$Time,meta_data$Unit))
  #install.packages("ggplot2")
  library(ggplot2)
  PCAplot<-ggplot(PCi,aes(x=PC1,y=PC2,col=time_point))+
  geom_point(size=3,alpha=0.5)
  return(PCAplot)
  
  
}
