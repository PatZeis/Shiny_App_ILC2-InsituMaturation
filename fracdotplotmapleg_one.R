# alternative color palette 
#colorPalette = c("grey", "tan1", "red", "#7a0f09", "black")
#  + scale_colour_gradientn(colours=alpha(colorRampPalette(colorPalette)(100), 0.8))

fracdotplot <- function( object, genes, cluster=NULL, samples=NULL, limup, limdown, zsc=T, map=T, leg=T) {
  library(ggplot2)
  library(RColorBrewer)
  ndata <- as.matrix(object@ndata * min(object@counts)) + 0.1
  ndata <- data.frame(ndata) 
  if ( !is.null(cluster) & !is.null(samples)) {
    stop("define either clusters OR samples")
  }
  if (is.null(cluster) & is.null(samples)) {
    stop("define either clusters OR samples")
  }
  if (!is.null(cluster)) {
  genevec <- c()
  clustervec <- c()
  fraction <- c()
  scaled_mean <- c()
  log2mean <- c()
  for ( i in 1:length(genes)) {
    repgene <- rep(genes[i], length(cluster))
    meang <- mean(as.numeric(object@ndata[genes[i],]))
    sdg <- sd(object@ndata[genes[i],])
    repclus <- c()
    frac <- c()
    cent_mean <- c()
    log2_mean <- c()
    for ( n in 1:length(cluster)) {
      clus <- names(object@cpart[object@cpart == cluster[n]])
      leng_clus <- length(clus)
      if (zsc==T) {
        leng_gene_in_clus <- length(which(object@ndata[genes[i], clus] > 0))
      }
      else {leng_gene_in_clus <- length(which(ndata[genes[i], clus] > 0.1))}
      frac <- c(frac, leng_gene_in_clus/leng_clus)
      #repclus <- c(repclus, paste("cl",cluster[n], sep="_"))
      repclus <- c(repclus, cluster[n])
      cent_mean <- c(cent_mean, (mean(as.numeric(object@ndata[genes[i], clus])) - meang)/sdg)
      log2_mean <- c(log2_mean, log2(mean(as.numeric(ndata[genes[i], clus]))))
    }
    genevec <- c(genevec, repgene) 
    clustervec <- c(clustervec, repclus)
    fraction <- c(fraction, frac)
    scaled_mean <- c(scaled_mean, cent_mean)
    log2mean <- c(log2mean, log2_mean)
  }
  if ( zsc==T ) {
    data <- data.frame(Gene = factor(genevec, levels = genes) , Cluster = factor(clustervec, levels = cluster), Fraction = fraction, Expression = scaled_mean )
  }
  else {
    data <- data.frame(Gene = factor(genevec, levels = genes) , Cluster = factor(clustervec, levels = cluster), Fraction = fraction, Expression = log2mean )  
  }
  data[which(data$Expression > limup), "Expression"] <- limup
  data[which(data$Expression < limdown), "Expression"] <- limdown
  ColorRamp <- colorRampPalette(rev(brewer.pal(n = 7,name = "RdYlBu")))(100)
  
  frac <- ggplot(data, aes(x = Gene, y = Cluster))  + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_blank()) + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())                                                                    
  
  if ( map==T && leg==F) {
    print(frac + geom_point(aes(size = Fraction, color = Expression))  + scale_colour_gradientn(colours = ColorRamp) + theme(legend.position="none"))
  }
  if (leg==T && map==F){
    print(frac + theme(axis.line = element_line(colour = "black")) + theme(axis.title = element_text(color = "black"), axis.text = element_text(color = "black"),axis.ticks = element_line(color = "black"),axis.text.x = element_text(angle = 90, hjust = 1)))
  }
  if (map==T && leg==T) {
    print(frac + geom_point(aes(size = Fraction, color = Expression))  + scale_colour_gradientn(colours = ColorRamp) + theme(axis.line = element_line(colour = "black")) + theme(axis.title = element_text(color = "black"), axis.text = element_text(color = "black"),axis.ticks = element_line(color = "black"),axis.text.x = element_text(angle = 90, hjust = 1)) )
  }
}

  if (!is.null(samples)) {
  genevec <- c()
  samplevec <- c()
  fraction <- c()
  fraction_log <- c()
  scaled_mean <- c()
  log2mean <- c()
  for ( i in 1:length(genes)) {
    repgene <- rep(genes[i], length(samples))
    meang <- mean(as.numeric(object@ndata[genes[i],]))
    sdg <- sd(object@ndata[genes[i],])
    repsamp <- c()
    frac <- c()
    frac_log <- c()
    cent_mean <- c()
    log2_mean <- c()
    for ( n in 1:length(samples)) {
      samp <- colnames(object@ndata)[grep(samples[n], colnames(object@ndata))]
      leng_samp <- length(samp)
      leng_gene_in_samp <- length(which(object@ndata[genes[i], samp ]> 0))
      frac <- c(frac, leng_gene_in_samp/leng_samp)
      repsamp <- c(repsamp, samples[n])
      cent_mean <- c(cent_mean, (mean(as.numeric(object@ndata[genes[i], samp])) - meang)/sdg)
      log2_mean <- c(log2_mean, log2(mean(as.numeric(ndata[genes[i], samp]))))
    }
    genevec <- c(genevec, repgene) 
    samplevec <- c(samplevec, repsamp)
    fraction <- c(fraction, frac)
    scaled_mean <- c(scaled_mean, cent_mean)
    log2mean <- c(log2mean, log2_mean)
  }
  if (zsc==T) {
    data <- data.frame(Gene = factor(genevec, levels = genes) , Sample = factor(samplevec, levels = samples), Fraction = fraction, Expression = scaled_mean )
  }
  else {
    data <- data.frame(Gene = factor(genevec, levels = genes) , Sample = factor(samplevec, levels = samples), Fraction = fraction, Expression = log2mean )  
  }
  data[which(data$Expression > limup), "Expression"] <- limup
  data[which(data$Expression < limdown), "Expression"] <- limdown
  ColorRamp <- colorRampPalette(rev(brewer.pal(n = 7,name = "RdYlBu")))(100)
  
  frac <- ggplot(data, aes(x = Gene, y = Sample))  + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_blank()) + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())                                                                    
  
  if ( map==T && leg==F) {
    print(frac + geom_point(aes(size = Fraction, color = Expression))  + scale_colour_gradientn(colours = ColorRamp) + theme(legend.position="none"))
  }
  if (leg==T && map==F){
    print(frac + theme(axis.line = element_line(colour = "black")) + theme(axis.title = element_text(color = "black"), axis.text = element_text(color = "black"),axis.ticks = element_line(color = "black"),axis.text.x = element_text(angle = 90, hjust = 1)))
  }
  if (map==T && leg==T) {
    print(frac + geom_point(aes(size = Fraction, color = Expression))  + scale_colour_gradientn(colours = ColorRamp) + theme(axis.line = element_line(colour = "black")) + theme(axis.title = element_text(color = "black"), axis.text = element_text(color = "black"),axis.ticks = element_line(color = "black"),axis.text.x = element_text(angle = 90, hjust = 1)) )
  }
  }
}