plotexpression <- function (x, y, g, n, col = NULL, name = NULL, cluster = FALSE, 
                            alpha = 0.5, types = NULL, cex = 3, ylim = NULL, map = TRUE, 
                            leg = TRUE, lwd = 2.5, legendpos="topleft", samp=F, samppart=NULL) {
  library(RColorBrewer)
  set1 <- brewer.pal(length(g), "Set1")
  set2 <- brewer.pal(length(g), "Set2")
  set4 <- brewer.pal(7, "Set2")
  set3 <- brewer.pal(7, "Set1")
  set3 <- c(set3[7], set4[6],set3[5], set3[3], set3[4], set3[2], set3[1]) 
  if (length(g) >= 6) {set1[6] <- set2[6]}
  for ( i in 1:length(g)){
    cl <- unique(y[n])
    set.seed(111111)
    if (is.null(col)) 
      col <- sample(rainbow(max(y)))
    xlim <- c(1, length(n))
    if (!is.null(types)) 
      xlim[1] <- 1.25 * xlim[1]
    z <-  x[g[i], n]
    if (is.null(name)) 
      name <- g[i]
    if (leg) {
      ylab = "Expression"
    }
    else {
      ylab = NA
    }
    if (leg) {
      #main = name
      main = ""
    }
    else {
      main = NA
    }
    if (i == 1){
      if (is.null(ylim)) {
        plot(c(1, length(n)), c(min(z), max(z)), cex = 0, axes = FALSE, 
             xlab = "", ylab = ylab, main = main, xlim = xlim)
      }
      else {
        plot(c(1, length(n)), c(min(z), max(z)), cex = 0, axes = T, 
             xlab = "", ylab = ylab, main = main, xlim = xlim, 
             ylim = ylim, xaxt='n')
      }}
    if (i==1) {
      if (map) {
        if (!is.null(types)) {
          coloc <- rainbow(length(unique(types)))
          syms <- c()
          for (i in 1:length(unique(types))) {
            f <- types == sort(unique(types))[i]
            syms <- append(syms, ((i - 1)%%25) + 1)
            points((1:length(n))[f], t(z)[f], col = coloc[i], 
                   pch = ((i - 1)%%25) + 1, cex = 1)
          }
        }
        else {
          for (i in 1:length(cl)) {
            f <- y[n] == cl[i]
            points((1:length(n))[f], t(z)[f], pch = 20, cex = cex, 
                   col = col[cl[i]])
          }
        }
      }}
    if (leg) {
      u <- 1:length(n)
      v <- as.vector(t(z))
      zc <- predict(loess(v ~ u, span = alpha))
      zc[zc < 0] <- 0.1
      lines(u, zc, lwd = lwd, col=set1[i])
    }
  }
  width <- .45
  
  k <- 1:length(y)
  
  rect(xleft = k-width, ybottom = rep(-0.2, length(y)), xright =  k + width, ytop = rep(0, length(y)), col = col[y], border = NA)

    if ( samp == T) {
      if ( is.null(samppart)){
        stop("set samp part ")
      }
      rect(xleft = k-width, ybottom = rep(-0.5, length(y)), xright =  k + width, ytop = rep(-0.3, length(y)), col = set3[samppart], border = NA)
    }
  
  if (!leg) 
    box(col = "white")
  else legend(legendpos, g, col=set1, pch=20)
}