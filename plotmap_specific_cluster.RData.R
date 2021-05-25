plotmap2 <- function (object, final = TRUE, tp = 1, fr = FALSE, cex = 0.5, cluster=NULL) 
{
  if (length(object@tsne) == 0 & length(object@fr) == 0) 
    stop("run comptsne/compfr before plotmap")
  if (final & length(object@cpart) == 0) 
    stop("run findoutliers before plotmap")
  if (!final & length(object@cluster$kpart) == 0) 
    stop("run clustexp before plotmap")
  if (!is.numeric(tp) | (is.numeric(tp) & tp > 1 | tp < 0)) 
    stop("tp has to be a number between 0 and 1 (transparency)")
  if (!is.logical(fr)) 
    stop("fr has to be TRUE or FALSE")
  part <- if (final) 
    object@cpart
  else object@cluster$kpart
  if (fr | dim(object@tsne)[1] == 0) 
    d <- object@fr
  else d <- object@tsne
  row.names(d) <- names(part)
  plot(d, xlab = "", ylab = "", cex = 0, axes = FALSE)
  for (i in 1:max(part)) {
    if (sum(part == i) > 0) {
      
      if ( i %in% cluster) {
        points(d[part == i, 1], d[part == i, 2], col = adjustcolor(object@fcol[i], tp), pch = 20, cex = cex)}
    
    else { points(d[part == i, 1], d[part == i, 2], col = adjustcolor("lightgrey", tp), pch = 20, cex = cex)}
  } }                                                                                  
  for (i in 1:length(cluster)) {
    if (sum(part == cluster[i]) > 0) {
      points(d[object@medoids[cluster[i]], 1], d[object@medoids[cluster[i]], 
                                                 2], col = adjustcolor(object@fcol[cluster[i]], tp), pch = 20, 
             cex = 4)
    if (sum(part == cluster[i]) > 0) 
      points(d[object@medoids[cluster[i]], 1], d[object@medoids[cluster[i]], 
                                                 2], col = adjustcolor("white", tp), pch = 20, 
             cex = 3)
    if (sum(part == cluster[i]) > 0) 
      text(d[object@medoids[cluster[i]], 1], d[object@medoids[cluster[i]], 
                                               2], cluster[i], col = adjustcolor("black", tp), cex = 0.75, 
           font = 4)
  }}
}