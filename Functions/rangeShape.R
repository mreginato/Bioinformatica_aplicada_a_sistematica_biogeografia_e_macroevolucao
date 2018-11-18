rangeShape <- function(data, out_shape, asc_in, thresholds, buffer.extent=5, circles=TRUE, overlay=TRUE, format=".tif", fill.holes=F) {
  getwd() -> wd
  colnames(data) <- c("Species", "x", "y")
  unique(data) -> data
  unique(as.character(data$Species)) -> taxa
  for (i in 1:length(taxa)) {
    as.character(taxa[i]) -> sp0
    cat("Range of",sp0, fill=T)
    data[data$Species == sp0,] -> sp0.data
    if (nrow(sp0.data) == 1) {
      rbind(sp0.data, sp0.data) -> sp0.data
    }
    coordinates(sp0.data) = ~x+y
    setwd(asc_in)
    raster(paste(sp0,format,sep="")) -> tlayer -> layer2
    layer2[cellFromXY(layer2, sp0.data)] <- 1
    thresholds[sp0] -> t0
    layer2[layer2 < t0] <- NA
    if (circles) {
      polygons(circles(sp0.data, d=buffer.extent)) -> c0
      mask(layer2, c0) -> int
      crop(int, c0) -> int
      
    } else {
      extent(sp0.data) -> ext0
      ext0 ++ buffer.extent -> ext0
      intersect(layer2, ext0) -> int
    }
    rasterToPolygons(int, dissolve=F) -> conv2
    gUnaryUnion(conv2) -> convF
    if (overlay == T) {
      out0 <- lapply(convF@polygons , slot , "Polygons")[[1]]
      plist <- vector("list", length=length(out0))
      if (length(plist) > 1) {
        unlist(lapply(out0, FUN=function(x)(x@hole))) -> w0
        for (k in 1:length(out0)) {
          Polygons(list(Polygon(out0[[k]])), k) -> plist[[k]]
        }
        plist[which(w0)] -> w0
        SpatialPolygons(w0) -> w0
        SpatialPolygons(plist) -> convS
        na.omit(over(sp0.data, convS)) -> over0
        SpatialPolygons(plist[unique(over0)]) -> convF
        gUnaryUnion(convF) -> convF
        if (fill.holes == F) {
          if (length(w0) > 0) {
            gDifference(convF, w0) -> convF
          }
        }
      }
    } 
    convF<-SpatialPolygonsDataFrame(convF,data=as.data.frame("1"))
    setwd(out_shape)
    suppressWarnings(writePolyShape(convF, sp0))     
  }
  setwd(wd)  
}