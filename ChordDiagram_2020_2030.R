# Required libraries
library(raster)
library(circlize)

# Working directory
setwd("D:/1. COURSES/Doctoral_S3/Data/lulcmaps/")

# LULC classes names and color codes
lulc_classes <- c("Forest","Rangeland","Wetland","Water body","Cropland","Built-up")
lulc_colors <- setNames(c("#2E8B57","#7CFC00","#00CED1","#1E90FF","#FFD700","#FF0000"), lulc_classes)

# Raster image loading
cat("Loading LULC rasters for 2020 and 2030...\n")
lulc_2020 <- raster("LULC2020.tif")
lulc_2030 <- raster("LULC2030.tif")

# Image compatibility checking
if (!compareRaster(lulc_2020, lulc_2030, extent=TRUE, rowcol=TRUE, crs=TRUE, res=TRUE)) {
  stop("ERROR: The rasters do not match!")
} else cat("Raster properties match.\n")

# Values extraction and null value removal
v1 <- getValues(lulc_2020)
v2 <- getValues(lulc_2030)
valid <- !is.na(v1) & !is.na(v2)
v1 <- v1[valid]
v2 <- v2[valid]

# Transition matrix construction
tm <- matrix(0, nrow=6, ncol=6, dimnames=list(lulc_classes, lulc_classes))
for (k in seq_along(v1)) {
  i <- v1[k]; j <- v2[k]
  if (i >= 1 && i <= 6 && j >= 1 && j <= 6) tm[i, j] <- tm[i, j] + 1
}
write.csv(tm, "transition_matrix_2020_2030.csv", row.names=TRUE)

# Total pixels
total_pixels <- sum(tm)

# Positions for links
row_totals <- rowSums(tm)
col_totals <- colSums(tm)
get_positions <- function(vals, total) {
  if (total == 0) return(list(starts=numeric(0), ends=numeric(0)))
  props <- vals / total
  starts <- cumsum(c(0, props))[1:length(vals)]
  ends <- starts + props
  list(starts=starts, ends=ends)
}
from_pos <- lapply(seq_len(6), function(i) get_positions(tm[i,], row_totals[i]))
to_pos   <- lapply(seq_len(6), function(j) get_positions(tm[,j], col_totals[j]))

# Chord diagram preparation
tiff("ChordDiagram_2020_2030.tiff", width=6, height=6, units="in", res=600, compression="lzw")
par(mar=rep(0,4), oma=rep(0,4), xpd=NA)

# Initialize circos
circos.clear()
circos.par(start.degree=90, gap.degree=3, track.margin=c(0,0), cell.padding=c(0,0,0,0))
circos.initialize(factors=lulc_classes, xlim=matrix(c(rep(0,6), rep(1,6)), ncol=2))

# First track (outer) with LULC class labels
circos.trackPlotRegion(
  track.index = 1, ylim = c(0, 1), track.height = 0.18,
  bg.col = lulc_colors, bg.border = "black",
  panel.fun = function(x, y) {
    sec <- get.cell.meta.data("sector.index")
    xlim <- get.cell.meta.data("xlim"); ylim <- get.cell.meta.data("ylim")
    circos.text(mean(xlim), mean(ylim), labels = sec,
                facing = "bending.inside", niceFacing = TRUE,
                cex = 1,
                col = "black", 
                family = "Arial",
                font = 2)
  }
)
# Second (empty) buffer track
circos.trackPlotRegion(track.index = 2, ylim = c(0, 1), track.height = 0.05,
                       bg.col = NA, bg.border = NA, panel.fun = function(x, y) {})

# Non-overlapping links
min_lwd <- 0.5; max_lwd <- 15
for (i in seq_len(6)) {
  for (j in seq_len(6)) {
    val <- tm[i, j]
    if (val > 0) {
      rp <- from_pos[[i]]
      tp <- to_pos[[j]]
      x1 <- c(rp$starts[j], rp$ends[j])
      x2 <- c(tp$starts[i], tp$ends[i])
      prop <- val / total_pixels
      lwd_val <- min_lwd + prop * (max_lwd - min_lwd)
      circos.link(sector.index1=lulc_classes[i], point1=x1,
                  sector.index2=lulc_classes[j], point2=x2,
                  col=adjustcolor(lulc_colors[lulc_classes[i]], alpha.f=0.4),
                  border=NA, lwd=lwd_val)
    }
  }
}

# Tick marks and percentage labels
tick_positions <- seq(0, 1, length.out = 5)
label_positions <- tick_positions
label_texts <- c("0%", "25%", "50%", "75%")

op <- par(family = "Arial")
for (i in seq_along(lulc_classes)) {
  # Exterior ticks (length = 0.3)
  circos.axis(
    h = "top", major.at = tick_positions, labels = rep("", 14),
    sector.index = lulc_classes[i], track.index = 1,
    col = "black", lwd = 0.6, major.tick.length = 0.3
  )
  
  # Percentage labels above the ticks
  for (j in seq_along(label_positions)) {
    x <- label_positions[j]
    y <- 1.4
    circos.text(
      x = x, y = y,
      labels = label_texts[j],
      sector.index = lulc_classes[i], track.index = 1,
      facing = "outside", niceFacing = TRUE,
      cex = 0.8, col = "black", family = "Arial",
      font = 2
    )
  }
}
par(op)

dev.off()
cat("✔ Chord diagram saved in the working directory\n")