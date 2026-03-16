library(marmap) ; library(shape) ; library(raster)

# Download bathymetry for North Atlantic
dat <- getNOAA.bathy(-90,10, 0, 70, res=15, keep=TRUE)

# Create subsets for land and sea in two distinct bathy objects
ll <- dat ; ll[ll<0] <- NA
ss <- dat ; ss[ss>=0] <- NA

# Create layers containing data for only a specified depth range for appropriate coloring 
 z1 <- dat ;  z1[ z1 >= -5000 |  z1 < min(dat)] <- NA
 z2 <- dat ;  z2[ z2 >= -3000 |  z2 <    -5000] <- NA
 z3 <- dat ;  z3[ z3 >= -2000 |  z3 <    -3000] <- NA
 z4 <- dat ;  z4[ z4 >= -1000 |  z4 <    -2000] <- NA
 z5 <- dat ;  z5[ z5 >=  -750 |  z5 <    -1000] <- NA
 z6 <- dat ;  z6[ z6 >=  -500 |  z6 <     -750] <- NA
 z7 <- dat ;  z7[ z7 >=  -250 |  z7 <     -500] <- NA
 z8 <- dat ;  z8[ z8 >=  -100 |  z8 <     -250] <- NA
 z9 <- dat ;  z9[ z9 >=   -50 |  z9 <     -100] <- NA
z10 <- dat ; z10[z10 >=     0 | z10 <      -50] <- NA

# Create a layer containing shading information for the map
ra <- marmap::as.raster(dat)
slope <- terrain(ra, opt='slope')
aspect <- terrain(ra, opt='aspect')
ra.shade <- hillShade(slope, aspect, angle = 40, direction = 270)
shade <- as.bathy(ra.shade)

# Create color palettes for land and sea
sea <- c("#E7E8B0","#CED69B","#B7D091","#A5D38E","#9AD790","#97D593","#87D1DA","#87B9E3","#84A8DA","#8E8DCC")
greys <- c(grey(0.3), grey(0.38), grey(0.40))
bl <- colorRampPalette(sea)
gr <- colorRampPalette(greys)

### Plot with shadows
par(mar=c(4,4,2,7)) # Get some extra space in the right margin to add the color scale
plot(shade, image = TRUE, land = TRUE, lwd = 0, bpal = grey(30:100/100), las=1) # Shadows layer
plot(   z1, image = TRUE, land = TRUE, lwd = 0, add=TRUE, bpal = col2alpha(sea[10], 0.5)) # Add one layer at a time with alpha transparency
plot(   z2, image = TRUE, land = TRUE, lwd = 0, add=TRUE, bpal = col2alpha(sea[ 9], 0.5)) # Add one layer at a time with alpha transparency
plot(   z3, image = TRUE, land = TRUE, lwd = 0, add=TRUE, bpal = col2alpha(sea[ 8], 0.5)) # Add one layer at a time with alpha transparency
plot(   z4, image = TRUE, land = TRUE, lwd = 0, add=TRUE, bpal = col2alpha(sea[ 7], 0.5)) # Add one layer at a time with alpha transparency
plot(   z5, image = TRUE, land = TRUE, lwd = 0, add=TRUE, bpal = col2alpha(sea[ 6], 0.5)) # Add one layer at a time with alpha transparency
plot(   z6, image = TRUE, land = TRUE, lwd = 0, add=TRUE, bpal = col2alpha(sea[ 5], 0.5)) # Add one layer at a time with alpha transparency
plot(   z7, image = TRUE, land = TRUE, lwd = 0, add=TRUE, bpal = col2alpha(sea[ 4], 0.5)) # Add one layer at a time with alpha transparency
plot(   z8, image = TRUE, land = TRUE, lwd = 0, add=TRUE, bpal = col2alpha(sea[ 3], 0.5)) # Add one layer at a time with alpha transparency
plot(   z9, image = TRUE, land = TRUE, lwd = 0, add=TRUE, bpal = col2alpha(sea[ 2], 0.5)) # Add one layer at a time with alpha transparency
plot(  z10, image = TRUE, land = TRUE, lwd = 0, add=TRUE, bpal = col2alpha(sea[ 1], 0.5)) # Add one layer at a time with alpha transparency

plot(ll, image = TRUE, land = T, lwd = 0, add=TRUE, bpal = col2alpha(gr(100), 0.7))	# Same for land

plot(dat, deep =     0, shallow =     0, step = 0, lwd = 0.40, add = TRUE) # Add coastline
plot(dat, deep =   -50, shallow =   -50, step = 0, lwd = 0.08, add = TRUE) # Add   -50m isobath
plot(dat, deep =  -100, shallow =  -100, step = 0, lwd = 0.08, add = TRUE) # Add  -100m isobath
plot(dat, deep =  -250, shallow =  -250, step = 0, lwd = 0.08, add = TRUE) # Add  -250m isobath
plot(dat, deep =  -500, shallow =  -500, step = 0, lwd = 0.08, add = TRUE) # Add  -500m isobath
plot(dat, deep =  -750, shallow =  -750, step = 0, lwd = 0.08, add = TRUE) # Add  -750m isobath
plot(dat, deep = -1000, shallow = -1000, step = 0, lwd = 0.08, add = TRUE) # Add -1000m isobath
plot(dat, deep = -2000, shallow = -2000, step = 0, lwd = 0.08, add = TRUE) # Add -2000m isobath
plot(dat, deep = -3000, shallow = -3000, step = 0, lwd = 0.08, add = TRUE) # Add -3000m isobath
plot(dat, deep = -5000, shallow = -5000, step = 0, lwd = 0.08, add = TRUE) # Add -5000m isobath
plot(dat, deep = -9000, shallow = -9000, step = 0, lwd = 0.08, add = TRUE) # Add -9000m isobath

### Create color scale
lev <- rev(c(0, 50, 100, 250, 500, 750, 1000, 2000, 3000, 5000, 9000)) # Labels for the scale
col <- rev(sea)  # Colors for the scale
offset <- 3 # Distance between the map and the left side of the scale

# Some dirty code found online and slightly modified to adjust the position and width of the scale
opar <- par ; n <- length(col) ; bx <- par("usr") 
box.cx <- c(bx[2] + (bx[2] - bx[1]) / 1000, bx[2] + (bx[2] - bx[1]) / 1000 + (bx[2] - bx[1]) / 50)
box.cy <- c(bx[3], bx[3]) ; box.sy <- (bx[4] - bx[3]) / n 
xx <- rep(box.cx, each = 2)
 
# Draw colored rectangles
par(xpd = TRUE)
for(i in 1:n){
  yy <- c(box.cy[1] + (box.sy * (i - 1)),
  box.cy[1] + (box.sy * (i)),
  box.cy[1] + (box.sy * (i)),
  box.cy[1] + (box.sy * (i - 1)))
  polygon(xx+offset, yy, col = col[i], border = 1)
}

# Add scale title: use locator() to find appropriate coordinates
text(x=15.25, y=yy[2] + 2, "depth (m)")

# Add scale labels
par(new = TRUE)
plot(0, 0, type = "n", ylim = c(min(lev), max(lev)), yaxt = "n", ylab = "", xaxt = "n", xlab = "", frame.plot = FALSE)
bx2 <- par("usr")
axis(side = 4, las = 2, tick = FALSE, line = 1.5, at=seq(bx2[3], bx2[4], length.out=length(lev)), labels=lev)
par <- opar

