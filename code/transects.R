# Adam wrote me in Erlangen on 2025-10-21.

# you know what to do...
setwd("/mnt/sky/Dropbox/WorkSpace/2025-10-21_meemarine/bridge_transects")

library(terra) # basic rasters
library(fields) # plotting
library(viridisLite) # plotting

# example from Valdes et al. 2021 (Deep ocean temperatures...)
# This netCDF is from 85 Ma
big <- terra::rotate(terra::rast("data/texpr2o.pgclann.nc", subds="temp_ym_dpth"))

# the surface!
plot(big[[1]])

# get the depth as a numeric variable
once <- sapply(strsplit(names(big),"_"), function(x) x[5])
depth <- as.numeric(gsub("1=", "", once))

# The whole thing as a big array (easier to manipulate)
vals <- as.array(big)

# marginals
xy<- terra::xyFromCell(big, 1:prod(dim(big)[1:2]))
longitude<- unique(xy[,1])
latitude<- sort(unique(xy[,2])) # change to increasing

################################################################################
# maximum visualized depth
maxDepth <- 500

# 1. Latitudinal transects
# index of longitude
i <- 80
focalLong <- longitude[i]
message(focalLong)

# (lower to higher is required in imagePlot)
# map
par(mfrow=c(2,1))
plot(big[[1]], main="Surface Temp, deg. C")
abline(v=focalLong, col="red", lty=2)

# for the consistent ramping of the transects (close enough)
surfaceRange <- range(terra::values(big[[1]]), na.rm=TRUE)
breaks <- seq(surfaceRange[1], surfaceRange[2], length.out=256)

# the transect
# reverse latitude so it goes from south to north
transect <- vals[length(latitude):1, longitude==focalLong, ]
fields::imagePlot(z=transect, x=latitude, y=depth, ylim=c(maxDepth,0), xlim=c(90, -90),
	breaks=breaks, col=viridisLite::viridis(length(breaks)-1))

# Something is fishy with the north pole with this climate model run.
# Subsurface ocean? :D

################################################################################
# 2. Longtiudinal transects
# index of latitude
j <- 55
focalLat <- latitude[j]
message(focalLat)

# map
par(mfrow=c(2,1))
plot(big[[1]], main="Surface Temp, deg. C")
abline(h=focalLat, col="red", lty=2)

# the transect
transect <- vals[rev(latitude)==focalLat, , ]
fields::imagePlot(z=transect, y=depth, x=longitude, xlim=c(-180, 180), ylim=c(maxDepth, 0),
	breaks=breaks, col=viridisLite::viridis(length(breaks)-1))
