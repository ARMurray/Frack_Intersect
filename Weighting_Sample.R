library(sf)
library(ggplot2)

#The goal here is to iterate through the well pads and determine
#the areas of blocks that are at risk of contamination and to weight
#estimated well use into those areas so we can come up with a total
#number of wells at risk from each well pad

setwd("D:/OneDrive/OneDrive - University of North Carolina at Chapel Hill/Documents/My_Papers/Fracking")

#First, lets import the well pad and the block data and then project them to
#a projection that's good for our working area.
pads <- st_read("shapefiles/OH_Well.shp")
blocks <- st_read("shapefiles/OH_Blocks_Sub.shp")
# Project data
pads <- st_transform(pads,3158)
blocks <- st_transform(blocks, 3158)

# Add some data to the Census Blocks
# blocks$

# Create buffers around the well pads of 1,000 km
buf <- st_buffer(pads, 1000)
blk_pts <- st_centroid(blocks)
# This plot shows a group of Census blocks with estimated numbers of wells within each one.
# The buffer shown with the circle is 1 km from the fracking operation. #In order to estimate
# the number of wells within the buffer, we need to use area to weight the number of wells within
# each intersecting census block that are likely to also be within the 1 km buffer
p1 <- ggplot()+
  geom_sf(data = blocks)+
  geom_sf(data = buf, fill = NA, color = 'black', size = 2)+
  geom_sf(data = pads, size = 5, pch = 17, col = 'red')+
  geom_sf_label(data = blk_pts, mapping = aes(label = as.integer(Hybd_Blk_W)))+
  xlab("")+ylab("")

p1


# In the following loop, we will iterate through every oil/gas well to create buffers and intersections with
# the census blocks within the buffer area. The reason we need to iterate through one gas well at a time is
# because if another gas well exists within the buffer distance, they will overlap and conflict
# with eachothers geometries, giving us bad results.
outdf <- data.frame()
for (r in 1:nrow(pads)){
  row <- pads[r,]
  buffer <- st_buffer(row, 1000)
  intersects <- st_intersection(blocks,buffer)
  intersects$NewArea <- st_area(intersects)
  intersects$wells <- intersects$Hybd_Blk_W * (intersects$NewArea / intersects$Shape_Area)
  wells <- sum(intersects$wells)
  new <- data.frame("API_Number"=row$API,"Est_Wells"=wells)
  outdf <- rbind(outdf,new)
}

# Make pts based on centroids to display the number of wells in a plot
# NOTE: This will only pull information from the last iteration of the for loop above
# and will not include all of the well pads/blocks
pts <- st_centroid(intersects)
coords <- as.data.frame(st_coordinates(pts))
pts$X <- coords['X']
pts$Y <- coords['Y']

# Create a plot showing the estimated number of wells within each part of each 
# block that intersects the buffer area around the oil/gas well  
p2 <- ggplot()+
  geom_sf(data = intersects, size = 1.5)+
  geom_sf(data = row, size = 5, pch = 17, col = 'red')+
  geom_sf_label(data = pts, mapping = aes(label=as.integer(wells)))+
  xlab("")+ylab("")
  
p2
