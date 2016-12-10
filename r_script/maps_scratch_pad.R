

#sort alphabetically
# dat1 <- dat1[order(dat1$Neighbourhood), ]
# map$AREA_NAME <- map[order(map$AREA_NAME), ]
# large_pop$Neighbourhood <- large_pop[order(large_pop$Neighbourhood), ]

#### create maps of population, housing prices, etc using tmap ###

qtm(shp = map, fill = c("total_population", "home_price"), fill.palette = "Blues")

traffic <- qtm(shp = map, fill = "traffic_collisions", fill.palette = "Greens")

# tmfacets breaks down the map into each individual unit (neighbourhood)
tm_shape(map) +
   tm_fill("total_population", thres.poly = 0) +
   tm_facets("total_population", free.coords=TRUE, drop.shapes=TRUE) +
   tm_layout(legend.show = FALSE, title.position = c("center", "center"), title.size = 20)


###### making maps using spplot ######

# first, choose colour palettes
library(RColorBrewer)
my.col <- brewer.pal(n = 9, name = "YlOrRd")

spplot(map, "home_price", main = "Home Prices",
        col.regions = my.col, cuts = 8, col = "transparent")

# to add layer, first make object as layer-list, then plot with sp.layout
ttc1 <- list("sp.lines", ttc, col = "grey", lwd = 3)
ttc2 <- list("sp.lines", ttc, col = "white", lwd = 1)
spplot(map, "home_price", sp.layout = list(ttc), col.regions = my.col, cuts = 8, col = "transparent")

spplot(map, "home_price", sp.layout = list(ttc, col = "grey", size = 3), col.regions = my.col, cuts = 8, col = "transparent")
