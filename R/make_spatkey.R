# make_spatkey <- function(grid) {
#
#   cent <- st_centroid(grid$geometry)
#   centXY <- round(st_coordinates(cent),-1) %>%
#     as.data.frame() %>%
#     mutate(conus.grid.id = grid$conus.grid.id)
#
#   X <- sort(unique(centXY[,1]))
#   Y <- sort(unique(centXY[,2]))
#
#   NX <- length(X)
#   NY <- length(Y)
#
#   key1 <- c(1,10,5,14,9,4,13,8,3,12,7,2,11,6)
#   key2 <- c(10,5,14,9,4,13,8,3,12,7,2,11,6,1)
#
#
#   newcentXY1 <- matrix(NA,0,2)
#
#   for (a in 1:14){
#     x <- seq(key1[a],NX,14)
#     y <- seq(a,NY,14)
#     xy <- expand.grid(x,y)
#     xy <- cbind(X[xy[,1]],Y[xy[,2]])
#     newcentXY1 <- rbind(newcentXY1,xy)
#   }
#
#   newcentXY2 <- matrix(NA,0,2)
#
#   for (a in 1:14){
#     x <- seq(key2[a],NX,14)
#     y <- seq(a,NY,14)
#     xy <- expand.grid(x,y)
#     xy <- cbind(X[xy[,1]],Y[xy[,2]])
#     newcentXY2 <- rbind(newcentXY2,xy)
#   }
#
#   # plot(centXY$X, centXY$Y)
#   # points(newcentXY1[,1], newcentXY1[,2], col = "red", pch = 19)
#   # points(newcentXY2[,1], newcentXY2[,2], col = "blue", pch = 19)
#
#   newcentXY1 <- data.frame(cbind(1:nrow(newcentXY1),newcentXY1))
#   colnames(newcentXY1) <- c("cell","X","Y")
#
#   centXY1 <- data.frame(cbind(rep(NA,nrow(centXY)),centXY))
#   colnames(centXY1) <- c("cell","X","Y","conus.grid.id")
#
#   for (a in 1:nrow(newcentXY1)){
#     centXY1$cell[which(sqrt((centXY1$X-newcentXY1$X[a])^2 + (centXY1$Y-newcentXY1$Y[a])^2)<8000)] = newcentXY1$cell[a]
#   }
#
#   a <- centXY1 %>%
#     # Have to filter out missing (orphan) cells, otherwise NA will have more than 7 "neighbors"
#     filter(!is.na(cell)) %>%
#     group_by(cell) %>%
#     summarize(count=n())
#
#   if(max(a$count)<=7){
#     centXY <- centXY1
#     newcentXY <- newcentXY1
#   } else {
#     newcentXY2 <- data.frame(cbind(1:nrow(newcentXY2),newcentXY2))
#     colnames(newcentXY2) <- c("cell","X","Y")
#
#     centXY2 <- data.frame(cbind(rep(NA,nrow(centXY)),centXY))
#     colnames(centXY2) <- c("cell","X","Y","conus.grid.id")
#
#     for (a in 1:nrow(newcentXY2)){
#       centXY2$cell[which(sqrt((centXY2$X-newcentXY2$X[a])^2 + (centXY2$Y-newcentXY2$Y[a])^2)<8000)] = newcentXY1$cell[a]
#     }
#
#     centXY <- centXY2
#     newcentXY <- newcentXY2
#   }
#
#   # Isolate cells with NA and give them correct cell affiliations
#   centXY %>%
#     # Isolate centroids with missing cell assignments
#     filter(is.na(cell)) %>%
#     left_join(., grid, by = 'conus.grid.id') %>%
#     select(cell, conus.grid.id, geometry) %>%
#     st_as_sf() -> cellNA
#
#   NAind <- which(is.na(centXY$cell))
#
#   NB <- spdep::poly2nb(cellNA)
#   NBinfo <- spdep::nb2WB(NB)
#
#   cellNA$neighbors <- NBinfo$num
#   adj <- NBinfo$adj
#
#   cellNA$cell[1] <- 1
#
#   for (i in 1:nrow(cellNA)) {
#     if (cellNA$neighbors[i] == 0 & is.na(cellNA$cell[i])) {
#       cellNA$cell[i] = max(cellNA$cell, na.rm=T) + 1
#     } else if (cellNA$neighbors[i] > 0) {
#       if (is.na(cellNA$cell[i])) {
#         index <- adj[1:cellNA$neighbors[i]]
#         cellNA$cell[i] <- max(cellNA$cell, na.rm=T) + 1
#         # Give neighboring cells the same cell number
#         cellNA$cell[index] <- cellNA$cell[i]
#
#         # Remove the indices from adj that you just used
#         adj <- adj[-(1:cellNA$neighbors[i])]
#       } else {
#         # Remove the indices from adj that you just used
#         adj <- adj[-(1:cellNA$neighbors[i])]
#       }
#     }
#   }
#
#   cellNA <- select(cellNA, -neighbors)
#
#   centXY %>%
#     # Filter out missing cells - dealt with above
#     #filter(!is.na(cell)) %>%
#     left_join(grid, centXY, by = 'conus.grid.id') %>%
#     select(conus.grid.id, cell, geometry) -> spatkey1 #%>%
#   # Change cellIDs in cellNA to extend cell list from centXY
#   #bind_rows(., mutate(cellNA, cell = cell + max(centXY$cell, na.rm=T))) -> spatkey
#
#   spatkey1$cell[NAind] <- cellNA$cell
#
#   spatkey1 %>%
#     group_by(cell) %>%
#     mutate(geometry = st_union(geometry)) %>%
#     ungroup() %>%
#     select(cell, geometry) %>%
#     distinct() %>%
#     mutate(spat.grid.id = 1:n()) %>%
#     st_as_sf() -> tmp1
#
#   tmp1 %>%
#     st_drop_geometry() %>%
#     select(cell, spat.grid.id) %>%
#     full_join(spatkey1, ., by = 'cell') %>%
#     select(-geometry) -> spatkey
#
#   tmp1 %>% select(-cell) -> spat.grid
#
#   return(spatModel = list(spatkey = spatkey,
#                           spat.grid = spat.grid))
# }
