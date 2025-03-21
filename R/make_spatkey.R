#' Make coarse spatial grid
#'
#' @description Group a fine hexbin grid into groups of 7 hexbins to form a coarse spatial grid
#'
#'
#' @param grid (sf object) A grid in which one of the attributes is the conus.grid.id
#'
#' @returns A list which contains:
#' 1) spatkey - a dataframe which contains a column for the conus.grid.id (fine grid) and a column for the matching spat.grid.id (coarse grid)
#' 2) spat.grid - an sf dataframe which contains the spat.grid.id and the coarse grid geometry
#' @export
#'
#' @importFrom spdep poly2nb nb2WB
#'
#' @examples
#'


make_spatkey <- function(grid) {

  # Find the centroid of each grid cell
  cent <- sf::st_centroid(grid$geometry)

  # Round the coordinates
  centXY <- round(sf::st_coordinates(cent),-1) %>%
              as.data.frame() %>%
              dplyr::mutate(conus.grid.id = grid$conus.grid.id)

  # Sort the coorindates
  X <- sort(unique(centXY[,1]))
  Y <- sort(unique(centXY[,2]))

  NX <- length(X)
  NY <- length(Y)

  # These keys define where the central grouping cell will be
  key1 <- c(1,10,5,14,9,4,13,8,3,12,7,2,11,6)
  key2 <- c(10,5,14,9,4,13,8,3,12,7,2,11,6,1)


  newcentXY1 <- matrix(NA,0,2)

  for (a in 1:14){
    x <- seq(key1[a],NX,14)
    y <- seq(a,NY,14)
    xy <- expand.grid(x,y)
    xy <- cbind(X[xy[,1]],Y[xy[,2]])
    newcentXY1 <- rbind(newcentXY1,xy)
  }

  newcentXY2 <- matrix(NA,0,2)

  for (a in 1:14){
    x <- seq(key2[a],NX,14)
    y <- seq(a,NY,14)
    xy <- expand.grid(x,y)
    xy <- cbind(X[xy[,1]],Y[xy[,2]])
    newcentXY2 <- rbind(newcentXY2,xy)
  }

  # plot(centXY$X, centXY$Y)
  # points(newcentXY1[,1], newcentXY1[,2], col = "red", pch = 19)
  # points(newcentXY2[,1], newcentXY2[,2], col = "blue", pch = 19)

  newcentXY1 <- data.frame(cbind(1:nrow(newcentXY1),newcentXY1))
  colnames(newcentXY1) <- c("cell","X","Y")

  centXY1 <- data.frame(cbind(rep(NA,nrow(centXY)),centXY))
  colnames(centXY1) <- c("cell","X","Y","conus.grid.id")

  for (a in 1:nrow(newcentXY1)){
    centXY1$cell[which(sqrt((centXY1$X-newcentXY1$X[a])^2 + (centXY1$Y-newcentXY1$Y[a])^2)<8000)] = newcentXY1$cell[a]
  }

  a <- centXY1 %>%
          # Have to filter out missing (orphan) cells, otherwise NA will have more than 7 "neighbors"
          dplyr::filter(!is.na(cell)) %>%
          dplyr::group_by(cell) %>%
          dplyr::summarize(count = dplyr::n())

  if(max(a$count)<=7){
    centXY <- centXY1
    newcentXY <- newcentXY1
  } else {
    newcentXY2 <- data.frame(cbind(1:nrow(newcentXY2),newcentXY2))
    colnames(newcentXY2) <- c("cell","X","Y")

    centXY2 <- data.frame(cbind(rep(NA,nrow(centXY)),centXY))
    colnames(centXY2) <- c("cell","X","Y","conus.grid.id")

    for (a in 1:nrow(newcentXY2)){
      centXY2$cell[which(sqrt((centXY2$X-newcentXY2$X[a])^2 + (centXY2$Y-newcentXY2$Y[a])^2)<8000)] = newcentXY1$cell[a]
    }

    centXY <- centXY2
    newcentXY <- newcentXY2
  }

  # Isolate cells with NA and give them correct cell affiliations
  centXY %>%
    # Isolate centroids with missing cell assignments
    dplyr::filter(is.na(cell)) %>%
    dplyr::left_join(., grid, by = 'conus.grid.id') %>%
    dplyr::select(cell, conus.grid.id, geometry) %>%
    sf::st_as_sf() -> cellNA

  NAind <- which(is.na(centXY$cell))

  NB <- spdep::poly2nb(cellNA)
  NBinfo <- spdep::nb2WB(NB)

  cellNA$neighbors <- NBinfo$num
  adj <- NBinfo$adj

  cellNA$cell[1] <- 1

  for (i in 1:nrow(cellNA)) {
    if (cellNA$neighbors[i] == 0 & is.na(cellNA$cell[i])) {
      cellNA$cell[i] = max(cellNA$cell, na.rm=T) + 1
    } else if (cellNA$neighbors[i] > 0) {
      if (is.na(cellNA$cell[i])) {
        index <- adj[1:cellNA$neighbors[i]]
        cellNA$cell[i] <- max(cellNA$cell, na.rm=T) + 1
        # Give neighboring cells the same cell number
        cellNA$cell[index] <- cellNA$cell[i]

        # Remove the indices from adj that you just used
        adj <- adj[-(1:cellNA$neighbors[i])]
      } else {
        # Remove the indices from adj that you just used
        adj <- adj[-(1:cellNA$neighbors[i])]
      }
    }
  }

  cellNA <- dplyr::select(cellNA, -neighbors)

  centXY %>%
    dplyr::left_join(grid, centXY, by = 'conus.grid.id') %>%
    dplyr::select(conus.grid.id, cell, geometry) -> spatkey1

  spatkey1$cell[NAind] <- cellNA$cell

  spatkey1 %>%
    dplyr::group_by(cell) %>%
    dplyr::mutate(geometry = sf::st_union(geometry)) %>%
    dplyr::ungroup() %>%
    dplyr::select(cell, geometry) %>%
    dplyr::distinct() %>%
    dplyr::mutate(spat.grid.id = 1:dplyr::n()) %>%
    sf::st_as_sf() -> tmp1

  tmp1 %>%
    sf::st_drop_geometry() %>%
    dplyr::select(cell, spat.grid.id) %>%
    dplyr::full_join(spatkey1, ., by = 'cell') %>%
    dplyr::select(-geometry, -cell) -> spatkey

  dplyr::select(tmp, -cell) -> spat.grid

  return(spatModel = list(spatkey = spatkey,
                          spat.grid = spat.grid))
}
