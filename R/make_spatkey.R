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
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @importFrom sf st_centroid st_coordinates st_as_sf st_drop_geometry
#' @importFrom dplyr mutate filter group_by summarize distinct ungroup n
#'
#' @examples
#' \dontrun{
#'
#' spatial.grid <- make_spatkey(grid)
#'
#' }


make_spatkey <- function(grid, apothem) {

  # Find the centroid of each grid cell
  cent <- st_centroid(grid$geometry)

  # Round the coordinates
  centXY <- round(st_coordinates(cent),-1) %>%
    as.data.frame() %>%
    mutate(conus.grid.id = grid$conus.grid.id)

  # Sort the coordinates
  X <- sort(unique(centXY[,1]))
  Y <- sort(unique(centXY[,2]))

  NX <- length(X)
  NY <- length(Y)

  # These keys define where the central grouping cell will be
  key1 <- c(1,10,5,14,9,4,13,8,3,12,7,2,11,6)
  key2 <- c(10,5,14,9,4,13,8,3,12,7,2,11,6,1)


  # Define a sequence of centroids based on the location of coordinate Y
  newcentXY1 <- matrix(NA,0,2)
  newcentXY2 <- matrix(NA,0,2)

  for (a in 1:14){
    # Key 1
    y <- seq(key1[a],NY,14)
    x <- seq(a,NX,14)
    xy <- expand.grid(x,y)
    xy <- cbind(X[xy[,1]],Y[xy[,2]])
    newcentXY1 <- rbind(newcentXY1,xy)

    # Key 2
    y <- seq(key2[a],NY,14)
    x <- seq(a,NX,14)
    xy <- expand.grid(x,y)
    xy <- cbind(X[xy[,1]],Y[xy[,2]])
    newcentXY2 <- rbind(newcentXY2,xy)
  }

  # Calculate distance between selected centroids
  as.matrix(dist(newcentXY1)) -> tmp
  # Change all 0s (same centroid) to NA
  tmp[tmp==0] <- NA
  # Find minimum distance of selected centroids
  # If < 2*apothem then selected cells are too close, move to option 2


  if (min(tmp, na.rm=T) < 2*apothem) {
    # Define a sequence of centroids based on the location of coordinate X
    newcentXY1 <- matrix(NA,0,2)
    newcentXY2 <- matrix(NA,0,2)

    for (a in 1:14){
      # Key 1
      x <- seq(key1[a],NX,14)
      y <- seq(a,NY,14)
      xy <- expand.grid(x,y)
      xy <- cbind(X[xy[,1]],Y[xy[,2]])
      newcentXY1 <- rbind(newcentXY1,xy)

      # Key 2
      x <- seq(key2[a],NX,14)
      y <- seq(a,NY,14)
      xy <- expand.grid(x,y)
      xy <- cbind(X[xy[,1]],Y[xy[,2]])
      newcentXY2 <- rbind(newcentXY2,xy)
    }
  }

  # # Plotting check to ensure that centroids are selected correctly
  # plot(centXY$X, centXY$Y)
  # points(as.data.frame(newcentXY1)$V1, as.data.frame(newcentXY1)$V2, col = 'red', pch = 3)
  # points(as.data.frame(newcentXY2)$V1, as.data.frame(newcentXY2)$V2, col = 'hotpink', pch = 11)


  newcentXY1 <- data.frame(cbind(1:nrow(newcentXY1),newcentXY1))
  colnames(newcentXY1) <- c("cell","X","Y")

  centXY1 <- data.frame(cbind(rep(NA,nrow(centXY)),centXY))
  colnames(centXY1) <- c("cell","X","Y","conus.grid.id")

  # Calculate distances from centroid cell
  for (a in 1:nrow(newcentXY1)){
    centXY1$cell[which(sqrt((centXY1$X-newcentXY1$X[a])^2 + (centXY1$Y-newcentXY1$Y[a])^2)<apothem)] = newcentXY1$cell[a]
  }


  a <- centXY1 %>%
    # Filter out missing (orphan) cells, otherwise NA will have more than 7 "neighbors"
    filter(!is.na(.data$cell)) %>%
    group_by(.data$cell) %>%
    summarize(count = n())

  # If the max neighbor count is 7, then all set.
  # If not, then use Key 2
  if(max(a$count)==7){
    centXY <- centXY1
    newcentXY <- newcentXY1
  } else {
    newcentXY2 <- data.frame(cbind(1:nrow(newcentXY2),newcentXY2))
    colnames(newcentXY2) <- c("cell","X","Y")

    centXY2 <- data.frame(cbind(rep(NA,nrow(centXY)),centXY))
    colnames(centXY2) <- c("cell","X","Y","conus.grid.id")

    # Calculate distances from centroid cell
    for (a in 1:nrow(newcentXY2)){
      centXY2$cell[which(sqrt((centXY2$X-newcentXY2$X[a])^2 + (centXY2$Y-newcentXY2$Y[a])^2)<apothem)] = newcentXY1$cell[a]
    }

    centXY <- centXY2
    newcentXY <- newcentXY2
  }

  # Isolate cells with NA and give them correct cell affiliations
  centXY %>%
    # Isolate centroids with missing cell assignments
    filter(is.na(.data$cell)) %>%
    left_join(grid, by = 'conus.grid.id') %>%
    select("cell", "conus.grid.id", "geometry") %>%
    st_as_sf() -> cellNA

  NAind <- which(is.na(centXY$cell))

  # Calculate adjacent neighbors
  suppressWarnings(NB <- poly2nb(cellNA))
  NBinfo <- nb2WB(NB)

  cellNA$neighbors <- NBinfo$num
  adj <- NBinfo$adj

  cellNA$cell[1] <- 1

  # Use adjacency to group nearby cells
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

  cellNA <- select(cellNA, -"neighbors")

  centXY %>%
    left_join(grid, centXY, by = 'conus.grid.id') %>%
    select("conus.grid.id", "cell", "geometry") -> spatkey1

  spatkey1$cell[NAind] <- cellNA$cell

  spatkey1 %>%
    group_by(.data$cell) %>%
    mutate(geometry = st_union(.data$geometry)) %>%
    ungroup() %>%
    select("cell", "geometry") %>%
    distinct() %>%
    mutate(spat.grid.id = 1:n()) %>%
    st_as_sf() -> tmp1

  # # Plotting check to ensure grouping is performed correctly
  # ggplot(tmp1) + geom_sf()

  tmp1 %>%
    st_drop_geometry() %>%
    select("cell", "spat.grid.id") %>%
    full_join(spatkey1, .data, by = 'cell') %>%
    select(-"geometry", -"cell") -> spatkey

  select(tmp1, -"cell") -> spat.grid

  return(spatModel = list(spatkey = spatkey,
                          spat.grid = spat.grid))
}
