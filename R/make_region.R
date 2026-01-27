#' Make species region
#'
#' @description Define the region for inference using a buffer around external range boundaries.
#'
#' @param rangelist (list) list of ranges generated from get_range()
#' @param buffer (numeric) buffer size in meters
#' @param boundary (sf object) A polygon that says where to cut off the region (e.g., political borders)
#' @param grid (sf object) grid overlay
#' @param sub (logical) whether to buffer around centroid (sub = T) or around full range (sub = F)
#' @param lat.lo (numeric) lower boundary of latitude (degrees)
#' @param lat.hi (numeric) upper boundary of latitude (degrees)
#' @param lon.lo (numeric) lower boundary of longitude (degrees)
#' @param lon.hi (numeric) upper boundary of longitude (degrees)
#' @param continuous (logical) whether to keep only a continuous section of the range (T) or keep the whole range (F)
#' @param rm.clumps (logical) whether to remove clumps of cells of size clump.size
#' @param clump.size (numeric) number of cells in clumps to keep
#'
#' @returns A list containing ranges, the full region, sp.grid, and the boundary
#' @export
#'
#' @importFrom sf sf_use_s2 st_buffer st_centroid st_bbox st_as_sfc st_crop st_area st_intersection st_transform st_cast st_union st_as_sf st_touches st_geometry_type
#' @importFrom dplyr bind_rows summarize filter mutate select pull ungroup reframe
#' @importFrom magrittr "%>%"
#'
#' @examples
#'\dontrun{
#' # Predownload range boundaries of interest
#'
#' # Provide the path to each boundary
#' range.path <- c("GAP/aAZTOx_CONUS_Range_2001v1/",
#'                 "IUCN/")
#'
#' # Give each boundary a name
#' range.name <- c("GAP", "IUCN")
#'
#' rangelist <- get_range(range.path,
#'                        range.name,
#'                        crs = 4326)
#'
#' region <- make_region(rangelist,
#'                       buffer = 50000,
#'                       crs = 3857,
#'                       sub = F,
#'                       boundary = usa,
#'                       grid = conus.grid)
#'
#'}



make_region <- function(rangelist,
                        buffer,
                        boundary = NULL, # where to cut off region, CONUS in our case
                        grid, # what grid to use, conus.grid in our case
                        sub = F,
                        lat.lo = NA, # these should all be in degrees
                        lat.hi = NA,
                        lon.lo = NA,
                        lon.hi = NA,
                        continuous = F,
                        rm.clumps = F,
                        clump.size = 20) {

  sf_use_s2(FALSE)


  # check crs of grid
  tmp <- st_crs(grid)
  if (tmp$input != "EPSG:3857") {stop("grid must have crs = 3857")}

  # check grid cell sizes
  cellsize <- grid %>% mutate(area = round(as.numeric(st_area(.data$geometry))), 0) %>%
              pull(.data$area) %>%
              unique()

  if (length(cellsize) > 1) {stop("all grid cells must have equal sizes")}

  # make input objects correct crs
  for (r in 1:length(rangelist)) {
    rangelist[[r]] <- st_transform(rangelist[[r]], crs = 3857)
  }
  if (!is.null(boundary)) boundary <- st_transform(boundary, crs = 3857)




  grid.og <- grid %>% mutate(area = st_area(grid))



  # Combine ranges
  fullrangeout <- bind_rows(rangelist)
  fullrange <- fullrangeout %>% summarize(geometry = st_union(.data$geometry))

  # If sub == T, find centroid to add buffer to
  if (sub == T) {
    fullrange <- st_centroid(fullrange)
  }

  # Add buffer
  # If crs = 3857, dist = 100000 means 100,000m means 100km
  region <- fullrange %>% st_buffer(dist = buffer)

  # Crop using bounding box
  # Have to change bb to degrees so we can replaces values with input degrees
  bb <- st_bbox(region) %>% st_as_sfc() %>% st_transform(4326) %>% st_bbox()

  if (is.na(lon.hi) == F) {
    bb[3] <- lon.hi
  }
  if (is.na(lon.lo) == F) {
    bb[1] <- lon.lo
  }
  if (is.na(lat.hi) == F) {
    bb[4] <- lat.hi
  }
  if (is.na(lat.lo) == F) {
    bb[2] <- lat.lo
  }
  # now have to change back to desired crs
  bb <- st_as_sfc(bb) %>% st_transform(3857) %>% st_bbox()

  region <- st_crop(region, bb)


  # Crop to boundary
  if (!is.null(boundary))   suppressWarnings(region <- st_intersection(region, boundary))


  # if no part of range is within the boundary, return NA
  if (nrow(region) == 0) {
    dat <- NA
  } else {
    # Overlay with grid

    # this gets grid cells in region
    suppressWarnings(grida <- st_intersection(region, grid))

    # find cells that are too small and remove (these are along the edges of the boundary)
    gridb <- grida %>%
      mutate(area = round(as.numeric(st_area(.data$geometry))), 0) %>%
      filter(.data$area >= cellsize)
      # inner_join(st_drop_geometry(grid.og), by = "conus.grid.id")# %>%
      # filter(area.x == area.y)
    cat("Removing small cells\n")
    if (nrow(gridb) == 0) stop("No remaining grid cells")

    # # Find which grid cells are split into multiple pieces (eg by water) and remove
    # suppressWarnings(
    #   grid1 <- gridb %>%
    #     st_cast("MULTIPOLYGON") %>%
    #     st_cast("POLYGON")
    # )
    #
    # mult <- as.data.frame(table(grid1$conus.grid.id)) %>%
    #   filter(.data$Freq > 1) %>%
    #   pull(.data$Var1)
    #
    # if (length(mult) > 0) {
    #   gridb <- gridb[-which(gridb$conus.grid.id %in% mult),]
    # }

    # Find cells that are not continuous and remove
    if (continuous == T) {
      grid2 <- gridb %>%
        st_union() %>%
        st_as_sf() %>%
        st_cast("POLYGON") %>%
        mutate(area = st_area(.data$x)) %>%
        filter(.data$area == max(.data$area))
      suppressWarnings(gridc <- st_intersection(gridb, grid2))
      cat("Making continuous range\n")
    } else if (continuous == F) {
      gridc <- gridb
    }
    if (nrow(gridc) == 0) stop("No remaining grid cells")


    # Find and remove cells that are in clumps of less than X cells
    if (rm.clumps == T) {
      # group neighboring cells
      grid1 <- gridc %>%
                summarize(geometry = st_union(.data$geometry, by_feature = F)) %>%
                st_cast("POLYGON") %>%
                mutate(group.id = 1:nrow(.data))

      # see how many cells are in each group
      suppressWarnings(tmp <- st_intersection(gridc, grid1))
      tmp <- tmp %>%
        mutate(type = sf::st_geometry_type(.data$geometry)) %>%
        filter(.data$type == "POLYGON") %>%
        group_by(.data$group.id) %>%
        summarize(n = n())
      rm.group <- filter(tmp, n < clump.size)

        # # remove cells in group of < 20 (area < 500000000)
        # mutate(area = as.numeric(st_area(.data$geometry))) %>%
        # filter(.data$area >= cell.size * clump.size)
      suppressWarnings(gridd <- st_intersection(gridc, grid1) %>% ungroup())
      gridd <- gridd %>%
                mutate(type = sf::st_geometry_type(.data$geometry)) %>%
                filter(.data$type == "POLYGON",
                       .data$group.id %in% rm.group$group.id == F)
      cat("Removing clumps of cells\n")
    } else {
      gridd <- gridc
    }
    if (nrow(gridd) == 0) stop("No remaining grid cells")




    # find which cells are orphans and remove
    tmp <- st_touches(gridd)
    tmp1 <- unlist(lapply(tmp, length))
    orphans <- which(tmp1 <= 1)

    cat("Removing cells with <=1 neighbor...\n")

    while (length(orphans) > 0) {

      cat(paste0("   ", (length(orphans)), " remaining ", "\n"))
      gridd <- gridd[-orphans,]

      tmp <- st_touches(gridd)
      tmp1 <- unlist(lapply(tmp, length))
      orphans <- which(tmp1 <= 1)
    }
    if (nrow(gridd) == 0) stop("No remaining grid cells")


    # get correct columns
    gridd <- select(gridd, "conus.grid.id") %>%
              mutate(conus.grid.id = as.character(.data$conus.grid.id))


    # Save output
    dat <- list(range = fullrangeout,
                region = region,
                sp.grid = gridd,
                boundary = boundary)

    return(dat)

  }

}
