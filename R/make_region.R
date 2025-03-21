#' Make species region
#'
#' @description Define the region for inference using a buffer around external range boundaries.
#'
#' @param rangelist (list) List of ranges generated from get_range()
#' @param buffer (numeric) Buffer size, unit depends on CRS
#' @param boundary (sf object) A that says where to cut off the region
#' @param grid (sf object) with grid
#' @param crs default is 3857 so that buffer size can be meters
#' @param sub whether to buffer around centroid (sub = T) or around full range (sub = F)
#' @param lat.lo (numeric) lower boundary of latitude (degrees)
#' @param lat.hi (numeric) upper boundary of latitude (degrees)
#' @param lon.lo (numeric) lower boundary of longitude (degrees)
#' @param lon.lo (numeric) upper boundary of longitude (degrees)
#' @param continuous whether to keep only a continuous section of the range (T) or keep the whole range (F)
#' @param rm.clumps whether to remove clumps of cells of size clump.size
#' @param clump.size number of cells in clumps to keep
#'
#' @returns A list containing ranges, the full region, sp.grid, and the boundary
#' @export
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
                        boundary, # where to cut off region, CONUS in our case
                        grid, # what grid to use, conus.grid in our case
                        crs = 3857, # use 3857 so buffer can be meters
                        sub = F,
                        lat.lo = NA, # these should all be in degrees
                        lat.hi = NA,
                        lon.lo = NA,
                        lon.hi = NA,
                        continuous = F,
                        rm.clumps = F,
                        clump.size = 20,
                        cell.size = 24999998) {

  sf::sf_use_s2(FALSE)


  # Put ranges into correct CRS
  for (r in 1:length(rangelist)) {
    rangelist[[r]] <- sf::st_transform(rangelist[[r]], crs = crs)
  }

  # Combine ranges
  fullrangeout <- dplyr::bind_rows(rangelist)
  fullrange <- fullrangeout %>% dplyr:summarize(geometry = sf::st_union(geometry))

  # If sub == T, find centroid to add buffer to
  if (sub == T) {
    fullrange1 <- sf::st_centroid(fullrange)
  }

  # Add buffer
  # If crs = 3857, dist = 100000 means 100,000m means 100km
  region <- fullrange %>% sf::st_buffer(dist = buffer)

  # Crop using bounding box
  # Have to change bb to degrees so we can replaces values with input degrees
  bb <- sf::st_bbox(region) %>% sf::st_as_sfc() %>% sf::st_transform(4326) %>% sf::st_bbox()

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
  bb <- sf::st_as_sfc(bb) %>% sf::st_transform(crs) %>% sf::st_bbox()

  region <- sf::st_crop(region, bb)


  # Crop to boundary
  region <- sf::st_intersection(region, sf::st_transform(boundary, crs))


  # Overlay with grid

  # this gets grid cells but edge cells are cut off
  grid <- sf::st_intersection(region, sf::st_transform(grid, crs = crs))

  # this gets the whole grid cells
  grida <- sf::st_transform(grid, crs = crs) %>%
            dplyr::filter(conus.grid.id %in% grid$conus.grid.id)

  # find cells that are too small and remove (these are along the edges of the boundary)
  gridb <- grida %>%
            dplyr::mutate(area = as.numeric(sf::st_area(geometry))) %>%
            dplyr::filter(area >= cell.size) %>%
            dplyr::select(!area)

  # Find which grid cells are split into multiple pieces (eg by water) and remove
  grid1 <- gridb %>%
            sf::st_cast("MULTIPOLYGON") %>%
            sf::st_cast("POLYGON")
  mult <- as.data.frame(table(grid1$conus.grid.id)) %>%
            dplyr::filter(Freq > 1) %>%
            dplyr::pull(Var1)

  if (length(mult) > 0) {
    gridb <- gridb[-which(gridb$conus.grid.id %in% mult),]
  }

  # Find cells that are not continuous and remove
  if (continuous == T) {
    grid2 <- gridb %>%
      sf::st_union() %>%
      sf::st_as_sf() %>%
      sf::st_cast("POLYGON") %>%
      dplyr::mutate(area = sf::st_area(x)) %>%
      dplyr::filter(area == max(area))
    gridc <- sf::st_intersection(gridb, grid2)
  } else if (continuous == F) {
    gridc <- gridb
  }


  if (rm.clumps == T) {
    # Find and remove cells that are in clumps of less than 20 cells
    grid1 <- gridc %>%
      # group neighboring cells
      dplyr::summarize(geometry = sf::st_union(geometry, by_feature = F)) %>%
      sf::st_cast("POLYGON")%>%

      # remove cells in group of < 20 (area < 500000000)
      dplyr::mutate(area = as.numeric(sf::st_area(geometry))) %>%
      dplyr::filter(area >= cellsize * clump.size)
    gridd <- sf::st_intersection(gridc, grid1) %>%
      ungroup()
  } else {
    gridd <- gridc
  }



  # find which cells are orphans and remove
  tmp <- sf::st_touches(gridd)
  tmp1 <- unlist(lapply(tmp, length))
  orphans <- which(tmp1 <= 1)

  cat("Removing cells with <=1 neighbor...\n")

  while (length(orphans) > 0) {

    cat(paste0((length(orphans)), " remaining ", "\n"))
    gridd <- gridd[-orphans,]

    tmp <- sf::st_touches(gridd)
    tmp1 <- unlist(lapply(tmp, length))
    orphans <- which(tmp1 <= 1)
  }




  # Save output
  dat <- list(range = fullrangeout,
              region = region,
              sp.grid = gridd,
              boundary = boundary)

  return(dat)
}




#
#
# make_region <- function(sp.code,
#                         buffer = 100000,
#                         sub = F,
#                         usa.path = "data/USA/",
#                         crs = 3857,
#                         overwrite = F,
#                         lat.lo = NA,
#                         lat.hi = NA,
#                         lon.lo = NA,
#                         lon.hi = NA,
#                         continuous = T,
#                         rm.clumps = F) {
#   sf::sf_use_s2(FALSE)
#
#   latlon <- paste0("_", lon.lo, "_", lat.lo, "_", lon.hi, "_", lat.hi)
#
#   # if it already exists, load it
#   if (file.exists(paste0("data/species/", sp.code, "/region-", buffer, "-", sub, latlon, ".rds")) & overwrite == F) {
#     dat <- read_rds(paste0("data/species/", sp.code, "/region-", buffer, "-", sub, latlon, ".rds"))
#   } else {
#
#     range <- get_range(sp.code)
#     if (nrow(range[[1]]) == 0) {
#       region <- NA
#       return(region)
#     } else {
#
#       for (r in 1:length(range)) {
#         range[[r]] <- st_transform(range[[r]], crs = crs)
#       }
#
#
#       # combine ranges
#       if (length(range) == 1) {
#         full <- range[[r]]
#         full1 <- range[[r]]
#       } else if (length(range) == 2) {
#         full <- bind_rows(range[[1]], range[[2]])
#         full1 <- st_union(range[[1]], range[[2]]) %>%
#           summarize()
#       } else if (length(range) > 2) {
#         stop("Not equipped to handle more than 2 ranges")
#       }
#
#
#       if (sub == T) {
#         full1 <- st_centroid(full)[1,]
#       }
#
#       # add buffer
#       # dist = 100000 means 100,000m means 100km
#       region <- full1 %>%
#         st_buffer(dist = buffer)
#
#       # crop with bounding box
#       # since we use degrees to crop, switch region to degree projection first
#       bb <- st_bbox(st_transform(region, 4326))
#       if (is.na(lon.hi) == F) {
#         bb[3] <- lon.hi
#       }
#       if (is.na(lon.lo) == F) {
#         bb[1] <- lon.lo
#       }
#       if (is.na(lat.hi) == F) {
#         bb[4] <- lat.hi
#       }
#       if (is.na(lat.lo) == F) {
#         bb[2] <- lat.lo
#       }
#
#       region <- st_crop(st_transform(region, 4326), bb) %>%
#         st_transform(crs)
#
#
#       exclude <- c("Alaska", "Hawaii", "Commonwealth of the Northern Mariana Islands",
#                    "American Samoa", "United States Virgin Islands", "Guam", "Puerto Rico")
#
#       usa <- st_read("data/USA/maps/cb_2018_us_state_500k/cb_2018_us_state_500k.shp") %>%
#         filter((NAME %in% exclude) == F) %>%
#         # Change CRS from 4269 (NAD83) to 3857 (WGS84) (degrees to meters)
#         st_transform(crs = crs)
#
#       usa1 <- usa %>%
#         st_union()
#
#
#       # now get intersection with usa grid
#       if (file.exists(paste0(usa.path, "grid-covar.rdata"))) {
#         load(paste0(usa.path, "grid-covar.rdata"))
#         region <- st_intersection(region, usa1)
#
#         if (nrow(region) > 0) {
#           usa.grid1 <- st_transform(conus.grid, crs = crs)
#
#
#           # this gets grid cells but with some cut off
#           grid <- st_intersection(region, usa.grid1)
#
#           # this gets the whole grid cells and assigns sp.grid.id
#           grida <- usa.grid1 %>%
#             filter(conus.grid.id %in% grid$conus.grid.id)
#
#           # find cells that are too small and remove
#           gridb <- grida %>%
#             mutate(area = as.numeric(st_area(geometry))) %>%
#             filter(area > 24999998) %>%
#             select(!area)
#
#           # Find which grid cells are split into multiple pieces (eg by water) and remove
#           grid1 <- gridb %>%
#             st_cast("MULTIPOLYGON") %>%
#             st_cast("POLYGON")
#           mult <- as.data.frame(table(grid1$conus.grid.id)) %>%
#             filter(Freq > 1) %>%
#             pull(Var1)
#
#           if (length(mult) > 0) {
#             grid <- grid[-which(grid$conus.grid.id %in% mult),]
#           }
#
#           # Find cells that are not continuous and remove
#           if (continuous == T) {
#             grid2 <- gridb %>%
#               st_union() %>%
#               st_as_sf() %>%
#               st_cast("POLYGON") %>%
#               mutate(area = st_area(x)) %>%
#               filter(area == max(area))
#             gridc <- st_intersection(gridb, grid2)
#           } else if (continuous == F) {
#             gridc <- gridb
#           }
#
#
#           if (rm.clumps == T) {
#             # Find and remove cells that are in clumps of less than 20 cells
#             grid1 <- gridc %>%
#               # group neighboring cells
#               summarize(geometry = st_union(geometry, by_feature = F)) %>%
#               st_cast("POLYGON")%>%
#
#               # remove cells in group of < 20 (area < 500000000)
#               mutate(area = as.numeric(st_area(geometry))) %>%
#               filter(area >= 500000000)
#             gridd <- st_intersection(gridc, grid1) %>%
#               ungroup()
#             #gridd <- gridc
#           } else {
#             gridd <- gridc
#           }
#
#
#
#           # find which cells are orphans and remove
#           tmp <- st_touches(gridd)
#           tmp1 <- unlist(lapply(tmp, length))
#           orphans <- which(tmp1 <= 1)
#
#           cat("Removing cells with <=1 neighbor...\n")
#
#           while (length(orphans) > 0) {
#
#             cat(paste0((length(orphans)), " remaining ", "\n"))
#             gridd <- gridd[-orphans,]
#
#             tmp <- st_touches(gridd)
#             tmp1 <- unlist(lapply(tmp, length))
#             orphans <- which(tmp1 <= 1)
#           }
#
#
#           gride <- gridd %>%
#             mutate(sp.grid.id = row_number())
#
#
#
#           dat <- list(range = full,
#                       region = region,
#                       sp.grid = gride,
#                       conus = usa)
#         } else {
#           dat <- NA
#         }
#
#
#       } else {
#         cat("USA Grid must be generated on this computer before sp.grid can be produced.")
#         dat <- list(range = full,
#                     region = region,
#                     conus = usa)
#       }
#
#       write_rds(dat, file = paste0("data/species/", sp.code, "/region-", buffer, "-", sub, latlon, ".rds"))
#
#       return(dat)
#
#     }
#
#
#   }
#
#
# }
