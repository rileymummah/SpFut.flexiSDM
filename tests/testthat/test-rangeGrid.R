# Predownload range boundaries of interest

# Provide the path to each boundary
range.path <- c("GAP/aAZTOx_CONUS_Range_2001v1/",
                "IUCN/")

# Give each boundary a name
range.name <- c("GAP", "IUCN")

rangelist <- get_range(range.path,
                       range.name,
                       crs = 4326)

