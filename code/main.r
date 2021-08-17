# Packages ----------------------------------------------------------------
#Set pkgs
pkgs = c("EAlidaR",
         "Rcpp",
         "sf",
         "raster",
         "rayshader",
         "magick",
         "osmdata",
         "av")

#Install pkgs
installed_packages = pkgs %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}
#Load pkgs
invisible(lapply(pkgs, library, character.only = TRUE))


# Data --------------------------------------------------------------------
#Get lidar data  // XY cordinates from  https://gridreferencefinder.com/

if (file.exists("east_devon.Rds")) {
  east_devon = readRDS("east_devon.Rds")
} else {
  east_devon = get_from_xy(
    xy = c(296675, 088025),
    radius = 1000,
    resolution = 1,
    model_type = 'DSM'
  )
  
  saveRDS(east_devon, "east_devon.Rds")
}

#Load gpx data from kamoot
p = read_sf("Bike_Ride_14_08_2021_16_52.gpx", layer = "tracks") 
  
p = st_transform(p$geometry,crs = crs(east_devon))

pp = read_sf("Bike_Ride_14_08_2021_16_52.gpx", layer = "track_points") # track points


# Functions ---------------------------------------------------------------

#Convert to coordinate system specified by EPSG code
convert_coords = function(lat, long, from = CRS("+init=epsg:4326"), to) {
  data = data.frame(long = long, lat = lat)
  coordinates(data) = ~ long + lat
  proj4string(data) = from
  xy = data.frame(sp::spTransform(data, to))
  colnames(xy) = c("x", "y")
  return(unlist(xy))
}
#Calculate route points as line
points2line_trajectory = function(p) {
  c = st_coordinates(p)
  i = seq(nrow(p) - 2)
  l = purrr::map(i, ~ sf::st_linestring(c[.x:(.x + 1),]))
  s = purrr::map_dbl(i, function(x) {
    geosphere::distHaversine(c[x,], c[(x + 1),]) /
      as.numeric(p$time[x + 1] - p$time[x])
  })
  lfc = sf::st_sfc(l)
  a = seq(length(lfc)) + 1 # sequence to subset
  p_data = cbind(sf::st_set_geometry(p[a,], NULL), s)
  sf::st_sf(p_data, geometry = lfc)
}

# Main --------------------------------------------------------------------

#Convert data to matrix and small matrix for testing
east_devon_mat = raster_to_matrix(east_devon)
east_devon_small = resize_matrix(east_devon_mat)

# Create Basemap based on lat long range of site --------------------------
lat_range = c(50.536222, 50.899663)
long_range = c(-3.716782, -2.756851)


utm_bbox = convert_coords(lat = lat_range,
                          long = long_range,
                          to = crs(east_devon))

extent_zoomed = extent(utm_bbox[1], utm_bbox[2], utm_bbox[3], utm_bbox[4])
east_devon_zoom = crop(east_devon, extent_zoomed)
east_devon_zoom_mat = raster_to_matrix(east_devon_zoom)

maxcolor = "#e6dbc8"
mincolor = "#b6bba5"
contour_color = "#7d4911"

if (file.exists("basemap.Rds")) {
  basemap = readRDS("basemap.Rds")
} else {
  basemap = east_devon_zoom_mat %>%
    height_shade() %>%
    add_overlay(sphere_shade(
      east_devon_zoom_mat,
      texture = "bw",
      colorintensity = 5
    ),
    alphalayer = 0.5) %>%
    add_shadow(lamb_shade(east_devon_zoom_mat), 0) %>%
    add_shadow(ambient_shade(east_devon_zoom_mat), 0) %>%
    add_shadow(texture_shade(
      east_devon_zoom_mat,
      detail = 8 / 10,
      contrast = 9,
      brightness = 11
    ),
    0.1)
}


#saveRDS(basemap, "basemap.Rds")
#Route overlay
east_devon_lines = st_transform(p, crs = crs(east_devon))

#OS roads overlay
osm_bbox = c(long_range[1], lat_range[1], long_range[2], lat_range[2])
east_devon_highway = opq(osm_bbox) %>%
  add_osm_feature("highway") %>%
  osmdata_sf()

east_devon_roads = st_transform(east_devon_highway$osm_lines, crs = crs(east_devon))

east_devon_footpaths = subset(east_devon_roads,
                       highway == "footway")

east_devon_cyclepaths = subset(east_devon_roads,
                        highway == "cycleway")

east_devon_roads = subset(
  east_devon_roads,
  highway = c(
    "unclassified",
    "secondary",
    "tertiary",
    "residential",
    "service"
  )
)

east_devon_footpaths = st_transform(east_devon_footpaths$geometry, crs = crs(east_devon))
east_devon_roads = st_transform(east_devon_roads$geometry, crs = crs(east_devon))
east_devon_cyclepaths = st_transform(east_devon_cyclepaths$geometry, crs = crs(east_devon))

# Plot map ----------------------------------------------------------------
basemap %>%
  add_overlay(
    generate_line_overlay(
      east_devon_lines,
      extent = extent_zoomed,
      linewidth = 12,
      color = "#2d8a91",
      heightmap = east_devon_zoom_mat
    )
  ) %>%
 plot_map(title_text = "Saturday bike ride", title_offset = c(15,15),
                 title_bar_color = "grey5", title_color = "white", title_bar_alpha = 1)

basemap %>%
  add_overlay(
    generate_line_overlay(
      east_devon_lines,
      extent = extent_zoomed,
      linewidth = 6,
      color = "#2d8a91",
      heightmap = east_devon_zoom_mat
    )
  ) %>% 
  plot_3d(east_devon_zoom_mat, windowsize = c(1200, 800))
render_camera(
  theta = 240,
  phi = 30,
  zoom = 0.5,
  fov = 60
)
