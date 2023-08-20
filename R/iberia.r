#############################################
# 3D forest height with R
# Milos Popovic 2023/08/20
#############################################

# PLEASE INSTALL THIS VERSION OF GGPLOT2
# devtools::install_version(
#     "ggplot2", version = "3.3.6", 
#     repos = "http://cran.us.r-project.org")

libs <- c(
    "tidyverse", "sf", "geodata",
    "terra", "classInt", "rayshader"
)

installed_libs <- libs %in% rownames(
    installed.packages()
)

if (any(installed_libs == F)) {
    install.packages(
        libs[!installed_libs]
    )
}

invisible(lapply(
    libs,
    library,
    character.only = T
))

# 1. DOWNLOAD ETH DATA
#---------------------
urls <- c(
    "https://share.phys.ethz.ch/~pf/nlangdata/ETH_GlobalCanopyHeight_10m_2020_version1/3deg_cogs/ETH_GlobalCanopyHeight_10m_2020_N42W012_Map.tif",
    "https://share.phys.ethz.ch/~pf/nlangdata/ETH_GlobalCanopyHeight_10m_2020_version1/3deg_cogs/ETH_GlobalCanopyHeight_10m_2020_N42W009_Map.tif",
    "https://share.phys.ethz.ch/~pf/nlangdata/ETH_GlobalCanopyHeight_10m_2020_version1/3deg_cogs/ETH_GlobalCanopyHeight_10m_2020_N42W006_Map.tif",
    "https://share.phys.ethz.ch/~pf/nlangdata/ETH_GlobalCanopyHeight_10m_2020_version1/3deg_cogs/ETH_GlobalCanopyHeight_10m_2020_N42W003_Map.tif",
    "https://share.phys.ethz.ch/~pf/nlangdata/ETH_GlobalCanopyHeight_10m_2020_version1/3deg_cogs/ETH_GlobalCanopyHeight_10m_2020_N42E000_Map.tif",
    "https://share.phys.ethz.ch/~pf/nlangdata/ETH_GlobalCanopyHeight_10m_2020_version1/3deg_cogs/ETH_GlobalCanopyHeight_10m_2020_N42E003_Map.tif",
    "https://share.phys.ethz.ch/~pf/nlangdata/ETH_GlobalCanopyHeight_10m_2020_version1/3deg_cogs/ETH_GlobalCanopyHeight_10m_2020_N39W012_Map.tif",
    "https://share.phys.ethz.ch/~pf/nlangdata/ETH_GlobalCanopyHeight_10m_2020_version1/3deg_cogs/ETH_GlobalCanopyHeight_10m_2020_N39W009_Map.tif",
    "https://share.phys.ethz.ch/~pf/nlangdata/ETH_GlobalCanopyHeight_10m_2020_version1/3deg_cogs/ETH_GlobalCanopyHeight_10m_2020_N39W006_Map.tif",
    "https://share.phys.ethz.ch/~pf/nlangdata/ETH_GlobalCanopyHeight_10m_2020_version1/3deg_cogs/ETH_GlobalCanopyHeight_10m_2020_N39W003_Map.tif",
    "https://share.phys.ethz.ch/~pf/nlangdata/ETH_GlobalCanopyHeight_10m_2020_version1/3deg_cogs/ETH_GlobalCanopyHeight_10m_2020_N39E000_Map.tif",
    "https://share.phys.ethz.ch/~pf/nlangdata/ETH_GlobalCanopyHeight_10m_2020_version1/3deg_cogs/ETH_GlobalCanopyHeight_10m_2020_N36W012_Map.tif",
    "https://share.phys.ethz.ch/~pf/nlangdata/ETH_GlobalCanopyHeight_10m_2020_version1/3deg_cogs/ETH_GlobalCanopyHeight_10m_2020_N36W009_Map.tif",
    "https://share.phys.ethz.ch/~pf/nlangdata/ETH_GlobalCanopyHeight_10m_2020_version1/3deg_cogs/ETH_GlobalCanopyHeight_10m_2020_N36W006_Map.tif",
    "https://share.phys.ethz.ch/~pf/nlangdata/ETH_GlobalCanopyHeight_10m_2020_version1/3deg_cogs/ETH_GlobalCanopyHeight_10m_2020_N36W003_Map.tif",
    "https://share.phys.ethz.ch/~pf/nlangdata/ETH_GlobalCanopyHeight_10m_2020_version1/3deg_cogs/ETH_GlobalCanopyHeight_10m_2020_N36E000_Map.tif"
)

for (url in urls) {
    download.file(
        url,
        destfile = basename(url),
        mode = "wb"
    )
}

raster_files <-
    list.files(
        path = getwd(),
        pattern = "ETH_GlobalCanopyHeight",
        full.names = T
    )

# 2. iberia POLYGON
#--------------------
sf::sf_use_s2(F)
get_country_admin1 <- function() {
    main_path <- getwd()
    country_admin1 <- geodata::gadm(
        country = c("ESP", "PRT"),
        level = 1,
        path = main_path
    ) |>
        sf::st_as_sf()

    return(country_admin1)
}

country_admin1 <- get_country_admin1()

country_borders <- country_admin1 |>
    dplyr::filter(
        !NAME_1 %in% c(
            "Ceuta y Melilla",
            "Islas Baleares",
            "Islas Canarias",
            "Azores", "Madeira"
        )
    ) |>
    sf::st_as_sf() |>
    sf::st_union()

# 3. LOAD FOREST HEIGHT
#----------------------

forest_height_list <- lapply(
    raster_files,
    terra::rast
)

forest_height_rasters <- lapply(
    forest_height_list,
    function(x) {
        terra::crop(
            x,
            terra::vect(
                country_borders
            ),
            snap = "in",
            mask = T
        )
    }
)

forest_height_mosaic <- do.call(
    terra::mosaic,
    forest_height_rasters
)

crs_lambert <-
    "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_frfs"

forest_height_iberia <- forest_height_mosaic |>
    terra::aggregate(
        fact = 20
    ) |>
    terra::project(crs_lambert)

# 4. RASTER TO DATAFRAME
#-----------------------

forest_height_iberia_df <- forest_height_iberia |>
    as.data.frame(
        xy = T
    )

head(forest_height_iberia_df)
names(forest_height_iberia_df)[3] <- "height"

# 5. BREAKS
#----------

breaks <- classInt::classIntervals(
    forest_height_iberia_df$height,
    n = 4,
    style = "equal"
)$brks

# 6. COLORS
#----------

cols <-
    c(
        "white", "#ffd3af", "#fbe06e",
        "#6daa55", "#205544"
    )

texture <- colorRampPalette(
    cols,
    bias = 2
)(6)

# 7. GGPLOT2
#-----------

p <- ggplot(
    forest_height_iberia_df
) +
    geom_raster(
        aes(
            x = x,
            y = y,
            fill = height
        )
    ) +
    scale_fill_gradientn(
        name = "height (m)",
        colors = texture,
        breaks = round(breaks, 0)
    ) +
    coord_sf(crs = crs_lambert) +
    guides(
        fill = guide_legend(
            direction = "vertical",
            keyheight = unit(5, "mm"),
            keywidth = unit(5, "mm"),
            title.position = "top",
            label.position = "right",
            title.hjust = .5,
            label.hjust = .5,
            ncol = 1,
            byrow = F
        )
    ) +
    theme_minimal() +
    theme(
        axis.line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = c(0.9, .3),
        legend.title = element_text(
            size = 11, color = "grey10"
        ),
        legend.text = element_text(
            size = 10, color = "grey10"
        ),
        panel.grid.major = element_line(
                color = "white"
            ),
        panel.grid.minor = element_line(
                color = "white"
            ),
        plot.background = element_rect(
                fill = "white",
                color = NA
            ),
        legend.background = element_rect(
            fill = "white", color = NA
        ),
        panel.border = element_blank(),
        plot.margin = unit(
            c(
                t = 0, r = 0,
                b = 0, l = 0
            ), "lines"
        )
    )

# 8. RENDER SCENE
#----------------

h <- nrow(forest_height_iberia)
w <- ncol(forest_height_iberia)

rayshader::plot_gg(
    ggobj = p,
    width = w / 1000,
    height = h / 1000,
    scale = 150,
    solid = F,
    shadow = T,
    shadowcolor = "white",
    shadow_intensity = 1,
    background = "white",
    offset_edges = F,
    sunangle = 315,
    window.size = c(800, 800),
    zoom = .5,
    phi = 30,
    theta = -30,
    multicore = T
)

rayshader::render_camera(
    phi = 80,
    zoom = .625,
    theta = 5
)

# 9. RENDER OBJECT
#-----------------

rayshader::render_highquality(
    filename = "iberia-forest-height-2020.png",
    preview = T,
    parallel = T,
    interactive = F,
    light = T,
    lightdirection = c(
        315, 320, 315, 320
    ),
    lightintensity = c(
        1200, 1750, 600, 700
    ),
    lightaltitude = c(
        15, 15, 80, 80
    ),
    ground_material =
        rayrender::microfacet(
            roughness = .6
        ),
    width = 5000,
    height = 5000
)
