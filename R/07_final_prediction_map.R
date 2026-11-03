

# 07_final_prediction_map.R
# Create annual, monthly and annual-from-monthly abundance maps

source(here::here("R", "config.R"))

library(terra)
library(ggplot2)
library(patchwork)
library(scales)
library(purrr)

# Settings --------------------------------------------------------------------

make_dir(paths$figures)

models <- c("M3", "M4")
map_clip_q <- 0.99

# Prediction files ------------------------------------------------------------

pred_file <- \(x) file.path(paths$predictions, x)

annual_files <- set_names(
  map(models, \(m) pred_file(paste0("annual_", m, ".tif"))),
  models
)

monthly_files <- set_names(
  map(models, \(m) {
    map(1:12, \(i) {
      pred_file(sprintf("monthly_%s_%02d.tif", m, i))
    })
  }),
  models
)

derived_files <- set_names(
  map(models, \(m) {
    pred_file(paste0("annual_from_monthly_", m, ".tif"))
  }),
  models
)

# Training support ------------------------------------------------------------

read_supported <- \(f) {
  
  r <- rast(f)
  
  leverage_file <- sub(
    "\\.tif$",
    "_leverage_ratio.tif",
    f
  )
  
  if (file.exists(leverage_file)) {
    leverage <- rast(leverage_file)
    r <- mask(r, ifel(leverage > 1, NA, 1))
  }
  
  r
}

# Map extent ------------------------------------------------------------------

map_extent <- as.vector(
  ext(rast(annual_files[[1]]))
)

# Common colour limits --------------------------------------------------------

get_limits <- \(files) {
  
  upper <- max(
    map_dbl(files, \(f) {
      
      r <- read_supported(f)
      v <- values(r, mat = FALSE)
      v <- v[is.finite(v)]
      
      if (length(v) == 0) return(NA_real_)
      
      quantile(
        v,
        probs = map_clip_q,
        na.rm = TRUE,
        names = FALSE
      )
    }),
    na.rm = TRUE
  )
  
  c(0, upper)
}

# Plot function ---------------------------------------------------------------

plot_map <- \(file, title, limits = NULL) {
  
  r <- rast(file)
  
  leverage_file <- sub(
    "\\.tif$",
    "_leverage_ratio.tif",
    file
  )
  
  if (file.exists(leverage_file)) {
    
    leverage <- rast(leverage_file)
    
    r <- ifel(
      leverage > 1,
      NA,
      r
    )
  }
  
  names(r) <- "prediction"
  
  map_data <- as.data.frame(
    r,
    xy = TRUE,
    na.rm = FALSE
  )
  
  if (is.null(limits)) {
    limits <- get_limits(file)
  }
  
  ggplot(map_data, aes(x = x, y = y, fill = prediction)) +
    geom_raster() +
    scale_fill_viridis_c(
      name = "Predicted\nabundance",
      option = "C",
      limits = limits,
      oob = scales::squish,
      na.value = "grey85"
    ) +
    coord_equal(
      xlim = map_extent[1:2],
      ylim = map_extent[3:4],
      expand = FALSE
    ) +
    labs(
      title = title,
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(
        face = "bold",
        hjust = 0.5
      ),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      panel.grid = element_blank(),
      legend.position = "right"
    )
}

# Model labels ----------------------------------------------------------------

get_model_label <- \(m) {
  
  if (exists("model_labels") &&
      !is.null(model_labels) &&
      m %in% names(model_labels)) {
    
    return(model_labels[[m]])
  }
  
  m
}

# Annual prediction maps ------------------------------------------------------

iwalk(annual_files, \(f, m) {
  
  p <- plot_map(
    f,
    paste("Annual abundance:", get_model_label(m))
  )
  
  ggsave(
    filename = file.path(
      paths$figures,
      paste0("annual_", m, ".png")
    ),
    plot = p,
    width = 9,
    height = 7,
    dpi = 300
  )
})

# Annual maps derived from monthly predictions -------------------------------

iwalk(derived_files, \(f, m) {
  
  if (file.exists(f)) {
    
    p <- plot_map(
      f,
      paste("Annual mean from monthly predictions:", get_model_label(m))
    )
    
    ggsave(
      filename = file.path(
        paths$figures,
        paste0("annual_from_monthly_", m, ".png")
      ),
      plot = p,
      width = 9,
      height = 7,
      dpi = 300
    )
  }
})

# Monthly prediction maps -----------------------------------------------------

iwalk(monthly_files, \(files, m) {
  
  limits <- get_limits(files)
  
  plots <- map2(files, month.abb, \(f, month) {
    
    plot_map(
      f,
      month,
      limits
    )
  })
  
  monthly_plot <- wrap_plots(
    plots,
    ncol = 4
  ) +
    plot_annotation(
      title = paste(
        "Monthly malaria vector abundance:",
        get_model_label(m)
      )
    )
  
  ggsave(
    filename = file.path(
      paths$figures,
      paste0("monthly_predictions_", m, ".png")
    ),
    plot = monthly_plot,
    width = 14,
    height = 10,
    dpi = 300
  )
})


