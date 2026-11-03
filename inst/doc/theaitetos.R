## -----------------------------------------------------------------------------
# Core function for dynamic 3D-to-2D projection and multi-mode wave superposition
generate_dynamic_octahedron <- function(mode = "baseline", k = NULL, t = 0, rotate = TRUE) {
  
  # Golden ratio constant used for biological capsid configurations

  phi <- (1 + sqrt(5)) / 2
  
  if (is.null(k)) {
    k <- switch(mode,
                "baseline" = 8.0,        # Fine-tuned harmonic baseline state
                "fluorite" = 14.0,       # X-ray crystal lattice (Bragg, 1914)
                "capsid"   = 3.24 * phi, # Capsid resonance via Golden Ratio
                "universe" = 23.0)       # High frequency holographic boundary interference
  }
  
  vertices <- matrix(c(
     1,  0,  0,  -1,  0,  0,
     0,  1,  0,   0, -1,  0,
     0,  0,  1,   0,  0, -1
  ), ncol = 3, byrow = TRUE)
  
  if(rotate) {
    cos_t <- cos(t * 0.5)
    sin_t <- sin(t * 0.5)
    cx <- vertices[, 1] * cos_t + vertices[, 3] * sin_t
    cy <- vertices[, 2] * cos_t - vertices[, 3] * sin_t
  } else {
    # Protected pure Euclidean projection (Euclid Elements Book XIII)
    cx <- vertices[, 1]
    cy <- vertices[, 2]
  }
  
  grid_size <- 200
  raumzeit_grid <- expand.grid(
    X = seq(-2.5, 2.5, length.out = grid_size),
    Y = seq(-2.5, 2.5, length.out = grid_size)
  )
  wellen_feld <- numeric(nrow(raumzeit_grid))
  for(i in 1:length(cx)) {
    # 
   r <- sqrt((raumzeit_grid$X - cx[i])^2 + (raumzeit_grid$Y - cy[i])^2)
    welle <- cos(k * r - t) / (sqrt(r) + 0.1)
    wellen_feld <- wellen_feld + welle
  }
  
  raumzeit_grid$Intensitaet <- wellen_feld^2
  stopifnot(sum(raumzeit_grid$Intensitaet) > 0)
  return(raumzeit_grid)
}

## ----fig.align='center', fig.width=6, fig.height=5, echo=FALSE----------------
library(ggplot2)
# Define the 6 primary spatial vertex coordinates (3D) according to Euclid
vertices_3d <- data.frame(
  X = c(1, -1, 0,  0, 0,  0),
  Y = c(0,  0, 1, -1, 0,  0),
  Z = c(0,  0, 0,  0, 1, -1)
)

# Precisely map the edge connections of the regular octahedron
edges <- data.frame(
  x =    c(1, 1, 1, 1, -1, -1, -1, -1,  0,  0,  0,  0),
  y =    c(0, 0, 0, 0,  0,  0,  0,  0,  1, -1,  1, -1),
  z =    c(0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0),
  xend = c(0, 0, 0, 0,  0,  0,  0,  0,  1,  1, -1, -1),
  yend = c(1, -1, 0, 0, 1, -1, 0, 0,  0,  0,  0,  0),
  zend = c(0, 0, 1, -1,  0,  0, 1, -1,  0,  0,  0,  0)
)

# Compute the 3D-to-2D parallel projection to render a clean spatial oblique view
project_3d <- function(x, y, z) {
  x_proj <- x + 0.4 * z
  y_proj <- y + 0.4 * z
  return(data.frame(x = x_proj, y = y_proj))
}

p_vert <- project_3d(vertices_3d$X, vertices_3d$Y, vertices_3d$Z)
p_edges_start <- project_3d(edges$x, edges$y, edges$z)
p_edges_end <- project_3d(edges$xend, edges$yend, edges$zend)


faces <- data.frame(
  x = c(p_vert$x[c(1,3,5)], p_vert$x[c(1,4,5)], p_vert$x[c(2,3,5)], p_vert$x[c(2,4,5)]),
  y = c(p_vert$y[c(1,3,5)], p_vert$y[c(1,4,5)], p_vert$y[c(2,3,5)], p_vert$y[c(2,4,5)]),
  group = rep(1:4, each = 3)
)

ggplot() +
  geom_polygon(data = faces, aes(x = x, y = y, group = group), 
               fill = "#8A2BE2", alpha = 0.25) +
  geom_segment(aes(x = p_edges_start$x, y = p_edges_start$y, 
                   xend = p_edges_end$x, yend = p_edges_end$y),
               color = "#FFA500", linewidth = 0.8) +
  geom_point(aes(x = p_vert$x, y = p_vert$y), 
             color = "#FFFFFF", size = 4) +
  #Euklidisches 3D-Raumkreuz fuer Theaitetos :)
  labs(title = "Figure 1: Euclidean 3D Framework") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#05050A", color = NA),
    panel.background = element_rect(fill = "#05050A", color = NA),
    plot.title = element_text(color = "#FFA500", family = "mono", size = 11, face = "bold", hjust = 0.5),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

## ----fig.align='center', fig.width=10, fig.height=5, echo=FALSE---------------
# Re-defining the internal core architecture for the demonstration chunk
generate_vignette_octahedron <- function(mode = "baseline", t = 0, rotate = TRUE) {
  phi <- (1 + sqrt(5)) / 2
  k <- switch(mode,
              "baseline" = 8.0,
              "capsid"   = 3.24 * phi)
  
  vertices <- matrix(c(
     1,  0,  0,  -1,  0,  0,
     0,  1,  0,   0, -1,  0,
     0,  0,  1,   0,  0, -1
  ), ncol = 3, byrow = TRUE)
  
  if(rotate) {
    cos_t <- cos(t * 0.5)
    sin_t <- sin(t * 0.5)
    cx <- vertices[, 1] * cos_t + vertices[, 3] * sin_t
    cy <- vertices[, 2] * cos_t - vertices[, 3] * sin_t
  } else {
    cx <- vertices[, 1]
    cy <- vertices[, 2]
  }
  
  grid_size <- 200
  raumzeit_grid <- expand.grid(
    X = seq(-2.5, 2.5, length.out = grid_size),
    Y = seq(-2.5, 2.5, length.out = grid_size)
  )
  wellen_feld <- numeric(nrow(raumzeit_grid))
  for(i in 1:length(cx)) {
    # CRAN FIX: Perfectly clean mathematical syntax for Euclidean distance
     r <- sqrt((raumzeit_grid$X - cx[i])^2 + (raumzeit_grid$Y - cy[i])^2)
    welle <- cos(k * r - t) / (sqrt(r) + 0.1)
    wellen_feld <- wellen_feld + welle
  }
  return(wellen_feld^2)
}

# === CRAN FIX START ===
# Save the current graphical parameters of the user
oldpar <- par(no.readonly = TRUE)
# === CRAN FIX END ===

# Set up side-by-side plotting area with a deep black background
par(mfrow = c(1, 2), mar = c(1, 1, 3, 1), bg = "#05050A")

# 1. Generate and plot the static baseline matrix (Left Panel)
intensitaet_static <- generate_vignette_octahedron(mode = "baseline", t = 0, rotate = FALSE)
matrix_static <- matrix(intensitaet_static, nrow = 200)

image(matrix_static, 
      col = colorRampPalette(c("#05050A", "#4B0082", "#8A2BE2", "#DAA520", "#FFFFFF"))(256), 
      axes = FALSE, main = "// FIELD LOG: Static Baseline", col.main = "#FFA500", cex.main = 0.8)

# 2. Generate and plot the pulsing capsid resonance matrix (Right Panel)
intensitaet_dynamic <- generate_vignette_octahedron(mode = "capsid", t = pi/4, rotate = TRUE)
matrix_dynamic <- matrix(intensitaet_dynamic, nrow = 200)

image(matrix_dynamic, 
      col = colorRampPalette(c("#05050A", "#4B0082", "#8A2BE2", "#FF6347", "#FFA500", "#FFFFFF"))(256), 
      axes = FALSE, main = "// FIELD LOG: Pulsing Symmetry", col.main = "#FFA500", cex.main = 0.8)

# === CRAN FIX START ===
# Immediately restore the user's original graphical parameters
par(oldpar)
# === CRAN FIX END ===


