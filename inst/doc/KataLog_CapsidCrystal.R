## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.width = 6,
  fig.height = 5
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(theaitetos)
library(ggplot2)

# 1. Berechne das Interferenzfeld im harmonischen Baseline-Zustand (k = 8.0)
# 1. Compute the interference field in the harmonic baseline state (k = 8.0)
wellen_daten <- generate_dynamic_octahedron(k = 8.0, t = pi/4, rotate = TRUE)

# 2. Plotten des Quantenhologramms in Katas - Solar-Fluorit-Palette
# 2. Render the quantum hologram in Kata's signature Solar-Fluorite palette
plot_quantum_hologram(wellen_daten, title_text = "// KATA LOG: Stereometrische Kapsid-Resonanz")

