# ==========================================================================
#  ++ [ T h e  _  A I  _  t e t o s ] : ANCIENT INTELLIGENCE TRIBUTE (2026)
# --------------------------------------------------------------------------
#  >> REPRODUCIBLE DEMONSTRATION & SYSTEM VALIDATION RUN
#  >> Core Developer: Katharina Maria Brecht (Kata)
# ==========================================================================

# 1. Establish deterministic seed for mathematical reproducibility
set.seed(42)
phase_time <- pi / 4  # Active phase-space rotation

# ==========================================================================
# >> INTERACTIVE DIMENSION SELECTION FOR THOSE WHO TRAVEL WITH ME
# ==========================================================================
menu_options <- c(
  "Capsid Mode (Protein configurations based on Caspar & Klug)",
  "Fluorite Mode (Crystal lattices according to Bragg)",
  "Holo_Universe Mode (Kata's cosmic boundary-plane interference)",
  "Standard Baseline (Classical harmonic ground oscillation)"
)

# Sichere interaktive Weiche: Menue oeffnet sich nur, wenn ein Mensch davor sitzt
if (interactive()) {
  message("\n// Welcome to the matrix of Theaitetos!")
  message("Please select which realm you wish to compute from the menu below:")
  user_selection <- utils::menu(menu_options, title = "Choose your cosmic dimension:")

  selected_mode <- switch(user_selection,
                          "capsid",
                          "fluorite",
                          "universe",
                          "baseline")
} else {
  # Dieser Block laeuft vollautomatisch auf dem Testserver ohne Blockieren durch
  user_selection <- 1
  selected_mode <- "capsid"
}

# Safetynet: If the user aborts the menu (inputs 0)
if (is.null(selected_mode) || user_selection == 0) {
  message("Loading standard Capsid Mode as a protective measure...")
  selected_mode <- "capsid"
}

# ==========================================================================
# 2. Compute the scalar interference field via core engine
# ==========================================================================
message("\nEngine initialized. Computing mode: '", selected_mode, "'...")

wave_field_data <- theaitetos::generate_dynamic_octahedron(
  mode = selected_mode,
  k = NULL,
  t = phase_time,
  rotate = TRUE
)

# 3. Render the crystalline quantum hologram
dynamic_title <- switch(selected_mode,
                        "capsid"   = "// KATA LOG: Stereometric Capsid Resonance",
                        "fluorite" = "// KATA LOG: Mineralogical Crystal Lattice Interference",
                        "universe" = "// KATA LOG: Holographic Universe [Boundary-Plane Projection]",
                        "baseline" = "// KATA LOG: Harmonic Baseline Oscillation"
)

message("Generating interpolated quantum hologram in Kata's signature Solar Palette...")

# Das print() sorgt dafuer, dass die Grafik im interaktiven Modus verlaesslich aufpoppt
print(theaitetos::plot_quantum_hologram(
  field_data = wave_field_data,
  title_text = dynamic_title
))

# ==========================================================================
# My worlds currently converge at the intersection of mathematics, biology,
# mineralogy, astronomy, and quantum physics.
# where do we go next? :)
# ==========================================================================
