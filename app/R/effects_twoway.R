# ANOVAlab — effects_twoway.R
# (Archivo de apoyo si se quisiera extender a cálculos adicionales de efectos e interacciones,
# siguiendo UT1: efectos simples y efecto de interacción como desviación respecto a
# media general + efectos simples.)

# Efecto de interacción estimado (ab)_{ij} = x̄_{ij} - (x̄.. + a_i + b_j)
interaction_effects <- function(means_AB, means_A, means_B, grand) {
  dplyr::left_join(
    means_AB,
    dplyr::rename(means_A, media_A = media),
    by = "A"
  ) |>
    dplyr::left_join(dplyr::rename(means_B, media_B = media), by = "B") |>
    dplyr::mutate(`efecto interacción (ab)` = media - (grand + (media_A - grand) + (media_B - grand))) |>
    dplyr::select(A, B, media, `efecto interacción (ab)`)
}