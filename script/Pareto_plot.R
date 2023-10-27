Pareto_plot <- function(mod){
  paint_coeffs <- coefficients(mod)[-1] # We discard the intercept
  paint_coeffs <- paint_coeffs[!is.na(paint_coeffs)] # Discard NA effects
  paint_effects <- data.frame(
    Effect = names(paint_coeffs),
    Value  = unname(paint_coeffs),
    AbsoluteValue = abs(unname(paint_coeffs)),
    Sign   = as.character(unname(sign(paint_coeffs)))
  )
  p <- ggplot(paint_effects, aes(AbsoluteValue, reorder(Effect, -AbsoluteValue, abs))) +
    geom_col(aes(fill = Sign)) +
    xlab("Magnitude of effect") +
    ylab("Effect") +
    theme_minimal()
  return(p)
}