my_theme <- function (...) {
  my_theme <- ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold",
                                       hjust = 0,
                                       size = ggplot2::rel(1.5),
                                       margin = ggplot2::margin(0, 0, 4, 0)),
    panel.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.9),
    legend.box.background = ggplot2::element_rect(color = "black", linewidth = 0.9),
    legend.key = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    axis.ticks.length = unit(-0.15, "cm"),
    axis.text.x = ggplot2::element_text(color = "black", margin = ggplot2::margin(4, 0, 0, 0)),
    axis.text.y = ggplot2::element_text(color = "black", margin = ggplot2::margin(0, 4, 0, 0)),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(8, 0, 0, 0)),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0, 8, 0, 0), angle = 90),
    ..., complete = TRUE)
  my_theme
}

expander <- function(ex = c(0, 0), ey = c(0, 1)) {
  list(
    scale_x_continuous(expand = ex),
    scale_y_continuous(expand = ey)
  )
}
