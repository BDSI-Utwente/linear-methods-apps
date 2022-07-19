old <- theme_update(
    panel.background = element_blank(),
    legend.key = element_blank(),
    legend.background = element_blank(),
    strip.background = element_rect(fill = "white", colour = "black"),
    plot.background = element_rect(fill = "white"),
    # panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    # strip.text = element_text(size = 16, color = myRed),
    # axis.title.y = element_text(color = myRed, hjust = 0, face = "italic"),
    # axis.title.x = element_text(color = myRed, hjust = 0, face = "italic"),
    axis.text = element_text(color = "black")
    # legend.position = "none"
)
