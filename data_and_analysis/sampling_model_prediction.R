library(ggplot2)

# Create the "plot" sub-folder for the saved figures (does nothing if it exists)
dir.create("plots", showWarnings = FALSE)

# Colours ---------------------------------------------------------------------
# Figure 1: navy -> teal -> mint (dark = d of 0, light = d of 0.5)
colours_fig1 <- c("#272A79", "#00779C", "#45B1AF", "#91DABA")

# Figure 2: viridis, reversed so that alpha = 0 (the flat line) is the lightest.
# end = 0.9 skips the palest yellow; change to end = 1 for the full range.
colours_fig2 <- rev(viridisLite::viridis(256, end = 0.9))


# =============================================================================
# Figure 1:  E[P_hat(A)] = (1 - 2d) * P(A) + d
# =============================================================================

# Step 1: make the data, one row per (P(A), d) combination
fig1_data <- expand.grid(
  P = seq(0, 1, by = 0.01),     # x-axis: true probability P(A)
  d = seq(0, 0.5, by = 0.05)    # one line per value of d (11 lines)
)
fig1_data$E <- (1 - 2 * fig1_data$d) * fig1_data$P + fig1_data$d

# Step 2: draw the plot
fig1 <- ggplot(fig1_data, aes(x = P, y = E, colour = d, group = d)) +
  geom_line(linewidth = 1) +
  scale_colour_gradientn(
    colours = colours_fig1,
    limits  = c(0, 0.5),
    breaks  = c(0, 0.125, 0.25, 0.375, 0.5)
  ) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("0.00", "0.25", "0.50", "0.75", "1.00")) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("0.00", "0.25", "0.50", "0.75", "1.00")) +
  coord_equal() +               # square plotting area
  labs(
    x      = "P(A)",
    y      = expression(E(hat(P)(A))),
    colour = "d"
  ) +
  theme_bw(base_size = 15) +
  theme(
    plot.title          = element_text(face = "italic", size = 13),  # long title: keep it smaller so it fits
    plot.title.position = "plot",
    panel.grid.minor    = element_blank(),
    axis.text           = element_text(colour = "black", size = 13),
    legend.title        = element_text(size = 17),
    legend.text         = element_text(size = 13),
    legend.key.height   = unit(1.2, "cm"),   # colour bar is 5x this tall
    legend.key.width    = unit(0.45, "cm")
  )

# Step 3: save
# width and height are in inches
ggsave("plots/effect_of_d.pdf", fig1, width = 5, height = 4)
#ggsave("plots/effect_of_d.png", fig1, width = 5, height = 4, dpi = 300, bg = "white")

# =============================================================================
# Figure 2:  E[P_hat(A)] = 2 * alpha * P(A) + 1/2 + b - alpha
# =============================================================================

# Step 1: make the data, one row per (P(A), alpha, b) combination
fig2_data <- expand.grid(
  P     = seq(0, 1, by = 0.001),   # x-axis (fine grid so lines end neatly)
  alpha = seq(0, 1, by = 0.1),     # one line per value of alpha (11 lines)
  b     = c(0, 0.15, -0.15)        # one panel per value of b
)
fig2_data$E <- 2 * fig2_data$alpha * fig2_data$P + 0.5 + fig2_data$b - fig2_data$alpha

# Keep only the part of each line inside the y-range -0.3 to 1.3
# (we draw outside the panel later for the labels, so ggplot can't trim them)
fig2_data <- subset(fig2_data, E >= -0.3 & E <= 1.3)

# Panel titles, in the order b = 0, 0.15, -0.15
panel_order     <- c("b = 0", "b = 0.15", "b = -0.15")
fig2_data$panel <- factor(paste("b =", fig2_data$b), levels = panel_order)

# Point where all lines cross in each panel: P(A) = 0.5, E = 0.5 + b
cross_points <- data.frame(
  panel = factor(panel_order, levels = panel_order),
  P     = 0.5,
  E     = c(0.50, 0.65, 0.35),
  label = c("0.50", "0.65", "0.35")
)

# Dashed y = x reference line, only in the b = 0.15 and b = -0.15 panels
diagonal_panels <- data.frame(
  panel = factor(c("b = 0.15", "b = -0.15"), levels = panel_order)
)

# Step 2: draw the plot
fig2 <- ggplot(fig2_data, aes(x = P, y = E)) +
  # grey band between 0 and 1, with grey lines at its edges
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 1, fill = "grey95") +
  geom_hline(yintercept = c(0, 1), colour = "grey75") +
  # dashed y = x line
  geom_segment(data = diagonal_panels, aes(x = 0, xend = 1, y = 0, yend = 1),
               linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  # the model lines, coloured by alpha
  geom_line(aes(colour = alpha, group = alpha), linewidth = 0.8) +
  # white crossing point
  geom_point(data = cross_points, shape = 21, size = 2.6,
             fill = "white", colour = "black") +
  # value label to the right of each panel: a small tick, then the number
  geom_segment(data = cross_points, aes(x = 1.03, xend = 1.06, y = E, yend = E),
               linewidth = 0.4) +
  geom_text(data = cross_points, aes(x = 1.08, y = E, label = label),
            hjust = 0, size = 4.5) +
  # three panels side by side
  facet_wrap(~ panel, nrow = 1) +
  scale_colour_gradientn(
    colours = colours_fig2,
    limits  = c(0, 1),
    breaks  = c(0, 0.25, 0.5, 0.75, 1),
    labels  = c("0.00", "0.25", "0.50", "0.75", "1.00")
  ) +
  scale_x_continuous(breaks = c(0, 0.4, 0.8), labels = c("0.0", "0.4", "0.8")) +
  scale_y_continuous(breaks = c(0, 0.5, 1),   labels = c("0.0", "0.5", "1.0")) +
  # clip = "off" lets the value labels sit outside the panels
  coord_cartesian(xlim = c(0, 1), ylim = c(-0.3, 1.3), clip = "off") +
  labs(
    x      = "P(A)",
    y      = expression(E*"["*hat(P)(A)*"]"),
    colour = expression(alpha)
  ) +
  theme_classic(base_size = 15) +
  theme(
    plot.title          = element_text(face = "italic", size = 15),
    plot.title.position = "plot",
    axis.text           = element_text(colour = "black", size = 13),
    strip.background    = element_blank(),   # no box around panel titles
    strip.text          = element_text(size = 15),
    panel.spacing       = unit(1.5, "cm"),   # room for the value labels
    legend.title        = element_text(size = 17),
    legend.text         = element_text(size = 13),
    legend.key.height   = unit(1.1, "cm"),   # colour bar is 5x this tall
    legend.key.width    = unit(0.45, "cm"),
    legend.box.spacing  = unit(1.5, "cm")
  )

fig2

# Step 3: save
# width and height are in inches
ggsave("plots/quantum_sequential_sampler.pdf", fig2, width = 8, height = 3.6)
#ggsave("plots/quantum_sequential_sampler.png", fig2, width = 8, height = 3.6, dpi = 300, bg = "white")