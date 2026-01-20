library(ggplot2)
library(tibble)
library(tidyr)

# Title and border color: #DAD7CD
color_background <- "#231F20"
color_delay <- "#F3DFA2"
color_control <- "#D17C4A"
color_treatment <- "#7EBDC2"

f <- function(x) {
  -1 / (1 + exp(10 - x))
}

f_inverse <- function(y) {
  10 - log(-1 - 1 / y)
}

geom_delay <- function(height, delay) {
  geom_errorbar(
    data = tibble::tibble(
      from = f_inverse(height) + delay,
      to = f_inverse(height),
      value = height
    ),
    mapping = aes(xmin = from, xmax = to, y = value),
    linetype = 1,
    linewidth = 3,
    color = color_delay,
    width = 0.1
  )
}

delay <- 2.5

data <- tibble(
  time = seq(from = 6, to = 16, length.out = 1e3),
  control = f(time),
  treatment = f(time - delay)
) |>
  pivot_longer(cols = c("treatment", "control"))

knots <- tibble(
  time = seq(from = 6, to = 16, length.out = 4),
  control = f(time),
  treatment = f(time - delay)
) |>
  pivot_longer(cols = c("treatment", "control"))

plot <- ggplot() +
  geom_point(
    data = data,
    mapping = aes(x = time, y = value, color = name)
  ) +
  geom_point(
    data = knots,
    mapping = aes(x = time, y = value, color = name),
    size = 10
  ) +
  geom_delay(-0.25, delay) +
  geom_delay(-0.75, delay) +
  theme_void() +
  scale_color_manual(
    values = c(control = color_control, treatment = color_treatment)
  ) +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = color_background)
  )

ggsave("image.png", plot, width = 10, height = 4)
