summarize_parameters <- function(fit) {
  lapply(
    c("alpha", "theta", "gamma", "phi", "rho"),
    function(name) {
      keep <- c("confidence", "parameter", "estimate", "lower", "upper")
      bind_rows(
        `0.5` = pmrm_estimates(fit, parameter = name, confidence = 0.5) |>
          mutate(parameter = paste0(parameter, "_", seq_len(n()))),
        `0.95` = pmrm_estimates(fit, parameter = name, confidence = 0.95) |>
          mutate(parameter = paste0(parameter, "_", seq_len(n()))),
        .id = "confidence"
      ) |>
        select(any_of(keep))
    }
  ) |>
    bind_rows()
}

summarize_coverage <- function(simulations) {
  simulations |>
    group_by(scenario, parameter, confidence) |>
    summarize(coverage = mean(cover))
}

summarize_convergence <- function(simulations) {
  simulations |>
    group_by(scenario) |>
    summarize(convergence = mean(convergence))
}

visualize_coverage <- function(coverage) {
  ggplot(coverage) +
    geom_hline(
      aes(yintercept = as.numeric(confidence)),
      linetype = "dashed",
      color = "gray50"
    ) +
    geom_point(aes(x = parameter, y = coverage, color = confidence)) +
    geom_line(
      aes(
        x = parameter,
        y = coverage,
        color = confidence,
        group = confidence
      )
    ) +
    facet_wrap(~scenario, scales = "free_x") +
    scale_y_continuous(
      limits = c(0, 1),
      labels = scales::label_percent(accuracy = 1)
    ) +
    theme_gray(16) +
    theme(axis.text.x = element_text(angle = 90))
}
