library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("dplyr", "ggplot2", "mmrm", "pmrm", "readr", "tibble"),
  format = "qs",
  controller = crew::crew_controller_local(workers = 10L)
)

tar_source()

targets_setup <- list(
  tar_target(index, seq_len(1000L)) # integer index of each simulation
)

targets_simulations <- tar_map(
  names = any_of("name"),
  values = tibble::tibble(
    name = c(
      "linear_decline_nonproportional",
      "linear_decline_proportional",
      "linear_slowing_nonproportional",
      "linear_slowing_proportional",
      "exponential_decline_nonproportional",
      "exponential_decline_proportional",
      "exponential_slowing_nonproportional",
      "exponential_slowing_proportional"
    ),
    simulate = rlang::syms(paste0("simulate_", gsub("^[^_]*_", "", name))),
    get_scenario = rlang::syms(paste0("get_scenario_", name))
  ),
  tar_target(name = scenario, command = get_scenario()),
  tar_target(
    name = simulations,
    command = simulate(scenario),
    pattern = map(index)
  )
)

targets_results <- list(
  tar_target(
    name = simulations,
    command = bind_rows(
      linear_decline_nonproportional = simulations_linear_decline_nonproportional,
      linear_decline_proportional = simulations_linear_decline_proportional,
      linear_slowing_nonproportional = simulations_linear_slowing_nonproportional,
      linear_slowing_proportional = simulations_linear_slowing_proportional,
      exponential_decline_nonproportional = simulations_exponential_decline_nonproportional,
      exponential_decline_proportional = simulations_exponential_decline_proportional,
      exponential_slowing_nonproportional = simulations_exponential_slowing_nonproportional,
      exponential_slowing_proportional = simulations_exponential_slowing_proportional,
      .id = "scenario"
    )
  ),
  tar_target(
    name = coverage,
    command = summarize_coverage(simulations)
  ),
  tar_target(
    name = convergence,
    command = summarize_convergence(simulations)
  ),
  tar_target(
    name = plot_coverage,
    command = visualize_coverage(coverage)
  ),
  tar_target(
    name = file_convergence,
    command = {
      write_csv(convergence, "convergence.csv")
      "convergence.csv"
    },
    format = "file"
  ),
  tar_target(
    name = file_coverage,
    command = {
      ggsave(
        "coverage.png",
        visualize_coverage(coverage),
        width = 12,
        height = 10
      )
      "coverage.png"
    },
    format = "file"
  ),
  tar_target(
    name = file_version,
    command = {
      version <- tibble::tibble(
        version = packageVersion("pmrm"),
        commit = packageDescription("pmrm")$GithubSHA1
      )
      write_csv(version, "version.csv")
      "version.csv"
    },
    format = "file"
  )
)

list(
  targets_setup,
  targets_simulations,
  targets_results
)
