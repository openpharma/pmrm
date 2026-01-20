test_that("pmrm_data()", {
  raw <- pmrm_simulate_decline(gamma = c(1, 2))
  raw$y[1] <- NA_real_
  data <- pmrm_data(
    data = raw,
    outcome = "y",
    time = "t",
    patient = "patient",
    visit = "visit",
    arm = "arm",
    covariates = ~ w_1 + w_2
  )
  expect_true(tibble::is_tibble(data))
  expect_true(inherits(data, "pmrm_data"))
  expect_equal(nrow(data), nrow(raw) - 1L)
  expect_false(anyNA(data$y))
  expect_equal(
    pmrm_data_labels(data),
    list(
      outcome = "y",
      time = "t",
      patient = "patient",
      visit = "visit",
      arm = "arm",
      covariates = ~ w_1 + w_2
    )
  )
})

test_that("pmrm_data() bad covariates", {
  raw <- pmrm_simulate_decline(gamma = c(1, 2))
  raw$y[1] <- NA_real_
  expect_error(
    pmrm_data(
      data = raw,
      outcome = "y",
      time = "t",
      patient = "patient",
      visit = "visit",
      arm = "arm",
      covariates = ~ w_1 + w_3
    ),
    class = "pmrm_error"
  )
})

test_that("pmrm_data() cleans numerics", {
  set.seed(0L)
  data <- expand.grid(j = seq_len(3L), i = seq_len(5L) / 10)
  data$k <- c(rep(1L, 9L), rep(2L, 6L))
  data$y <- sample.int(nrow(data))
  data$time <- as.numeric(data$j - 1L)
  data <- data[sample.int(15L), ]
  out <- pmrm_data(
    data,
    outcome = "y",
    patient = "i",
    visit = "j",
    arm = "k",
    time = "time"
  )
  expect_true(is.factor(out$i))
  expect_false(is.ordered(out$i))
  for (name in c("j", "k")) {
    expect_true(is.ordered(out[[name]]))
  }
  expect_equal(
    as.numeric(as.character(out$i)),
    rep(seq_len(5L) / 10, each = 3L)
  )
  expect_equal(levels(out$i), as.character(seq_len(5L) / 10))
  expect_equal(as.integer(as.character(out$j)), rep(seq_len(3L), times = 5L))
  expect_equal(levels(out$j), as.character(seq_len(3L)))
  expect_equal(
    as.integer(as.character(out$k)),
    c(rep(1L, 9L), rep(2L, 6L))
  )
  expect_equal(levels(out$k), as.character(seq_len(2L)))
})

test_that("pmrm_data() cleans characters and factors", {
  set.seed(0L)
  for (ordered in c(TRUE, FALSE)) {
    for (factors in c(TRUE, FALSE)) {
      data <- expand.grid(
        j = letters[seq_len(3L)],
        i = letters[seq_len(5L)],
        stringsAsFactors = factors
      )
      data$k <- letters[c(rep(1L, 9L), rep(2L, 6L))]
      data$y <- sample.int(nrow(data))
      data$time <- as.numeric(0 + (data$j == "b") + 2 * (data$j == "c"))
      if (factors && ordered) {
        for (name in c("i", "j", "k")) {
          data[[name]] <- ordered(data[[name]])
        }
      }
      data <- data[sample.int(15L), ]
      out <- pmrm_data(
        data,
        outcome = "y",
        patient = "i",
        visit = "j",
        arm = "k",
        time = "time"
      )
      expect_true(is.factor(out$i))
      expect_false(is.ordered(out$i))
      for (name in c("j", "k")) {
        expect_true(is.ordered(out[[name]]))
      }
      expect_equal(
        as.character(out$i),
        letters[rep(seq_len(5L), each = 3L)]
      )
      expect_equal(levels(out$i), letters[seq_len(5L)])
      expect_equal(as.character(out$j), letters[rep(seq_len(3L), times = 5L)])
      expect_equal(levels(out$j), letters[seq_len(3L)])
      expect_equal(
        as.character(out$k),
        letters[c(rep(1L, 9L), rep(2L, 6L))]
      )
      expect_equal(levels(out$k), letters[seq_len(2L)])
    }
  }
})

test_that("pmrm_data() removes missing outcome rows", {
  set.seed(0L)
  data <- expand.grid(j = seq_len(3L), i = seq_len(5L) / 10)
  data$time <- as.numeric(data$j - 1L)
  data$k <- c(rep(1L, 9L), rep(2L, 6L))
  data$y <- seq_len(nrow(data))
  data$y[c(2L, 5L)] <- NA_real_
  data <- data[sample.int(15L), ]
  out <- pmrm_data(
    data,
    outcome = "y",
    patient = "i",
    visit = "j",
    arm = "k",
    time = "time"
  )
  expect_equal(nrow(out), 13L)
  expect_equal(out$y, setdiff(seq_len(15L), c(2L, 5L)))
})
