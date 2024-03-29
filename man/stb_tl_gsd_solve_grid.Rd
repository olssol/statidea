% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stb_tls_gsd.R
\name{stb_tl_gsd_solve_grid}
\alias{stb_tl_gsd_solve_grid}
\title{GSD for one interim analysis}
\usage{
stb_tl_gsd_solve_grid(
  info_fracs = c(0.2, 1),
  boundary = c(0.001, NA),
  boundary_type = c("alpha", "alpha_spent", "zscore"),
  alpha = 0.025,
  power = 0.9,
  n_grid = 10000,
  tol = 1e-06
)
}
\description{
GSD for one interim analysis
}
