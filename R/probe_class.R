#' @importFrom cli cli_rule cli_text cli_alert_success cli_alert_danger
#'   cli_alert_info cli_end cli_ul
#'
#' @export
print.teems_probe <- function(x, ...) {
  cli::cli_rule("teems structural probe")
  cli::cli_text("condensed system: {x$vecsize} x {x$vecsize}")
  for (pattern in c("structural", "realized")) {
    p <- x[[pattern]]
    if (is.null(p)) next
    lbl <- if (pattern %=% "structural") {
      "structural pattern"
    } else {
      "realized pattern (nonzero at base data)"
    }
    if (!p$defective) {
      cli::cli_alert_success("{lbl}: full structural rank {p$rank} of {p$n}")
    } else {
      cli::cli_alert_danger(
        "{lbl}: structurally singular — rank {p$rank} of {p$n}
        ({p$unmatched_rows} unmatched equation{?s},
        {p$unmatched_cols} unmatched variable{?s})"
      )
      if (NROW(p$under_by_var)) {
        agg <- paste0(p$under_by_var$name, " ×", p$under_by_var$count)
        cli::cli_text("  under-determined by variable: {.val {agg}}")
      }
      if (NROW(p$over_by_eq)) {
        agg <- paste0(p$over_by_eq$name, " ×", p$over_by_eq$count)
        cli::cli_text("  over-constrained by equation: {.val {agg}}")
      }
      if (!is.null(p$dm)) {
        cli::cli_text(
          "  DM blocks: under {p$dm$m1} x {p$dm$n1},
          well {p$dm$m2} x {p$dm$n2}, over {p$dm$m3} x {p$dm$n3}"
        )
      }
    }
  }
  if (!is.null(x$cores)) {
    cli::cli_text(
      "fine DM: {x$cores$sq_comps} strongly connected component{?s} —
      {x$cores$cores_gt1} simultaneous core{?s} (>1 element),
      largest {x$cores$largest}"
    )
    if (NROW(x$cores$top)) {
      eqs <- x$cores$top$eqs[[1]]
      preview <- utils::head(paste0(eqs$name, " ×", eqs$count), 6L)
      cli::cli_text("  largest core by equation: {.val {preview}}")
    }
  }
  if (NROW(x$statements)) {
    cli::cli_text(
      "{NROW(x$statements)} equation statement{?s},
      {NROW(x$incidence)} statement-variable incidence{?s}"
    )
  }
  if (!is.null(x$structure)) {
    cli::cli_text(
      "ordering evidence: chain {x$structure$chain_source %|||% 'none'},
      partition {x$structure$partition_source %|||% 'none'}"
    )
  }
  .probe_print_condense(x$condense)
  invisible(x)
}

#' @export
summary.teems_probe <- function(object, ...) {
  print(object)
  cli::cli_rule()
  if (NROW(object$defects)) {
    cli::cli_text("defect elements (capped upstream at 200 per list):")
    print(object$defects, n = 20)
  }
  if (NROW(object$incidence)) {
    dense <- object$incidence[order(-object$incidence$weight), ]
    cli::cli_text("heaviest statement-variable incidences:")
    print(utils::head(dense, 10))
  }
  invisible(object)
}

#' @title Plot a structural probe
#' @export
#' @description Visualizations of a [`ems_probe()`] result:
#'   * `"incidence"`: the equation-system structure — a spy-style
#'   matrix of equation statements (rows, declaration order) by
#'   referenced variables (columns, first-appearance order), shaded by
#'   element-level incidence weight.
#'   * `"dm"`: the coarse Dulmage-Mendelsohn localization of a
#'   structurally singular system — the under-, well- and
#'   over-determined blocks to scale (only available when the probe
#'   found defects).
#'   * `"cores"`: the fine-decomposition core structure — sizes of the
#'   irreducible simultaneous cores and the composition of the largest
#'   core by equation statement (requires `fine = TRUE`).
#' @param x A `teems_probe` object from [`ems_probe()`].
#' @param type Character length 1: `"incidence"` (default), `"dm"`, or
#'   `"cores"`.
#' @param max_labels Integer length 1 (default `60L`). Axis labels are
#'   dropped when a dimension exceeds this count.
#' @param ... Unused.
#' @return The input, invisibly. Base-graphics side effects; the
#'   underlying tibbles (`x$incidence`, `x$cores`, pattern blocks) are
#'   exposed for custom plotting.
#' @importFrom graphics axis barplot box image layout mtext par rect
#'   text title
#' @importFrom grDevices gray.colors
plot.teems_probe <- function(x,
                             type = c("incidence", "dm", "cores"),
                             max_labels = 60L,
                             ...) {
  type <- match.arg(type)
  switch(type,
    "incidence" = .probe_plot_incidence(x, max_labels = max_labels),
    "dm" = .probe_plot_dm(x),
    "cores" = .probe_plot_cores(x)
  )
  invisible(x)
}

#' @keywords internal
#' @noRd
.probe_plot_incidence <- function(x,
                                  max_labels = 60L) {
  inc <- x$incidence
  if (!NROW(inc)) {
    .cli_action(
      msg = "This probe carries no statement incidence data (report
      version {x$version %|||% 1}); rerun against a solver image with
      probe report version 2.",
      action = "abort"
    )
  }
  eqs <- unique(inc$eq)
  vars <- unique(inc$var)
  m <- matrix(NA_real_, nrow = length(eqs), ncol = length(vars))
  m[cbind(
    match(inc$eq, eqs),
    match(inc$var, vars)
  )] <- log1p(inc$weight)

  op <- graphics::par(mar = c(3, 8, 8, 1))
  on.exit(graphics::par(op), add = TRUE)
  graphics::image(
    x = seq_along(vars),
    y = seq_along(eqs),
    z = t(m[rev(seq_along(eqs)), , drop = FALSE]),
    col = rev(grDevices::gray.colors(64, start = 0.05, end = 0.92)),
    axes = FALSE,
    xlab = "",
    ylab = ""
  )
  graphics::box()
  if (length(vars) <= max_labels) {
    graphics::axis(3,
      at = seq_along(vars), labels = vars,
      las = 2, cex.axis = 0.55, tick = FALSE, line = -0.5
    )
  }
  if (length(eqs) <= max_labels) {
    graphics::axis(2,
      at = seq_along(eqs), labels = rev(eqs),
      las = 2, cex.axis = 0.55, tick = FALSE, line = -0.5
    )
  }
  graphics::mtext(
    sprintf(
      "equation-system structure: %d statements x %d variables (%d incidences)",
      length(eqs), length(vars), NROW(inc)
    ),
    side = 1, line = 1, cex = 0.8
  )
  invisible(NULL)
}

#' @keywords internal
#' @noRd
.probe_plot_dm <- function(x) {
  p <- x$structural
  if (is.null(p) || !p$defective || is.null(p$dm)) {
    p <- x$realized
  }
  if (is.null(p) || !p$defective || is.null(p$dm)) {
    .cli_action(probe_err$no_defects_dm,
      action = "abort"
    )
  }
  dm <- p$dm
  n <- p$n
  op <- graphics::par(mar = c(4, 4, 3, 1))
  on.exit(graphics::par(op), add = TRUE)
  graphics::plot(
    NA,
    xlim = c(0, n), ylim = c(0, n),
    xlab = "variables (columns)", ylab = "equations (rows)",
    axes = FALSE, asp = 1
  )
  graphics::axis(1)
  graphics::axis(2)
  graphics::box()
  # DM order: under-determined columns first, unmatched rows last;
  # rows are counted from the top (y flipped). Degenerate blocks (a few
  # rows/columns in a 10^4+ system) get a minimum visible marker size,
  # anchored at their true corner, with the label set beside them; the
  # well block is drawn first so the markers stay on top.
  min_px <- n / 50
  blocks <- list(
    list(
      x0 = dm$n1, y1 = n - dm$m1, w = dm$n2, h = dm$m2,
      col = "gray82", lab = "well", anchor = "left"
    ),
    list(
      x0 = 0, y1 = n, w = dm$n1, h = dm$m1,
      col = "#c9573b", lab = "under", anchor = "left"
    ),
    list(
      x0 = dm$n1 + dm$n2, y1 = n - dm$m1 - dm$m2, w = dm$n3, h = dm$m3,
      col = "#d99a3d", lab = "over", anchor = "right"
    )
  )
  for (b in blocks) {
    if (b$w <= 0 && b$h <= 0) next
    w_d <- max(b$w, min_px)
    h_d <- max(b$h, min_px)
    if (b$anchor %=% "right") {
      x1 <- min(b$x0 + b$w, n)
      x0 <- x1 - w_d
    } else {
      x0 <- b$x0
      x1 <- min(x0 + w_d, n)
    }
    y1 <- b$y1
    y0 <- max(y1 - h_d, 0)
    y1 <- min(y0 + h_d, n) # keep the marker its minimum size at the edges
    graphics::rect(x0, y0, x1, y1, col = b$col, border = "gray30")
    lab <- sprintf("%s %d x %d", b$lab, b$h, b$w)
    if (b$w > n / 8 && b$h > n / 8) {
      graphics::text((x0 + x1) / 2, (y0 + y1) / 2, lab, cex = 0.8)
    } else if (x1 < n / 2) {
      graphics::text(x1 + n / 90, (y0 + y1) / 2, lab, cex = 0.75, adj = 0)
    } else {
      graphics::text(x0 - n / 90, (y0 + y1) / 2, lab, cex = 0.75, adj = 1)
    }
  }
  graphics::title(
    main = sprintf(
      "Dulmage-Mendelsohn localization (%s pattern): rank %d of %d",
      if (identical(p, x$structural)) "structural" else "realized",
      p$rank, p$n
    ),
    cex.main = 0.9
  )
  invisible(NULL)
}

#' @keywords internal
#' @noRd
.probe_plot_cores <- function(x) {
  if (is.null(x$cores)) {
    .cli_action(probe_err$no_fine,
      action = "abort"
    )
  }
  op <- graphics::par(mfrow = c(1, 2), mar = c(5, 5, 3, 1))
  on.exit(graphics::par(op), add = TRUE)

  sizes <- x$cores$sizes
  nontriv <- sizes[sizes$size > 1, , drop = FALSE]
  singletons <- sum(sizes$count[sizes$size == 1])
  if (NROW(nontriv)) {
    heights <- rep(nontriv$size, nontriv$count)
    heights <- sort(heights, decreasing = TRUE)
    graphics::barplot(
      heights,
      log = if (max(heights) / max(min(heights), 1) > 50) "y" else "",
      col = "gray55", border = NA,
      xlab = "simultaneous cores", ylab = "core size (rows)",
      main = sprintf(
        "%d cores > 1 element; %d recursive rows",
        sum(nontriv$count), singletons
      ),
      cex.main = 0.85
    )
  } else {
    graphics::plot.new()
    graphics::title(main = "no simultaneous cores: fully recursive system",
      cex.main = 0.85
    )
  }

  top <- x$cores$top
  if (NROW(top)) {
    eqs <- top$eqs[[1]]
    shown <- utils::head(eqs, 12L)
    other <- sum(eqs$count) - sum(shown$count)
    heights <- shown$count
    labels <- shown$name
    cols <- rep("gray35", length(heights))
    if (other > 0) {
      heights <- c(heights, other)
      labels <- c(labels, sprintf("(+%d eqs)", NROW(eqs) - NROW(shown)))
      cols <- c(cols, "gray75")
    }
    graphics::barplot(
      rev(heights),
      names.arg = rev(labels),
      horiz = TRUE, las = 1, col = rev(cols), border = NA,
      cex.names = 0.65,
      xlab = "rows in the largest core",
      main = sprintf("largest core: %d rows", top$size[[1]]),
      cex.main = 0.85
    )
  } else {
    graphics::plot.new()
    graphics::title(main = "no core composition recorded", cex.main = 0.85)
  }
  invisible(NULL)
}
