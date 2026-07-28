# align_move_fixed() ----------------------------------------------------
#
# A drop-in replacement for moveVis::align_move() (github.com/16EAGLE/moveVis,
# R/align_move.R as currently released). Same inputs, same output — a move2
# object with every individual aligned to a common time grid, ready for
# moveVis::frames_spatial(). Built from only exported move2/sf/s2/units
# functions (no internal moveVis::: calls), so it stays usable even though
# it doesn't share code with the installed package.
#
# Why this exists: the released align_move() has two confirmed bugs (as of
# mid-2026, neither yet fixed upstream), both fixed below at the point they
# applied in the original:
#
#   1. github.com/16EAGLE/moveVis/issues/131 (open) — errors on a move2
#      object carrying unused track-ID factor levels (e.g. after
#      dplyr::filter()), because split() defaults to drop = FALSE.
#   2. An internal normalized-distance calculation divides by
#      diff(range(timestamps), units = "s") — base R's diff() has no
#      `units` argument, so for any track spanning more than a day the
#      denominator is silently computed in days, not seconds. Every
#      individual's interpolated positions then collapse onto its own last
#      recorded fix instead of moving between fixes.
#
# Confirmed working on a 3-individual, 2-week demo subset of the Athi-
# Kaputiei wildebeest dataset, 2026-06-27 (see 03_trajectory_creation_
# visualization.Rmd, Section 4). If you scale this up to the full dataset,
# rerun the sanity check in that Rmd (distinct-coordinate count per
# individual after alignment) before trusting it further.
#
# Used in: 03_trajectory_creation_visualization.Rmd, Section 4.5–4.6.

align_move_fixed <- function(m, res = "minimum", start_end_time = NULL, fill_na_values = TRUE) {

  # fix #1 (moveVis#131): dplyr::filter() drops unused tracks' rows but not
  # the track-ID column's factor *levels* for those tracks. split() below
  # defaults to drop = FALSE, so stale levels become spurious <2-location
  # "tracks" and trip the check just below. Drop them first.
  trk_col <- move2::mt_track_id_column(m)
  if (is.factor(m[[trk_col]])) m[[trk_col]] <- droplevels(m[[trk_col]])

  m_tracks <- split(m, move2::mt_track_id(m))
  m_length <- sapply(m_tracks, nrow)
  if (any(m_length < 2)) {
    stop("At least one individual track has fewer than 2 locations after subsetting.")
  }

  # resolve `res` to a units(time) object, same logic as the original
  if (inherits(res, "character")) {
    res <- switch(res,
      "minimum" = , "min"    = min(move2::mt_time_lags(m), na.rm = TRUE),
      "maximum" = , "max"    = max(move2::mt_time_lags(m), na.rm = TRUE),
      "mean"    = round(mean(move2::mt_time_lags(m), na.rm = TRUE)),
      "median"  = round(stats::median(move2::mt_time_lags(m), na.rm = TRUE))
    )
  }

  if (is.null(start_end_time)) start_end_time <- range(move2::mt_time(m), na.rm = TRUE)

  # this looks redundant with the `interpolated` column built below, but it
  # isn't: colnames(m) is used downstream (after cbind-ing the aligned rows)
  # to select/reorder columns via match(colnames(m), colnames(m_aligned)) —
  # any column not in colnames(m) gets silently dropped by that step. Without
  # "interpolated" already present on `m` here, that drop empties the final
  # filter (`m_aligned[m_aligned$interpolated, ]` becomes `m_aligned[NULL, ]`,
  # i.e. zero rows) further down. Omitting this line was an earlier mistake
  # in this patch, caught when WB_aligned came back with 0 rows per track.
  m$interpolated <- FALSE

  m_sf_points <- lapply(m_tracks, function(.m) { class(.m) <- setdiff(class(.m), "move2"); .m })
  coords      <- lapply(m_sf_points, sf::st_coordinates)
  m_sf_lines  <- lapply(coords, function(x)
    sf::st_sf(geometry = sf::st_sfc(sf::st_linestring(x), crs = sf::st_crs(m))))

  times_target_full <- seq.POSIXt(start_end_time[1], start_end_time[2], by = units::set_units(res, "s"))
  times_target <- lapply(m_tracks, function(x) {
    ts <- move2::mt_time(x)
    times_target_full[times_target_full >= min(ts) & times_target_full <= max(ts)]
  })

  # *** FIX *** (bug #2 above): the released align_move() divides by
  # diff(range(mt_time(...)), units = "s") here, which silently ignores
  # `units =` (base diff() has no such argument) and returns days for any
  # track spanning more than a day. Using difftime() on both numerator AND
  # denominator keeps both sides in seconds, so nd stays inside [0, 1].
  nd <- lapply(seq_along(m_tracks), function(i) {
    ts <- move2::mt_time(m_tracks[[i]])
    as.numeric(difftime(times_target[[i]], min(ts), units = "secs")) /
      as.numeric(difftime(max(ts), min(ts), units = "secs"))
  })

  if (all(sf::st_is_longlat(m), sf::sf_use_s2())) {
    m_aligned <- mapply(.m = m_sf_lines, .nd = nd, function(.m, .nd)
      sf::st_as_sf(s2::s2_interpolate_normalized(sf::st_geometry(.m), .nd)), SIMPLIFY = TRUE)
  } else {
    m_aligned <- mapply(.m = m_sf_lines, .nd = nd, function(.m, .nd)
      sf::st_line_interpolate(sf::st_geometry(.m), .nd), SIMPLIFY = TRUE)
  }

  m_aligned <- lapply(seq_along(m_tracks), function(i) sf::st_sf(
    interpolated = c(rep(FALSE, nrow(m_tracks[[i]])), rep(TRUE, length(m_aligned[[i]]))),
    track     = names(m_tracks)[i],
    timestamp = c(move2::mt_time(m_tracks[[i]]), times_target[[i]]),
    geometry  = c(sf::st_geometry(m_tracks[[i]]), m_aligned[[i]])
  ))
  m_aligned <- do.call(rbind, m_aligned)
  colnames(m_aligned) <- c("interpolated", move2::mt_track_id_column(m), move2::mt_time_column(m), attr(m, "sf_column"))

  names_attr <- names(m)[!(names(m) %in% names(m_aligned))]
  df_attr <- data.frame(matrix(NA, nrow(m_aligned), length(names_attr)))
  colnames(df_attr) <- names_attr
  m_aligned <- cbind(m_aligned, df_attr)
  m_aligned <- m_aligned[, match(colnames(m), colnames(m_aligned))]

  m_sf <- m
  class(m_sf) <- setdiff(class(m_sf), "move2")
  m_aligned <- rbind(m_sf, m_aligned)

  m_aligned <- move2::mt_as_move2(m_aligned, time_column = move2::mt_time_column(m), track_id_column = move2::mt_track_id_column(m))
  move2::mt_track_id(m_aligned) <- as.factor(move2::mt_track_id(m_aligned))
  m_aligned <- move2::mt_set_track_data(m_aligned, move2::mt_track_data(m))

  m_aligned <- m_aligned[order(m_aligned$timestamp), ]
  m_aligned <- m_aligned[order(move2::mt_track_id(m_aligned)), ]

  if (isTRUE(fill_na_values) && length(names_attr) > 0) {
    m_aligned <- do.call(rbind, lapply(split(m_aligned, move2::mt_track_id(m_aligned)), function(m_track) {
      for (col in names_attr) {
        this_attr <- m_track[[col]]
        m_track[[col]] <- sapply(seq_along(this_attr), function(i) {
          if (!is.na(this_attr[i])) return(this_attr[i])
          left  <- if (i == 1) NULL else seq_len(i - 1)
          right <- if (i == length(this_attr)) NULL else (i + 1):length(this_attr)
          cand <- c(if (!is.null(left)) left[which(!is.na(this_attr[left]))[1]],
                    if (!is.null(right)) right[which(!is.na(this_attr[right]))[1]])
          cand_diff <- abs(sapply(cand, function(j)
            difftime(m_track[[move2::mt_time_column(m_track)]][j],
                      m_track[[move2::mt_time_column(m_track)]][i], units = "secs")))
          this_attr[cand[which.min(cand_diff)]]
        })
      }
      m_track
    }))
  }

  m_aligned <- m_aligned[m_aligned$interpolated, ]
  m_aligned$interpolated <- NULL
  m_aligned
}
