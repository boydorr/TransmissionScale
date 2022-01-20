# Functions for gif & bezier curves
# see ?ggforce::geom_bezier2 for more details
# from = data.frame with two columns (lat/long)
# to = data.frame with two columns (lat/long)
# frac is where you want the curve to peak along the line (i.e. 0.5 is halfway)
# transform is the function to create the distance scale (you can play with this scaling too)
# adapted from Dudas et al. Curonia
# https://github.com/evogytis/baltic/blob/master/curonia.ipynb
# Output = data.frame with to/mid/from
get_bezier_pts <- function(from, to, frac = 0.8, 
                           transform = function(x) sqrt(1 / x) * 0.5, 
                           min_dist = 50) {
  sign <- ifelse(from[, "long"] > to[, "long"], -1, 1)
  slope <- (to[, "lat"] - from[, "lat"]) / (to[, "long"] - from[, "long"])
  distance <- sqrt((to[, "lat"] - from[, "lat"])^2 + (to[, "long"] - from[, "long"])^2)
  height <- transform(distance)
  
  hdist <- sqrt(height^2 + (distance * frac)^2) # distance between desired height and point along line
  hdist[distance < min_dist] <- 0
    
  from <- data.frame(long = from[, "long"], lat = from[, "lat"], index = 1, group = 1:nrow(from))
  ctrl <- data.frame(
    long = from[, "long"] + hdist * cos(atan(height / distance / frac) + atan(slope)) * sign,
    lat = from[, "lat"] + hdist * sin(atan(height / distance / frac) + atan(slope)) * sign,
    index = 2, group = 1:nrow(from)
  ) # the magic control point
  to <- data.frame(long = to[, "long"], lat = to[, "lat"], index = 3, group = 1:nrow(from))
  
  df <- do.call(rbind, list(from, ctrl, to))
  df %>%
    group_by(group) %>%
    arrange(index) -> df
  return(df) # return coordinate df sorted
}

# Function for making gif
# fade out controls the # of steps 
make_gif <- function(bez_pts, 
                     top_chain_hist,
                     cols_chains, 
                     sd_shape, 
                     date_seqs = seq.Date(from = ymd("2002-01-01"), 
                                          to = ymd("2015-01-01"), 
                                          by = "month"), 
                     fade_out = 1) {
  
  step_alpha <- 0.9/fade_out
  bez_pts$alpha <- 0.9
  infectious <- exposed <- slice(bez_pts, 0)
  
  for (i in seq_len(length(date_seqs))) {
    infectious$alpha <- infectious$alpha - step_alpha
    infectious_now <- filter(bez_pts, date_infectious == date_seqs[i], 
                             index == 1)
    infectious <- bind_rows(infectious_now, 
                            filter(infectious, alpha != 0))
    exposed_now <- filter(bez_pts, id_progen %in% infectious_now$id_case)
    exposed <- bind_rows(filter(exposed, date_infectious >= date_seqs[i]), 
                         exposed_now)
    
    p_map <- ggplot() +
      geom_sf(data = sd_shape, fill = "black", color = "black") +
      # lines from infectious to exposed
      geom_bezier2(
        data = exposed_now, 
        aes(x = long, y = lat, group = group, 
            color = chain_id),
        n = 1000,
        alpha = 0.85, size = 1.2
      ) +
      # exposed case locs
      geom_point(data = filter(exposed, index == 1), 
                 aes(x = x_coord_to, y = y_coord_to, color = chain_id, 
                     shape = is.na(id_progen)), fill = "black", size = 2) + 
      # infectious case locs
      geom_point(data = infectious, 
                 aes(x = x_coord_to, y = y_coord_to, fill = chain_id, 
                     color = chain_id, 
                     shape = is.na(id_progen), 
                     size = is.na(id_progen), alpha = alpha)) + 
      scale_shape_manual(values = c(21, 22), guide = "none") +
      scale_size_manual(values = c(2, 3.5), guide = "none") +
      scale_color_manual(values = cols_chains, guide = "none", 
                         aesthetics = c("color", "fill")) +
      scale_alpha_identity() +
      theme_map() + annotation_scale(width_hint = 0.3)
    
    p_ts <- 
      ggplot(top_chain_hist) +
      geom_col(aes(x = month, y = cases, fill = chain_id), 
               position = position_stack(reverse = TRUE)) +
      scale_x_date(breaks = date_seqs[i], date_labels = "%B %Y") + 
      scale_fill_manual(values = cols_chains, guide = "none") +
      geom_vline(xintercept = date_seqs[i]) +
      labs(x = "", y = "Number of cases") +
      cowplot::theme_minimal_hgrid() 
    
    p <- p_ts + p_map + plot_layout(heights = c(1, 3))
    print(p)
    ani.pause()
    
  }
}

