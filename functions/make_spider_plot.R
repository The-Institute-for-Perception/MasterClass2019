### This function creates a spider plot from descriptive data

make_spider_plot <- function(data, title) {
    plot_data <- data %>%
       gather(key = Attribute, value = Score) %>%
       mutate(Score = scales::rescale(Score)) %>%
       arrange(Attribute)
   #browser()
   ggplot(plot_data, aes(x = Attribute, y = Score)) +
      geom_polygon(#aes(group = !!sym(grouping_var), color = !!sym(grouping_var)), 
                   fill = NA, size = 1, show.legend = FALSE, aes(group = 1, color = 1)) +
      geom_line(#aes(group = !!sym(grouping_var), color = !!sym(grouping_var)), 
         size = 1, aes(group = 1, color = 1)) +
      theme(strip.text.x = element_text(size = rel(0.8)),
            axis.text.x = element_text(size = rel(1.2), face = "bold"),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_line("grey", 0.25, "solid"),
            panel.grid.major.x = element_line("grey", 0.25, "solid"),
            panel.grid.minor.x = element_blank(),
            panel.background = element_blank(),
            legend.background = element_blank(),
            legend.key = element_blank(),
            panel.border = element_blank(),
            axis.line.y = element_blank(),
            legend.text = element_text(size = rel(1)),
            legend.position = "none") +
      scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) +
      xlab("") + ylab("") +
      #guides(color = guide_legend(ncol=1)) +
      coord_radar()   +
      ggtitle(title)
}

coord_radar <- function (theta = "x", start = 0, direction = 1) 
{
   theta <- match.arg(theta, c("x", "y"))
   r <- if (theta == "x") 
      "y"
   else "x"
   
   #dirty
   rename_data <- function(coord, data) {
      if (coord$theta == "y") {
         plyr::rename(data, c("y" = "theta", "x" = "r"), warn_missing = FALSE)
      } else {
         plyr::rename(data, c("y" = "r", "x" = "theta"), warn_missing = FALSE)
      }
   }
   theta_rescale <- function(coord, x, scale_details) {
      rotate <- function(x) (x + coord$start) %% (2 * pi) * coord$direction
      rotate(scales::rescale(x, c(0, 2 * pi), scale_details$theta.range))
   }
   
   r_rescale <- function(coord, x, scale_details) {
      scales::rescale(x, c(0, 0.4), scale_details$r.range)
   }
   
   ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
           direction = sign(direction),
           is_linear = function(coord) TRUE,
           render_bg = function(self, scale_details, theme) {
              scale_details <- rename_data(self, scale_details)
              
              theta <- if (length(scale_details$theta.major) > 0)
                 theta_rescale(self, scale_details$theta.major, scale_details)
              thetamin <- if (length(scale_details$theta.minor) > 0)
                 theta_rescale(self, scale_details$theta.minor, scale_details)
              thetafine <- seq(0, 2 * pi, length.out = 100)
              
              rfine <- c(r_rescale(self, scale_details$r.major, scale_details))
              
              # This gets the proper theme element for theta and r grid lines:
              #   panel.grid.major.x or .y
              majortheta <- paste("panel.grid.major.", self$theta, sep = "")
              minortheta <- paste("panel.grid.minor.", self$theta, sep = "")
              majorr     <- paste("panel.grid.major.", self$r,     sep = "")
              
              ggplot2:::ggname("grill", grid::grobTree(
                 ggplot2:::element_render(theme, "panel.background"),
                 if (length(theta) > 0) ggplot2:::element_render(
                    theme, majortheta, name = "angle",
                    x = c(rbind(0, 0.45 * sin(theta))) + 0.5,
                    y = c(rbind(0, 0.45 * cos(theta))) + 0.5,
                    id.lengths = rep(2, length(theta)),
                    default.units = "native"
                 ),
                 if (length(thetamin) > 0) ggplot2:::element_render(
                    theme, minortheta, name = "angle",
                    x = c(rbind(0, 0.45 * sin(thetamin))) + 0.5,
                    y = c(rbind(0, 0.45 * cos(thetamin))) + 0.5,
                    id.lengths = rep(2, length(thetamin)),
                    default.units = "native"
                 ),
                 
                 ggplot2:::element_render(
                    theme, majorr, name = "radius",
                    x = rep(rfine, each = length(thetafine)) * sin(thetafine) + 0.5,
                    y = rep(rfine, each = length(thetafine)) * cos(thetafine) + 0.5,
                    id.lengths = rep(length(thetafine), length(rfine)),
                    default.units = "native"
                 )
              ))
           })
}
