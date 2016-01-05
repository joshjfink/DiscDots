##########
#' Create Gauge Matrix
#'
#' @param values Number of  filled in dots for each variable
#' @return matrix of 2's and 1's to be fed into plotting functions.
gen_mat <- function(values, var_names, ...) {
  n <- length(values)
  m <- matrix(nrow=n, ncol=max(values))
  for(i in 1:n){
    g_len <- max(values)
    t_ <- rep(1, values[i])
    f_ <- rep(0, g_len-values[i])
    ifelse(values[i]==g_len,
           m[i,] <- rep(2, values[i]),
           m[i,] <- factor(t(c(t_, f_)))
    )
  }
  return(m)
}


##########
#' Create Gauge Dataframe
#'
#' @param values Number of  filled in dots for each variable
#' @param var_names Names of variables
#' @return dataframe of 2's and 1's to be fed into plotting functions.
gen_df <- function(values, var_names,...) {
  m <- gen_mat(values, var_names)
  mdat <- data.frame(t(m))
  colnames(mdat) <- var_names
  mdat$var <- factor(1:nrow(mdat))
  mdat2 <- melt(mdat)
  return(mdat2)
}

#' Foundation Theme (from "ggthemes" package)
#'
#' This theme is designed to be a foundation from which to build new
#' themes, and not meant to be used directly. \code{theme_foundation}
#' is a complete theme with only minimal number of elements defined.
#'
#' It is easier to create new themes by extending this one rather
#' than \code{theme_gray} or \code{theme_bw}, because those themes
#' those themes define elements deep in the hierarchy.
#'
#' @inheritParams ggplot2::theme_grey
#'
#' @family themes
#' @export
theme_foundation <- function(base_size=12, base_family="") {
  thm <- theme_gray(base_size = base_size, base_family = base_family)
  for (i in names(thm)) {
    if ("colour" %in% names(thm[[i]])) {
      thm[[i]]["colour"] <- list(NULL)
    }
    if ("fill" %in% names(thm[[i]])) {
      thm[[i]]["fill"] <- list(NULL)
    }
  }
  thm
}

#' Default theme for DiscDots
#'
#' Theme generates the style of plot in the documentation/github example
#'
#' @inheritParams ggplot2::theme_grey
theme_ddots <- function(base_size=26, base_family="Josefin Sans") {
  theme_foundation(base_size = base_size,
                   base_family = base_family) +
    theme(
      rect = element_rect(colour = "black", fill = "white"),
      text = element_text(colour = "black"),
      panel.background = element_rect(fill = NA, colour = NA),
      panel.border = element_rect(fill = NA, colour = NA),
      axis.title = element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_text(face="bold", hjust = 0),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position="none")+ theme(axis.ticks.length = unit(.5, "cm"))
}

#' Google Docs color palette (discrete)
#'
#' Color palettes from Google Docs.
#'
#' @family colour gdocs
#' @export
#' @examples
#' library(scales)
#' show_col(gdocs_pal()(20))
gdocs_pal <- function() {
  manual_pal(unname(ggthemes_data$gdocs))
}


#' Google Docs color scales
#'
#' Color scales from Google Docs.
#'
#' @inheritParams ggplot2::scale_colour_hue
#' @family colour gdocs
#' @rdname scale_gdocs
#' @export
#' @seealso See \code{\link{theme_gdocs}} for examples.
scale_fill_gdocs <- function(...) {
  discrete_scale("fill", "gdocs", gdocs_pal(), ...)
}

#' @export
#' @rdname scale_gdocs
scale_colour_gdocs <- function(...) {
  discrete_scale("colour", "gdocs", gdocs_pal(), ...)
}

#' @export
#' @rdname scale_gdocs
scale_color_gdocs <- scale_colour_gdocs

##########
#' Create discrete dot plot
#'
#' @param values Number of  filled in dots for each variable
#' @return dot plot
DiscDots <- function(values, var_names, ...) {
  dot_df <- gen_df(values, var_names) 
  sgcols <- c("#E4E7EC", "#3A7AA6") 
  return(# Run Test 
    ggplot(data = dot_df, aes(x = reorder(variable, value), y = as.numeric(var)))+ 
    geom_point(aes(color=factor(value)), size=10) 
    + theme_ddots()
    + scale_color_manual(values=sgcols) 
    + coord_flip()
    )
}
