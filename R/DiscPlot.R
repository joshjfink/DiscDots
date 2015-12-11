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

#' Foundation Theme
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

#' Theme with Google Docs Chart defaults
#'
#' Theme similar to the default look of charts in Google Docs.
#'
#' @inheritParams ggplot2::theme_grey
#' @export
#' @family themes gdocs
#' @examples
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' (d <- qplot(carat, price, data=dsamp, colour=clarity)
#'  + theme_gdocs()
#'  + ggtitle("Diamonds")
#'  + scale_color_gdocs())
theme_gdocs <- function(base_size=12, base_family="sans") {
  theme_foundation(base_size = base_size,
                   base_family = base_family) +
    theme(rect = element_rect(colour = "black", fill = "white"),
          line = element_line(colour = "black"),
          text = element_text(colour = "black"),
          plot.title = element_text(face = "bold",
                                    # 16 pt, bold, align left
                                    size = rel(1.33), hjust = 0),
          panel.background = element_rect(fill = NA, colour = NA),
          panel.border = element_rect(fill = NA, colour = NA),
          # 12 pt
          axis.title = element_text(face = "italic"),
          # 12 pt
          axis.text = element_text(),
          axis.line = element_line(colour="black"),
          axis.ticks = element_blank(),
          panel.grid.major = element_line(colour = "#CCCCCC"),
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = NA),
          legend.position = "right",
          legend.direction = "vertical")
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
    geom_point(aes(color=factor(value)), size=10) +
    theme_gdocs()+ scale_color_manual(values=sgcols)+
    theme( axis.text.x=element_blank(),panel.grid.major.y = element_blank(), axis.line = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank()) +
    coord_flip()+theme(axis.line.y=element_blank(), axis.ticks.y=element_blank()) +
    theme( axis.text.x=element_blank() ,panel.grid.major.y = element_blank()) +theme(legend.position="none", axis.line = element_blank())+theme( axis.text.x=element_blank() ,panel.grid.major.y = element_blank()) +theme(legend.position="none", axis.line = element_blank())+ labs(list(title = "Title", x = "", y = "")))
}
