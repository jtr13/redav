#' Create a missing value plot in the style of `extracat::visna()` (no longer available on CRAN)
#'
#'
#' @param x data.frame or object that can be coerced to one
#' 
#' @param num_char numeric number of characters per column name (to prevent overlapping labels), uses base R \code{abbreviate()} to shorten. Note that this value will not be adhered to if doing so creates duplicate column names.
#' 
#' @param max_rows numeric maximum number of rows (missing patterns) to display
#' 
#' @param max_cols numeric maxium number of columns to display
#' 
#' @param percent logical if \code{TRUE}, % of rows and columns are shown, if \code{FALSE}, actual numbers of rows and columns are shown (defaults to \code{TRUE})
#'
#' @export
#'
plot_missing <- function(x, percent = TRUE, num_char = 100, 
                         max_rows = NULL, max_cols = NULL,
                         long_name_cols= NULL) {	

  na_count_all <- data.frame(is.na(x)) %>%	
    dplyr::group_by_all() %>%	
    dplyr::count(name = "count", sort = TRUE) %>%	
    dplyr::ungroup() %>%	
    tibble::rownames_to_column("pattern")	
  
  if (!is.null(max_rows)) na_count_all <- na_count_all %>% 
      dplyr::filter(as.numeric(pattern) <= max_rows)
  
  na_count_all <- na_count_all %>% 
    dplyr::mutate(pattern = factor(.data$pattern, levels = nrow(na_count_all):1))

  
  # count the number of columns with missing values; will be used later to determine if there's a "none missing" pattern	
  na_count_all <- na_count_all %>% 	
    dplyr::rowwise() %>%	
    dplyr::mutate(num_missing_cols = sum(dplyr::c_across(where(is.logical))))	
  
  # data frame for missing patterns bar chart	
  na_count_by_pattern <- na_count_all[,c("pattern", "count", "num_missing_cols")]
  na_count_by_pattern$none_missing <- ifelse(na_count_by_pattern$num_missing_cols == 0, TRUE, FALSE)

  # data frame for missing by column bar chart	
  na_count_by_column <- data.frame(is.na(x)) %>%	
    colSums() %>% 	
    sort(decreasing = TRUE) %>% 	
    tibble::enframe(name = "var", value = "count")	
  
  if (!is.null(max_cols)) na_count_by_column <- na_count_by_column %>%
    dplyr::slice(1:max_cols)
  
  # tidy and sort na_count_all by column counts	
  na_count_all_tidy <- na_count_all %>% 	
    tidyr::pivot_longer(where(is.logical), names_to = "variable") %>%	
    dplyr::filter(variable %in% na_count_by_column$var) %>%
    dplyr::mutate(variable = factor(.data$variable, levels = na_count_by_column$var))  %>% 	
    dplyr::mutate(none_missing = ifelse(.data$num_missing_cols == 0, TRUE, FALSE))
  
  # calculate the % of data shown
  perc_shown <- 100*sum(na_count_by_pattern$count)/nrow(x)
  
  # main plot
  main_plot <- na_count_all_tidy %>%
    ggplot2::ggplot(ggplot2::aes(.data$variable, .data$pattern, fill = factor(.data$value), alpha = .data$none_missing)) +	
    ggplot2::geom_tile(color = "white") +	
    ggplot2::scale_x_discrete(labels = abbreviate(levels(na_count_all_tidy$variable), num_char)) +
    ggplot2::scale_fill_manual(values = c("grey70", "mediumpurple")) +	
    ggplot2::scale_alpha_manual(values = c(.7, 1)) +	
    ggplot2::labs(y = "missing pattern",
                  caption = paste0("These missing patterns represent ",
                                   round(perc_shown, 1), "% of the rows.")) +	
    ggplot2::guides(fill = "none", alpha = "none") +	
    ggplot2::theme_classic(12)
  
  # check for "none missing" pattern
  none_missing_pattern <- na_count_by_pattern %>%
    dplyr::filter(.data$none_missing) %>% dplyr::pull(.data$pattern)
  
  if (length(none_missing_pattern) > 0) {	
    column_count <- min(c(ncol(na_count_all)-2, max_cols+1))
    main_plot <- main_plot +	
      ggplot2::annotate("text", x = column_count/2,	
               y = nrow(na_count_all) + 1 - as.numeric(as.character(none_missing_pattern)),	
               label = "complete cases")	
  }	
  
  # margin plots
  
  denom <- ifelse(percent, nrow(x)/100, 1)
  
  # top
  missing_by_column_plot <- 
    na_count_by_column %>% 
    dplyr::mutate(var = abbreviate(var, num_char)) %>% 
    ggplot2::ggplot(ggplot2::aes(forcats::fct_inorder(.data$var), .data$count/denom)) +	
    ggplot2::geom_col(fill = "cornflowerblue", alpha = .7) +
    ggplot2::scale_y_continuous(expand = c(0, 0), n.breaks = 3) +	
    ggplot2::labs(x = "",
                  y = ifelse(percent, "% rows \n missing:", "num rows \n missing:")) +	
    ggplot2::theme_linedraw(12) + 	
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),	
          panel.grid.minor.x = ggplot2::element_blank(),
          )	
  # right
  missing_by_pattern_plot <- 
    ggplot2::ggplot(na_count_by_pattern, ggplot2::aes(.data$pattern, .data$count/denom, alpha = .data$none_missing)) +
    ggplot2::geom_col(fill = "cornflowerblue") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = c(0, 0), n.breaks = 3) +
    ggplot2::scale_alpha_manual(values = c(.7, 1)) +
    ggplot2::labs(x = "", y = ifelse(percent, "% rows", "row count")) +
    ggplot2::guides(alpha = "none") +
    ggplot2::theme_linedraw(12) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), 
          panel.grid.minor.y = ggplot2::element_blank())
  
  if (percent) {	
    missing_by_column_plot <- missing_by_column_plot +
      ggplot2::scale_y_continuous(expand = c(0, 0), n.breaks = 5,
                         limits = c(0, 100))	
    missing_by_pattern_plot <- missing_by_pattern_plot +
      ggplot2::scale_y_continuous(expand = c(0, 0), n.breaks = 5,
                         limits = c(0, 100))	
  }	
  
  # Rotates the x-axis labels for both the Main and Top charts to 45 degrees to better display columns with long names.
  # For the Top chart, moves the x-axis from the bottom to the top.
  if (!is.null(long_name_cols) && long_name_cols==TRUE ) {
    main_plot <- main_plot +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    missing_by_column_plot <- missing_by_column_plot +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 0)) +
      ggplot2::scale_x_discrete(position = "top")
  }
  
  missing_by_column_plot + patchwork::plot_spacer() + 	
    main_plot + missing_by_pattern_plot + 	
    patchwork::plot_layout(widths = c(4, 1), heights = c(1, 4))
}

utils::globalVariables("where")