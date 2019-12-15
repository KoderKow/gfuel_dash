## theme_kow ----
theme_kow <- function(base_size = 12, base_family = "sans") {
  
  kow_black <- "#1A1A1A"
  kow_light_black <- "#404040"
  kow_light_grey <- "#d9d9d9"
  
  update_geom(c("col", "bar", "boxplot", "point"))
  
  base_kow_theme(base_size = base_size, base_family = base_family) +
    
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "plain",
        size = ggplot2::rel(20 / 12),
        hjust = 0,
        colour = kow_black
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0,
        size = ggplot2::rel(1),
        face = "plain",
        colour = kow_black
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0,
        size = ggplot2::rel(1),
        face = "plain",
        colour = kow_black
      ),
      panel.background = ggplot2::element_rect(
        fill = NA,
        colour = NA
      ),
      panel.border = ggplot2::element_rect(
        fill = NA,
        colour = NA
      ),
      strip.text = ggplot2::element_text(
        hjust = 0,
        size = ggplot2::rel(1),
        colour = kow_black,
        face = "plain"
      ),
      strip.background = ggplot2::element_rect(
        colour = NA,
        fill = NA
      ),
      axis.title = ggplot2::element_text(
        face = "plain",
        colour = kow_black,
        size = ggplot2::rel(1)
      ),
      axis.text = ggplot2::element_text(
        face = "plain",
        colour = kow_black,
        size = ggplot2::rel(1)
      ),
      axis.line = ggplot2::element_line(colour = kow_black),
      axis.line.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = kow_light_grey),
      panel.grid.minor = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(colour = NA),
      legend.text = ggplot2::element_text(
        size = ggplot2::rel(1),
        colour = kow_black
      ),
      legend.title = ggplot2::element_text(
        size = ggplot2::rel(1),
        colour = kow_black,
        face = "plain"
      ),
      legend.key = ggplot2::element_rect(
        fill = "white",
        colour = "white"
      ),
      legend.position = "right",
      legend.direction = "vertical",
      legend.box.background = ggplot2::element_blank()
    )
}

## kow_colors ----
kow_colors <- function() {
  list(
    theme = c(
      sunset_orange = "#f25f5c",
      lapis_lazuli = "#247ba0",
      green_sheen = "#70c1b3",
      mustard = "#ffe066",
      deep_koamaru = "#3b3561",
      dark_liver = "#50514f",
      sweet_brown = "#ac3931",
      slate_blue = "#6369d1",
      dark_sea_green = "#83b692",
      flax = "#e9d985",
      ucla_blue = "#586994",
      tulip = "#fe938c",
      livid = "#6699cc",
      nadeshiko_pink = "#edadc7",
      paynes_grey = "#546a76",
      mindaro = "#dbfe87"
    ),
    text_colors = c(
      black = "#1A1A1A",
      light_black = "#303030"
    )
  )
}

## scale_color_kow ----
scale_color_kow <- function(...) {
  ggplot2::discrete_scale("colour", "kow", kow_pallette(), ...)
}

## scale_fill_kow ----
scale_fill_kow <- function(...) {
  ggplot2::discrete_scale("fill", "kow", kow_pallette(), ...)
}

## kow_pallette ----
kow_pallette <- function() {
  values <- kow_colors()$theme
  names(values) <- NULL
  f <- scales::manual_pal(values)
  attr(f, "max_n") <- length(values)
  f
}

## update_geom ----
update_geom <- function(geom) {
  purrr::map(geom, ~ {
    update_geom_defaults(
      geom = .x,
      new = if (.x == "point") {
        list(
          alpha = 0.9,
          size = 3
        )
      } else {
        list(
          color = "#1A1A1A",
          alpha = 0.9
        )
      }
    )
  })
}

## base_kow_theme ----
base_kow_theme <- function(base_size = 12, base_family = "") {
  thm <- ggplot2::theme_grey(base_size = base_size, base_family = base_family)
  
  for (i in names(thm)) {
    if ("colour" %in% names(thm[[i]])) {
      thm[[i]]["colour"] <- list(NULL)
    }
    
    if ("fill" %in% names(thm[[i]])) {
      thm[[i]]["fill"] <- list(NULL)
    }
  }
  
  thm + ggplot2::theme(
    panel.border = ggplot2::element_rect(fill = NA),
    legend.background = ggplot2::element_rect(colour = NA),
    line = ggplot2::element_line(colour = "black"),
    rect = ggplot2::element_rect(
      fill = "white",
      colour = "black"
    ),
    text = ggplot2::element_text(colour = "black")
  )
}

## snake_to ----
snake_to <- function(object, format = "title", acronyms = NULL, names_only = FALSE, ggplot_title = FALSE) {
  
  if (names_only == TRUE & object_check(object, "ggplot")) {
    stop("names_only = TRUE and an object of class 'ggplot' cannot be used together.")
  }
  
  if (object_check(object, "data.frame")) {
    names_cleaned <- names(object) %>%
      stringr::str_replace_all(
        pattern = "_",
        replacement = " "
      )
    
  } else if (object_check(object, "ggplot")) {
    # names_cleaned <- c(object$labels$x, object$labels$y) %>%
    #   stringr::str_replace_all("_", " ")
    list_names <- names(object$labels)
    
    names_cleaned <- object$labels %>%
      purrr::map(~ {
        stringr::str_replace_all(
          string = .x,
          pattern = "_",
          replacement = " "
        )
      })
    
  } else if (object_check(object, "character_vector")) {
    names_cleaned <- object %>%
      stringr::str_replace_all(
        pattern = "_",
        replacement = " "
      )
    
  } else {
    stop("Object's class is not supported. Object's class needs to be a data.frame, ggplot or a character vector.")
  }
  
  if (format == "title") {
    names_cleaned <- names_cleaned %>%
      stringr::str_to_title()
  } else if (format == "lower") {
    names_cleaned <- names_cleaned %>%
      stringr::str_to_lower()
  } else if (format == "normal") {
    names_cleaned
  } else if (format == "sentence") {
    names_cleaned <- names_cleaned %>%
      stringr::str_to_sentence()
  } else if (format == "upper") {
    names_cleaned <- names_cleaned %>%
      stringr::str_to_upper()
  } else {
    stop("Please choose between c(\"title\", \"sentence\", \"lower\", \"upper\", \"normal\") for the format.")
  }
  
  if (!is.null(acronyms)) {
    words_to_cap <- stringr::str_c(
      "\\b",
      stringr::str_to_title(acronyms),
      "\\b|\\b",
      stringr::str_to_lower(acronyms),
      "\\b|\\b",
      stringr::str_to_upper(acronyms),
      "\\b",
      collapse = "|"
    )
    
    names_cleaned <- names_cleaned %>%
      stringr::str_replace_all(
        pattern = words_to_cap,
        replacement = stringr::str_to_upper
      )
  }
  
  if (names_only | object_check(object, "character_vector")) {
    names_cleaned
  } else if (object_check(object, "data.frame")) {
    names(object) <- names_cleaned
    object
  } else if (object_check(object, "ggplot")) {
    object$labels <- names_cleaned %>%
      purrr::map(~ .x) %>%
      purrr::set_names(list_names)
    
    if (ggplot_title) {
      object$labels$title <- paste(
        "Relation Between",
        object$labels$x,
        "and",
        object$labels$y
      )
    }
    object
  }
}

object_check <- function(object, class) {
  if (class == "character_vector") {
    purrr::is_vector(object) & purrr::is_character(object)
  } else {
    any(class(object) == class)
  }
}