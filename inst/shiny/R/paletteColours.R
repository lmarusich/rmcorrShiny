## Adding RColorBrewer here also because 'brewer.pal.info' is required to load
## the UI at this moment.
library("RColorBrewer")
library("pals")

# For the palette UI
library("shinyWidgets")

# Prepare the palette picker
colors_pal <- lapply(
  X = split(
    x = brewer.pal.info,
    f = factor(brewer.pal.info$category,
               labels = c("Diverging", "Qualitative", "Sequential"))
  ),
  FUN = rownames
)

colors_pal <- colors_pal[-2] #Drop Qualitative, second list element


#this is just the number of colors to show in the color picker - doesn't affect the plot
ncolors <- 12

#Color palettes from pals
colors_pal$Pals_Recommended <- c('coolwarm', 'parula', 'ocean.haline',
                                 'cubicl', 'kovesi.rainbow', 'ocean.phase', 'viridis')



# Get all colors given a palette name(s)
get_brewer_name <- function(name) {
  pals <- brewer.pal.info[rownames(brewer.pal.info) %in% name, ]
  res <- lapply(
    X = seq_len(nrow(pals)),
    FUN = function(i) {
      brewer.pal(n = pals$maxcolors[i], name = rownames(pals)[i])
    }
  )
  unlist(res)
}

background_pals <- sapply(unlist(colors_pal, use.names = FALSE),
                          get_brewer_name)

 background_pals$coolwarm        <- coolwarm(n = ncolors)
 background_pals$parula          <- parula(n = ncolors)
 background_pals$ocean.haline    <- ocean.haline(n = ncolors)
 background_pals$cubicl          <- cubicl(n = ncolors)
 background_pals$kovesi.rainbow  <- kovesi.rainbow(n = ncolors)
 background_pals$ocean.phase     <- ocean.phase(n = ncolors)
 background_pals$viridis         <- viridis(n = ncolors)

# Calc linear gradient for CSS
linear_gradient <- function(cols) {
  x <- round(seq(from = 0, to = 100, length.out = length(cols)+1))
  ind <- c(1, rep(seq_along(x)[-c(1, length(x))], each = 2), length(x))
  m <- matrix(data = paste0(x[ind], "%"), ncol = 2, byrow = TRUE)
  res <- lapply(
    X = seq_len(nrow(m)),
    FUN = function(i) {
      paste(paste(cols[i], m[i, 1]), paste(cols[i], m[i, 2]), sep = ", ")
    }
  )
  res <- unlist(res)
  res <- paste(res, collapse = ", ")
  paste0("linear-gradient(to right, ", res, ");")
}

background_pals <- unlist(lapply(X = background_pals, FUN = linear_gradient))


#length(colors_pal)
colortext_pals <- rep(c("white", "black", "white"),
                      times = sapply(colors_pal, length))
