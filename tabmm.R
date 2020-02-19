tbmm <- function(dfx){
    tabx <- NULL
    lgt <- dim(dfx)[1]
    for (i in 1:dim(dfx)[2]) {
      nn <- str_split(names(dfx)[i], "_")[[1]][2]
      varx <- na.omit(dfx[, i])
      lgx <- length(varx)
      pcx <- signif(lgx * 100 / lgt, 2)
      lgg <- paste0(lgx, " (", pcx, ")")
      moy <- signif(mean(varx, na.rm = TRUE), 3)
      ss <- signif(sd(varx, na.rm = TRUE), 3)
      mm <- paste0(moy, " ± ", ss)
      med <- quantile(varx, na.rm = TRUE)[3]
      inf <- quantile(varx, na.rm = TRUE)[2]
      sup <- quantile(varx, na.rm = TRUE)[4]
      md <- paste0(med, " [ ", inf, " ; ", sup, " ]")
      ligx <- c(nn, lgg, mm, md)
      tabx <- rbind(tabx, ligx)
    }
    kable(
      tabx,
      col.names = c(" ", "taux remplissage", "moyenne", "médiane"),
      caption = "Scores - Sortie",
      row.names = FALSE
    ) %>%
      kable_styling(bootstrap_options = "striped", full_width = FALSE)
  }