#' @title Color Palettes to Use
#' @description Frequently used color palettes.
#' @param palette The color palette to use. Options are gigg.
#' @param type Discreet or Fade. Discreet is useful for categorical data. Fade is useful for cardinal data.
#' @return Returns a dataframe with colorName, hex, and rgb.
#' @export
#'
colorsToUse <- function(palette, type = "Discreet"){
  palette <- tolower(palette)

  if(palette == "gigg"){
    discreetColors <- data.frame(colorName = c("fireEngineRed", "deepKoamaru", "persianGreen"
                                        , "maximumYellowRed", "graniteGray")
                          , hex = c("#D12229", "#28335C", "#00A499", "#F3C454"
                                    , "#63666A")
                          , rgb = c("rgb(209,34,41)", "rgb(40,51,92)", "rgb(0,164,153)"
                                    , "rgb(243,196,84)", "rgb(99,102,106)")
                          , stringsAsFactors = F
                          )
    fadeColors <- data.frame(colorName = c("persianGreen", "jungleGreen", "shinyShamrock"
                                           , "olivine", "darkKhaki", "maximumYellowRed"
                                           , "indianYellow", "tigersEye", "mediumVermilion"
                                           , "vermilion", "fireEngineRed")
                             , hex = c("#00A499", "#31AA8B", "#61B17D", "#92B770", "#C2BE62"
                                       , "#F3C454", "#ECA44B", "#E58343", "#DF633A"
                                       , "#D84232", "#D12229")
                             , rgb = c("rgb(0,164,153)", "rgb(49,170,139)", "rgb(97,177,125)"
                                       , "rgb(146,183,112)", "rgb(194,190,98)", "rgb(243,196,84)"
                                       , "rgb(236,164,75)", "rgb(229,131,67)", "rgb(223,99,58)"
                                       , "rgb(216,66,50)", "rgb(209,34,41)")
                             , stringsAsFactors = F
                             )

  }
  if(type == "Discreet"){
    return(discreetColors)
  }else{
    return(fadeColors)
  }

}
