#' Home module UI function
#'
#' @noRd
mod_home_UI <- function(id, i18n, .tr){

  ## From https://shiny.rstudio.com/articles/modules.html
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  ##
  ## UI Elements ######
  ##

  ## + Hero section =======
  ## image first on mobile
  hero_img <- div(
    class = "col-md-6 order-1 order-md-2 text-center",
    tags$img(
      src = "assets/landing-img.jpg",
      class = "img-fluid hero-image shadow-sm"
    ),
    style = "margin-bottom: 24px;"
  )

  hero_txt <- div(
    class = "col-md-6 order-2 order-md-1",
    p(class = "hero-title", i18n$t(.tr$hero_title)),
    p(class = "hero-subtitle mt-3", i18n$t(.tr$hero_txt)),
    br(),
    ## ACTION BUTTON
    actionButton(
      inputId = ns("to_tool"),
      label = i18n$t(.tr$hero_btn),
      class = "btn btn-primary btn-lg px-4"
    )
  )


  ## + Features section =======
  card1 <- card(
    class = "feature-card shadow-sm",
    card_body(
      h3("Arena Analytics", style = "font-weight:700;"),
      p(i18n$t(.tr$feat1_p1)),
      p(
        i18n$t(.tr$feat1_p2),
        tags$a(
          href = "https://github.com/openforis/arenalytics",
          target = "_blank",
          tags$span("Github", bsicons::bs_icon("github"))
        )
      ),
      p(
        i18n$t(.tr$feat1_p3), HTML("&nbsp;"),
        actionButton(inputId = ns("to_tool2"), label = i18n$t(.tr$feat1_btn))
      )
    )
  )

  card2 <- card(
    class = "feature-card shadow-sm",
    card_body(
      h3("OpenForis Arena", style = "font-weight:700;"),
      p(
      i18n$t(.tr$feat2_p1),
      tags$a(
        href = "https://www.openforis.org/arena/",
        target = "_blank",
        span("OpenForis Arena", tags$img(src = "assets/logo-arena.png", height = "20px"), ".")
        )
      ),
      p(i18n$t(.tr$feat2_p2)),
      p(i18n$t(.tr$feat2_p3))
    )
  )

  card3 <- card(
    class = "feature-card shadow-sm",
    card_body(
      h3(i18n$t(.tr$feat3_title), style = "font-weight:700;"),
      p(i18n$t(.tr$feat3_p1)),
      p(i18n$t(.tr$feat3_p2)),
      p(
        i18n$t(.tr$feat3_p3), HTML("&nbsp;"),
        actionButton(inputId = ns("to_about"), label = i18n$t(.tr$feat3_btn))
      )
    )
  )


  ##
  ## UI elements wrapped in a tagList() function ######
  ##

  tagList(

    # HERO SECTION
    div(
      class = "container hero-section",
      div(
        class = "row align-items-center",
        hero_img, hero_txt
      )
    ),

    # FEAT. SECTION
    div(
      class = "container section-padding",
      div(
        class = "row g-4",
        div(class = "col-md-4", card1),
        div(class = "col-md-4", card2),
        div(class = "col-md-4", card3)
      )
    ),

    br(),

  ) ## END tagList

} ## END module UI function
