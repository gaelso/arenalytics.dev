#' Home module UI function
#'
#' @noRd
mod_home_UI <- function(id, i18n){

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
    h5(class = "hero-title", i18n$t("A Smarter Way to Explore Data")),
    p(
      class = "hero-subtitle mt-3",
      i18n$t("Transform complex data into clear insights with a beautiful, intuitive interface.")
    ),
    br(),
    ## ACTION BUTTON
    actionButton(
      inputId = ns("to_tool"),
      label = i18n$t("Get Started"),
      class = "btn btn-primary btn-lg px-4"
    )
  )


  ## + Features section =======
  card1 <- card(
    class = "feature-card shadow-sm",
    card_body(
      h4("Fast & Interactive"),
      p("Real-time data updates and dynamic visualizations.")
    )
  )

  card2 <- card(
    class = "feature-card shadow-sm",
    card_body(
      h4("Modern Design"),
      p("Clean layout optimized for desktop and mobile.")
    )
  )

  card3 <- card(
    class = "feature-card shadow-sm",
    card_body(
      h4("Powerful Analytics"),
      p("Built with robust R infrastructure and scalable logic."),
      p("For more information, go to:", HTML("&nbsp;"),
        actionButton(inputId = ns("to_about"), label = "About")),
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
