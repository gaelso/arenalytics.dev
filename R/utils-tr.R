#' Translation keys
#'
#' Returns a named list of all i18n translation keys used in the app.
#' Each value must exactly match a key present in `inst/assets/translations.json`.
#'
#' @return A named list of character strings.
#' @keywords internal
#' @noRd
.tr_keys <- function() {
  list(
    ## main function
    app_title = "Analytical Dashboard for OpenForis Arena",
    nav_home  = "Home",
    nav_tool  = "Tool",
    nav_about = "About",

    ## Home UI
    hero_title = "Powerful analytics for your OpenForis Arena data",
    hero_txt = "Explore your data from OpenForis Arena, gain insights and develop report-ready figures and statistics.",
    hero_btn = "Get Started",
    feat1_p1 = "A dashboard shinyApp, arenalytics, shipped as a R package, unlocking the powerful statistics of R with the visual convenience of dashboards.",
    feat1_p2 = "For more details about the app developement or if any issue, find us on:",
    feat1_p3 = "Start playing with your data:",
    feat1_btn = "Here",
    feat2_p1 = "Arena Analytics is tailored to work with",
    feat2_p2 = "OpenForis Arena helps you create a survey, collect and clean data, and develop a processing chain for calculating additional variables from your data.",
    feat2_p3 = "The outcome of this process is a ZIP file of semi-aggregated data that will serve as input here.",
    feat3_title = "Need help?",
    feat3_p1 = "Tutorials, examples and additional resources can help you harness the analytical power of Arena Analytics.",
    feat3_p2 = "The full package documentation, as well as case studies and tutorials have been developed to help you navigate through the dashboard capabilities.",
    feat3_p3 = "For more information, go to:",
    feat3_btn = "About", ## Not pasted twice in assets/translations.json

    ## Tool UI
    ac1_title = "Load ZIP file",
    ac1_p1 = "The dashboard requires a ZIP file that is produced by running the processing chain from your OpenForis Arena survey in Rstudio (local or online).",
    ac1_p2 = "Once this file is produced, upload here:",
    #ac1_input1 = "Browse...",
    #ac1_input2 = "No file selected",
    ac1_msg_nodata = "No data uploaded.",
    ac1_msg_ok = "Data structure OK.",
    ac1_msg_err = "Incorrect data uploaded.",
    ac1_btn = "Read data"




  )
}
