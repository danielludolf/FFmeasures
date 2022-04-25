#' Recode ACS' PUMS Datasets to Forsyth Futures' Labeling
#'
#' This function recodes the ACS' PUMS variables to match Forsyth Futures' labeling standards
#' @details Warning: Make sure to make a column titled 'year' before using pums_recode()
#' @param data The PUMS dataset object
#' @keywords pums_recode
#' @export
#' @importFrom magrittr "%>%"
#' @examples
#'  year <- 2019
#'  tidycensus::get_pums(variables = c("RAC1P","HISP","SEX","POVPIP","AGEP","SCHL"), state = "NC", puma = c("01801","01802","01803"),
#'           year = year, survey = "acs1", rep_weights = "person") %>%
#'    dplyr::rename_all(~str_to_upper(.x)) %>%
#'    # Make sure to make a column titled 'year' before using pums_recode()
#'    dplyr::mutate(year = year) %>%
#'    pums_recode()

pums_recode <- function(data){

  data %>%
    # Age Group Recode
    mutate(`Age Group` = tryCatch(cut(as.numeric(AGEP), breaks = c(0, 18, 35, 65, Inf),
                                      labels = c("Under 18 years old", "18-34 years old", "35-64 years old", "65+ years old"), right = FALSE),
                                  error = function(e) NULL),
    # Race/Ethnicity Recode
           `Race/Ethnicity` = tryCatch(ifelse(as.numeric(HISP) > 1, "Hispanic/Latino",
                                              ifelse(as.numeric(RAC1P) == 1, "White, NH",
                                                     ifelse(as.numeric(RAC1P) ==2, "Black/AA, NH",
                                                            NA_character_))),
                                       error = function(e) NULL),
    # Sex Recode
            Sex = tryCatch(ifelse(as.numeric(SEX) == 1, "Male", "Female"),
                           error = function(e) NULL),
    # Sex of Adults Recode
           `Sex of Adults` = tryCatch(ifelse(as.numeric(AGEP) < 18, NA_character_,
                                             ifelse(as.numeric(SEX) == 1, "Male", "Female")),
                                      error = function(e) NULL),
    # Associate's Degree or More Recode
           `Educational Attainment` = tryCatch(ifelse(AGEP <= 24 | is.na(AGEP), NA_character_,
                                                      ifelse(as.numeric(SCHL) >= 20, "Associate's Degree or More", "Less than Associate's Degree")),
                                               error = function(e) NULL),
    # Highest Level of Educational Attainment Recode
           `Highest Level of Educational Attainment` = tryCatch(ifelse(as.numeric(AGEP) >= 25 & year < 2008,
                                                                       as.character(cut(as.numeric(SCHL),
                                                                                        breaks = c(0, 9, 12, 13, 14, Inf),
                                                                                        labels = c("High School Diploma or Less",
                                                                                                   "Some College, No Degree",
                                                                                                   "Associate's Degree",
                                                                                                   "Bachelor's Degree",
                                                                                                   "More than a Bachelor's Degree"), right = FALSE)), NA_character_),
                                                                error = function(e) NULL),
    # Highest Level of Educational Attainment
           `Highest Level of Educational Attainment` = tryCatch(ifelse(as.numeric(AGEP) >= 25 & year > 2007,
                                                                       as.character(cut(as.numeric(SCHL),
                                                                                        breaks = c(0, 18, 20, 21, 22, Inf),
                                                                                        labels = c("High School Diploma or Less",
                                                                                                   "Some College, No Degree",
                                                                                                   "Associate's Degree",
                                                                                                   "Bachelor's Degree",
                                                                                                   "More than a Bachelor's Degree"), right = FALSE)), NA_character_),
                                                                error = function(e) NULL),
    # County Recode
           `County` = tryCatch(ifelse(year < 2012, dplyr::case_when(
                                                     PUMA == `1800` ~ "Forsyth County, NC",
                                                     PUMA == `1900` ~ "Forsyth County, NC",
                                                     PUMA == `1601` ~ "Guilford County, NC",
                                                     PUMA == `1602` ~ "Guilford County, NC",
                                                     PUMA == `1700` ~ "Guilford County, NC",
                                                     PUMA == `2801` ~ "Durham County, NC",
                                                     PUMA == `2802` ~ "Durham County, NC",
                                                     TRUE ~ NA_character_)),
             error = function(e) NULL),
    # County Recode
           `County` = tryCatch(ifelse(year > 2011, dplyr::case_when(
                                                     PUMA == `1801` ~ "Forsyth County, NC",
                                                     PUMA == `1802` ~ "Forsyth County, NC",
                                                     PUMA == `1803` ~ "Forsyth County, NC",
                                                     PUMA == `1701` ~ "Guilford County, NC",
                                                     PUMA == `1702` ~ "Guilford County, NC",
                                                     PUMA == `1703` ~ "Guilford County, NC",
                                                     PUMA == `1704` ~ "Guilford County, NC",
                                                     PUMA == `1301` ~ "Durham County, NC",
                                                     PUMA == `1302` ~ "Durham County, NC",
                                                     TRUE ~ NA_character_)),
             error = function(e) NULL),
    # Poverty Recode
           Poverty = tryCatch(dplyr::case_when(
                               POVPIP < 100 ~ "Below 100% FPL",
                               dplyr::between(POVPIP, 100, 200) ~ "Between 100-200% FPL",
                               POVPIP > 200 ~ "Above 200% FPL",
                               TRUE ~ NA_character_),
             error = function(e) NULL)
    )

}

# # helper function for the volunteerism function
# percent_moe_calc <- function(parameter_b, population, percentage){
#
#   sqrt((parameter_b/population)*percentage*(1-percentage)) * 1.96
#
# }
#
# volunteerism <- function(year, CENSUS_KEY){
#
#   # TODO 2002 has different variable names
#   if(!year %in% seq(2002, 2019)){stop("Please select a valid year. The Current Population Survey: Volunteering and Civic Life Supplement year range is from 2002 to 2019.",
#                                       call. = F)}
#
#   data <- tryCatch({censusapi::getCensus(name = "cps/volunteer/sep",
#                                 vintage = year,
#                                 # TODO think deeper on how we want to do the census key. should it be an argument or an error message at the beginning
#                                 key = CENSUS_KEY,
#                                 # variable definitions in attachment 7: https://www2.census.gov/programs-surveys/cps/techdocs/cpssept19.pdf
#                                 vars = c("PWNRWGT", "PRSUPVOL", "PES16A", "PTDTRACE", "PEHSPNON"),
#                                 region = "state:37")},
#                    error = function(e) {message("Error: A Census API key is required and can be requested at https://api.census.gov/data/key_signup.html. Please add your Census key to your .Renviron - see instructions at https://github.com/hrecht/censusapi#api-key-setup")})
#
#   if(!is.null(data)){
#
#     data %>%
#       dplyr::mutate(
#       volunteer_status = dplyr::case_when(
#         PRSUPVOL == "1" | PES16A == "1" ~ "Yes",
#         PRSUPVOL == "2" ~ "No"),
#       race_ethnicity = dplyr::case_when(
#         PEHSPNON == "1" ~ "Hispanic/Latino",
#         PTDTRACE == "1" ~ "White, NH",
#         PTDTRACE == "2" ~ "Black, NH")
#     ) %>%
#     # filtering out missing data
#     dplyr::filter(!is.na(volunteer_status),
#                   !is.na(race_ethnicity)) %>%
#     # calculating population by race/ethnicity
#     dplyr::group_by(race_ethnicity) %>%
#     dplyr::mutate(pop = sum(as.numeric(PWNRWGT))) %>%
#     dplyr::ungroup() %>%
#     # calculating total volunteers / not volunteers
#     dplyr::group_by(pop, volunteer_status, race_ethnicity) %>%
#     dplyr::summarise(vol_pop = sum(as.numeric(PWNRWGT))) %>%
#     dplyr::ungroup() %>%
#     # adding b_parameters from table 7 of attachment 16 https://www2.census.gov/programs-surveys/cps/techdocs/cpssept19.pdf
#     # TODO each year b and a parameters will be diferent
#       dplyr::mutate(b_parameter = dplyr::case_when(
#       race_ethnicity == "Hispanic/Latino" ~ 10191,
#       race_ethnicity == "White, NH" ~ 7469,
#       race_ethnicity == "Black, NH" ~ 8067
#     ),
#     a_parameter = dplyr::case_when(
#       race_ethnicity == "Hispanic/Latino" ~ -0.000233,
#       race_ethnicity == "White, NH" ~ -0.000033,
#       race_ethnicity == "Black, NH" ~ -0.000138
#     ),
#     percentage = vol_pop / pop,
#     vol_pop_moe = sqrt((a_parameter * vol_pop^2) + (b_parameter * vol_pop)) * 1.96,
#     percentage_moe = percent_moe_calc(b_parameter, pop, percentage)) %>%
#     dplyr::filter(volunteer_status == "Yes") %>%
#     dplyr::select(`Race/Ethnicity` = race_ethnicity, Total = vol_pop, `Total MOE` = vol_pop_moe,
#                   Estimate = percentage, `Estimate MOE` = percentage_moe)
#
#   }
#
# }
#
# # f510269e416c08d666d3c2213ec475b675ea29f7
# volunteerism(2016, 'f510269e416c08d666d3c2213ec475b675ea29f7') %>%
#   adjust_row_order(c(2,1,3))
#
#
