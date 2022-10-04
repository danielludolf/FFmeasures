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

  message("Make sure to make a column titled 'year' before using pums_recode()")

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
                               POVPIP == -1 ~ NA_character_,
                               dplyr::between(POVPIP, 0, 99) ~ "Below 100% FPL",
                               dplyr::between(POVPIP, 100, 200) ~ "Between 100-200% FPL",
                               POVPIP > 200 ~ "Above 200% FPL",
                               TRUE ~ NA_character_),
             error = function(e) NULL)
    )

}

# helper function for the volunteerism function
percent_moe_calc <- function(parameter_b, population, percentage){

  sqrt((parameter_b/population)*percentage*(1-percentage)) * 1.96

}

#' @importFrom magrittr "%>%"
volunteerism <- function(year){

  # TODO 2002 has different variable names
  if(!year %in% seq(2002, 2019)){stop("Please select a valid year. The Current Population Survey: Volunteering and Civic Life Supplement year range is from 2002 to 2019.",
                                      call. = F)}

  data <- censusapi::getCensus(name = "cps/volunteer/sep",
                                vintage = year,
                                # TODO think deeper on how we want to do the census key. should it be an argument or an error message at the beginning
                                # key = CENSUS_KEY,
                                # variable definitions in attachment 7: https://www2.census.gov/programs-surveys/cps/techdocs/cpssept19.pdf
                                vars = c("PWNRWGT", "PRSUPVOL", "PES16A", "PTDTRACE", "PEHSPNON"),
                                region = "state:37")

    data %>%
      dplyr::mutate(
      volunteer_status = dplyr::case_when(
        PRSUPVOL == "1" | PES16A == "1" ~ "Yes",
        PRSUPVOL == "2" ~ "No"),
      race_ethnicity = dplyr::case_when(
        PEHSPNON == "1" ~ "Hispanic/Latino",
        PTDTRACE == "1" ~ "White, NH",
        PTDTRACE == "2" ~ "Black, NH")
    ) %>%
    # filtering out missing data
    dplyr::filter(!is.na(volunteer_status),
                  !is.na(race_ethnicity)) %>%
    # calculating population by race/ethnicity
    dplyr::group_by(race_ethnicity) %>%
    dplyr::mutate(pop = sum(as.numeric(PWNRWGT))) %>%
    dplyr::ungroup() %>%
    # calculating total volunteers / not volunteers
    dplyr::group_by(pop, volunteer_status, race_ethnicity) %>%
    dplyr::summarise(vol_pop = sum(as.numeric(PWNRWGT))) %>%
    dplyr::ungroup() %>%
    # adding b_parameters from table 7 of attachment 16 https://www2.census.gov/programs-surveys/cps/techdocs/cpssept19.pdf
    # TODO each year b and a parameters will be diferent
      dplyr::mutate(b_parameter = dplyr::case_when(
      race_ethnicity == "Hispanic/Latino" ~ 10191,
      race_ethnicity == "White, NH" ~ 7469,
      race_ethnicity == "Black, NH" ~ 8067
    ),
    a_parameter = dplyr::case_when(
      race_ethnicity == "Hispanic/Latino" ~ -0.000233,
      race_ethnicity == "White, NH" ~ -0.000033,
      race_ethnicity == "Black, NH" ~ -0.000138
    ),
    percentage = vol_pop / pop,
    vol_pop_moe = sqrt((a_parameter * vol_pop^2) + (b_parameter * vol_pop)) * 1.96,
    percentage_moe = percent_moe_calc(b_parameter, pop, percentage)) %>%
    dplyr::filter(volunteer_status == "Yes") %>%
    dplyr::select(`Race/Ethnicity` = race_ethnicity, Total = vol_pop, `Total MOE` = vol_pop_moe,
                  Estimate = percentage, `Estimate MOE` = percentage_moe)


}

# volunteerism(2019) %>%
#   adjust_row_order(c(2,1,3))

#' Install a CENSUS API Key in Your .Renviron File for Repeated Use
#'
#' This function will add your CENSUS API key to your .Renviron file so it can be called securely
#' without being stored in your code. After you have installed your key, it can be called any time
#' by typing Sys.getenv("CENSUS_KEY") and can be used in package functions by simply typing
#' CENSUS_KEY If you do not have an .Renviron file, the function will create on for you. If
#' you already have an .Renviron file, the function will append the key to your existing file,
#' while making a backup of your original file for disaster recovery purposes.
#'
#' @details Warning: Make sure to make a column titled 'year' before using pums_recode()
#' @param key The API key provided to you from the Census formated in quotes. A key can be acquired at http://api.census.gov/data/key_signup.html
#' @param overwrite If this is set to TRUE, it will overwrite an existing CENSUS_KEY that you already have in your .Renviron file.
#' @param install If TRUE, will install the key in your .Renviron file for use in future sessions. Defaults to FALSE.
#' @keywords census_key
#' @export
#' @examples
#'  census_key("INSERT Census API Key Here", overwrite = FALSE, install = TRUE)

census_key <- function(key, overwrite = FALSE, install = FALSE) {
  if (install) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")
    if (file.exists(renv)) {
      file.copy(renv, file.path(home, ".Renviron_backup"))
    }
    if (!file.exists(renv)) {
      file.create(renv)
    }
    else {
      if (isTRUE(overwrite)) {
        message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
        oldenv = read.table(renv, stringsAsFactors = FALSE)
        newenv <- oldenv[-grep("CENSUS_KEY", oldenv),
        ]
        write.table(newenv, renv, quote = FALSE, sep = "\n",
                    col.names = FALSE, row.names = FALSE)
      }
      else {
        tv <- readLines(renv)
        if (any(grepl("CENSUS_KEY", tv))) {
          stop("A CENSUS_KEY already exists. You can overwrite it with the argument overwrite=TRUE",
               call. = FALSE)
        }
      }
    }
    keyconcat <- paste0("CENSUS_KEY='", key, "'")
    write(keyconcat, renv, sep = "\n", append = TRUE)
    message("Your API key has been stored in your .Renviron and can be accessed by Sys.getenv(\"CENSUS_KEY\"). \nTo use now, restart R or run `readRenviron(\"~/.Renviron\")`")
    return(key)
  }
  else {
    message("To install your API key for use in future sessions, run this function with `install = TRUE`.")
    Sys.setenv(CENSUS_KEY = key)
  }
}


