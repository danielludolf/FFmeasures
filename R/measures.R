
# helper function for the volunteerism function
percent_moe_calc <- function(parameter_b, population, percentage){

  sqrt((parameter_b/population)*percentage*(1-percentage)) * 1.96

}

library(magrittr)

volunteerism <- function(year, CENSUS_KEY){

  # TODO 2002 has different variable names
  if(!year %in% seq(2002, 2019)){stop("Please select a valid year. The Current Population Survey: Volunteering and Civic Life Supplement year range is from 2002 to 2019.",
                                      call. = F)}

  data <- tryCatch({censusapi::getCensus(name = "cps/volunteer/sep",
                                vintage = year,
                                # TODO think deeper on how we want to do the census key. should it be an argument or an error message at the beginning
                                key = CENSUS_KEY,
                                # variable definitions in attachment 7: https://www2.census.gov/programs-surveys/cps/techdocs/cpssept19.pdf
                                vars = c("PWNRWGT", "PRSUPVOL", "PES16A", "PTDTRACE", "PEHSPNON"),
                                region = "state:37")},
                   error = function(e) {message("Error: A Census API key is required and can be requested at https://api.census.gov/data/key_signup.html. Please add your Census key to your .Renviron - see instructions at https://github.com/hrecht/censusapi#api-key-setup")})

  if(!is.null(data)){

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

}

# f510269e416c08d666d3c2213ec475b675ea29f7
volunteerism(2019, 'f510269e416c08d666d3c2213ec475b675ea29f7') %>%
  adjust_row_order(c(2,1,3))




