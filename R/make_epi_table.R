
#' Make a 2x2 Contingency Table
#'
#' This function takes vectors of case, exposure, and time data to
#' produce 2x2 contingency tables for case-control, case-noncase, and
#' case-person time data. Syntax and output matches the format of ir,
#' cc, and cs functions in Stata.
#'
#' @param cases A binary (0/1) numeric vector of either
#'    case-noncase or case-control data, where 1 is always a case and 0 is
#'    either a noncase or a control.
#' @param exposed A binary (0/1) numeric vector of exposure status.
#'    1 is always exposed and 0 is always unexposed.
#' @param time An optional numeric vector of person-time to event.
#' @param type Indicates type of contingency table to be made. Use CI
#'    for cumulative incidence, case-noncase data. Use CC for case-control data.
#'    Use IR for incidence rate, case + person-time data. If no type is provided,
#'    the function defaults to CI.
#'
#' @returns A 2x2 contingency table of case and exposure margins.
#' @export
#'
#' @examples
#' NEED TO PUT EXAMPLES IN HERE
make_epi_table <- function(cases,
                           exposed,
                           time=NULL,
                           type = c("CI", "CC", "IR")) {
  type <- match.arg(type)

  # THROW ERRORS IF CASES, EXPOSED, OR TIME ARE NON-NUMERIC VARIABLES
  if(!is.numeric(cases)) stop("Case status must be a numeric 0/1 variable.")
  if(!is.numeric(exposed)) stop ("Exposure status must be a numeric 0/1 variable.")
  if(!is.null(time) & !is.numeric(time)) stop("Person-time must be a numeric variable.")

  # THROW ERROR IF USER SELECTS IR BUT DOESN'T PASS IN A TIME VARIABLE
  if(type=="IR" & is.null(time)) stop("A person-time variable must be included for incidence rate table calculations.")

  # THROW ERROR IF CASES/EXPOSED ARE NOT 0/1 CODED
  if(sum(unique(cases)) != 1) stop("Case status must be coded as binary 0/1.")
  if(sum(unique(exposed)) != 1) stop("Exposure status must be coded as binary 0/1.")

  if(type == "IR") {
    # Define numbers for table
    a <- sum(cases[exposed==1])
    b <- sum(cases[exposed==0])
    n1 <- sum(time[exposed==1])
    n0 <- sum(time[exposed==0])

    mat <- matrix(c(a, b, n1, n0), nrow = 2, byrow = TRUE)
    colnames(mat) <- c("Exposed", "Unexposed")
    rownames(mat) <- c("Cases", "Person-time")
    return(
      stats::addmargins(as.table(mat), margin = 2, FUN = list(Total = sum),
                 quiet = TRUE)
    )
  }

  # Factorize cases & exposed to customize output
  exposed_f <- factor(exposed, levels = c(1, 0),
                      labels = c("Exposed", "Unexposed"))

  if(type == "CI") {
    cases_f <- factor(cases, levels = c(1, 0),
                      labels = c("Cases", "Noncases"))
  }

  if(type == "CC") {
    cases_f <- factor(cases, levels = c(1, 0),
                      labels = c("Cases", "Controls"))
  }

  return(stats::addmargins(table(cases_f, exposed_f, dnn = NULL),
                    FUN = list(Total = sum),
                    quiet = TRUE))

}

# USERS SHOULD SPECIFY TYPE - otherwise default is cumulative incidence

# For person-time (incidence rate) data
# cases, exposed, time

# For cumulative incidence (case/noncase) data
# stata does case, exposed, pop (summary, 1 row for each combo
# of case and exposed)
# might be more intuitive for users to do
# case status, exposure status then have function summarise within


# for case-control odds ratio data (case/control)
# stata also does case exposed pop
# still probably need case status and exposure status


# may also want to create make_table_i function
# mirroring stata iri/csi functionality -
# makes table from direct number inputs
# stata does both the table making AND the summary calculations
# in the same function - this could be useful


make_epi_tablei <- function() {

}
