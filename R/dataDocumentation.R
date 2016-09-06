#' Construct information for the ADPwide response data set
#'
#' The required construct information for the Adoption data
#'
#' @format A data frame with 1 row and 8 columns:
#' \describe{
#'   \item{cons.ID}{A unique numerical ID to map items to constructs}
#'   \item{long.name}{The name of the construct}
#'   \item{short.name}{An abbreviation of the construct name}
#'   \item{cat1}{The name of the first category}
#'   \item{cat2}{The name of the second category}
#'   \item{cat3}{The name of the third category}
#'   \item{cat4}{The name of the fourth category}
#'   \item{cat5}{The name of the fifth category}
#' }
#'
#' @source Amy Arneson's Measurement 1 study, collected in Fall 2013
"ADPcons"

#' Item information for the ADPwide response data set
#'
#' The required item information for the Adoption data
#'
#' @format A data frame with 13 rows and 10 columns
#' \describe{
#'   \item{item.ID}{A unique numerical ID for the item}
#'   \item{item.name}{The name of the item}
#'   \item{cons.ID}{The unique numerical construct ID that maps the construct to
#'                  the item}
#'   \item{item.type}{A character string indicating the type of the item (This
#'                    column is only used internally in BASS)}
#'   \item{fixed}{A logical indicating if the item has a fixed parameter(s)
#'                (This is only to be used internally in BASS)}
#'   \item{cat1}{A logical indicating if the category is present in the scoring
#'               guide for the item}
#'   \item{cat2}{A logical indicating if the category is present in the scoring
#'               guide for the item}
#'   \item{cat3}{A logical indicating if the category is present in the scoring
#'               guide for the item}
#'   \item{cat4}{A logical indicating if the category is present in the scoring
#'               guide for the item}
#'   \item{cat5}{A logical indicating if the category is present in the scoring
#'               guide for the item}
#' }
#'
#' @source Amy Arneson's Measurement 1 study, collected in Fall 2013
"ADPitem"

#' External variables for the respondents in the SUPwide and ADPwide response
#' data set
#'
#' Values of external variables that were measured in the SUP/ADP study
#'
#' @format A data frame with 38 rows and 8 columns
#' \describe{
#'   \item{q1}{A categorical variable indicating the subject the respondent
#'             taught in the 13-14 school year}
#'   \item{ELA.SS}{A string variable indicating if the respondent is an English-
#'                 Language Arts or Social Studies teacher}
#'   \item{CORE}{A string variable indicating if the respondent teaches a 'core'
#'               (state-tested) subject}
#'   \item{q2}{The number of years the respondent has taught}
#'   \item{experience}{A string indicating if the teacher has 1-5 years of
#'                     experience or more ("6+")}
#'   \item{q3}{The number of years the respondent has taught in the district}
#'   \item{tenure.Y.N}{A string indicating if the teacher has tenure}
#'   \item{only.job}{A string indicating if the teacher's current job was the
#'                   only teaching job he or she has held}
#' }
#'
#' @source Amy Arneson's Measurement 1 study, collected in Fall 2013
"ADPvars"

#' Response data for the Adoption study
#'
#' The response data (scores) for the items aligned to the Adoption construct
#'
#' @format A data frame with 38 rows and 13 columns.
#' \describe{
#'   The row names are the respondent IDs.  The column names are the items.  The
#'   values are the score received.  All items were scored polytomously.
#' }
#'
#' @source Amy Arneson's Measurement 1 study, collected in Fall 2013
"ADPwide"

#' Construct information for the SUPwide response data set
#'
#' The required construct information for the Support data
#'
#' @format A data frame with 1 row and 8 columns:
#' \describe{
#'   \item{cons.ID}{A unique numerical ID to map items to constructs}
#'   \item{long.name}{The name of the construct}
#'   \item{short.name}{An abbreviation of the construct name}
#'   \item{cat1}{The name of the first category}
#'   \item{cat2}{The name of the second category}
#'   \item{cat3}{The name of the third category}
#'   \item{cat4}{The name of the fourth category}
#'   \item{cat5}{The name of the fifth category}
#' }
#'
#' @source Amy Arneson's Measurement 1 study, collected in Fall 2013
"SUPcons"

#' Item information for the SUPwide response data set
#'
#' The required item information for the Support data
#'
#' @format A data frame with 15 rows and 10 columns
#' \describe{
#'   \item{item.ID}{A unique numerical ID for the item}
#'   \item{item.name}{The name of the item}
#'   \item{cons.ID}{The unique numerical construct ID that maps the construct to
#'                  the item}
#'   \item{item.type}{A character string indicating the type of the item (This
#'                    column is only used internally in BASS)}
#'   \item{fixed}{A logical indicating if the item has a fixed parameter(s)
#'                (This is only to be used internally in BASS)}
#'   \item{cat1}{A logical indicating if the category is present in the scoring
#'               guide for the item}
#'   \item{cat2}{A logical indicating if the category is present in the scoring
#'               guide for the item}
#'   \item{cat3}{A logical indicating if the category is present in the scoring
#'               guide for the item}
#'   \item{cat4}{A logical indicating if the category is present in the scoring
#'               guide for the item}
#'   \item{cat5}{A logical indicating if the category is present in the scoring
#'               guide for the item}
#' }
#'
#' @source Amy Arneson's Measurement 1 study, collected in Fall 2013
"SUPitem"

#' External variables for the respondents in the SUPwide and ADPwide response
#' data set
#'
#' Values of external variables that were measured in the SUP/ADP study
#'
#' @format A data frame with 38 rows and 8 columns
#' \describe{
#'   \item{q1}{A categorical variable indicating the subject the respondent
#'             taught in the 13-14 school year}
#'   \item{ELA.SS}{A string variable indicating if the respondent is an English-
#'                 Language Arts or Social Studies teacher}
#'   \item{CORE}{A string variable indicating if the respondent teaches a 'core'
#'               (state-tested) subject}
#'   \item{q2}{The number of years the respondent has taught}
#'   \item{experience}{A string indicating if the teacher has 1-5 years of
#'                     experience or more ("6+")}
#'   \item{q3}{The number of years the respondent has taught in the district}
#'   \item{tenure.Y.N}{A string indicating if the teacher has tenure}
#'   \item{only.job}{A string indicating if the teacher's current job was the
#'                   only teaching job he or she has held}
#' }
#'
#' @source Amy Arneson's Measurement 1 study, collected in Fall 2013
"SUPvars"

#' Response data for the Support study
#'
#' The response data (scores) for the items aligned to the Support construct
#'
#' @format A data frame with 38 rows and 15 columns.
#' \describe{
#'   The row names are the respondent IDs.  The column names are the items.  The
#'   values are the score received.  All items were scored polytomously.
#' }
#'
#' @source Amy Arneson's Measurement 1 study, collected in Fall 2013
"SUPwide"

#' Construct information for the combined ADP/SUP study
#'
#' The required construct information for the Adoption & Support data
#'
#' @format A data frame with 2 rows and 8 columns:
#' \describe{
#'   \item{cons.ID}{A unique numerical ID to map items to constructs}
#'   \item{long.name}{The name of the construct}
#'   \item{short.name}{An abbreviation of the construct name}
#'   \item{cat1}{The name of the first category}
#'   \item{cat2}{The name of the second category}
#'   \item{cat3}{The name of the third category}
#'   \item{cat4}{The name of the fourth category}
#'   \item{cat5}{The name of the fifth category}
#' }
#'
#' @source Amy Arneson's Measurement 1 study, collected in Fall 2013
"AMYcons"

#' Item information for the combined ADP/SUP study
#'
#' The required item information for the Support data
#'
#' @format A data frame with 28 rows and 10 columns
#' \describe{
#'   \item{item.ID}{A unique numerical ID for the item}
#'   \item{item.name}{The name of the item}
#'   \item{cons.ID}{The unique numerical construct ID that maps the construct to
#'                  the item}
#'   \item{item.type}{A character string indicating the type of the item (This
#'                    column is only used internally in BASS)}
#'   \item{fixed}{A logical indicating if the item has a fixed parameter(s)
#'                (This is only to be used internally in BASS)}
#'   \item{cat1}{A logical indicating if the category is present in the scoring
#'               guide for the item}
#'   \item{cat2}{A logical indicating if the category is present in the scoring
#'               guide for the item}
#'   \item{cat3}{A logical indicating if the category is present in the scoring
#'               guide for the item}
#'   \item{cat4}{A logical indicating if the category is present in the scoring
#'               guide for the item}
#'   \item{cat5}{A logical indicating if the category is present in the scoring
#'               guide for the item}
#' }
#'
#' @source Amy Arneson's Measurement 1 study, collected in Fall 2013
"AMYitem"

#' External variables for the respondents in the combined ADP/SUP study
#'
#' Values of external variables that were measured in the SUP/ADP study
#'
#' @format A data frame with 38 rows and 8 columns
#' \describe{
#'   \item{q1}{A categorical variable indicating the subject the respondent
#'             taught in the 13-14 school year}
#'   \item{ELA.SS}{A string variable indicating if the respondent is an English-
#'                 Language Arts or Social Studies teacher}
#'   \item{CORE}{A string variable indicating if the respondent teaches a 'core'
#'               (state-tested) subject}
#'   \item{q2}{The number of years the respondent has taught}
#'   \item{experience}{A string indicating if the teacher has 1-5 years of
#'                     experience or more ("6+")}
#'   \item{q3}{The number of years the respondent has taught in the district}
#'   \item{tenure.Y.N}{A string indicating if the teacher has tenure}
#'   \item{only.job}{A string indicating if the teacher's current job was the
#'                   only teaching job he or she has held}
#' }
#'
#' @source Amy Arneson's Measurement 1 study, collected in Fall 2013
"AMYvars"

#' Response data for the combined ADP/SUP study
#'
#' The response data (scores) for the items aligned to the Support construct
#'
#' @format A data frame with 38 rows and 28 columns.
#' \describe{
#'   The row names are the respondent IDs.  The column names are the items.  The
#'   values are the score received.  All items were scored polytomously.
#' }
#'
#' @source Amy Arneson's Measurement 1 study, collected in Fall 2013
"AMYwide"
