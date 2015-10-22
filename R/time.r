#-------------------------------------------------------------------------------
# Licence:
# Copyright (c) 2012-2015 Luzzi Valerio for Gecosistema S.r.l.
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
# OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
# WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
# OTHER DEALINGS IN THE SOFTWARE.
#
# Name:        time.r
# Purpose:     date & time utilities
#
# Author:      Luzzi Valerio
#
# Created:     12/10/2015
#-------------------------------------------------------------------------------

#' isDate
#'
#' This function returns TRUE is text is a Date.
#' @param text - a string
#' @keywords isDate
#' @export
#' @return returns TRUE if \code{text} is a Date
#' @examples
#' isDate("2015-10-12")
isDate<-function(text){res = tryCatch(length(as.Date(text))>0,error=function(e){return(FALSE)});return(res)}

#' date2julian
#'
#' This function returns julian number for a date
#' @param date - the orginal date
#' @keywords date2julian,julian
#' @export
#' @return returns the julian number for \code{date}.
#' @examples
#' date2julian("2015-10-12")
date2julian<-function(date){return(strftime(date,"%j"))}

#' julian2date
#'
#' This function returns the date for julian number YYYYjjj
#' @param j - the julian number in the form YYYYjjj
#' @keywords julian2date
#' @export
#' @return returns the date number for \code{j}.
#' @examples
#' julian2date("2015221")
julian2date<-function(j){ j= as.numeric(j);return(strptime(j,"%Y%j"))}

#' dtos
#'
#' This function returns a date string fromatted
#' @param date   - the original date
#' @param format - the format.
#' @keywords dtos
#' @export
#' @return returns a date in a string form according to the format.
#' @examples
#' dtos("2015-10-12","%d/%m/%Y")
dtos<-function(date,format="%Y-%m-%d"){
  if (is.null(date))
    date= Sys.time()

  return(strftime(date,format))
}
