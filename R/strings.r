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
# Name:			strings.r
# Purpose:      string utilities
#
# Author:      Luzzi Valerio
#
# Created:     08/10/2015
#-------------------------------------------------------------------------------

#' contains
#'
#' This function returns TRUE if a "search" string is in "text" string
#' @param text
#' @param search - the string to search in "text"
#' @keywords contains
#' @export
#' @return returns TRUE if a \code{search} is in \code{text}.
#' @examples
#' contains("hello world","hello")
contains<-function(text,search){return(length(grep(search,text))>0)}

#' alltrim
#'
#' This function returns text trimmed
#' @param text - the orginal text
#' @keywords alltrim
#' @export
#' @return returns \code{text} trimmed.
#' @examples
#' alltrim("   hello world   ")
alltrim <- function (text) gsub("^\\s+|\\s+$", "",text)

#' upper
#'
#' This function returns text uppercased.
#' @param text - the orginal text
#' @keywords upper
#' @export
#' @return returns \code{text} uppercased.
#' @examples
#' upper("hello world")
upper<-function(text)toupper(text)

#' lower
#'
#' This function returns text lowercased.
#' @param text - the orginal text
#' @keywords lower
#' @export
#' @return returns \code{text} lowercased.
#' @examples
#' lower("hello world")
lower<-function(text)tolower(text)

#' left
#'
#' This function returns first n characters of a text.
#' @param text - the orginal text
#' @param n    - number of characters
#' @keywords left
#' @export
#' @return returns first \code{n} characters of \code{text}.
#' @examples
#' left("hello world",5)
left<-function(text,n)substr(text,1,n)

#' juststem
#'
#' This function returns the filename stem.
#' @param text - the orginal text
#' @keywords juststem
#' @export
#' @return returns \code{text} stemmed.
#' @examples
#' juststem("/hello/world.tif")
juststem<-function(text) sub("^([^.]*).*", "\\1", basename(text))

#' justpath
#'
#' This function returns the directory name of a path.
#' @param pathname - the orginal text
#' @keywords justpath
#' @export
#' @return returns the dirname of \code{pathname}.
#' @examples
#' justpath("/hello/world.tif")
justpath<-function(pathname)dirname(pathname)

#' justext
#'
#' This function returns the extension of a filename.
#' @param pathname - the filename
#' @keywords justext
#' @export
#' @return returns the extension of \code{pathname}.
#' @examples
#' justext("/hello/world.tif")
justext<-function(text) substr(text,nchar(text)-2,nchar(text))

#' forceext
#'
#' This function returns a new filepath with a new extension.
#' @param pathname - the filename
#' @param ext - the new extension
#' @keywords forceext
#' @export
#' @return returns a new filename with a new extension \code{ext}.
#' @examples
#' forceext("/hello/world.tif")
forceext<-function(text,ext) sub("^([^.]*).*", paste("\\1",ext,sep="."), text)

#' mkdirs
#'
#' This function create directory if not already exists.
#' @param pathname - the path
#' @keywords mkdirs
#' @export
#' @return returns ??.
#' @examples
#' mkdirs("/abc/cde/efg")
mkdirs<-function(pathname)dir.create(pathname,recursive=TRUE,showWarnings = FALSE)

#' isString
#'
#' This function returns true if argument is a string.
#' @param text - the argument
#' @keywords isString
#' @export
#' @return returns TRUE if  \code{text} is a string.
#' @examples
#' isString("hello world")
isString<-function(text){return(class(text)=='character')}


#' sformat
#'
#' This function returns true if argument is a string.
#' @param text - the argument
#' @keywords sformat
#' @export
#' @return returns TRUE if  \code{text} is a string.
#' @examples
#' sformat("hello {0}",c("world","word"))
sformat<-function(text,args){
  for(j in 1:length(args)){
    txt2replace = paste("\\{",j,"\\}",sep="")
    text = gsub(txt2replace,args[j],text)
  }
  return(text)
}

#' logger
#'
#' This function write a log file "log.txt".
#' @param text - the argument
#' @keywords logger
#' @export
#' @examples
#' logger("hello world")
logger<-function(text){
  f=file("log.txt",open="at");
  text = paste(dtos(NULL,"%Y-%m-%d %H:%M:%S"),"-",text)
  writeLines(text,f);
  print(text);
  close(f);
}

progression<-function(text,perc){f=file("interp.progress",open="at");writeLines(text,f);print(text);close(f);}

vardump<-function(variable){
  print("-----------------------------------");
  print(class(variable))
  print("---");
  print(head(as.data.frame(variable)));
  print("-----------------------------------");
}

