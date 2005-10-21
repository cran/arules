\name{itemCoding}
\alias{decode}
\alias{decode,numeric-method}
\alias{decode,list-method}
\alias{encode}
\alias{encode,numeric-method}
\alias{encode,list-method}
\alias{encode,character-method}
\alias{recode}
\alias{recode,itemMatrix-method}
\title{Item Coding -- Handling Item Labels and Column IDs Conversions}
\description{
  Provides the generic functions 
  and the S4 methods for 
  converting item labels into column IDs used in the binary matrix 
  representation and vice versa.

  \code{decode} converts from the numeric (column IDs) representation to
  readable item labels. 
  \code{decode} is used by \code{\link{LIST}}.

  \code{encode} converts from readable item labels to an itemMatrix
  using a given coding. With this method it is possible to create 
  several compatible \code{itemMatrix} objects (i.e., use the same
  binary representation for items) from data. 

  \code{recode} recodes an \code{itemMatrix} object so its coding is compatible
  with another object.
}
\usage{
decode(x, code, \ldots)

encode(x, code, \ldots)
\S4method{encode}{list}(x, code, itemMatrix = TRUE)
\S4method{encode}{character}(x, code, itemMatrix = TRUE)
\S4method{encode}{numeric}(x, code, itemMatrix = TRUE)

recode(x, code, \ldots)
\S4method{recode}{itemMatrix}(x, code = NULL, match = NULL, \ldots)
}
\arguments{
    \item{x}{ a vector or a list of vectors of character strings 
	(for \code{encode}) or of numeric (for \code{decode}), or
	an object of class \code{itemMatrix} (for \code{recode}).}
    \item{code}{a vector of character strings used for coding where 
    the position of an item label in the vector gives the item's column ID.  
    The used \code{code} can be obtained from \code{itemMatrix},  
    \code{transactions} and \code{associations} by the 
    method \code{itemLabels}.}
    \item{itemMatrix}{return an object of class \code{itemMatrix}.}
    \item{match}{an \code{itemMatrix} object whose coding \code{x} 
	should match.}
    \item{\ldots}{further arguments.}
}
\value{
   \code{recode} always returns an object
      of class \code{itemMatrix}.
   
   For \code{encode} with \code{itemMatrix = TRUE} an object
   of class \code{itemMatrix} is returned.
   Otherwise the result is of the same type as \code{x}, e.g., a
   list or a vector.
}
\seealso{
  \code{\link{LIST}},
  \code{\link{associations-class}},
  \code{\link{itemMatrix-class}}
}
\examples{
data("Adult")

### Example 1: Manual decoding
### get code
code <- itemLabels(Adult)
head(code)

### get undecoded list and decode in a second step
list <- LIST(Adult[1:5], decode = FALSE)
list

decode(list, code = code)

### Example 2: Manually create an itemMatrix 
data <- list(
    c("income=small", "age=Young"),
    c("income=large", "age=Middle-aged")
    )

iM <- encode(data, code)
iM

inspect(iM)

### use the itemMatrix to create transactions
as(iM, "transactions")


### Example 3: use recode
### select first 100 transactions and all education-related items
sub <- Adult[1:100, "education"]
itemLabels(sub)
image(sub)

### recode to match Adult again
sub.recoded <- recode(sub, match = Adult)
image(sub.recoded)
}
\keyword{manip}
