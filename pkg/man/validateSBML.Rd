%$Date$
%$Author$
\name{validateSBML}
\alias{validateSBML}

\title{SBML validator}

\description{This function uses the SBML validator at http://sbml.org/ to validate an SBML model.}

\usage{
    validateSBML(filename, 
                output="text",
                units=FALSE,
                overall=FALSE,
                identifier=FALSE,
                MathML=FALSE, 
                SBO=FALSE,
                overdetermined=FALSE,
                modeling=FALSE)    
}


\arguments{
  \item{filename}{The path to the SBML file.}
  \item{output}{The SBML validator has four output options: text, xml, xhtml and json (JavaScript Object Notation).}
  \item{units}{Disable the units consistency check. Default \code{FALSE}.}
  \item{overall}{Disable the overall SBML consistency check. Default \code{FALSE}.}
  \item{identifier}{Disable the identifier consistency check. Default \code{FALSE}.}
  \item{MathML}{Disable the MathML consistency check. Default \code{FALSE}.}
  \item{SBO}{Disable the SBO consistency check. Default \code{FALSE}.}
  \item{overdetermined}{Disable the overdetermined model check. Default \code{FALSE}.}
  \item{modeling}{Disable the modeling practice check. Default \code{FALSE}.}
}

\value{
    \code{validateSBML}: Returns a character string of the form: text, xml, xhtml and json.
}

\author{Colin Gillespie}
\references{http://sbml.org/Facilities/Validator}

\examples{
    
\dontrun{
library(SBMLModels)
data(Decay)

#Write data to a file
filename = "Decay.xml"
ff = file("Decay.xml", "w")  # open an output file connection
cat(DecayModel, file = ff)

rst=validateSBML(filename, units=TRUE)
cat(rst)

rst=validateSBML(filename, output="xml", units=TRUE, SBO=TRUE)
cat(rst)
}

}

\keyword{htest}

