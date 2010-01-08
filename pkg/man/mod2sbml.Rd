%$Date$
%$Author$
\name{mod2sbml}
\alias{mod2sbml}

\title{SBML Shorthand to SBML convertor}

\description{SBML-shorthand is a language for describing systems biology models. It is designed
primarily for translation (compilation) into SBML. It is not associated with any particular
modelling or simulation tool. SBML itself is not intended to be read by humans or written
hand -- SBML-shorthand is. Note that SBML-shorthand is not intended to cover
every feature in SBML -- just the most commonly used aspects.

SBML-shorthand files have the file extension .mod.
}

\usage{
    mod2sbml(mod, asText=FALSE, verbose=FALSE)
    
}


\arguments{
  \item{mod}{A SBML shorthand string or file.}
  \item{asText}{logical value indicating that the first argument, \code{mod}, should be treated as text to parse, and not the name of a file. Default \code{FALSE}.}
  \item{verbose}{Useful for debugging the web-services. Default \code{FALSE}.}
}

\value{
    \code{mod2sbml}: Returns a SBML string.
}

\author{Colin Gillespie}

\seealso{See also \code{\link{validateSBML}}.}
\examples{
    
\dontrun{
library(SBMLModels)
data(Decay)
sbml = mod2sbml(DecayMod, TRUE)
cat(sbml)
}

}
\references{
Wilkinson, D. J., \url{http://www.staff.ncl.ac.uk/d.j.wilkinson/software/sbml-sh/}

Gillespie, C. S., et al. 2006. Tools for the SBML community, Bioinformatics. 22;628-629.

}
\keyword{htest}

