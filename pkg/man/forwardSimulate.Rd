%$Date$
%$Author$
\name{forwardSimulate}
\alias{forwardSimulate}
\alias{isForwardSimReady}
\alias{getForwardSimResult}

\title{Direct access to the simulator}

\description{These functions provide direct access to the gillespie2 simulator that BASIS uses. These web-services do not require a username and password. After you have retrieved your data, your model and data are deleted from the basis system.}

\usage{
    forwardSimulate(sbml, max_time, iters, asText=FALSE, verbose=FALSE)
    isForwardSimReady(sid, verbose=FALSE)
    getForwardSimResult(sid, verbose=FALSE)
}


\arguments{
  \item{sid}{A session id obtained from the \code{forwardSimulate} web-service. }
  \item{sbml}{A valid sbml model. It is recommended that the \code{\link{validateSBML}} web-service is used to ensure that the model valid SBML.}
  \item{asText}{logical value indicating that the first argument, \code{sbml}, should be treated as string to parse, and not the name of a file.}
  \item{max_time}{The model is simulated from time 0,...,\code{max_time}, i.e. the final simulation point.}
  \item{iters}{The level of thinning used when storing the output. For example if \code{max\_time}=100000 and \code{iters}=1000, then we store the outputted times t=0, 10, 20, ...., 100000}
  \item{verbose}{Useful for debugging the web-services. Default \code{FALSE}.}




}

\value{
    \code{forwardSimulate}: Returns a session id (sid).
    
    \code{isForwardSimReady}: Returns TRUE or FALSE.
    
    \code{getForwardSimResult}: A data frame of results.
}

\author{Colin Gillespie}


\examples{
    
\dontrun{
library(SBMLModels)
data(Decay)

sid = forwardSimulate(DecayModel, 30, 150)
isForwardSimReady(sid)
results = getForwardSimResult(sid)

plot(results$Time, results$X, type='l')

}

}

\keyword{htest}

