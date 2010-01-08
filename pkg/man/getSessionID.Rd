%$Date$
%$Author$
\name{getSessionID}
\alias{getSessionID}
\alias{putSBML}
\alias{simulateModel}
\alias{isBasisFinished}
\alias{getRawData}

\title{BASIS web-services}

\description{A description of the BASIS web-services that can be accessed through R. All BASIS webservices below require a username and password, which can be obtained from \url{http://www.basis.ncl.ac.uk}}

\usage{
    getSessionID(username, password, verbose=FALSE)
    putSBML(sid, sbmlModel, asText=FALSE, verbose=FALSE)
    simulateModel(sid, model_urn, name, max_time, sims, iters, verbose=FALSE)
    isBasisFinished(sid, sim_urn, verbose=FALSE)
    getRawData(sid, sim_urn, verbose=FALSE)
}


\arguments{
  \item{username}{Your BASIS username.}
  \item{password}{Your BASIS password.}  
  \item{sid}{A session id obtained from the \code{getSessionID} web-service. The session id is valid for 24 hours after the last use.}
  \item{sbmlModel}{A valid sbml model. It is recommended that the \code{\link{validateSBML}} web-service is used to ensure that the model valid SBML.}
  \item{asText}{logical value indicating that the first argument, \code{sbmlModel}, should be treated as string to parse, and not the name of a file.}
  \item{model_urn}{A BASIS model urn. Every model entered into the BASIS system is assigned a model Uniform Resource Name (URN) as a unique identifier. The model URN has the form \code{urn:basis.ncl:model:\#1} where \code{\#1} is an integer.}
  \item{name}{A string giving a name to the stochastic simulation.}
  \item{max_time}{The model is simulated from time 0,...,\code{max_time}, i.e. the final simulation point.}
  \item{sims}{Since BASIS uses a stochastic simulator, this means that each simulation is different. \code{sims} determines the number of simulations carried out.}
  \item{iters}{The level of thinning used when storing the output. For example if \code{max\_time}=100000 and \code{iters}=1000, then we store the outputted times t=0, 10, 20, ...., 100000}
  \item{sim_urn}{All simulation groups (cohorts of stochastic simulations) are given a simulation group URN of the form \code{urn:basis.ncl:model:\#1:simulation:\#2-\#3:\#4} where \code{\#1} is an integer and refers to the model being simulated, \code{\#2}
is the time of the final simulation point, \code{\#3} is the level of thinning used when storing the output, \code{\#4} is an integer (ensures that
the urn is unique) and \code{\#5} is an optional number referring to a specific simulation. So for example \code{urn:basis.ncl:model:401:simulation:100000-1000:418} refers to the simulation of model \code{urn:basis.ncl:model:401} from time 0 to time 100000 and outputting values at times t=0,10,20,..., 100000.}
  \item{verbose}{Useful for debugging the web-services. Default \code{FALSE}.}
}

\value{
    \code{getSessionID}: Returns a session id (sid). This session id is valid for any web-service interface to the BASIS system.
    
    \code{putSBML}: Puts the SBML Model into BASIS and returns a model_urn.
    
    \code{simulateModel}: Simulates the model and returns a simulation_urn
    
    \code{isBasisFinished}: Returns `Waiting', `Running', or `Finished'.
    
    \code{getRawData}: A data frame of results.
}

\author{Colin Gillespie}

\seealso{See also some other stuff.}
\examples{
    
\dontrun{
library(SBMLModels)
data(Decay)

#getSessionId
username = 
password = 
sid = getSessionID(username, password)

#putSBML
model_urn = putSBML(sid, DecayModel, asText=TRUE)

#simulate
name = "My first simulation"; max_time = "10"; sims = "10"; iters = "15";
sim_urn = simulateModel(sid, model_urn, name, max_time, sims, iters)

#isBasisFinished 
isBasisFinished(sid, sim_urn)
#getRawData
my_data = getRawData(sid, sim_urn)


#Plotting your results
attach(my_data)

#Get a single time vector
T = time[sim_no==1]

#Plot a simulation or two
plot(T, X[sim_no==1], type='l')
lines(T, X[sim_no==2], col=2)

#Easy to get means and sds as well
mean_X = tapply(X, time, mean)
sd_X = tapply(X, time, sd)
plot(T, mean_X, type='l')
lines(T, mean_X-2*sd_X, col=2, lty=2)
lines(T, mean_X+2*sd_X, col=2, lty=2)


#Something more fancy. Requires the lattice package
library(lattice)
xyplot(X ~ time | factor(sim_no), type='l')
}

}

\keyword{htest}

