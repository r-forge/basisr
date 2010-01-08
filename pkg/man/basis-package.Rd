%$Date$
%$Author$
\name{basisR-package}
\alias{basisR-package}
\alias{basisR}
\alias{basisr}

\docType{package}

\title{Biology of ageing e-Science simulation and simulation system}

\description{
The primary objective
of BASIS is to help advance the understanding of the complex biology
of ageing, where many different mechanisms act and interact at a range
of different levels.  Discrete stochastic simulation modelling is
particularly relevant to biological investigation.  For example, when
considering sub-cellular models of biochemical processes where
critical species have low copy-number, discrete simulation deals with
the qualitative difference between the complete absence of a molecule
from a system, and its presence at very low levels.  This is unlike
models based on the continuum approximation which often simply cannot
deal with the extinction of a species.  Stochastic
simulation is generally appropriate for describing biological systems
as their intrinsic complexity often leads to small interactions
(e.g. environmental interactions or cell-cell interactions) which are
not included explicitly in a model. Thus stochastic models often
provide an excellent framework for dealing with the combined effect of
many unmodelled weak processes and interactions. Furthermore, these
models encompass those needed to describe and design time course
biological experiments in which it is not possible to have complete
control over the initial conditions of the system (e.g. cell cycle
synchrony or isogenicity across a population of cells).  Stochastic
models play a particularly important role in ageing research. In this
area, ageing is typically described as the cumulative result of small
amounts of random damage and the propagation of this damage throughout
the lifetime of a cell or organism requires appropriate
characterisation of random effects, such as DNA damage, protein damage
or the accumulation of DNA mutations which lead to tumorogenesis.

Although BASIS has been designed with ageing research in mind, its
capabilities are generic to a wide range of other biological
systems. Our system aims to make both existing and new models
accessible to the research community in a way that facilitates users
to adapt models and to run simulations themselves. It also makes
publicly available a relatively complex, powerful and expensive
computational architecture that is necessary for inferring parameters
in biological stochastic models when using cohorts of simulation
results. BASIS has adopted the Systems Biology Markup Language (SBML) as a model description language. This is an
XML-based computer-readable format for representing models of
biochemical reaction networks.

The BASIS project is supported by a team from a wide range of
disciplines including the biological sciences, mathematical and
statistical sciences, and computer science. A key aim is to facilitate
collaboration between experimental scientists and mathematical
modellers. By sharing and
integrating models and data, advances have already been made in our
understanding of ageing. BASIS also provides open source downloadable
tools in addition to a comprehensive online simulation system and
modelling environment.
}

\author{
Maintainer: Colin Gillespie <c.gillespie@ncl.ac.uk>
}
\references{See \url{http://www.basis.ncl.ac.uk/}}


\keyword{package}
\keyword{distribution}

\seealso{See also \code{\link{getSessionID}} and \code{\link{validateSBML}}}

