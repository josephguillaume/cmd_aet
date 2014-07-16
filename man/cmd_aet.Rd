\name{cmd_aet}
\alias{cmd_aet}
\alias{cmd_aet.sim}
\title{ IHACRES Catchment Moisture Deficit (CMD) model with actual evapotranspiration}
\description{
  The Catchment Moisture Deficit (CMD) effective rainfall model for IHACRES.
  It is a conceptual-type model, where input rainfall is partitioned explicitly into
  drainage, evapo-transpiration, and changes in catchment moisture.
  This version uses actual evapotranspiration as input such that it only calculates drainage.
}
\usage{
cmd_aet.sim(DATA, e, d, shape = 0,
        M_0 = d/2, return_state = FALSE)
}
\arguments{
  \item{DATA}{
    a \code{\link{ts}}-like object with named columns:
    \describe{
      \item{\code{P}}{ time series of areal rainfall depths, usually in mm. }
      \item{\code{aET}}{ time series of actual evapo-transpiration. }
    }
  }
  \item{e}{ scaling factor for actual evapo-transpiration. }
  \item{d}{ CMD threshold for producing flow. }
  \item{shape}{
    defines form of the \eqn{dU/dP} relationship:
    \code{shape = 0} is the linear form, \code{shape = 1} is the trigonometric
    form, and \code{shape > 1} is the power form.
  }
  \item{M_0}{ starting CMD value. }
  \item{return_state}{ to return state variables as well as the effective rainfall. }
}
\details{
  The mass balance step is:
  \deqn{M[t] = M[t-1] - P[t] + E_T[t] + U[t]}

  where \eqn{M} represents catchment moisture deficit (CMD), constrained
  below by 0 (the nominal fully saturated level).  P is catchment areal
  rainfall, \eqn{E_T} is evapo-transpiration, and U is drainage
  (effective rainfall). All are, typically, in units of mm per time
  step.

  Rainfall effectiveness (i.e. drainage proportion) is a simple
  \emph{instantaneous} function of the CMD, with a threshold at \eqn{M =
  d}. In the default linear form this is:

  \deqn{\frac{\mathrm{d}U}{\mathrm{d}P} = 1 - \min(1, M/d)}{
    dU/dP = 1 - min(1, M/d)}

  The trigonometric form is

  \deqn{\frac{\mathrm{d}U}{\mathrm{d}P} = 1 - \min(1, \sin^2(\pi M / 2d))}{
    dU/dP = 1 - min(1, sin^2(pi M / 2d))}

  The power form is

  \deqn{\frac{\mathrm{d}U}{\mathrm{d}P} = 1 - \min(1, (M/d)^b)}{
    dU/dP = 1 - min(1, (M/d)^b)}

  The actual drainage each time step involves the integral of these relations.

  Evapo-transpiration is taken from input and not calculated.
}
\value{
  \code{cmd_aet.sim} returns the modelled time series of effective rainfall,
  or if \code{return_state = TRUE}, a multi-variate time series with named
  columns \code{U} (effective rainfall), \code{CMD} and
  \code{ET} (evapo-transpiration \eqn{E_T}).
}
\author{ Joseph Guillaume \email{josephguillaume@gmail.com} }
\seealso{
  \code{\link{cmd.sim}}, 
  \code{\link{hydromad}(sma = "cmd")} to work with models as objects (recommended).
}
\examples{
data(Cotter)
x <- Cotter[1:1000]

orig <- hydromad(x,sma="cmd",f=0.6781,e=0.1271,d=506.9382)
x$aET <- predict(orig,U=TRUE,return_state=TRUE)$ET

mod <- hydromad(x,sma="cmd_aet",e=1,d=506.9382)

all.equal(fitted(mod,U=TRUE),fitted(orig,U=TRUE))
}
\keyword{ models }

