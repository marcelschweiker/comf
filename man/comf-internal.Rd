\name{comf-internal}
\alias{comf-internal}
\alias{bisect}
\alias{calcet}
\alias{hcvF}
\alias{hcvG}
\alias{hcvM}
\alias{mrsw1}
\alias{mrsw2}
\alias{metaTherm}
\alias{mshiv}
\alias{pVapor}
\alias{QLoad}
\alias{QperReq}
\alias{Qskin_evap}
\alias{tskReq}
\alias{vbl}
\alias{vblCdilStr}
\alias{wcEx}
\alias{wcXCheck}
\alias{wdEx}
\alias{wdExLw}
\alias{wdXCheck}
\alias{listOfRequests}
\alias{pckgCheck}
\title{
Internal Functions Used by Other Functions of this Package
}
\description{
A few internal functions called by other functions in this package
}
\usage{
bisect(fn, lower, upper, tol = 1e-07, ...)
calcet(top, ta, pha, w, im, phaEt, imStar)
hcvF(v, tcl, ta)
hcvG(va, met, basMet)
hcvM(v)
mrsw1(tcrSet, tskSet, tcr, tsk)
mrsw2(tcrSet, tskSet, tcr, tsk, Qbl)
metaTherm(met, basMet)
mshiv(tcrSet, tskSet, tcr, tsk)
pVapor(tCel, phi)
QLoad(met, pmv, basMet)
QperReq(qmet)
Qskin_evap(qmet, pa)
tskReq(qmet)
vbl(tcrSet, tskSet, tcr, tsk)
vblCdilStr(cdil, str1, tcrSet, tskSet, tcr, tsk)
wcEx(cp, t1, too)
wcXCheck(t1, too)
wdEx(t1, too, pv1, pvo)
wdExLw(too, pvso, pv1, pvo)
wdXCheck(p1, poo)
listOfRequests()
pckgCheck()
}
\arguments{
depending on the function
}
\value{
depending on the function
}

\author{
All functions, but \code{bisect, listOfRequests, and pckgCheck} were coded by Masanori Shukuya and translated into R-language by Marcel Schweiker. The function \code{bisect} is taken from a forum entry and with permission by Ravi Varadhan. Functions \code{listOfRequests and pckgCheck} were written by Marcel Schweiker.
}
\examples{
## none
}

\keyword{ internal }
