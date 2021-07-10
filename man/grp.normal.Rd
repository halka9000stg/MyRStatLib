\name{grp.normal}
\alias{grp.normal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
grp.normal(t, cl, x0, x1, l0, l1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t}{
%%     ~~Describe \code{t} here~~
}
  \item{cl}{
%%     ~~Describe \code{cl} here~~
}
  \item{x0}{
%%     ~~Describe \code{x0} here~~
}
  \item{x1}{
%%     ~~Describe \code{x1} here~~
}
  \item{l0}{
%%     ~~Describe \code{l0} here~~
}
  \item{l1}{
%%     ~~Describe \code{l1} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (t, cl, x0, x1, l0, l1) 
{
    ln = nrow(t)
    for (i in 1:ln) {
        if (i == 1) {
            d = F
        }
        else {
            d = T
        }
        curve(add = d, dnorm(x, t[i, 1], t[i, 2]), xlim = c(x0, 
            x1), type = "l", col = cl[i])
    }
    title("���K���z")
    vc = numeric(ln)
    for (j in 1:ln) {
        vc[j] = paste("N(", t[j, 1], ",", t[j, 2], ")")
    }
    legend(l0, l1, vc, lty = 1, col = cl)
    abline(v = t[, 1], col = "gray", lty = 2)
    for (k in 1:ln) {
        mtext(text = paste("��=", t[k, 1], sep = ""), at = t[k, 
            1], col = "gray")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
