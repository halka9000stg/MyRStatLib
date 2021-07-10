\name{confin.mean_kn}
\alias{confin.mean_kn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
confin.mean_kn(vec, y, pop_var)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vec}{
%%     ~~Describe \code{vec} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{pop_var}{
%%     ~~Describe \code{pop_var} here~~
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
function (vec, y, pop_var) 
{
    len = length(vec)
    mn = mean(vec)
    vr = var(vec)
    alpha = 1 - y
    al_hf = alpha/2
    normval_hf = qnorm(al_hf)
    lower = mn - normval_hf * sqrt(pop_var/len)
    upper = mn + normval_hf * sqrt(pop_var/len)
    c(lower, upper)
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
