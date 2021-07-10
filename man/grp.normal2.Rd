\name{grp.normal2}
\alias{grp.normal2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
grp.normal2(vec, cl, x0, x1, l0, l1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vec}{
%%     ~~Describe \code{vec} here~~
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
function (vec, cl, x0, x1, l0, l1) 
{
    ln1 = length(vec)/2
    mat = matrix(vec, nrow = ln1, ncol = 2)
    mat
    seiki(mat, cl, x0, x1, l0, l1)
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
