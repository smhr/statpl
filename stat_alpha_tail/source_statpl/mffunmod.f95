module mffunmod
use mathmod
implicit none
! 1. 
!function plpdf (x,a,b)
! Power Law Probability Density
! 2.
!function pltpdf (x,alpha,xmin,xmax)
! Truncated Power Law Probability Density


contains

! 1.
function plpdf (x,a,b)
! Power Law Probability Density
! a, b are the limits
implicit none
double precision, intent(in) :: x,a,b
double precision :: plpdf
plpdf = b*a**b/x**(b+1)
end function plpdf

!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!! !!!!!!!

! 2.
function pltpdf (x,alpha,xmin,xmax)
! Truncated Power Law Probability Density
implicit none
double precision, intent(in) :: x,alpha,xmin,xmax
double precision :: pltpdf
double precision :: kappa
kappa=(1.-alpha)/(xmax**(1.-alpha)-xmin**(1.-alpha))
pltpdf=kappa*x**(-alpha)
if (x > xmax) pltpdf=0.
if (x < xmin) pltpdf=0.
end function pltpdf

!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!! !!!!!!!

function genmasspl(idum,beta,pmin,pmax)
! Generate random data from a power law
implicit none
integer, intent(inout) :: idum
double precision,intent(in) :: beta,pmin,pmax
double precision :: genmasspl
double precision :: kappa,plim
double precision :: rtemp
if(beta < 0) then
write(*,*) "Wrong sign in exponent in GENMASSPL"
write(*,*) "per definitionem beta>=0, but got",beta
end if
plim=pmax
kappa= (1.-beta)/(plim**(1.-beta) - pmin**(1.-beta))
rtemp=ran(idum)
genmasspl=(rtemp*(1.-beta)/kappa + pmin**(1.-beta))**(1./(1.-beta))
end function genmasspl

!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!

function intpdfpl (m,beta,pmin,pmax)
! integrated Power Law pdf
implicit none
double precision, intent(in) :: m,beta,pmin,pmax
double precision :: intpdfpl
intpdfpl=0.
if(beta < 0) then
write(*,*) "Wrong sign in exponent in INTPDFPL"
write(*,*) "per definitionem beta>=0, but got",beta
end if
if (m>=pmin .AND. m<pmax) intpdfpl=( m**(1.-beta) - pmin**(1.-beta))/( pmax**(1.-beta) - pmin**(1.-beta))
if (m>pmax) intpdfpl=1.
end function intpdfpl


!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!

function invintpdfpl (x,beta,pmin,pmax)
! inverse integrated Power Law pdf
implicit none
double precision, intent(in) :: x,beta,pmin,pmax
double precision :: invintpdfpl
invintpdfpl=0.
if(beta < 0) then
write(*,*) "Wrong sign in exponent in INTPDFPL"
write(*,*) "per definitionem beta>=0, but got",beta
end if
if (x>=0 .AND. x<1) invintpdfpl=( x*( pmax**(1.-beta) - pmin**(1.-beta) ) + pmin**(1.-beta) )**(1./(1-beta))
end function invintpdfpl


!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!

function burrcdf(x,eta,tau,lambda)
! Cumulative Burr distribution 
implicit none
double precision, intent(in) :: x,eta,tau,lambda
double precision :: burrcdf
burrcdf=1. - (eta/(eta+x**tau))**lambda
end function burrcdf


function exppdf(x,theta,a,b)
! Exponential probability density
implicit none
double precision, intent(in) :: x,theta,a,b
double precision :: exppdf
double precision :: kappa
exppdf=1./b*exp( (a-x)/b )
kappa=theta/( exp(-theta*a)-exp(-theta*b) )
exppdf=kappa*exp(-theta*x)
end function exppdf



function intexppdf(x,theta,a,b)
! Integrated exponential pdf
implicit none
double precision, intent(in) :: x,theta,a,b
double precision :: intexppdf
double precision :: kappa
kappa=theta/( exp(-theta*a)-exp(-theta*b) )
intexppdf=-kappa/theta*( exp(-theta*x) - exp(-theta*a) )
end function intexppdf


function genmassexp(idum,a,b)
! generates random variates from a exponential pdf
implicit none
integer, intent(inout) :: idum
double precision, intent(in) :: a,b
double precision :: genmassexp
double precision :: rtemp
rtemp=ran(idum)

genmassexp=a-b*log(1-rtemp)

end function genmassexp


function lamersdynevolv(m,t,t4)
! Dynamical evolution for star clusters from Lamers et al.
! Parameter t4 is the lifetime of a 10**4 Msun cluster
implicit none
double precision :: m,lamersdynevolv,t0,t,t4
double precision :: gamma,mtmp
gamma=0.62
t0=(t4/660.)**(1./0.967)
mtmp=m*(1 - gamma*t/t0)**(1/gamma)
lamersdynevolv=mtmp
end function lamersdynevolv

end module mffunmod
