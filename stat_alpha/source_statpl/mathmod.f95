module mathmod
use utilmod
implicit none

!module procedure amoeba(p,y,ftol,func,iter)

contains

!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!

function binko (nin,kin)
implicit none
integer, intent(in) :: nin,kin
double precision :: binko
integer :: n,k,i
!double precision :: tmp1,tmp2
binko=1
k=kin
n=nin
if (k > 0 .AND. k  < n) then

!if (k < float(n)/2. ) then
!k=n-k
!end if

!binko=fak(n)/fak(k)/fak(n-k)
binko=1.
do i=1,n
!binko=binko*float(i)
binko=binko*(n-i)/(i+1)
end do
!binko=binko/fak(n-k)

binko=fak(nin)/fak(kin)/fak(nin-kin)

end if

if (n==k) then
binko=1
end if

if (k==0) then
binko=1
end if

end function binko

!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!

function fak (n) 
implicit none 
integer, intent(in) :: n
double precision :: fak
integer :: i
fak=1
do i=1,n
fak=fak*float(i)
end do
end function fak

!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!


integer function iminloc (x)
implicit none
double precision, dimension(:), intent(in) :: x
integer, dimension(1) :: imin
imin=minloc(x)
iminloc=imin(1)
end function iminloc

integer function imaxloc (x)
implicit none
double precision, dimension(:), intent(in) :: x
integer, dimension(1) :: imax
imax=maxloc(x)
imaxloc=imax(1)
end function imaxloc

!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!

function ran(idum)
implicit none
integer, intent(inout) :: idum
double precision ::ran
ran=ran2(idum)
!call random_number(ran)
end function ran

!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!


FUNCTION ran2(idum)
      INTEGER, intent(inout) ::idum
	  integer :: IM1,IM2,IMM1,IA1,IA2,IQ1,IQ2,IR1,IR2,NTAB,NDIV
      double precision ran2,AM,EPS,RNMX
      PARAMETER (IM1=2147483563,IM2=2147483399,AM=1./float(IM1),IMM1=IM1-1, &
&      IA1=40014,IA2=40692,IQ1=53668,IQ2=52774,IR1=12211,IR2=3791, &
&      NTAB=32,NDIV=1+IMM1/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
      INTEGER idum2,j,k,iv(NTAB),iy
      SAVE iv,iy,idum2
      DATA idum2/123456789/, iv/NTAB*0/, iy/0/
      if (idum.le.0) then
        idum=max(-idum,1)
        idum2=idum
        do  j=NTAB+8,1,-1
          k=idum/IQ1
          idum=IA1*(idum-k*IQ1)-k*IR1
          if (idum.lt.0) idum=idum+IM1
          if (j.le.NTAB) iv(j)=idum
	end do
        iy=iv(1)
      endif
      k=idum/IQ1
      idum=IA1*(idum-k*IQ1)-k*IR1
      if (idum.lt.0) idum=idum+IM1
      k=idum2/IQ2
      idum2=IA2*(idum2-k*IQ2)-k*IR2
      if (idum2.lt.0) idum2=idum2+IM2
      j=1+iy/NDIV
      iy=iv(j)-idum2
      iv(j)=idum
      if(iy.lt.1)iy=iy+IMM1
      ran2=min(AM*iy,RNMX)
      return
END FUNCTION ran2

!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!


SUBROUTINE sort_quick(arr)
	IMPLICIT NONE
	DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: arr
	INTEGER, PARAMETER :: NN=15, NSTACK=50
	DOUBLE PRECISION :: a
	INTEGER :: n,k,i,j,jstack,l,r
	INTEGER, DIMENSION(NSTACK) :: istack
	n=size(arr)
	jstack=0
	l=1
	r=n
	do
		if (r-l < NN) then
			do j=l+1,r
				a=arr(j)
				do i=j-1,l,-1
					if (arr(i) <= a) exit
					arr(i+1)=arr(i)
				end do
				arr(i+1)=a
			end do
			if (jstack == 0) RETURN
			r=istack(jstack)
			l=istack(jstack-1)
			jstack=jstack-2
		else
			k=(l+r)/2
			call swap(arr(k),arr(l+1),.TRUE.)
			call swap(arr(l),arr(r),arr(l)>arr(r))
			call swap(arr(l+1),arr(r),arr(l+1)>arr(r))
			call swap(arr(l),arr(l+1),arr(l)>arr(l+1))
			i=l+1
			j=r
			a=arr(l+1)
			do
				do
					i=i+1
					if (arr(i) >= a) exit
				end do
				do
					j=j-1
					if (arr(j) <= a) exit
				end do
				if (j < i) exit
				call swap(arr(i),arr(j),.TRUE.)
			end do
			arr(l+1)=arr(j)
			arr(j)=a
			jstack=jstack+2
			if (jstack > NSTACK) print *,'sort: NSTACK too small'
			if (r-i+1 >= j-l) then
				istack(jstack)=r
				istack(jstack-1)=i
				r=j-1
			else
				istack(jstack)=j-1
				istack(jstack-1)=l
				l=i
			end if
		end if
	end do
END SUBROUTINE sort_quick

	SUBROUTINE sort(arr)
	IMPLICIT NONE
	double precision, DIMENSION(:), INTENT(INOUT) :: arr
	INTEGER :: i,n
	n=size(arr)
	do i=n/2,1,-1
		call sift_down(i,n)
	end do
	do i=n,2,-1
		call swap(arr(1),arr(i))
		call sift_down(1,i-1)
	end do
	CONTAINS
!BL
	SUBROUTINE sift_down(l,r)
	INTEGER, INTENT(IN) :: l,r
	INTEGER :: j,jold
	double precision :: a
	a=arr(l)
	jold=l
	j=l+l
	do
		if (j > r) exit
		if (j < r) then
			if (arr(j) < arr(j+1)) j=j+1
		end if
		if (a >= arr(j)) exit
		arr(jold)=arr(j)
		jold=j
		j=j+j
	end do
	arr(jold)=a
	END SUBROUTINE sift_down
	END SUBROUTINE sort


!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!


end module mathmod
