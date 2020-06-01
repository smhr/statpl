module utilmod
implicit none

interface swap
	module procedure swap_r, masked_swap_r, swap_rv
end interface
contains

subroutine error(string,i)
character(len=*), intent(in) :: string
integer, intent(in), optional :: i
print *,"ERROR   ",string,i
read (*,*)
end subroutine error


SUBROUTINE swap_r (a,b)
	DOUBLE PRECISION, INTENT(INOUT) :: a,b
!	LOGICAL, INTENT(IN) :: mask
	DOUBLE PRECISION :: swp
!	if (mask) then
		swp=a
		a=b
		b=swp
!	end if
END SUBROUTINE swap_r

SUBROUTINE masked_swap_r (a,b,mask)
	DOUBLE PRECISION, INTENT(INOUT) :: a,b
	LOGICAL, INTENT(IN) :: mask
	DOUBLE PRECISION :: swp
	if (mask) then
		swp=a
		a=b
		b=swp
	end if
END SUBROUTINE masked_swap_r

SUBROUTINE swap_rv(a,b)
	DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: a,b
	DOUBLE PRECISION, DIMENSION(SIZE(a)) :: dum
	dum=a
	a=b
	b=dum
END SUBROUTINE swap_rv

function cadd(c1,c2)
implicit none
character (len=50) :: c1, c2,cadd
cadd=trim(c1)//trim(c2)
end function

function path(dir,fil)
implicit none
character (len=50) :: path,dir,fil
path=trim(dir)//'/'//trim(fil)
end function path

function numberoflines(filename)
implicit none
character (len=50) :: filename
integer :: numberoflines,i,stat
open(99,file=filename,action="read")
i=0
stat=0
do
read (99,*,iostat=stat)
if (stat/= 0) exit
i=i+1
end do
close(99)
numberoflines=i
end function numberoflines

subroutine diagnostic(dia,text)
implicit none
logical :: dia
character (len=100) :: text
if (dia) write(*,*) text
end subroutine diagnostic

end module utilmod
