logical function constraint(x,idim)
!
!    real*4    x(idim)         == input vector x, checked under consideration of constraints
!    integer   idim            == dimension of input vector x
!
    implicit none
!
    integer,intent(in)  :: idim
    real*4,intent(in)   :: x(idim)
!
    constraint=.true.   
! 
!
end function constraint
