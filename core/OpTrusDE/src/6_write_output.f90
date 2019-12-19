subroutine write_output(fdat,fname,M_tot, W_ext, sigma_max)

    use datmod

    implicit none

    type(global_data)::  fdat

    integer :: i
    real*4            :: M_tot, W_ext, sigma_max
    character(len=20), intent(in) :: fname

    open(unit=20, file=fname, ACTION="write", STATUS="replace")

    write (20, *) 'NODAL FORCES'
    write (20, *) '    Node ID   Fx               Fy'
    do i=1,fdat%n_nodes
         write (20, *) i, fdat%P_nodal(2*i-1), fdat%P_nodal(2*i)
    end do

    write (20, *) 'NODAL DISPLACEMENTS'
    write (20, *) '    Node ID   dx               dy'
    do i=1,fdat%n_nodes
         write (20, *) i, fdat%D_nodal(2*i-1), fdat%D_nodal(2*i)
    end do

    write (20, *) 'ELEMENT FORCES'
    write (20, *) ' Element ID   F'
    do i=1,fdat%n_elements
         write (20, *) i, fdat%F_all(i)
    end do

    write (20, *) 'TOTAL MASS'
    write (20, *)  M_tot

    write (20, *) 'EXTERNAL WORK'
    write (20, *)  W_ext

    write (20, *) 'MAXIMUM STRESS'
    write (20, *)  sigma_max

    write (20, *) 'EIGENVALUES'
    write (20, *)  fdat%eigen_f(:)
    write (20, *) 'EIGENVECTORS'

    do i=1,fdat%dmsn
         write (20, *) fdat%eigen_v(i,:)
    end do
    close(20)

end subroutine write_output
