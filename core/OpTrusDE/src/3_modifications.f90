subroutine modification_matrices(fdat)

    use datmod
    implicit none

    type(global_data)::  fdat

    integer :: i, j, k, cond
    real  :: theta, c_theta, s_theta, pi

    pi = 3.14159265359

    do i = 1,fdat%dmsn
        fdat%R_sup(i,i) = 1.0
    end do

    fdat%n_uknown_P = sum(fdat%BCs(:,2:3))

    k = 1
    do i = 1, fdat%n_BCs

        if (fdat%BCs(i,2) .EQ. 1) then
            fdat%ind_V(fdat%dmsn - fdat%n_uknown_P + k) = 2*fdat%BCs(i,1)-1
            k = k + 1
        end if
        if (fdat%BCs(i,3) .EQ. 1) then
            fdat%ind_V(fdat%dmsn - fdat%n_uknown_P + k) = 2*fdat%BCs(i,1)
            k = k + 1
        end if


        if (fdat%BCs(i,4) .NE. 0) then
            theta = pi*fdat%BCs(i,4)/180
            c_theta = cos(theta)
            s_theta = sin(theta)

            fdat%R_sup(2*fdat%BCs(i,1)-1,2*fdat%BCs(i,1)-1) = c_theta
            fdat%R_sup(2*fdat%BCs(i,1)  ,2*fdat%BCs(i,1)  ) = c_theta
            fdat%R_sup(2*fdat%BCs(i,1)-1,2*fdat%BCs(i,1)  ) = s_theta
            fdat%R_sup(2*fdat%BCs(i,1)  ,2*fdat%BCs(i,1)-2) = -s_theta
        end if
    end do

    k = 1
    do i = 1, fdat%dmsn
        cond = 0
        do j = 1, fdat%n_uknown_P
            if (i .EQ. fdat%ind_V(fdat%dmsn-fdat%n_uknown_P+j)) then
                cond = 1
            end if
        end do
        if (cond .EQ. 0) then
            fdat%ind_V(k) = i
            k = k + 1
        end if
    end do

    do i = 1, fdat%dmsn
        do j = 1, fdat%dmsn
            if (fdat%ind_V(i) .EQ. j) then
                fdat%V_incident(i,j) = 1
            end if
        end do
    end do

return
end subroutine modification_matrices
