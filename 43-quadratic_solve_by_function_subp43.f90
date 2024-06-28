!Write a FORTRAN program to solve a quadratic equation ax^2+bx^2+cx+d=0 and print 
 !the roots(real or complex) by function Subprogram 

!problem 43 


program quadratic_solve_by_function_subpr
    implicit none
    real :: a, b, c, d, root1, root2
    integer :: result

    ! coefficients
    print *, 'Enter coefficients a, b, and c for the quadratic equation ax^2 +bx^2 + cx + d = 0:'
    read *, a, b, c, d

    ! Check if the leading coefficient is zero
    if (a+b == 0.0) then
        print *, "The coefficient a must not be zero for a quadratic equation."
        stop
    endif

    ! Call the function
    result = find_roots(a, b, c, d, root1, root2)
    if (result == 0) then
        print *, "The roots are real and different:"
        print 72, root1,root2
		72 format(2X, "first root : ",f8.3,"     second root: ",f8.3)
    else if (result == 1) then
        print *, "The roots are real and equal:"
        print *, "Root are : ", root1
    else
        print *, "The roots are complex:"
        print 4, root1,root2,root1,root2
		4 format(2X, "first root : ",f8.3,"+i",f8.3,"     second root: ",f8.3,"-i",f8.3)
    end if

contains

    integer function find_roots(a, b, c, d, root1, root2)
        implicit none
        real, intent(in) :: a, b, c	,d
        real, intent(out) :: root1, root2
        real :: disc

        ! Calculate the discriminant
        disc = c**2 - 4.0*(a+b)*d

        if (disc > 0.0) then
            root1 = (-c + sqrt(disc)) / (2.0 * (a+b))
            root2 = (-c - sqrt(disc)) / (2.0 * (a+b))
            find_roots = 0  
        else if (disc == 0.0) then
            root1 = -c / (2.0 * (a+b))
            root2 = root1
            find_roots = 1  
        else
            root1 = -c / (2.0 * (a+b))
            root2 = sqrt(abs(disc)) / (2.0 * (a+b))
            find_roots = 2  
        end if
    end function find_roots

end program quadratic_solve_by_function_subpr
