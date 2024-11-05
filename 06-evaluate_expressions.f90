program evaluate_expressions
    IMPLICIT NONE
    real :: a, b, c, d, r, x, u, v
    integer :: serial_num

    do
        print*, "Enter the serial number (0 to exit): "
        read*, serial_num
        if (serial_num == 0) EXIT

        print*, "Type the values of a, b, r, d :"
        read*, a, b, r, d

        ! Check for undefined conditions in u calculation
        if (r == d .or. b == 0) THEN
            print*, "The equation for U is undefined due to b=0 or (r=d)."
            cycle  ! Go to the next iteration if u is undefined

        else 
            u = abs(a + (1.0 / b)) / sin(r - d)
            print*, "Value of U is:", u

            print*, "Type the values of c and x:"
            read*, c, x

            ! Check for undefined conditions in v calculation
            if (u == 0) then
                print*, "The equation for V is undefined because u = 0."
            else 
                v = (c * (1.0 / u) - u * cos(x)) / b
                print*, "Value of V is:", v
            end if
        end if
    end do

end program evaluate_expressions
