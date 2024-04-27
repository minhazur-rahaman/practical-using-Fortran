program prime_numbers_in_range
    IMPLICIT NONE
    
    integer :: start, end, i, count, num
    logical :: is_prime
    
    ! Input the range in between you want to know the num number is prime 
    print *, "Enter the starting number of the range:"
    read *, start
    
    print *, "Enter the ending number of the range:"
    read *, end
    
    ! Initialize count of prime numbers otherwise compilar will take random number
    count = 0
    
    ! Loop 
    do num = start, end
        ! Assume num is prime initially
        is_prime = .true.
        
        ! Check if num is divisible by any number from 2 to sqrt(num)

		     ! first e oii real number er s_root ber korbo tarpor samne int like real number ta abr interger kore nibo
        do i = 2, int(sqrt(real(num)))
            if (mod(num, i) == 0) then
                ! num is not prime if it is divisible by i
                is_prime = .false.
                exit
            end if
        end do
        
        ! If num is prime, print it and increment the count
        if (is_prime .and. num > 1) then
            print *, num, "is a prime number."
            count = count + 1
        end if
    end do
    
    ! Output the total count of prime numbers
    print *, "Total prime numbers in the range [", start, ", ", end, "] are:", count
    
end program prime_numbers_in_range
 