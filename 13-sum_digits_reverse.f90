program sum_digits_reverse
    IMPLICIT NONE
    
    integer :: number, original_number, remainder, sum, reversed_number
    
    ! Input the integer number
    print *, "Enter an integer number:"
    read *, number
    
    ! Store the original number for reversing later
    original_number = number
    
    ! Initialize sum and reversed_number
    sum = 0
    reversed_number = 0
    
    ! Calculate sum of digits which you input earlyer
    do while (number /= 0)
        remainder = mod(number, 10)
        sum = sum + remainder
        number = number / 10
    end do
    
    ! Calculate the reverse of the original number
    do while (original_number /= 0)
        remainder = mod(original_number, 10)
        reversed_number = reversed_number * 10 + remainder
        original_number = original_number / 10
    end do
    
    ! Output the sum of digits and the reverse of the number
    print *, "Sum of digits:", sum
    print *, "Reverse of the number:", reversed_number
    
end program sum_digits_reverse