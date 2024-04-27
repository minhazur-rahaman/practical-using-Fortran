program fibonacci_sequence
IMPLICIT NONE 
integer :: n, i, fib1, fib2, next
    
    ! Input the number of terms
    print *, "Enter the number of Fibonacci terms to print:"
    read *, n
    
    ! Initialize first two Fibonacci numbers
    fib1 = 0
    fib2 = 1
    
    ! Output first two Fibonacci numbers
    print *, "Fibonacci sequence up to", n, "terms:"
    print *, fib1
    print *, fib2
    
    ! Calculate and output remaining Fibonacci numbers
    do i = 3, n
        next = fib1 + fib2
        print *, next
        fib1 = fib2
        fib2 = next
    end do

 end program fibonacci_sequence
