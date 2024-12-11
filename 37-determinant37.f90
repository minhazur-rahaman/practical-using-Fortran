program matrix_operations
    implicit none
    integer, parameter :: n = 3  ! Set the size of the matrix
    real :: matrix(n, n), transpose(n, n), inverse(n, n)
    integer :: i, j
    logical :: singular

    ! Reading the matrix
    print *, 'Enter the elements of the matrix (', n, 'x', n, '):'
    do i = 1, n
        do j = 1, n
            read *, matrix(i, j)
        end do
    end do

    ! Printing the original matrix
    print *, 'Original Matrix:'
    call print_matrix(matrix)

    ! Calculating the transpose of the matrix
    transpose = transpose_matrix(matrix)

    ! Printing the transpose
    print *, 'Transpose of the Matrix:'
    call print_matrix(transpose)

    ! Calculating the inverse of the matrix
    call inverse_matrix(matrix, inverse, singular)
    if (singular) then
        print *, 'The matrix is singular and its inverse does not exist.'
    else
        print *, 'Inverse of the Matrix:'
        call print_matrix(inverse)
    end if

contains

    ! Subroutine to print a matrix
    subroutine print_matrix(mat)
        real, intent(in) :: mat(n, n)
        do i = 1, n
            print *, (mat(i, j), j = 1, n)
        end do
    end subroutine print_matrix

    ! Function to calculate the transpose of a matrix
    function transpose_matrix(mat) result(trans)
        real, intent(in) :: mat(n, n)
        real :: trans(n, n)
        do i = 1, n
            do j = 1, n
                trans(j, i) = mat(i, j)
            end do
        end do
    end function transpose_matrix

    ! Subroutine to calculate the inverse of a matrix
    subroutine inverse_matrix(mat, inv, singular)
        real, intent(in) :: mat(n, n)
        real, intent(out) :: inv(n, n)
        logical, intent(out) :: singular
        real :: a(n, n), identity(n, n)
        integer :: ipiv(n), info

        a = mat
        inv = 0.0
        singular = .false.
        identity = 0.0
        do i = 1, n
            identity(i, i) = 1.0
        end do

        call sgesv(n, n, a, n, ipiv, identity, n, info)

        if (info /= 0) then
            singular = .true.
        else
            inv = identity
        end if
    end subroutine inverse_matrix

end program matrix_operations
