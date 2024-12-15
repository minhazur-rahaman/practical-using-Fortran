PROGRAM MatrixTransposeInverse
    IMPLICIT NONE

    INTEGER :: n, i, j
    REAL :: A(10,10), Transpose(10,10), Inverse(10,10)
    !REAL :: Det

    ! Input matrix size
    PRINT *, "Enter the size of the matrix (n x n):"
    READ *, n

    ! Input matrix elements
    PRINT *, "Enter the elements of the matrix:"
            READ *, ((A(i, j),j=1,n),i=1,n)

    ! Calculate the transpose
    DO i = 1, n
        DO j = 1, n
            Transpose(j, i) = A(i, j)
        END DO
    END DO

    ! Print the transpose
    PRINT *, "Transpose of the matrix:"
    DO i = 1, n
        DO j = 1, n
            WRITE(*,'(F8.3)', ADVANCE='NO') Transpose(i, j)
        END DO
        PRINT *
    END DO

    ! Calculate the inverse using Gaussian elimination
    CALL InverseMatrix(A, Inverse, n)

    ! Print the inverse
    PRINT *, "Inverse of the matrix:"
    DO i = 1, n
        DO j = 1, n
            WRITE(*,'(F8.3)', ADVANCE='NO') Inverse(i, j)
        END DO
        PRINT *
    END DO

END PROGRAM MatrixTransposeInverse

SUBROUTINE InverseMatrix(A, Inverse, n)
    IMPLICIT NONE
    INTEGER :: n, i, j, k
    REAL :: A(10,10), Inverse(10,10), Ratio

    ! Initialize Inverse matrix as an identity matrix
    Inverse = 0.0
    DO i = 1, n
        Inverse(i, i) = 1.0
    END DO

    ! Augment A with the identity matrix
    DO i = 1, n
        DO j = 1, n
        !    Temp(j) = A(i, j)
       ! END DO
        !DO j = 1, n
            A(i, n + j) = Inverse(i, j)
        END DO
    END DO

    ! Perform Gaussian elimination
    DO i = 1, n
        ! Make the diagonal contain all 1's
        IF (A(i, i) == 0.0) THEN
            PRINT *, "Matrix is singular, cannot find inverse."
            RETURN
        END IF
        Ratio = 1.0 / A(i, i)
        DO j = 1, 2*n
            A(i, j) = A(i, j) * Ratio
        END DO

        ! Make the rest of the column 0
        DO j = 1, n
            IF (j /= i) THEN
                Ratio = A(j, i)
                DO k = 1, 2*n
                    A(j, k) = A(j, k) - Ratio * A(i, k)
                END DO
            END IF
        END DO
    END DO

    ! Extract the Inverse matrix
    DO i = 1, n
        DO j = 1, n
            Inverse(i, j) = A(i, n + j)
        END DO
    END DO

END SUBROUTINE InverseMatrix