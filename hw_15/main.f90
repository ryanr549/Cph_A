MODULE CHAO
IMPLICIT NONE
REAL(KIND=8) :: pi = 3.1415927
CONTAINS
REAL(KIND=8) FUNCTION y(lambda, x)
    REAL(KIND=8), INTENT(IN) :: lambda, x
    y = lambda * SIN(pi * x)
END FUNCTION y
SUBROUTINE Iteration(step, ites, lnum, counts, filename)
    REAL(KIND=8), INTENT(IN) :: step
    INTEGER(KIND=4), INTENT(IN) :: lnum, ites, counts
    CHARACTER(LEN=*), INTENT(IN) :: filename
    REAL(KIND=8) :: lambda, x, sol(lnum*counts, 2)
    INTEGER(KIND=4) :: i, j
    lambda = 0
    DO i = 1, lnum
        x = 1
        DO j = 1, ites - counts
            x = y(lambda, x)
        END DO
        DO j = 1, counts
            x = y(lambda, x)
            sol((i-1)*counts + j, 1) = lambda
            sol((i-1)*counts + j, 2) = x
        END DO
        lambda = lambda + step
    END DO
    OPEN (1, file=filename)
    WRITE (1, *) sol
    CLOSE (1)
END SUBROUTINE Iteration
END MODULE CHAO

PROGRAM MAIN
    USE CHAO    
    IMPLICIT NONE    
    CALL Iteration(0.00001_8, 10000, 100000, 30, 'sol.dat')
END PROGRAM MAIN
