PROGRAM MAIN
    IMPLICIT NONE
    integer :: m(2, 3), i, j
    DO i = 1, 2
    DO j = 1, 3
        m(i, j) = i
    END DO
    END DO
    print *,m
END PROGRAM MAIN
