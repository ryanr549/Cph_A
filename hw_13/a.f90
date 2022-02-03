MODULE Metropolis
IMPLICIT NONE

CONTAINS
SUBROUTINE Sample1(a, b, gam, num)
    REAL(KIND=8) ,INTENT(IN) :: gam
    REAL(KIND=8) :: rand(num), integral
    REAL(KIND=8) xt, x(0:num), r, seed
    INTEGER(KIND=4), INTENT(IN) :: a, b, num
    INTEGER(KIND=4) :: i
    x(0) = 1
    integral = 0
    CALL RANDOM_NUMBER(seed) 
    ! 用FORTRAN自带的随机数生成器生成16807生成器的种子
    CALL Schrage(num, int(2147483647 * seed), rand)
    DO i = 1, num
        xt = - gam * LOG(rand(i))
        r = (xt / x(i-1))**(a - 1) * EXP(-(xt - x(i-1)/b)) * EXP((xt - x(i-1))/gam)
        IF(rand(i) < MIN(1.0, r)) THEN
            x(i) = xt 
        ELSE
            x(i) = x(i-1) ! 若建议未通过，仍保留结果与上一步相同. 
        END IF
        integral = real(integral * (i-1)) / i + (x(i) - a * b)**2 / i 
        ! 按步求平均值，以防止求和溢出
    END DO
    OPEN (1, ACCESS='append', file='integral.dat')
    WRITE (1, *) integral
    CLOSE(1)
    print *, integral
END SUBROUTINE Sample1

SUBROUTINE Sample2(a, b, gam, num)
    REAL(KIND=8) ,INTENT(IN) :: gam
    REAL(KIND=8) :: rand(num), integral
    REAL(KIND=8) xt, x(0:num), r, seed
    INTEGER(KIND=4), INTENT(IN) :: a, b, num
    INTEGER(KIND=4) :: i
    x(0) = 1
    integral = 0
    CALL RANDOM_NUMBER(seed) 
    ! 用FORTRAN自带的随机数生成器生成16807生成器的种子
    CALL Schrage(num, int(2147483647 * seed), rand)
    DO i = 1, num
        xt = - gam * LOG(rand(i))
        r = (xt / x(i-1))**(a - 1) * (xt - a * b)**2 / (x(i-1) - a * b)**2&
            * EXP(-(xt - x(i-1)/b)) * EXP((xt - x(i-1))/gam)
        IF(rand(i) < MIN(1.0, r)) THEN
            x(i) = xt 
        ELSE
            x(i) = x(i-1) ! 若建议未通过，仍保留结果与上一步相同. 
        END IF
        integral = real(integral * (i-1)) / i + 1.0 / i 
        ! 直接求抽样结果x的平均值
    END DO
    OPEN (1, ACCESS='append', file='integral_2.dat')
    WRITE (1, *) integral
    CLOSE(1)
    print *, integral
END SUBROUTINE Sample2
END MODULE Metropolis

SUBROUTINE Schrage(num, z0, rand)
    !Schrage随机数生成器子程序,将均匀随机数序列存放在数组rand中
    IMPLICIT NONE
    INTEGER(KIND=4) :: N = 1, num
    INTEGER :: m = 2147483647, a = 16807, q = 127773, r = 2836, In(num), z0
    REAL(KIND=8), INTENT(INOUT) :: rand(num)
    In(1) = z0 !将传入值z0作为种子
    rand(1) = REAL(In(1))/m
    DO N = 1, num - 1
        In(N + 1) = a * MOD(In(N), q) - r * INT(In(N) / q)
        IF (In(N + 1) < 0) THEN !若值小于零，按Schrage方法加m
            In(N + 1) = In(N + 1) + m
        END IF
        rand(N + 1) = REAL(In(N + 1))/m !得到第N+1个随机数
    END DO
END SUBROUTINE Schrage

PROGRAM MAIN
    USE Metropolis
    IMPLICIT NONE
    INTEGER(KIND=4) :: i
    REAL(KIND=8) :: gam
    DO i = 1, 250
        gam = 0.1 * i
        CALL Sample1(3, 2, gam, 1000000)
    END DO
END PROGRAM MAIN
