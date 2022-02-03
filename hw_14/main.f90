MODULE Metropolis
IMPLICIT NONE
CONTAINS
    SUBROUTINE Sample(x0, y0, beta, num, step, filename)
        CHARACTER(LEN=*), INTENT(IN) :: filename
        REAL(KIND=8), INTENT(IN) :: beta, x0, y0, step
        REAL(KIND=8) :: rand(3 * num), xt(2), x(0:num, 2), d, seed
        INTEGER(KIND=4), INTENT(IN) :: num
        INTEGER(KIND=4) :: i
        x(0, 1) = x0
        x(0, 2) = y0 ! 初始化起步点
        CALL RANDOM_NUMBER(seed)
        ! 用FORTRAN自带的随机数生成器生成16807生成器的种子
        CALL Schrage(3 * num, int(2147483647 * seed), rand)
        DO i = 1, num
            xt(1) = x(i-1, 1) + step * (rand(i) - 0.5)
            xt(2) = x(i-1, 2) + step * (rand(2 * i) - 0.5) 
            ! xt储存建议的一步，是否接受取决于随机数的判断
            d = beta * (H(xt(1), xt(2)) - H(x(i-1, 1), x(i-1, 2))) ! 计算能量差d
            IF(d < 0) THEN
                x(i, :) = xt(:) ! 若能量减小则直接接收
            ELSE
                IF(rand(3 * i) < EXP(-d)) THEN
                    x(i, :) = xt(:) 
                    ! 若能量增加，使用rand(3*i)与Bolzmann因子EXP(-d)比较来进行判断
                ELSE
                    x(i, :) = x(i-1, :) 
                    ! 若前面两次判断都为假，则抽样失败，点与上一个点相同
                END IF
            END IF
        END DO
        OPEN (1, file=filename)
        WRITE (1, *) x
        CLOSE (1)
    END SUBROUTINE Sample
    
    SUBROUTINE Integrate(num, filename)
        CHARACTER(LEN=*) :: filename
        INTEGER(KIND=4) :: num
        INTEGER(KIND=4) :: i
        REAL(KIND=8), DIMENSION(0:num, 2) :: x
        REAL(KIND=8) :: i1, i2, i3
        OPEN (1, file=filename)
        READ (1, *) x
        CLOSE (1)
        i1 = 0
        i2 = 0
        i3 = 0
        DO i = 1, num
            i1 = real(i1 * (i-1)) / i + x(i, 1)**2 / i
            i2 = real(i2 * (i-1)) / i + x(i, 2)**2 / i
            i3 = real(i3 * (i-1)) / i + (x(i, 1)**2 + x(i, 2)**2) / i
            ! 按步更新平均值，可防止求和溢出
        END DO
        print *, 'i1 = ', i1
        print *, 'i2 = ', i2
        print *, 'i3 = ', i3
    END SUBROUTINE Integrate
    
    REAL(KIND=8) FUNCTION H(x, y)
        REAL(KIND=8), INTENT(IN) :: x, y
        H = -2 * (x**2 + y**2) + 0.5 * (x**4 + y**4) + 0.5 * (x - y)**4
    END FUNCTION H
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
    CALL Sample(10.0_8, -10.0_8, 0.2_8, 100000, 1.0_8, '0_2.dat')
    print *, 'beta = 0.2:'
    CALL Integrate(100000, '0_2.dat')
    CALL Sample(10.0_8, -10.0_8, 1.0_8, 100000, 1.0_8, '1_0.dat')
    print *, 'beta = 1.0:'
    CALL Integrate(100000, '1_0.dat')
    CALL Sample(10.0_8, -10.0_8, 5.0_8, 100000, 5.0_8, '5_0.dat')
    print *, 'beta = 5.0:'
    CALL Integrate(100000, '5_0.dat')
END PROGRAM MAIN
