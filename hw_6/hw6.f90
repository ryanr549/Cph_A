MODULE Sam !抽样程序模块
    IMPLICIT NONE
    REAL(KIND=8) ,PARAMETER :: S = 1.56463163641954 !为了定义S这一全局变量使用模块
    REAL(KIND=8) ,PARAMETER :: PI = 3.1415926

    CONTAINS
    FUNCTION F(x)
        REAL(KIND=8) :: F, x
        F = 1 / (1 + 4 * x**4)
    END FUNCTION F
    FUNCTION P(x)
        REAL(KIND=8) :: P, x
        P = exp(-PI * x**2)
    END FUNCTION P
    FUNCTION G(x) !用函数定义g(x)表达式
        REAL(KIND=8) :: G, x
        G = log((2 * x**2 + 2 * x + 1)/(2 * x**2 - 2 * x + 1)) / 8 + atan(1 - 2 * x) / 4 + atan(1 + 2 * x) / 4
    END FUNCTION G
    FUNCTION H(x, xi_1) !定义要求解的方程左端h(x)
        REAL(KIND=8) :: xi_1, H, x
        H = G(x) - G(-3.0_8) - S * xi_1
    END FUNCTION H

    FUNCTION Sec(x0, x1, err, xi_1) !弦截法(Secant Method)求方程数值解
        REAL(KIND=8) ,INTENT(IN) :: err, x0, x1, xi_1
        REAL(KIND=8) :: Sec, tmp, tmpx0, tmpx1, diff
        INTEGER(KIND=8) :: i
        tmpx0 = x0
        tmpx1 = x1
        diff = x1 - x0
        DO WHILE (diff > err)
            tmp = tmpx1
            tmpx1 = tmpx1 - (tmpx1 - tmpx0) * H(tmpx1, xi_1) / (H(tmpx1, xi_1) - H(tmpx0, xi_1))
            tmpx0 = tmp
            diff = tmpx1 - tmpx0
        END DO
        Sec = tmpx1
    END FUNCTION Sec

    SUBROUTINE Sample(n) !进行舍选法抽样
        REAL(KIND=8) ,DIMENSION(10**n) :: x, y, xi_x, xi_y
        INTEGER(KIND=4) :: n, i
        REAL(KIND=8), DIMENSION(10**n) :: z !数组用于存放抽样结果
        OPEN (1, file='x.dat')
        READ (1, *) x
        CLOSE (1)
        OPEN (1, file='y.dat')
        READ (1, *) y
        CLOSE (1)
        DO i = 1, 10**n
            xi_x(i) = Sec(-3.0_8, 3.0_8, 0.00001_8, x(i)) !全部按照种别值为8传入参数
            print *, xi_x(i)
            IF (y(i) * F(xi_x(i)) < P(xi_x(i))) THEN
                z(i) = xi_x(i)
            END IF
        END DO
    END SUBROUTINE Sample
END MODULE Sam

SUBROUTINE Schrage(P, z0, filename) !Schrage随机数生成器子程序
    IMPLICIT NONE
    INTEGER :: N = 1, P
    INTEGER :: m = 2147483647, a = 16807, q = 127773, r = 2836, In(10**P), z0
    REAL(KIND=8) :: z(10**P)
    CHARACTER(LEN=5) :: filename
    In(1) = z0 !将传入值z0作为种子
    z(1) = REAL(In(1))/m
    DO N = 1, 10**P - 1
        In(N + 1) = a*MOD(In(N), q) - r*INT(In(N)/q)
        IF (In(N + 1) < 0) THEN !若值小于零，按Schrage方法加m
            In(N + 1) = In(N + 1) + m
        END IF
        z(N + 1) = REAL(In(N + 1))/m !得到第N+1个随机数
    END DO
    OPEN (1, file=filename) !每次运行子程序按照传入参数filename生成数据文件
    DO N = 1, 10**P !将随机数按行存入文件
        WRITE (1, *) z(N)
        END DO
    CLOSE (1)
END SUBROUTINE Schrage

PROGRAM MAIN

    USE Sam
    INTEGER(KIND=4) :: intI = 4
    CALL Schrage(intI, 84651212, 'x.dat') !产生一对[0,1]之间均匀分布的随机数
    CALL Schrage(intI, 16545214, 'y.dat')
    CALL Sample(intI)
END PROGRAM MAIN
