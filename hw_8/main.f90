MODULE integrate !聚合两个积分程序的模块
    IMPLICIT NONE
    CONTAINS
        FUNCTION f(x) !定义被积函数
            REAL(KIND=8) :: f, x
            f = SQRT(x + 2 * SQRT(x))
        END FUNCTION f
        FUNCTION g(x, y, z, u, v)
            REAL(KIND=8) :: g, x, y, z, u, v
            g = 5 - x**2 + y**2 - z**2 + u**3 - v**3
        END FUNCTION g
        
        SUBROUTINE integrate_1(n) !一维Monte Carlo积分子程序
            REAL(KIND=8) ,DIMENSION(10**n):: x
            REAL(KIND=8) :: ave, res
            INTEGER(KIND=4) :: i, n
            CALL Schrage(n, 54123654, 'rand.dat') !用16807产生器产生一定数目的均匀随机数
            OPEN (1, file='rand.dat')
            READ (1, *) x
            CLOSE (1)
            ave = f(5 * x(1)) !将0到1间随机数x转换为0到5间均匀分布随机数
            DO i = 2, 10**n
                ave = ave + (f(5 * x(i)) - ave) / i !为防止溢出调整了平均值求法
            END DO
            res = 5 * ave !积分计算结果
            print *, '1d', res
        END SUBROUTINE integrate_1

        SUBROUTINE integrate_2(n) !五维Monte Carlo积分子程序
            REAL(KIND=8) ,DIMENSION(10**(n + 1)) :: dat
            REAL(KIND=8) ,DIMENSION(10**n, 5) :: x
            REAL(KIND=8) :: ave, res
            INTEGER(KIND=4) :: i, j, n
            CALL Schrage(n + 1, 8455214, 'rand.dat') !用16807产生器产生一定数目的均匀随机数
            OPEN (1, file='rand.dat')
            READ (1, *) dat
            CLOSE (1)
            DO i = 1, 10**n
                DO j = 1, 5
                    x(i, j) = dat(5 * i + j) !每隔5个数从前面产生的随机数序列中取一个值，对应5个维度
                END DO
            END DO
            ave = g((7.0/10) * x(1, 1), (4.0/7) * x(1, 2), &
                (9.0/10) * x(1, 3), 2.0 * x(1, 4), (13/11) * x(1, 5))
            DO i = 2, 10**n
                ave = ave + (g((7.0/10) * x(i, 1), (4.0/7) * x(i, 2), &
                    (9.0/10) * x(i, 3), 2.0 * x(i, 4), (13/11) * x(i, 5)) - ave) / i
            END DO
            res = (7.0/10) * (4.0/7) * (9.0/10) * 2 * (13.0/11) * ave
            print *, '5d', res
        END SUBROUTINE integrate_2
END MODULE integrate
SUBROUTINE Schrage(P, z0, filename) !Schrage随机数生成器子程序
    IMPLICIT NONE
    INTEGER :: N = 1, P
    INTEGER :: m = 2147483647, a = 16807, q = 127773, r = 2836, In(10**P), z0
    REAL(KIND=8) :: z(10**P)
    CHARACTER(LEN=8) :: filename
    In(1) = z0 !将传入值z0作为种子
    z(1) = REAL(In(1))/m
    DO N = 1, 10**P - 1
        In(N + 1) = a*MOD(In(N), q) - r*INT(In(N)/q)
        IF (In(N + 1) < 0) THEN !若值小于零，按Schrage方法加m
            In(N + 1) = In(N + 1) + m
        END IF
        z(N + 1) = REAL(In(N + 1))/m !得到第N+1个随机数
    END DO
    OPEN (1, file=trim(filename)) !每次运行子程序按照传入参数filename生成数据文件
    DO N = 1, 10**P !将随机数按行存入文件
        WRITE (1, *) z(N)
    END DO
    CLOSE (1)
END SUBROUTINE Schrage

PROGRAM MAIN
    USE integrate
    INTEGER(KIND=4) :: i
    DO i = 2, 7
        CALL integrate_1(i)
    END DO
    DO i = 2, 7
        CALL integrate_2(i)
    END DO
END PROGRAM MAIN
