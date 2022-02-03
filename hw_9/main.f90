MODULE DscSample !离散抽样模块
IMPLICIT NONE
CONTAINS

SUBROUTINE Poi(N, num, filename) !泊松分布的抽样以及统计量计算
    INTEGER(KIND=4) :: k, N, i, j, l, num
    REAL(KIND=8), DIMENSION(num * N) :: dat
    REAL(KIND=8), DIMENSION(num) :: g, xbar
    REAL(KIND=8), DIMENSION(num, N) :: x
    REAL(KIND=8), DIMENSION(0:8) :: sumprob
    REAL(KIND=8) :: maximum
    CHARACTER(LEN=*) :: filename
    CALL Schrage(num * N, 98412123, 'rand.dat') !共需要(n*N)个随机数
    OPEN (1, file='rand.dat')
    READ (1, *) dat
    CLOSE (1)
    sumprob(0) = f(0)
    DO i = 1, 8
        sumprob(i) = sumprob(i - 1) + f(i) !求分段节点
    END DO
    maximum = sumprob(8) 
    sumprob = sumprob / maximum !重新归一化
    DO i = 1, num
        DO j = 1, N
            DO l = 0, 8
                IF(dat((i * N - j + 1)) < sumprob(l)) THEN
                    x(i, j) = l !在指定概率区间内则取相应值
                    EXIT
                END IF
            END DO
        END DO
    END DO
    xbar = SUM(x, DIM=2) / N !通过数组运算求出每次循环中的平均值   
    g = SQRT(real(N) / 2) * (xbar - 2) !构造统计量
    OPEN (1, file=trim(filename))
    WRITE (1, *) g
    CLOSE (1)
    CONTAINS
        FUNCTION f(t) !Poisson分布密度函数表达式
            REAL(KIND=8) :: f
            INTEGER(KIND=4) :: t, y, m
            y = 1 !给阶乘值赋初值1
            IF(t .NE. 0) THEN
                DO m = 1, t
                    y = y * m !求阶乘
                END DO
            END IF
            f = (2**t * EXP(-2.0)) / y
        END FUNCTION f  
END SUBROUTINE Poi

SUBROUTINE Mydsc(N, num, filename) !自设离散分布的抽样与统计量计算
    INTEGER(KIND=4) :: k, N, i, j, l, num
    REAL(KIND=8), DIMENSION(num * N) :: dat
    REAL(KIND=8), DIMENSION(num) :: g, xbar
    REAL(KIND=8), DIMENSION(num, N) :: x
    REAL(KIND=8), DIMENSION(1:5) :: sumprob
    CHARACTER(LEN=*) :: filename
    CALL Schrage(num * N, 98412123, 'rand.dat') !共需要(n*N)个随机数
    sumprob(1) = 1.0 / 5
    DO i = 2, 5
        sumprob(i) = sumprob(i - 1) + 1.0 / 5
    END DO
    DO i = 1, num
        DO j = 1, N
            DO l = 1, 5
                IF(dat((i * N - j + 1)) < sumprob(l)) THEN
                    x(i, j) = l !在指定概率区间内则取相应值
                    EXIT
                END IF
            END DO
        END DO
    END DO
    xbar = SUM(x, DIM=2) / N !通过数组运算求出每次循环中的平均值   
    g = (xbar - 3) * SQRT(real(N) / 2) !计算统计量
    OPEN (1, file=filename)
    READ (1, *) g
    CLOSE (1)
END SUBROUTINE Mydsc
END MODULE DscSample

MODULE Sample !连续抽样模块
IMPLICIT NONE
    
CONTAINS
SUBROUTINE Expt(N, num, filename) !指数分布抽样与统计量计算
    INTEGER(KIND=4) :: N, num, i, j
    CHARACTER(LEN=*) :: filename
    REAL(KIND=8), DIMENSION(num * N) :: dat
    REAL(KIND=8), DIMENSION(num) :: g, xbar
    REAL(KIND=8), DIMENSION(num, N) :: x
    CALL Schrage(num * N, 7521233, 'rand.dat')
    OPEN (1, file='rand.dat')
    READ (1, *) dat
    CLOSE (1)
    DO i = 1, num
        DO j = 1, N
            x(i, j) = f(dat(i * N - j + 1)) !对0到1间均匀随机数直接抽样
        END DO
    END DO  
    xbar = SUM(x, DIM=2) / N !通过数组运算求出每次循环中的平均值   
    g = SQRT(real(N)) * (xbar - 1)
    OPEN (1, file=filename)
    WRITE (1, *) g
    CLOSE (1)
    CONTAINS
        FUNCTION f(t) !指数分布抽样函数
            REAL(KIND=8) :: f, t
            f = - LOG(t) !对f进行积分得到x平均值
        END FUNCTION f  
END SUBROUTINE Expt

SUBROUTINE Myctn(N, num, filename) !自设连续分布抽样与统计量计算
    INTEGER(KIND=4) :: N, num, i, j
    CHARACTER(LEN=*) :: filename
    REAL(KIND=8), DIMENSION(num * N) :: dat
    REAL(KIND=8), DIMENSION(num) :: g, xbar
    REAL(KIND=8), DIMENSION(num, N) :: x
    CALL Schrage(num * N, 64563218, 'rand.dat')
    OPEN (1, file='rand.dat')
    READ (1, *) dat
    CLOSE (1)
    DO i = 1, num
        DO j = 1, N
            x(i, j) = f(dat(i * N - j + 1)) !对0到1间均匀随机数直接抽样
        END DO
    END DO  
    xbar = SUM(x, DIM=2) / N !通过数组运算求出每次循环中的平均值   
    g = 2 * SQRT(real(N) / 5) * (3 * xbar - 1)
    OPEN (1, file=filename)
    WRITE (1, *) g
    CLOSE (1)
    CONTAINS
        FUNCTION f(t) !定义抽样函数
            REAL(KIND=8) :: f, t
            f = SQRT(2 * t)
        END FUNCTION f
END SUBROUTINE Myctn
END MODULE Sample

SUBROUTINE Schrage(num, z0, filename) !Schrage随机数生成器子程序
    IMPLICIT NONE
    INTEGER(KIND=4) :: N = 1, num
    INTEGER :: m = 2147483647, a = 16807, q = 127773, r = 2836, In(num), z0
    REAL(KIND=8) :: z(num)
    CHARACTER(LEN=8) :: filename
    In(1) = z0 !将传入值z0作为种子
    z(1) = REAL(In(1))/m
    DO N = 1, num - 1
        In(N + 1) = a*MOD(In(N), q) - r*INT(In(N)/q)
        IF (In(N + 1) < 0) THEN !若值小于零，按Schrage方法加m
            In(N + 1) = In(N + 1) + m
        END IF
        z(N + 1) = REAL(In(N + 1))/m !得到第N+1个随机数
    END DO
    OPEN (1, file=trim(filename)) !每次运行子程序按照传入参数filename生成数据文件
    DO N = 1, num !将随机数按行存入文件
        WRITE (1, *) z(N)
    END DO
    CLOSE (1)
END SUBROUTINE Schrage

PROGRAM MAIN
    USE DscSample
    USE Sample
    IMPLICIT NONE
    INTEGER(KIND=4) :: i, j
    CHARACTER(LEN=1) :: chari 
    DO i = 2, 6, 2
        WRITE (chari, "(I1)") i !将整型数值转化为字符型便于写入文件名
        CALL Poi(2, 10**i, 'poi_2_' // chari // '.dat') 
        CALL Poi(5, 10**i, 'poi_5_' // chari // '.dat')
        CALL Poi(10, 10**i, 'poi_10_' // chari // '.dat')
        WRITE (chari, "(I1)") i
        CALL Expt(2, 10**i, 'expt_2_' // chari // '.dat') 
        CALL Expt(5, 10**i, 'expt_5_' // chari // '.dat')
        CALL Expt(10, 10**i, 'expt_10_' // chari // '.dat')
        WRITE (chari, "(I1)") i
        CALL Poi(2, 10**i, 'mydsc_2_' // chari // '.dat') 
        CALL Poi(5, 10**i, 'mydsc_5_' // chari // '.dat')
        CALL Poi(10, 10**i, 'mydsc_10_' // chari // '.dat')
        WRITE (chari, "(I1)") i
        CALL Poi(2, 10**i, 'myctn_2_' // chari // '.dat') 
        CALL Poi(5, 10**i, 'myctn_5_' // chari // '.dat')
        CALL Poi(10, 10**i, 'myctn_10_' // chari // '.dat')
    END DO
END PROGRAM MAIN
