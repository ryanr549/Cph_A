MODULE RW
IMPLICIT NONE
    
CONTAINS
SUBROUTINE Generator(particles, steps, omega, k, t0, filename) !随机行走生成器子程序
    INTEGER(KIND=4), INTENT(IN) :: particles, steps, t0
    CHARACTER(LEN=*), INTENT(IN) :: filename
    REAL(KIND=4), INTENT(IN) :: omega, k
    INTEGER(KIND=4) :: i, j, v(steps, particles, 2), &
        vdotproduct(2:steps, particles)
    REAL(KIND=8) :: seed, dat(steps * particles * 2), C(2:steps)
    
    PRINT *, 'Running...Please wait...maybe it will take a few minutes:)'
    CALL RANDOM_NUMBER(seed) !用FORTRAN自带的随机数生成器生成16807生成器的种子
    CALL Schrage(2 * particles * steps, int(2147483647 * seed), 'rand.dat')
    OPEN (1, file='rand.dat')
    READ (1, *) dat
    CLOSE (1)
    DO i = 1, steps 
        DO j = 1, particles
            !对速度y分量抽样
            IF(dat(2 * i * j) < 0.5) THEN
                v(i, j, 2) = -1
            ELSE
                v(i, j, 2) = 1
            END IF
            !对速度x分量抽样
            IF(dat(2 * i * j - 1) < 0.5 - k * SIN(omega * (i + t0))) THEN
                v(i, j, 1) = -1
            ELSE
                v(i, j, 1) = 1
            END IF
        END DO
    END DO
    DO i = 2, steps
        DO j = 1, particles
        vdotproduct(i, j) = DOT_PRODUCT(v(i, j, :), v(1, j, :))
        END DO
        !对多个粒子求点积的期望值，即为自相关函数值
        C = real(SUM(vdotproduct, DIM=2)) / particles 
    END DO
    OPEN (1, file=filename)
    WRITE (1, *) C !将速度自相关函数值写入文件
    CLOSE (1)
    PRINT *, 'Finished!'
END SUBROUTINE Generator
END MODULE RW

SUBROUTINE Schrage(num, z0, filename) !Schrage随机数生成器子程序
    IMPLICIT NONE
    INTEGER(KIND=4) :: N = 1, num
    INTEGER :: m = 2147483647, a = 16807, q = 127773, r = 2836, In(num), z0
    REAL(KIND=8) :: z(num)
    CHARACTER(LEN=8) :: filename
    In(1) = z0 !将传入值z0作为种子
    z(1) = REAL(In(1))/m
    DO N = 1, num - 1
        In(N + 1) = a * MOD(In(N), q) - r * INT(In(N) / q)
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
    USE RW
    CALL Generator(100000, 200, 0.3, 0.5, 30, 'scor1.dat')
    CALL Generator(100000, 200, 0.5, 0.2, 2, 'scor2.dat')
    CALL Generator(100000, 200, 0.15, 0.3, 50, 'scor3.dat')
END PROGRAM MAIN
