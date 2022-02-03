PROGRAM MAIN
    CALL DscSample(5)
END PROGRAM MAIN

SUBROUTINE Sample(p) !舍选法抽样
    INTEGER(KIND=4) :: dat(228), intensity(114), p , i
    REAL(KIND=8) ,PARAMETER :: C = 0.36 !定义的常数C，是xi_1的分界线
    REAL(KIND=8) ,DIMENSION(10**p) :: xi_1, xi_2, xi_x, x
    OPEN (1, file='data.TXT')
    READ (1, *) dat
    CLOSE (1)
    DO i = 1, 114
        intensity(i) = dat(2 * i) !将data中的y值写入数组，索引i对应energy为(2899+i)eV
    END DO
    OPEN (1, file='xi_1.dat') !将均匀分布的两个随机数序列写入数组
    READ (1, *) xi_1
    CLOSE (1)
    OPEN (1, file='xi_2.dat')
    READ (1, *) xi_2
    CLOSE (1)
    DO i = 1, 10**p
        IF (xi_1(i) < C) THEN
            xi_x(i) = 2900 + (90 + 24 * 38000.0 / 5700) * xi_1(i)
            IF (5700 * xi_2(i) <= intensity(int(xi_x(i)) - 2899)) THEN
                x(i) = xi_x(i) !若上面的判断条件满足则取xi_x(i)作为抽样结果x(i)
            END IF
        ELSE
            xi_x(i) = 2990 + (90 * 5700.0 / 38000 + 24) * xi_1(i) - 90 * 5700.0 / 38000
            IF (38000 * xi_2(i) <= intensity(int(xi_x(i)) - 2899)) THEN
                x(i) = xi_x(i)
            END IF
        END IF
    END DO
    OPEN (1, file='smp.dat')
    DO i = 1, 10**p
        IF(x(i) .ne. 0) THEN
            WRITE (1,*) x(i) !将非零值(成功抽出的值)写入文件
        END IF
    END DO
    CLOSE (1)
END SUBROUTINE Sample

SUBROUTINE DscSample(p) !离散(discrete)直接抽样
    INTEGER(KIND=4) :: p, i, j
    INTEGER(KIND=8) :: S = 0, dat(228) !奇数项dat(2i-1)为x值,偶数项dat(2i)为y值
    REAL(KIND=8) :: sumprob(114) !几率求和值
    REAL(KIND=8), DIMENSION(10**p) :: xi, x
    OPEN (1, file='xi_1.dat') !从其中一个文件导入10^5个[0,1]上均匀分布的随机数
    READ (1, *) xi
    CLOSE (1)
    OPEN (1, file='data.TXT') !读取实验谱数据
    READ (1, *) dat
    CLOSE (1)
    DO i = 1, 114
        S = S + dat(2 * i) !求粒子总数
    END DO
    sumprob(1) = real(dat(2)) / S !使用real()函数转换为实型使除法有意义
    DO i = 2, 114
        sumprob(i) = sumprob(i - 1) + real(dat(2 * i)) / S
    END DO

    DO i = 1, 10**p
        DO j = 1, 114
            IF(xi(i) < sumprob(j)) THEN
                x(i) = dat(2 * j - 1)
                EXIT
            END IF
        END DO
    END DO
    OPEN (1, file='dscsmp.dat')
    WRITE (1, *) x
    CLOSE (1)
END SUBROUTINE DscSample




