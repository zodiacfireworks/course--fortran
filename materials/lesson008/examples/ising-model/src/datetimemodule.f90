MODULE DATETIMEMODULE
    IMPLICIT NONE

    REAL ( KIND = 8 ) :: dif_time
    REAL ( KIND = 8 ) :: tic_time
    REAL ( KIND = 8 ) :: toc_time

    INTEGER ( KIND = 4 ) :: current_time(8)

    CONTAINS
        !===========================================================================================
        FUNCTION DTTime( ) RESULT( now_time )
            INTEGER( KIND = 4 ) :: now_time(4)

            CALL DATE_AND_TIME( VALUES = current_time )

            now_time(1) = current_time(5)
            now_time(2) = current_time(6)
            now_time(3) = current_time(7)
            now_time(4) = current_time(8)

        END FUNCTION DTTime

        !===========================================================================================
        FUNCTION DTTime2Str( int_time, str_fmt ) RESULT( str_time )
            INTEGER( KIND = 4 ) :: int_time(4)
            CHARACTER( LEN = 3 ) :: str_fmt
            CHARACTER( LEN = 4 ) :: fmt_ind
            CHARACTER( LEN = 18 ) :: str_time

            IF ( str_fmt .EQ. 'hrs' ) THEN
                fmt_ind = 'hrs.'
            ELSE IF ( str_fmt .EQ. 'apm' ) THEN
                IF( int_time(1) .LT. 12 ) THEN
                    fmt_ind = 'a.m.'
                ELSE IF ( int_time(1) .EQ. 12 ) THEN
                    IF( int_time(2) .EQ. 0 .AND. int_time(3) .EQ. 0 ) THEN
                        fmt_ind = 'a.m.'
                    ELSE
                        fmt_ind = 'p.m.'
                    END IF
                ELSE IF ( int_time(1) .GE. 13 ) THEN
                    int_time(1) = int_time(1) - 12;
                    fmt_ind = 'p.m.'
                END IF
            ELSE
                WRITE(*,*) 'Warning:'
                WRITE(*,*) 'Format specifier not valid.'
                WRITE(*,*) 'Using "hrs." Format specifier by default'
                fmt_ind = 'hrs.'
            END IF

            WRITE( str_time, '(i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a4)' ) &
            int_time(1), ':', int_time(2), ':',            &
            int_time(3), '.', int_time(4), fmt_ind

        END FUNCTION DTTime2Str

        !===========================================================================================
        FUNCTION DTDate( ) RESULT( now_date )
            INTEGER( KIND = 4 ) :: now_date(3)

            CALL DATE_AND_TIME( VALUES = current_time )

            now_date(1) = current_time(3)
            now_date(2) = current_time(2)
            now_date(3) = current_time(1)

        END FUNCTION DTDate

        !===========================================================================================
        FUNCTION DTDate2Str( int_date, str_fmt ) RESULT( str_date )
            INTEGER( KIND = 4 ) :: int_date(3)
            CHARACTER( LEN = 3 ) :: str_fmt
            CHARACTER( LEN = 3 ) :: str_month
            CHARACTER( LEN = 11 ) :: str_date

            IF ( str_fmt .EQ. 'ltr' ) THEN
                SELECT CASE( int_date(2) )
                    CASE(  1 )
                        str_month = 'Jan'
                    CASE(  2 )
                        str_month = 'Feb'
                    CASE(  3 )
                        str_month = 'Mar'
                    CASE(  4 )
                        str_month = 'Apr'
                    CASE(  5 )
                        str_month = 'May'
                    CASE(  6 )
                        str_month = 'Jun'
                    CASE(  7 )
                        str_month = 'Jul'
                    CASE(  8 )
                        str_month = 'Aug'
                    CASE(  9 )
                        str_month = 'Sep'
                    CASE( 10 )
                        str_month = 'Oct'
                    CASE( 11 )
                        str_month = 'Nov'
                    CASE( 12 )
                        str_month = 'Dec'
                END SELECT

                WRITE( str_date, '(i2.2,1x,a3,1x,i4)' ) &
                int_date(1), str_month, int_date(3)

            ELSE
                IF ( str_fmt .NE. 'num' ) THEN
                    WRITE(*,*) 'Warning:'
                    WRITE(*,*) 'Format specifier not valid.'
                    WRITE(*,*) 'Using "num" Format specifier by default'
                END IF

                WRITE( str_date, '(i2.2,a1,i2.2,a1,i4)' ) &
                int_date(1), '.', int_date(2), '.', int_date(3)
            END IF
        END FUNCTION DTDate2Str

        !===========================================================================================
        FUNCTION TimeStampStr( stmp_fmt ) RESULT ( stmp_str )
            INTEGER( KIND = 4 ) :: stmp_time(4)
            INTEGER( KIND = 4 ) :: stmp_date(3)

            CHARACTER( LEN = 4 ) :: stmp_fmt

            CHARACTER( LEN = : ) , ALLOCATABLE :: stmp_str

            stmp_time = DTTime( )
            stmp_date = DTDate( )

            IF ( stmp_fmt .EQ. 'flat' ) THEN
                ALLOCATE( CHARACTER( LEN = 15 ) :: stmp_str )

                WRITE( stmp_str, '(i4.4,2i2.2,a1,3i2.2)') &
                stmp_date(3), stmp_date(2), stmp_date(1), '_', &
                stmp_time(1), stmp_time(2), stmp_time(3)
            ELSE
                IF ( stmp_fmt .NE. 'full' ) THEN
                    WRITE(*,*) 'Warning:'
                    WRITE(*,*) 'Format specifier not valid.'
                    WRITE(*,*) 'Using "num" Format specifier by default'
                END IF

                ALLOCATE( CHARACTER( LEN = 30 ) :: stmp_str )

                WRITE( stmp_str, '(a11,1x,a18)') &
                DTDate2Str( stmp_date , 'ltr' ), &
                DTTime2Str( stmp_time , 'hrs')
            END IF

        END FUNCTION TimeStampStr

        !===========================================================================================
        SUBROUTINE TimeStamp( )
            INTEGER( KIND = 4 ) :: stmp_time(4)
            INTEGER( KIND = 4 ) :: stmp_date(3)

            stmp_time = DTTime( )
            stmp_date = DTDate( )

            WRITE(*,'(a12,1x,a2,1x,a18)') DTDate2Str( stmp_date, 'ltr' ) , &
                                          'at',                            &
                                          DTTime2Str( stmp_time, 'hrs' )

            RETURN
        END SUBROUTINE TimeStamp

        !===========================================================================================
        SUBROUTINE DTTic( )
            CALL CPU_TIME( tic_time )

            RETURN
        END SUBROUTINE DTTic

        !===========================================================================================
        SUBROUTINE DTToc( )
            CHARACTER(LEN = 30) :: str_dif_time

            CALL CPU_TIME( toc_time )

            dif_time = toc_time - tic_time

            WRITE (str_dif_time,'(f30.3)') dif_time
            str_dif_time = adjustl(str_dif_time)

            WRITE (*,*) 'The process have been performed in ', &
                        TRIM( str_dif_time ), ' sec.'

            RETURN
        END SUBROUTINE DTToc

END MODULE DATETIMEMODULE
