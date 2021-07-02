program main
    use::iso_fortran_env
    implicit none

    integer,allocatable::primes(:)

    primes = generate_prime(100)

    print *, primes

contains 
    function generate_prime(num) result(primes)
        implicit none
        integer,intent(in)::num
        integer,allocatable::primes(:)
        integer,allocatable::number(:)

        integer::i,j,prime_candidate
        integer::num_nonzero

        if (num >= 2)then
            allocate(number(2:num))
            
            ! 初期化
            do i=2,num
                number(i) = i
            enddo

            ! ふるい
            do prime_candidate=2,nint(num/2d0)
                if (number(prime_candidate) /= 0)then
                    do i = prime_candidate + prime_candidate,num,prime_candidate
                        number(i) = 0
                    enddo
                endif
            enddo

            ! ぬく  
            num_nonzero = 0
            do i=2,num
                if(number(i) /= 0) num_nonzero = num_nonzero + 1
            enddo

            allocate(primes(num_nonzero))

            j = 1
            do i = 2, num
                if(number(i) /= 0)then
                    primes(j) = number(i)
                    j = j + 1
                endif
            enddo
        else
            allocate(primes(0))
        endif

    end function

end program