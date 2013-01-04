!
! Confidence interval following negative tests.
!
! Designed by William Dupont
! Coded by Dale Plummer
!
! June 14, 2012
!
! =========================================================================

	real function f(x,k,n,r,t,sigval)
!
! This is the function we want to find the 0 for.
! This function should return the value of the function at X
!
	implicit none
	real x
	integer k 		! number of waves of testing
	integer n 		! number of mouse cages in colony
	real r 			! growth rate
	real t 			! time between testing waves
	real sigval

	integer i


	f=1.
	do i=1,k
		f=f* ( (1.-x*(1.+r)**(t*(i-1.)))**n )
	end do
	f=f-1./sigval

	return
	end
	
! =========================================================================

	real function dfdx(x,k,n,r,t,sigval)
!
! The first derivative of f(x)
! This function should return the derivative of the function at X
!
	implicit none
	real x
	integer k 		! number of waves of testing
	integer n 		! number of mouse cages in colony
	real r 			! growth rate
	real t 			! time between testing waves
	real sigval

	integer i
	real f
	external f
	real f_result
	real a,b,c,a1

	dfdx=0.
	do i=1,k
		f_result=f(x,k,n,r,t,sigval)

		a1=(1.-x*(1.+r)**(t*(i-1.)))**n

		a=(f_result+(1/sigval))/a1

		b=(1.-x*(1.+r)**(t*(i-1.)))**(n-1.)
		
		c=(1+r)**(t*(i-1.))

		dfdx=dfdx + (-a*n*b*c)
	end do

	return
	end
	
! =========================================================================

	subroutine ci_following_neg_tests(k,n,r,t,cl,upperlim)
	implicit none
!
! Arguments to this subroutine.
!
	integer k		! number of waves of testing
	integer n		! number of mouse cages in colony
	real r			! growth rate
	real t			! time between testing waves
	real cl			! confidence lever (.95 or .99)
	integer status		! status indicator returned
	real upperlim		! upper confidence limit returned
!
! Functions.
!
	real f, dfdx		! function to be evaluated
	external f, dfdx	! derivative of f
!
! Parameters and intermediate variables for Newton-Raphson method.
!
 	REAL, PARAMETER :: TOLERANCE = .0001      ! tolerance for near-zero 
 	INTEGER, PARAMETER :: ITERMX = 1000       ! maximum number of iterations 
 	REAL X                                    ! current x 
 	REAL FVALUE                               ! current value of function f 
 	INTEGER :: ITER = 0                       ! current iteration number 

	real sigval

!
! Possible error return values:
!
! status=-1 ==> did not converge
! status=-2 ==> bad cl value
! status=-3 ==> Newton-Raphson not behaving
!
! cl must be .95 or .99, no exceptions.
!
	if (cl .eq. 0.95) then
		sigval=6.8259358
	else if (cl .eq. 0.99) then
		sigval=27.589859
	else
		upperlim=-2
		return
	end if
!
! Finds a root of the equation f(x) = 0 by the Newton-Raphson method 	
! See http://personalpages.manchester.ac.uk/staff/david.d.apsley/lectures/fortran/week3.pdf
!
! Set initial quess.
!
	x=0.0
		
	fvalue = f( x,k,n,r,t,sigval )           ! value of function 
	
! Loop until root found or maximum iterations reached 

	iter=0
	do while ( abs( fvalue ) > tolerance .and. iter <= itermx ) 
		x = x - fvalue / dfdx( x,k,n,r,t,sigval )   !update x by newton-raphson formula
		if (x .lt. 0.0 .or. x .gt. 1) then	! check that x is in interval (0,1)
			upperlim=-3
			return
		end if
		fvalue = f( x,k,n,r,t,sigval )         ! update value of function 
		iter = iter + 1                        ! update iteration number 
	end do 
	
! Output answer (or warn if not converged) 

	if ( abs( fvalue ) > tolerance ) then 
		upperlim=-1
	else 
		upperlim=x
	end if
	return
	end

! =========================================================================
! =========================================================================
!
! Driver program for testing purposes.
!
	program main
	implicit none

	integer k 		! number of waves of testing
	integer n 		! number of mouse cages in colony
	real r 			! growth rate
	real t 			! time between testing waves
	real cl			! confidence level (.95 or .99)
	
	real upperlim
	integer status

	integer n_array(13)
	real t_array(13)
	real r_array(13)
	integer test
	real sigval
!
! Test set for 1 testing wave.
!
!	n_array=(/25,50,100,200,300,400,500,700,1000,2000,4000,10000,100000/)
!	k=1
!
! Test set for 2 testing wave2.
!
	n_array=(/25,25,25,25,25,25,500,700,1000,2000,4000,10000,100000/)
	k=2
!
! Set confidence level desired.
!
	cl=.99
!
! Test values of t and r.
!
	t_array=(/1,10,1,1,10,10,1,1,1,1,1,1,1/)
	r_array=(/0.0,0.0,.1,.2,.1,.2,.1,.1,.1,.1,.1,.1,.1/)

	write(*,*) " k, cl = ", k, cl
!
! Loop over the test values.
!
	do test=1,13
		n=n_array(test)
		t=t_array(test)
		r=r_array(test)
	
		call ci_following_neg_tests(k,n,r,t,cl,upperlim)
	
		if (status .eq. 0) then
			write(*,20) " k, n, t, r, upperlim =", k,n,t,r,upperlim
20			format(a,i5,x,i6,x,f10.5,x,f10.5,x,f10.5)
		else
			write(*,*) " Error.  Status= ",status
		end if
	
	end do
	write(*,*) "Done!"
	end






