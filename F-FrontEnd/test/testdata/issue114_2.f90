MODULE mod1
IMPLICIT NONE

PRIVATE

CONTAINS 

  SUBROUTINE sub1()

  IMPLICIT NONE

  INTEGER :: ip,k,ke,ipend,itype

  !$acc data                                          &
  !$acc present(ptot_b,pp_nx_b,p0_b,dz_b,hhl_b,qrs_b) &
  !$acc present(qr_nx_b,qs_nx_b,qi_nx_b,qg_nx_b)

  !$acc parallel
  DO k = 1, ke
    !$acc loop gang vector
     DO ip=1,ipend
     END DO
  END DO
  !$acc end parallel
  
  SELECT CASE (itype)      
    CASE(3) 
      !$acc parallel
      DO k=1,ke
      !$acc loop gang vector
         DO ip=1,ipend
         END DO
       END DO
      !$acc end parallel
    CASE(4)  
     !$acc parallel
     DO k=1,ke
       !$acc loop gang vector
       DO ip=1,ipend
       END DO
     END DO
     !$acc end parallel  
  END SELECT
  !$acc end data

  END SUBROUTINE sub1

END MODULE mod1
