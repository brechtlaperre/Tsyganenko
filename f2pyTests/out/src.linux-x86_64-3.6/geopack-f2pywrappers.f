C     -*- fortran -*-
C     This file is autogenerated with f2py (version:2)
C     It contains Fortran 77 wrappers to fortran functions.

      subroutine f2pyinitgeopack2(setupfunc)
      external setupfunc
      real g(105)
      real h(105)
      real rec(105)
      common /geopack2/ g,h,rec
      call setupfunc(g,h,rec)
      end

      subroutine f2pyinitgeopack1(setupfunc)
      external setupfunc
      real aa(10)
      real sps
      real cps
      real bb(22)
      common /geopack1/ aa,sps,cps,bb
      call setupfunc(aa,sps,cps,bb)
      end

