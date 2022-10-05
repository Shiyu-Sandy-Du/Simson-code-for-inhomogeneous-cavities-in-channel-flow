c ***********************************************************************
c
c $HeadURL$
c $LastChangedDate$
c $LastChangedBy$
c $LastChangedRevision$
c
c ***********************************************************************
      subroutine locf(om2r,om2i,yb,xl,zl,xsc,zsc,eta,tc,
     &     loctyp,fp1,fpds1,fpds2,fpds3,fpds4,fpds5,fpds6,fpds7,fpds8,
     &     fpdds1,fpdds2,fpdds3,fpdds4,fpdds5,g1,g2,
     &     th2r,th2i,u2r,u2i)

c
c     Localized forcing
c
c==== loctyp=1:
c
c     Adding a volume force of the form
c     F=(ampx,ampy,ampz)*exp(-(y/yscale)**2)*g(x,z)*f(t)
c
c     zscale>0   g(x,z)=exp(-(x-xloc0)/xscale**2-(z/zscale)**2)
c     zscale<0   g(x,z)=exp(-(x-xloc0)/xscale**2)*cos((z-x*lskew)/zscale*2*pi)
c
c     tscale>0 f(t) is a smooth turn on   : f(t)=exp(-(t/tscale)**2)
c     tscale<0 f(t) is a smooth turn off  : f(t)=step(-t/tscale))
c     tscale=0 f(t)=1.
c
c     where step is defined in step.f
c
c     the volume force is only calculated if locfor is true
c     and the time is in the interval [0-5 tscale] or tscale<0
c
c
c==== loctyp=2:
c
c     Adding a volume force of the form
c     F=(ampx,ampy,ampz).*(g(1,z't),g(2,z't),g(3,z't))**fy(y),fx(x)
c
c     g(1,z't)=cos(zbet*z)*cos(tomeg*t)/(2*tomeg)
c     g(2,z't)=cos(zbet*z)*sin(tomeg*t)
c     g(3,z't)=-sin(zbet*z)*sin(tomeg*t)/(2*zbet)
c
c     fx(x)=step((x-xstart)/xrise)-step((x-xend)/xfall+1)
c
c     fy(y)=step((y-ystart)/yrise)-step((y-yend)/yfall+1)
c
c     where step is defined in step.f
c
c==== loctyp=3:
c
c     Adding a volume force of the form
c     F=(ampx,ampy,ampz)*exp(-(y/yscale)**2)*fx(x)*f(t)
c
c     xtype=0  f(x)=                 exp(-((x-xloc0)/xscale)**2)
c     xtype=1  f(x)=(x-xloc0)/xscale*exp(-((x-xloc0)/xscale)**2)
c
c     f(t)=sin(tomeg*t)
c
c==== loctyp=4:
c
c     Adding a volume force of the form
c     F=(ampx,ampy,ampz)*exp(-y/yscale)*ft(t)
c
c     f(t)=(step((t-tstart)/tscale))-step((t-tend)/tscale+1))*cos(tomeg*t)
c
c==== loctyp=5:
c
c     Related to loctyp=1
c
c     adding a volume force of the form
c     F=(ampx,ampy,ampz)*exp(-(y/yscale)**2)*g(x,z)*f(t)
c
c
c     zscale>0   g(x,z)=exp(-(x-xloc0)/xscale**2-(z/zscale)**2)
c     zscale<0   g(x,z)=cos((z-x*lskew)/zscale*2*pi)*exp(-(x-xloc0)/xscale**2)
c
c     tscale>0 f(t) is a smooth turn on   : f(t)=exp(-(t/tscale)**2)
c     tscale<0 f(t) is a smooth turn off  : f(t)=step(-t/tscale))
c     tscale=0 f(t)=1.
c
c     h1(t)=aomeg*sin(tomeg*tc) (note: aomeg is relative amplitude
c     between oscillation and stationary force)
c
c     where step is defined in step.f
c
c     the volume force is only calculated if locfor is true
c     and the time is in the interval [0-5 tscale] or tscale<0
c
c==== loctyp=6:
c
c     Adding a volume force of the form
c     F=(ampx,ampy,ampz)*exp(-(y/yscale)**2)*fx(x)*f(t)
c     useful for TS waves and corresponding secondary instability (K/H-type)
c
c     f(x)=exp(-((x-xloc0)/xscale)**2)
c
c     g(z) = cos(2pi/zl)
c
c     f2d(t)   = sin(tomeg*t)
c     f3d(t) = sin(tomeg3D*t)
c
c     F=(0,1,0)*exp(-(yc/yscale)**2)*(amp2d*f2d(t)*f(x) +
c                                     amp3d*f3d(t)*f(x)*g(z) )
c
c==== loctyp=7:
c
c     Adding a localised forcing of the temperature (line source)
c
c==== loctyp=8:
c
c     Approximated an impulse input
c
c     Adding a volume force of the form
c     F=(ampx,ampy,ampz)*exp(-((y-yloc)/yscale)**2)*fx(x)*f(t)
c
c     xtype=0  f(x)=                 exp(-((x-xloc0)/xscale)**2)
c
c     f(t)=exp(-((t-tstart)/tscale)**2)
c
c==== loctyp=10:
c
c     Variation of loctyp 1:
c     Localized force corresponding to haramonic force described 
c     in Högberg & Henningson, 1998
c
c     Adding a volume force of the form
c     F=(ampx,ampy,ampz)*exp(-(y/yscale)**2)*g(x,z)*f(t)*h1(t)
c
c     zscale>0   g(x,z)=exp(-(x-xloc0)/xscale**2-(z/zscale)**2)
c     zscale<0   g(x,z)=exp(-(x-xloc0)/xscale**2)*cos((z-x*lskew)/zscale*2*pi)
c
c     tscale>0 f(t) is a smooth turn on   : f(t)=exp(-(t/tscale)**2)
c     tscale<0 f(t) is a smooth turn off  : f(t)=step(-t/tscale))
c     tscale=0 f(t)=1.
c
c     where step is defined in step.f
c
c     h1(t)=cos(tomega*tc)
c
c     the volume force is only calculated if locfor is true
c     and the time is in the interval [0-5 tscale] or tscale<0
c
c==== loctyp=11:
c
c     Variation of loctyp 5:
c     Localized force corresponding to stationary and time-dependent 
c     force described in Högberg & Henningson, 1998
c
c     Adding a stationary and a time dependen force of the form
c     F=(ampx,ampy,ampz)*exp(-(y/yscale)**2)*g(x,z,t)*f(t)
c
c     zscale>0   g(x,z,t)=exp(-(x-xloc0)/xscale**2-(z/zscale)**2)+
c                         exp(-(x-xo)/xscale**2-(z/zscale)**2)*h1(t)
c     zscale<0   g(x,z,t)=exp(-(x-xloc0)/xscale**2)*
c              cos((z-x*lskew)/zscale*2*pi)+
c                     exp(-(x-xo)/xscale**2)*cos((z-x*lskew)/zscale*2*pi)*h1(t)
c
c     tscale>0 f(t) is a smooth turn on   : f(t)=exp(-(t/tscale)**2)
c     tscale<0 f(t) is a smooth turn off  : f(t)=step(-t/tscale))
c     tscale=0 f(t)=1.
c
c     where step is defined in step.f
c
c     h1(t)=aomeg*cos(tomega*tc)
c
c     the volume force is only calculated if locfor is true
c     and the time is in the interval [0-5 tscale] or tscale<0
c
c==== loctyp=12:
c
c     Cylinder roughness: F=A*chi(r,y)*f(t)
c     

      implicit none

      include 'par.f'

      integer yb,loctyp,ith
      real om2r(nxp/2+1,mby,nzd,3),om2i(nxp/2+1,mby,nzd,3)
      real th2r(nxp/2+1,mby,nzd,4*scalar),th2i(nxp/2+1,mby,nzd,4*scalar)
      real u2r(nxp/2+1,mby,nzd,3),u2i(nxp/2+1,mby,nzd,3)
      real xl,zl,xsc,zsc,tc
      real eta(nyp)
      real g1(nxp/2,nzd),g2(nxp/2,nzd)
      real fp1,fpds1,fpds4,fpds6,fpds7
      real fpds8,fpdds1,fpdds2,fpdds3,fpdds4,fpdds5

      real ampx,ampy,ampz,xscale,yscale,zscale,tscale,xloc0,lskew
      real xtype,xstart,xend,xrise,xfall,ystart,yend,yrise,yfall
      real zbet,tomeg,tstart,tend,xo,aomeg,y0,yscale1
      real amp,yloc0

      real xc1(nxp/2),xc2(nxp/2)
      integer x,y,z
      real xc11,xc22,fx1(nxp/2),fx2(nxp/2),f2x1(nxp/2),f2x2(nxp/2)
      real fy,dfy,f2y,ft,f2t,yc,zc,k2,h1
      real pi
      real amp2d,amp3d,tomeg3d,ft3d
      real rad,lam,lam0
      real depth,length,width,chi1,chi2
      integer iplane,num_Cav,i,fpds2
      real xcenter(50),zcenter(50),fpds3(50),fpds5(50)
      parameter (pi = 3.1415926535897932385)

      real mm2r(nzd,3),bbb
c
c     Functions
c
      real,external :: step,dstep,chi

c
c Note that we need a coordinate that runs between -xl/2 and xl/2
c regardless of the shift xsc, otherwise the force would be turned off
c abruptly when shifted out of the box
c

c
      if (loctyp.eq.13) then
c
c     Rename variables
c
         amp=fpdds1
         depth=fpds1
         num_Cav=fpds2
         xcenter=fpds3
         length=fpds4
         zcenter=fpds5
         width=fpds6
         tscale=fpds7        
c         iplane=int(fp1+0.5)


c        if (tscale.gt.0..and.tc.gt.5.*tscale) return

c        if (tscale.gt.0.) ft = exp(-(tc/tscale)**2)
c         if (tscale.lt.0.) ft = step(-(tc/tscale))
         if (tscale.eq.0.) ft = 1.
c	 xcenter(1) = 2.
c	 xcenter(2) = 4.
c	 xcenter(3) = 6.
c	 zcenter(1) = 1.57
c	 zcenter(2) = 0.
c	 zcenter(3) = -1.57
	
         do x=1,nxp/2
            xc11=real(2*x-1-nxp/2-1)/real(nxp)*xl+xsc
            xc11=xc11-int((xc11+xl/2.)/xl)*xl
            xc22=real(2*x-nxp/2-1)/real(nxp)*xl+xsc
            xc22=xc22-int((xc22+xl/2.)/xl)*xl
            
            do z=1,nzpc
               zc=zl*real(z-nzp/2-1)/real(nzp)+zsc
               do y=1,min(mby,nyp-yb+1)
                  yc=1.+eta(y+yb-1)

                  chi1 = chi(depth,num_Cav,xcenter,length,zcenter,
     &                width,xc11,yc,zc)
                  chi2 = chi(depth,num_Cav,xcenter,length,zcenter,
     &                width,xc22,yc,zc)
                  
                  om2r(x,y,z,1)=om2r(x,y,z,1)-amp*ft*u2r(x,y,z,1)*chi1
                  om2r(x,y,z,2)=om2r(x,y,z,2)-amp*ft*u2r(x,y,z,2)*chi1
                  om2r(x,y,z,3)=om2r(x,y,z,3)-amp*ft*u2r(x,y,z,3)*chi1
                  
                  om2i(x,y,z,1)=om2i(x,y,z,1)-amp*ft*u2i(x,y,z,1)*chi2
                  om2i(x,y,z,2)=om2i(x,y,z,2)-amp*ft*u2i(x,y,z,2)*chi2
                  om2i(x,y,z,3)=om2i(x,y,z,3)-amp*ft*u2i(x,y,z,3)*chi2

               end do
            end do
         end do
      end if



      if (loctyp.gt.13) then
         write(*,*) 'loctyp = ',loctyp,' not implemented.'
         call stopnow(5433332)
      end if

      end subroutine locf


c      real function chi(xLB,yLB,s,x1,y1,x3,x1c,x2c)
c
c Smooth step function:
c
c        / 0                                                 x3>=h+s
c        |                                                   r>=r0+s
c        |
c        | 0.5*(1+cos((r-r0)/s*pi))*0.5*(1+cos((x3-h)/s*pi)  h<x3<h+s
c        |                                                   r0<r<r0+s
c        |
c chi = <  0.5*(1+cos((r-r0)/s*pi))                          x3<=h
c        |                                                   r0<r<r0+s
c        |
c        | 0.5*(1+cos((x3-h)/s*pi))                          h<x3<h+s
c        |                                                   r<=r0
c        |
c        \ 1                                                 x3<=h; r<=r0
c
c
c Coordinate system:
c
c                x3
c                ^
c                |   __|__
c                |  /  .  \
c                |  \__|__/ 
c                |  |  .  | 
c                /--|  |  |------> x1
c               /   |  .  |
c              /    |  |  |
c             /     \__.__/
c            /         |
c           V
c          x2            
c

c      implicit none
      
c      real s,xLB,yLB,x1,y1,x3,x1c,x2c
      
c      real pi
c      parameter (pi = 3.1415926535897932385)
      
c      if (x1.le.(x1c-xLB/2-s).or.x1.ge.(x1c+xLB/2+s)) then
c         chi=0.     
c      elseif (x1.gt.(x1c-xLB/2-s).and.x1.lt.(x1c-xLB/2)) then
c         if (y1.le.(x2c-yLB/2-s)) then
c            chi=0.
c         elseif (y1.gt.(x2c-yLB/2-s).and.y1.le.(x2c-yLB/2)) then
c            chi=(-cos((x1-x1c+xLB/2+s)/s*pi)/2+0.5)*
c     &           (-cos((y1-x2c+yLB/2+s)/s*pi)/2+0.5)
c         elseif (y1.gt.(x2c-yLB/2).and.y1.lt.(x2c+yLB/2)) then
c            chi=-cos((x1-x1c+xLB/2+s)/s*pi)/2+0.5   
c         elseif (y1.ge.(x2c+yLB/2).and.y1.le.(x2c+yLB/2+s)) then
c           chi=(-cos((x1-x1c+xLB/2+s)/s*pi)/2+0.5)*
c     &           (cos((y1-x2c-yLB/2)/s*pi)/2+0.5)
c         else
c            chi=0.
c         endif
c      elseif (x1.gt.(x1c-xLB/2).and.x1.lt.(x1c+xLB/2)) then
c         if (y1.le.(x2c-yLB/2-s)) then
c            chi=0.
c         elseif (y1.gt.(x2c-yLB/2-s).and.y1.le.(x2c-yLB/2)) then
c            chi=-cos((y1-x2c+yLB/2+s)/s*pi)/2+0.5
c         elseif (y1.gt.(x2c-yLB/2).and.y1.lt.(x2c+yLB/2)) then
c            chi=1.
c         elseif (y1.ge.(x2c+yLB/2).and.y1.le.(x2c+yLB/2+s)) then
c            chi=cos((y1-x2c-yLB/2)/s*pi)/2+0.5
c         else
c            chi=0.
c         endif
c      elseif (x1.ge.(x1c+xLB/2).and.x1.lt.(x1c+xLB/2+s)) then
c         if (y1.le.(x2c-yLB/2-s)) then
c            chi=0.
c         elseif (y1.gt.(x2c-yLB/2-s).and.y1.le.(x2c-yLB/2)) then
c            chi=(cos((x1-x1c-xLB/2)/s*pi)/2+0.5)*
c     &           (-cos((y1-x2c+yLB/2+s)/s*pi)/2+0.5)        
c         elseif (y1.gt.(x2c-yLB/2).and.y1.lt.(x2c+yLB/2)) then
c            chi=cos((x1-x1c-xLB/2)/s*pi)/2+0.5   
c         elseif (y1.ge.(x2c+yLB/2).and.y1.le.(x2c+yLB/2+s)) then
c            chi=(cos((x1-x1c-xLB/2)/s*pi)/2+0.5)*
c     &           (cos((y1-x2c-yLB/2)/s*pi)/2+0.5)
c         else
c            chi=0.
c         endif
c      else
c         chi=0.
c      endif
c      
c      end function chi
      real function chi(depth,num_Cav,xcenter,length,zcenter,
     &  width,x1,y1,z1)

      implicit none      
      real depth,xcenter(50),length,zcenter(50),width,x1,y1,z1  
      real pi
      integer i,num_Cav
      parameter (pi = 3.1415926535897932385)
      
      if (y1.le.depth) then
      	chi=1.
      	do i=1,num_Cav
	  if (x1.ge.(xcenter(i)-length/2).and.
     &       x1.le.(xcenter(i)+length/2).and.
     &       z1.ge.(zcenter(i)-width/2).and.
     &       z1.le.(zcenter(i)+width/2)) then
	    chi=0.
	    exit
	  endif
	enddo
      else
      	chi=0.
      endif
      
      end function chi
