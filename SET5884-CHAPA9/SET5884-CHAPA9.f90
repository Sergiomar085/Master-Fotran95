!  SET5884CHAPA9.f90 
!
!  FUNCTIONS:
!  SET5884CHAPA9 - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: SET5884CHAPA9
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

!****************************************************************************
!  
!  SUBROTINA que fornece os valores das funções de forma 
!  para elemento triangular cúbico para coordenadas informadas
!  ksi3 = 1-ksi1-ksi2 variável usada para simplificar escrita
!
!****************************************************************************

    subroutine Fforma3(ksi1,ksi2,fi,dfi)
      implicit none
      real(8):: fi(10),dfi(10,2),ksi1,ksi2,ksi3
      ksi3=1.d0-ksi1-ksi2
      fi(1)=0.5d0*ksi1*(3.d0*ksi1-1.d0)*(3.d0*ksi1-2.d0)
      fi(2)=4.5d0*ksi1*ksi2*(3.d0*ksi1-1.d0)
      fi(3)=4.5d0*ksi1*ksi2*(3.d0*ksi2-1.d0)
      fi(4)=0.5d0*ksi2*(3.d0*ksi2-1.d0)*(3.d0*ksi2-2.d0)
      fi(5)=4.5d0*ksi3*ksi1*(3.d0*ksi1-1.d0)
      fi(6)=27.0d0*ksi1*ksi2*ksi3
      fi(7)=4.5d0*ksi2*ksi3*(3.d0*ksi2-1.d0)
      fi(8)=4.5d0*ksi3*ksi1*(3.d0*ksi3-1.d0)
      fi(9)=4.5d0*ksi2*ksi3*(3.d0*ksi3-1.d0)
      fi(10)=0.5d0*ksi3*(3.d0*ksi3-1.d0)*(3.d0*ksi3-2.d0)

      dfi(1,1)= 0.5d0*(3.d0*ksi1-1.d0)*(3.d0*ksi1-2.d0)+1.5d0*ksi1*(3.d0*ksi1-2.d0)+1.5d0*ksi1*(3.d0*ksi1-1.d0)
      dfi(1,2)= 0.d0
      dfi(2,1)= 4.5d0*ksi2*(3.d0*ksi1-1.d0)+4.5d0*ksi1*ksi2*3.d0
      dfi(2,2)= 4.5d0*ksi1*(3.d0*ksi1-1.d0)
      dfi(3,1)= 4.5d0*ksi2*(3.d0*ksi2-1.d0)
      dfi(3,2)= 4.5d0*ksi1*(3.d0*ksi2-1.d0)+4.5d0*ksi1*ksi2*3.d0
      dfi(4,1)= 0.d0
      dfi(4,2)= 0.5d0*(3.d0*ksi2-1.d0)*(3.d0*ksi2-2.d0)+1.5d0*ksi2*(3.d0*ksi2-2.d0)+1.5d0*ksi2*(3.d0*ksi2-1.d0)
      dfi(5,1)=-4.5d0*ksi1*(3.d0*ksi1-1.d0)+4.5d0*ksi3*(3.d0*ksi1-1.d0)+4.5d0*ksi3*ksi1*3.d0
      dfi(5,2)=-4.5d0*ksi1*(3.d0*ksi1-1.d0)
      dfi(6,1)= 27.d0*(ksi2*ksi3-ksi1*ksi2)
      dfi(6,2)= 27.d0*(ksi1*ksi3-ksi1*ksi2)
      dfi(7,1)=-4.5d0*ksi2*(3.d0*ksi2-1.d0)
      dfi(7,2)= 4.5d0*ksi3*(3.d0*ksi2-1.d0)-4.5d0*ksi2*(3.d0*ksi2-1.d0)+4.5d0*ksi3*ksi2*3.d0
      dfi(8,1)=-4.5d0*ksi1*(3.d0*ksi3-1.d0)+4.5d0*ksi3*(3.d0*ksi3-1.d0)-4.5d0*ksi3*ksi1*3.d0
      dfi(8,2)=-4.5d0*ksi1*(3.d0*ksi3-1.d0)-4.5d0*ksi1*ksi3*3.d0
      dfi(9,1)=-4.5d0*ksi2*(3.d0*ksi3-1.d0)-4.5d0*ksi2*ksi3*3.d0
      dfi(9,2)= 4.5d0*ksi3*(3.d0*ksi3-1.d0)-4.5d0*ksi2*(3.d0*ksi3-1.d0)-4.5d0*ksi3*ksi2*3.d0
      dfi(10,1)=-0.5d0*(3.d0*ksi3-1.d0)*(3.d0*ksi3-2.d0)-1.5d0*ksi3*(3.d0*ksi3-2.d0)-1.5d0*ksi3*(3.d0*ksi3-1.d0)
      dfi(10,2)= dfi(10,1)
      return
    end subroutine
    
!****************************************************************************
!  
!  Atribuição dos pontos de Hammer e seus pesos
!
!****************************************************************************

    subroutine hammer7(HAM,wh)
      implicit none
      real(8):: HAM(7,3),wh(7)
      ham(1,1)=(1./3.d0)
      ham(1,2)=(1./3.d0)
      ham(1,3)=(1./3.d0)
      wh(1)=(0.11250d0)

      ham(2,1)=(0.797426985353087d0)
      ham(2,2)=(0.101286507323456d0)
      ham(2,3)=(0.101286507323456d0)
      wh(2)=(0.125939180544827d0)/2.

      ham(3,1)=(0.101286507323456d0)
      ham(3,2)=(0.797426985353087d0)
      ham(3,3)=(0.101286507323456d0)
      wh(3)=(0.125939180544827d0)/2.

      ham(4,1)=(0.101286507323456d0)
      ham(4,2)=(0.101286507323456d0)
      ham(4,3)=(0.797426985353087d0)
      wh(4)=(0.125939180544827d0)/2.

      ham(5,1)=(0.470142064105115d0)
      ham(5,2)=(0.470142064105115d0)
      ham(5,3)=(0.059715871789770d0)
      wh(5)=(0.132394152788506d0)/2.

      ham(6,1)=(0.059715871789770d0)
      ham(6,2)=(0.470142064105115d0)
      ham(6,3)=(0.470142064105115d0)
      wh(6)=(0.132394152788506d0)/2.

      ham(7,1)=(0.470142064105115d0)
      ham(7,2)=(0.059715871789770d0)
      ham(7,3)=(0.470142064105115d0)
      wh(7)=(0.132394152788506d0)/2.
      return
    end subroutine

    subroutine Quad_Gauss2(GAUSS,wg)
      implicit none
      real(8):: GAUSS(2),wg(2)
      GAUSS(1)=-1.d0/sqrt(3.d0)
      wg(1)=1.d0
      
      GAUSS(2)=1.d0/sqrt(3.d0)
      wg(2)=1.d0
      return
    end subroutine
    
!****************************************************************************
!  
!  SUBROTINA que fornece os valores das funções de forma 
!  para elemento triangular quadrático 
!
!****************************************************************************

    subroutine Fforma2(ksi1,ksi2,fi_quad)
      implicit none
      real(8):: fi_quad(6),ksi1,ksi2
      
      fi_quad(1)=-1.d0*ksi1+2.d0*ksi1*ksi1
      fi_quad(2)=4.d0*ksi1*ksi2
      fi_quad(3)=-1.d0*ksi2+2.d0*ksi2*ksi2
      fi_quad(4)=4.d0*ksi1-4.d0*ksi1*ksi1-4.d0*ksi1*ksi2
      fi_quad(5)=4.d0*ksi2-4.d0*ksi1*ksi2-4.d0*ksi2*ksi2
      fi_quad(6)=1.d0-3.d0*ksi1-3.d0*ksi2+2.d0*ksi1*ksi1+4.d0*ksi1*ksi2+2.d0*ksi2*ksi2

      return
    end subroutine
    
    subroutine Fforma1(ksi,fi_uni,dfi_uni)
      implicit none
      real(8):: ksi,fi_uni(4),dfi_uni(4)
      fi_uni(1)= -9.d0/16.d0*ksi*ksi*ksi+9.d0/16.d0*ksi*ksi+ksi/16.d0-1.d0/16.d0
      fi_uni(2)= 27.d0/16.d0*ksi*ksi*ksi-9.d0/16.d0*ksi*ksi-27.d0/16.d0*ksi+9.d0/16.d0
      fi_uni(3)=-27.d0/16.d0*ksi*ksi*ksi-9.d0/16.d0*ksi*ksi+27.d0/16.d0*ksi+9.d0/16.d0
      fi_uni(4)=  9.d0/16.d0*ksi*ksi*ksi+9.d0/16.d0*ksi*ksi-ksi/16.d0-1.d0/16.d0
      
      dfi_uni(1)=-27.d0/16.d0*ksi*ksi+9.d0/8.d0*ksi+ 1.d0/16.d0
      dfi_uni(2)= 81.d0/16.d0*ksi*ksi-9.d0/8.d0*ksi-27.d0/16.d0
      dfi_uni(3)=-81.d0/16.d0*ksi*ksi-9.d0/8.d0*ksi+27.d0/16.d0
      dfi_uni(4)= 27.d0/16.d0*ksi*ksi+9.d0/8.d0*ksi- 1.d0/16.d0
      return
    end subroutine
    
!****************************************************************************
!  
!  DANIFICAÇÃO DO MATERIAL
!
!****************************************************************************
    
    subroutine Fdano(At,Ac,Bt,Bc,ed0,e_equiv,young,poisson,ep,dano)

      implicit none
      integer:: i
      real(8):: young,poisson,ed0,At,Ac,Bt,Bc,ep(3),mtx(3,3),inv_mtx(3,3),const,sigp(3),sigp_t(3),sigp_c(3)
      real(8):: ep_t(3),ep_c(3),ep_t_along,ep_c_along,along_vol,alfa_t,alfa_c,e_equiv,dt,dc,dano
      !.................
      mtx(:,1)=(/1.d0/young,-poisson/young,-poisson/young/)
      mtx(:,2)=(/-poisson/young,1.d0/young,-poisson/young/)
      mtx(:,3)=(/-poisson/young,-poisson/young,1.d0/young/)
      !.................
      inv_mtx(:,1)=(/(1-poisson),poisson,poisson/)
      inv_mtx(:,2)=(/poisson,(1-poisson),poisson/)
      inv_mtx(:,3)=(/poisson,poisson,(1-poisson)/)
      const=young/(1+poisson)/(1-2*poisson)
      inv_mtx=inv_mtx*const
      !.................
      sigp=matmul(inv_mtx,ep)
      !.................
      do i=1,3
          sigp_t(i)=0.5*(sigp(i)+abs(sigp(i)))
          sigp_c(i)=0.5*(sigp(i)-abs(sigp(i)))
      enddo
      ep_t=matmul(mtx,sigp_t)
      ep_c=matmul(mtx,sigp_c)
      ep_t_along=0.d0
      ep_c_along=0.d0

      do i=1,3
          ep_t_along=ep_t_along+0.5*(ep_t(i)+abs(ep_t(i)))
          ep_c_along=ep_c_along+0.5*(ep_c(i)+abs(ep_c(i)))
      enddo
      along_vol=ep_t_along+ep_c_along
      alfa_t=ep_t_along/along_vol
      alfa_c=ep_c_along/along_vol
      dt=1-ed0*(1-At)/e_equiv-At/exp(Bt*(e_equiv-ed0))
      dc=1-ed0*(1-Ac)/e_equiv-Ac/exp(Bc*(e_equiv-ed0))
      dano=alfa_t*dt+alfa_c*dc
      return
    end subroutine
    
!****************************************************************************
!  
!  INÍCIO ALGORITMO DINÂMICO
!
!****************************************************************************
    
    program SET5884CHAPA9

    !Declaração das Biliotecas
    use Sparse

    use lapack95
    use f95_precision

    implicit none

    ! Variables

    integer:: i,j,k,k1,k2,nnos,nel,nt,ia,ib,iz,ig,jg,l,o,z,passo,interacao,model,correcao,gambs_count,gambs_corrig,dim,nh,ih,iel,ep,ipiv3(6),ino
    integer:: nmats,nforces,ndispla,nthicks,count,comprimento,nnf,nef,ief,indexx(40),ii,jj,elem,no_f,inf,inc_side(3,4),npress,side,distr_inv(4)
    integer:: no1,no4,par_x1,par_x4,par_y1,par_y4,ng,inp,noh
    !inc_side - matriz de incidência dos nós do elemento de linha, em relação aos nós locais do elemento de chapa
    !no_f - nó da fibra
    !nnf - número de nós de fibra
    !nef - número de elementos de fibra
    !nmats - número de materiais
    !nforces - número de forças aplicadas
    !ndispla - número de deslocamentos conhecidos
    !nthicks - número de espessuras
    !count - contador do número de restrições nos graus de liberdade
    !comprimento - tamanho do vetor de restrições
    !dim - dimensão do problema
    !nh -  número de pontos de hammer
    !ih - contador para os pontos de hammer 
    !iel - contador para os elementos finitos
    !declara
    real(8):: tol,dedyba,dedyxg,rh_dy,d2edy2baxg,rh_dy2,erro,e1,e2,ham(7,3),wh(7),ksi1,ksi2,fi(10),dfi(10,2),J0,delta_t,tempo_passo
    real(8):: K_const,G_const,fiel,hl,mtx_L(7,6),Tr_mtx_L(6,7),mtx_inter_ham(20,20),mtx_inter_loc(20,20),B1,B2,B3,beta,gama,aux
    real(8):: incr1,incr2,young,poisson,density,fi_quad(6),v_sigx(6),v_sigy(6),v_talxy(6),v_talyx(6),v_dano(6),mtx_M(6,6),inv_mtx_M(6,6)
    real(8):: sigx_node2(6),sigy_node2(6),talxy_node2(6),talyx_node2(6),dano_node2(6),adm_coord(10,2),sigx_node(10),sigy_node(10),talxy_node(10),talyx_node(10),dano_node(10)
    real(8):: aux_a1,aux_b1,aux_2A1,aux_a2,aux_b2,aux_2A2,aux_a3,aux_b3,aux_2A3,duoA,cx1,cy1,cx2,cy2,cx3,cy3,ksi1_f,ksi2_f,ksi3_f,cxf,cyf
    real(8):: egf,sf,kt,l02,l2,fil_fib(4),mffi(4,40),hl_fib(4,4),fic_fib(40),seq_qx(4),seq_qy(4),Px0,Px1,Py0,Py1
    real(8):: aloc_qx(4),aloc_qy(4),gauss(2),wg(2),ksi,fi_uni(4),dfi_uni(4),vec_tg(2),mtx_inter_gauss(8,8),mtx_loc(8,8),qnode_loc(8),Feq_loc(8)
    real(8):: defp(3),e_equiv, At, Ac, Bt, Bc, ed0, dano, esp, tenp(3)
    real(8), dimension(:), allocatable:: young_old, young_new, Sd_old, Sd_new, young_original
    real(8), dimension(:), allocatable:: x,y,fig,dy,df,f_ext,g,sigx_ham, sigy_ham, talxy_ham, talyx_ham, dano_ham
    real(8), dimension(:), allocatable:: vec_campo,Fvol_add!,Rs,vec_vel,vec_ace,Qs
    real(8), dimension(:), allocatable:: thickness,xf,yf
    real(8), dimension(:,:), allocatable:: A0,A1,inv_A0,Id,C,Eg,S,DA1ab,DA1gz,DEab,DEgz,DS,D2E,pos_y,mtx_A,JA,sig_cauchy,mtx_inter_glob
    real(8), dimension(:,:), allocatable:: materials, vinc_, vinc, pos_dano, hg, hc_fib!,mtx_mg,mtx_c,inv_mtx_mg, 
    real(8), dimension(:,:,:), allocatable:: matsigma
    real(8), dimension(:,:), allocatable:: inc_fib,identify,pos_yf,inc_press,mtx_qnode!, SPMAT
    
    ! MODIFICADO
    real(8), dimension(:), allocatable:: dano_old

    !mtx_qnode - matriz de cargas nodais de cada lado carregado
    !pos_yf - matriz para guardar as posições para pós-processamento
    !xf - posição inicial dos nós de fibra
    !yf - posição atual dos nós de fibra
    !egf - Deformação de Green da fibra
    !sf - Tensor de Piola Kirchhoff da fibra
    !fil_fib - força interna local da fibra
    !hl_fib - matriz da hessiana local da fibra
    !fic_fib - força interna compatibilizada da fibra
    !hc_fib - matriz da hessiana compatibilizada da fibra
    !mffi - matriz das funções de forma para compatibilização da atuação das fibras na estrutura
    !real(8), dimension(:,:,:), allocatable:: pos_y
    !pos_y: tensor para guardar os valores de y para pós-processamento
    !mtx_A: matriz gradiente da função mudança de configuração
    !JA: Jacobiano da matriz A (mtx_A)
    !sig_cauchy: tensor de tensões de Cauchy
    !vec_campo: vetor das forças de campo
    !Fvol_add: vetor das forças de volume que devem ser adicionadas às forças externas
    !mtx_mg: matriz de massa global
    !mtx_c: matriz de amortecimento
    !vec_vel: vetor de velocidade
    !vec_ace: vetor de aceleração
    !tempo_passo: tempo referente ao passo de aplicação de carga
    !e_equiv: deformação equivalente
    !temp: vetor das tensões principais
    
    integer, dimension(:), allocatable:: ipiv, INDEXES
    integer, dimension(:,:), allocatable:: inc
    logical:: equilibrio
    character(16):: jump
    Type(sparse_matrix) :: SPMAT
    
    open(5,file='00-DADOS_PROCESSAMENTO.txt',status='old')
    open(10,file='01-DADOS_MALHA.txt',status='old')
    open(11,file='02-DADOS_FIBRAS.txt',status='old')
    open(12,file='03-DADOS_DANO.txt',status='old')
    open(80,file='04-POS_PROCESS_ACADVIEW.ogl',status='unknown')
    open(50,file='05-OUTPUT_DESL(Y).txt',status='unknown')
    open(60,file='06-OUTPUT_FORÇA_EXTERNA.txt',status='unknown')
    open(20,file='output.txt',status='unknown')
    !open(70,file='output_deslocamento_x.txt',status='unknown')
    !open(90,file='06-OUTPUT_FORÇA_EXTERNA.txt',status='unknown')
    !open(100,file='checklist.txt',status='unknown')
    !open(101,file='teste_xf.txt',status='unknown')
    !open(102,file='teste_yf.txt',status='unknown')

    ! Body of SET5884TRELIAPROGRAM1
    
    dim=2 !problema 2D
    inc_side(1,:)=(/1,2,3,4/)
    inc_side(2,:)=(/4,7,9,10/)
    inc_side(3,:)=(/10,8,5,1/)
    
    read(5,*)  jump,nt
    write(20,*)jump,nt
    read(5,*)  jump,tol
    write(20,*)jump,tol
    read(5,*)  jump,delta_t
    write(20,*)jump,delta_t
    read(5,*)
    write(20,*)
    read(5,*)  !||||| ESTADO PLANO (0-EPD / 1-EPT) |||||
    write(20,*)'||||| ESTADO PLANO (0-EPD / 1-EPT) |||||'
    read(5,*)  ep
    write(20,*)ep
    read(5,*)
    write(20,*)
    read(5,*)  !||||| Força de campo |||||
    write(20,*)'||||| Força de campo |||||'
    read(5,*)  !B1, B2
    write(20,*)'B1, B2'
    read(5,*)  B1,B2
    write(20,*)B1,B2
    
    read(10,*) jump,nnos !número de nós
    write(20,*)jump,nnos
    read(10,*) jump,nmats !número de materiais
    write(20,*)jump,nmats
    read(10,*) jump,nthicks
    write(20,*)jump,nthicks
    read(10,*) jump,nel !número de elementos
    write(20,*)jump,nel
    read(10,*) jump,nforces
    write(20,*)jump,nforces !número de forças aplicadas
    read(10,*) jump,npress
    write(20,*)jump,npress
    read(10,*) jump,ndispla
    write(20,*)jump,ndispla
    read(10,*)
    write(20,*)
    
    call hammer7(HAM,wh)
    nh=7 !7 pontos de hammer
    call Quad_Gauss2(gauss,wg)
    ng=2 !2 pontos de gauss
    
    allocate (x(dim*nnos),f_ext(dim*nnos),ipiv(dim))
    !allocate ()!,vec_vel(dim*nnos),vec_ace(dim*nnos),Qs(dim*nnos),Rs(dim*nnos)
    allocate (inc(nel,12),A0(dim,dim),inv_A0(dim,dim),A1(dim,dim),mtx_A(dim,dim),Id(dim,dim),C(dim,dim),Eg(dim,dim),S(dim,dim),DA1ab(dim,dim),DA1gz(dim,dim),DEab(dim,dim),DEgz(dim,dim),DS(dim,dim),D2E(dim,dim))
    allocate (JA(dim,dim),sig_cauchy(dim,dim))!,mtx_mg(dim*nnos,dim*nnos),mtx_c(dim*nnos,dim*nnos),inv_mtx_mg(dim*nnos,dim*nnos)
    allocate (hg(20,20), hc_fib(40,40))
    allocate (matsigma(nnos,6,nt))
    allocate (materials(nmats,3),vinc_(ndispla,5))
    allocate (thickness(nthicks))
    allocate (inc_press(npress,6),mtx_qnode(8,npress))
    allocate (young_old(nh*nel),young_new(nh*nel),Sd_old(nh*nel),Sd_new(nh*nel),young_original(nh*nel))
    !allocate (INDEXES(10))
    
    ! MODIFICADO
    allocate (dano_old(nh*nel))
    allocate (pos_dano(nh*nel,nt)) !dados do dano para pós processamento
    allocate (dy(dim*nnos))
    dano_old = 0.d0
    pos_dano = 0.d0
    dy=0.d0

    read(10,*) !NODE X Y
    write(20,*)'NODE X Y'
    do i=1,nnos !número de nós
        read(10,*) k,x(dim*(k-1)+1),x(dim*(k-1)+2)
        write(20,*)k,real(x(dim*(k-1)+1)),real(x(dim*(k-1)+2))
    enddo !i
    read(10,*)
    write(20,*)
    
    read(10,*) !MAT. YOUNG M. POISSON C. DENSITY
    write(20,*)'MAT. YOUNG M. POISSON C. DENSITY'
    do i=1,nmats !número de materiais
        read(10,*) k,materials(k,1),materials(k,2),materials(k,3)
        write(20,*)k,materials(k,1),materials(k,2),materials(k,3)
    enddo !i
    read(10,*)
    write(20,*)
    
    read(10,*) !THICK. VALUE
    write(20,*)'THICK. VALUE'
    do i=1,nthicks
        read(10,*) k,thickness(k)
        write(20,*)k,thickness(k)
    enddo !i
    read(10,*)
    write(20,*)
    
    read(10,*) !ELEM. NODES MAT. THICK.
    write(20,*)'ELEM. NODES MAT. THICK.'
    do i=1,nel
        read(10,*) k,inc(k,1),inc(k,2),inc(k,3),inc(k,4),inc(k,5),inc(k,6),inc(k,7),inc(k,8),inc(k,9),inc(k,10),inc(k,11),inc(k,12)
        write(20,*)k,inc(k,1),inc(k,2),inc(k,3),inc(k,4),inc(k,5),inc(k,6),inc(k,7),inc(k,8),inc(k,9),inc(k,10),inc(k,11),inc(k,12)
    enddo
    read(10,*)
    write(20,*)
    
    allocate (df(dim*nnos))
    df = 0.d0
    read(10,*) !FORCE NODE FX FY
    write(20,*)'FORCE NODE FX FY'
    do i=1,nforces
        read(10,*) jump,k,incr1,incr2
        write(20,*)jump,k,incr1,incr2
        df(2*(k-1)+1) = df(2*(k-1)+1)+incr1
        df(2*(k-1)+2) = df(2*(k-1)+2)+incr2
    enddo !i
    read(10,*)
    write(20,*)

    read(10,*) !PRESS. ELEM. SIDE SYSTEM PX0 PX1 PY0 PY1
    write(20,*)'PRESS. ELEM. SIDE SYSTEM PX0 PX1 PY0 PY1'
    do i=1,npress
        read(10,*) k,inc_press(k,1),inc_press(k,2),jump,inc_press(k,3),inc_press(k,4),inc_press(k,5),inc_press(k,6)
        write(20,*)k,inc_press(k,1),inc_press(k,2),jump,inc_press(k,3),inc_press(k,4),inc_press(k,5),inc_press(k,6)
        inc_press(k,2)=inc_press(k,2)+1 !Adaptação para linguagem do ACADMESH2D
    enddo !i
    read(10,*)
    write(20,*)
    !geração dos vetores de carga distribuída pra cada lado carregado
    do i=1,npress
        elem=int(inc_press(i,1)) !elemento em que se encontra o lado carregado
        side=int(inc_press(i,2)) !numeração local do lado carregado
        Px0=inc_press(i,3)
        Px1=inc_press(i,4)
        Py0=inc_press(i,5)
        Py1=inc_press(i,6)
        !cálculo dos valores de carga sequencial
        do j=1,4
            seq_qx(j)=Px0+(j-1)*(Px1-Px0)/3
            seq_qy(j)=Py0+(j-1)*(Py1-Py0)/3
        enddo
        no1=inc(elem,inc_side(side,1))
        no4=inc(elem,inc_side(side,4))
        par_x1=dim*(no1-1)+1
        par_x4=dim*(no4-1)+1
        par_y1=dim*(no1-1)+2
        par_y4=dim*(no4-1)+2
        !para alocar qy:
        if (x(par_x4)/=x(par_x1)) then !se verdadeiro: no4 e no1 não estão alinhados na vertical
            if (x(par_x4)<x(par_x1)) then !se verdadeiro: no4 está a esquerda do no1
                k=4
                do j=1,4
                    aloc_qy(k)=seq_qy(j)
                    k=k-1
                enddo !j
            else !se falso: no1 está a esquerda do no4
                aloc_qy=seq_qy !correspondência direta de índices
            endif
        else !se falso: no4 e no1 estão alinhados na vertical
            if (x(par_y4)<x(par_y1)) then !se verdadeiro: no4 está abaixo do no1
                k=4
                do j=1,4
                    aloc_qy(k)=seq_qy(j)
                    k=k-1
                enddo !j
            else !se falso: no1 está abaixo do no4
                aloc_qy=seq_qy !correspondência direta de índices
            endif
        endif
        !para alocar qx:
        if (x(par_y4)/=x(par_y1)) then !se verdadeiro: no1 e no4 não estão alinhados na horizontal
            if (x(par_y4)<x(par_y1)) then !se verdadeiro: no4 está abaixo do no1
                k=4
                do j=1,4
                    aloc_qx(k)=seq_qx(j)
                    k=k-1
                enddo !j
            else !se falso: no1 está abaixo do no4
                aloc_qx=seq_qx !correspondência direta de índices
            endif
        else !se falso: no1 e no4 estão alinhados na horizontal
            if (x(par_x4)<x(par_x1)) then !se verdadeiro: no4 está a esquerda do no1
                k=4
                do j=1,4
                    aloc_qx(k)=seq_qx(j)
                    k=k-1
                enddo !j
            else !se falso: no1 está a esquerda do no4
                aloc_qx=seq_qx !correspondência direta de índices
            endif
        endif
        do j=1,4 !loop nós locais
            mtx_qnode(dim*(j-1)+1,i)=aloc_qx(j)
            mtx_qnode(dim*(j-1)+2,i)=aloc_qy(j)
        enddo !j
    enddo !i - loop lados carregados
    
    read(10,*) !DISP. NODE X VALUE Y VALUE
    write(20,*)'DISP. NODE X VALUE Y VALUE'
    count=0
    do i=1,ndispla
        read(10,*) jump,vinc_(i,1),vinc_(i,2),vinc_(i,3),vinc_(i,4),vinc_(i,5)
        write(20,*)jump,vinc_(i,1),vinc_(i,2),vinc_(i,3),vinc_(i,4),vinc_(i,5)
        if (vinc_(i,2)==1) then
            count=count+1
        endif
        if (vinc_(i,4)==1) then
            count=count+1
        endif
    enddo !i
    
    ! FIM DA LEITURA DE DADOS DA MALHA
    
    ! INÍCIO DA LEITURA DAS FIBRAS
    read(11,*) jump,nnf
    read(11,*) jump,nef
    read(11,*)
    
    allocate (xf(dim*nnf),yf(dim*nnf),inc_fib(nef,4),identify(nnf,3),pos_yf(dim*nnf,nt))
    
    read(11,*) !no coordx coordy
    do i=1,nnf
        read(11,*) k,xf(dim*(k-1)+1),xf(dim*(k-1)+2)
    enddo !i
    read(11,*)
    read(11,*) !elem noi nof area young
    !inc_fib=0.d0
    do i=1,nef
        read(11,*) k,inc_fib(k,1),inc_fib(k,2),inc_fib(k,3),inc_fib(k,4)
    enddo !i
    ! FIM DA LEITURA DAS FIBRAS
    Call prepare_01(SPMAT, nnos, nel + nef, 2, iflag=1)
    
    ! LEITURA DOS PARÂMETROS DE DANO MAZARS:
    read(12,*) !At, Ac, Bt, Bc, ed0
    read(12,*) At, Ac, Bt, Bc, ed0
    Sd_old=ed0 !limite inicial de danificação
    Sd_new=Sd_old
    do iel=1,nel
        do ih=1,nh
            young_original(nh*(iel-1)+ih)=materials(inc(iel,11),1)
        enddo !ih
    enddo !iel
    young_old=young_original
    young_new=young_original
    
    Do i = 1,nel !antes da identificação das fibras
        Allocate(INDEXES(10)) !INDEXES1
        INDEXES = inc(i,1:10)
        Call add_elem_conec(SPMAT, i, 10, INDEXES)
    Enddo
    
    ! IDENTIFICAÇÃO DAS FIBRAS NOS ELEMENTOS - INÍCIO
    identify=0.d0
    do i=1,nnf
        cxf=xf(dim*(i-1)+1)
        cyf=xf(dim*(i-1)+2)
        do j=1,nel
            cx1=x(dim*(inc(j,1)-1)+1)
            cy1=x(dim*(inc(j,1)-1)+2)
            cx2=x(dim*(inc(j,4)-1)+1)
            cy2=x(dim*(inc(j,4)-1)+2)
            cx3=x(dim*(inc(j,10)-1)+1)
            cy3=x(dim*(inc(j,10)-1)+2)
            !-------------------------
            aux_a1=cx3-cx2
            aux_a2=cx1-cx3
            aux_a3=cx2-cx1
            aux_b1=cy2-cy3
            aux_b2=cy3-cy1
            aux_b3=cy1-cy2
            aux_2A1=cx2*cy3-cx3*cy2
            aux_2A2=cx3*cy1-cx1*cy3
            aux_2A3=cx1*cy2-cx2*cy1
            !-------------------------
            duoA=aux_b1*aux_a2-aux_b2*aux_a1
            ksi1_f=(aux_2A1+aux_b1*cxf+aux_a1*cyf)/duoA
            ksi2_f=(aux_2A2+aux_b2*cxf+aux_a2*cyf)/duoA
            ksi3_f=(aux_2A3+aux_b3*cxf+aux_a3*cyf)/duoA
            if ((ksi1_f>=0.d0).AND.(ksi2_f>=0.d0).AND.(ksi3_f>=0.d0)) then
                identify(i,1)=j
                identify(i,2)=ksi1_f
                identify(i,3)=ksi2_f
                !write(100,*) i,int(identify(i,1)),real(identify(i,2)),real(identify(i,3)),real(ksi3_f)
                !write(100,*) '************************************************************************'
                exit
            endif
        enddo !j
    enddo !i'
    
    ! IDENTIFICAÇÃO DAS FIBRAS NOS ELEMENTOS - FIM
    Do i = 1,nef
        Allocate (INDEXES(2*10)) !INDEXES2
        k1 = identify(inc_fib(i,1),1) ! Elemento de chapa que contém o nó inicial
        k2 = identify(inc_fib(i,2),1) ! Elemento de chapa que contém o nó final
        INDEXES(1:10) = inc(k1,1:10)
        INDEXES(10+1:2*10) = inc(k2,1:10)
        Call add_elem_conec(SPMAT,nel+i,2*10,INDEXES)
    Enddo
    Call prepare_02(SPMAT)
    
    comprimento=count
    allocate(vinc(count,3))
    vinc=0.d0
    count=0
    do i=1,ndispla
        if (vinc_(i,2)==1) then
            count=count+1
            vinc(count,1)=vinc_(i,1) !nó
            vinc(count,2)=1          !direção
            vinc(count,3)=vinc_(i,3) !valor
        endif        
        if (vinc_(i,4)==1) then
            count=count+1
            vinc(count,1)=vinc_(i,1) !nó
            vinc(count,2)=2          !direção
            vinc(count,3)=vinc_(i,5) !valor
        endif
    enddo !i
    do i=1,comprimento
        dy(dim*(vinc(i,1)-1)+vinc(i,2))=vinc(i,3)
    enddo !i
    
    allocate (vec_campo(dim*nnos),Fvol_add(dim*nnos))
    do i=1,nnos
        vec_campo(dim*(i-1)+1)=B1
        vec_campo(dim*(i-1)+2)=B2
    enddo !i

    !write(50,*) 'Resultados em deslocamentos'

    !Cálculo das forças equivalentes das cargas distribuídas
    do inp=1,npress !loop nos elementos de linha
        elem=inc_press(inp,1)
        side=inc_press(inp,2)
        mtx_loc=0.d0
        do ig=1,ng !loop nos pontos de gauss
            ksi=gauss(ig)
            !write(*,*) ksi,wg(ig)
            call Fforma1(ksi,fi_uni,dfi_uni)
            vec_tg=0.d0
            do i=1,dim
                do j=1,4
                    noh=inc(elem,inc_side(side,j))
                    vec_tg(i)=vec_tg(i)+dfi_uni(j)*x(dim*(noh-1)+i)
                enddo !j
            enddo !i
            write(*,*) 'confere vetor:',ig
            do i=1,2
                write(*,*) i,vec_tg(i)
            enddo !i
            write(*,*) '............................'
            J0=sqrt(vec_tg(1)*vec_tg(1)+vec_tg(2)*vec_tg(2))
            write(*,*) 'jacobiano:',ig
            write(*,*) J0
            write(*,*) '...........................'
            mtx_inter_gauss(:,1)=(/fi_uni(1)*fi_uni(1),0.d0,fi_uni(2)*fi_uni(1),0.d0,fi_uni(3)*fi_uni(1),0.d0,fi_uni(4)*fi_uni(1),0.d0/)
            mtx_inter_gauss(:,2)=(/0.d0,fi_uni(1)*fi_uni(1),0.d0,fi_uni(2)*fi_uni(1),0.d0,fi_uni(3)*fi_uni(1),0.d0,fi_uni(4)*fi_uni(1)/)
            mtx_inter_gauss(:,3)=(/fi_uni(1)*fi_uni(2),0.d0,fi_uni(2)*fi_uni(2),0.d0,fi_uni(3)*fi_uni(2),0.d0,fi_uni(4)*fi_uni(2),0.d0/)
            mtx_inter_gauss(:,4)=(/0.d0,fi_uni(1)*fi_uni(2),0.d0,fi_uni(2)*fi_uni(2),0.d0,fi_uni(3)*fi_uni(2),0.d0,fi_uni(4)*fi_uni(2)/)
            mtx_inter_gauss(:,5)=(/fi_uni(1)*fi_uni(3),0.d0,fi_uni(2)*fi_uni(3),0.d0,fi_uni(3)*fi_uni(3),0.d0,fi_uni(4)*fi_uni(3),0.d0/)
            mtx_inter_gauss(:,6)=(/0.d0,fi_uni(1)*fi_uni(3),0.d0,fi_uni(2)*fi_uni(3),0.d0,fi_uni(3)*fi_uni(3),0.d0,fi_uni(4)*fi_uni(3)/)
            mtx_inter_gauss(:,7)=(/fi_uni(1)*fi_uni(4),0.d0,fi_uni(2)*fi_uni(4),0.d0,fi_uni(3)*fi_uni(4),0.d0,fi_uni(4)*fi_uni(4),0.d0/)
            mtx_inter_gauss(:,8)=(/0.d0,fi_uni(1)*fi_uni(4),0.d0,fi_uni(2)*fi_uni(4),0.d0,fi_uni(3)*fi_uni(4),0.d0,fi_uni(4)*fi_uni(4)/)
            mtx_loc=mtx_loc+mtx_inter_gauss*J0*wg(ig) !montagem da matriz local independente
            write(*,*) 'valor 1 - p.gauss:',ig
            write(*,*) sum(mtx_inter_gauss(2,:))
            write(*,*) '...........................'
            write(*,*) 'valor 2 - p.gauss:',ig
            write(*,*) sum(mtx_inter_gauss(8,:))
            write(*,*) '...........................'
        enddo !ig
        qnode_loc=mtx_qnode(:,inp)
        Feq_loc=matmul(mtx_loc,qnode_loc)
        do i=1,4
            noh=inc(elem,inc_side(side,i))
            df(dim*(noh-1)+1)=df(dim*(noh-1)+1)+Feq_loc(dim*(i-1)+1)
            df(dim*(noh-1)+2)=df(dim*(noh-1)+2)+Feq_loc(dim*(i-1)+2)
        enddo !i
        write(*,*) sum(Feq_loc)
        write(*,*) '..............................................'
        do i=1,8
            write(*,*) i,Feq_loc(i)
        enddo
        write(*,*) '..............................................'
        do i=1,8
            write(*,*) i,qnode_loc(i)
        enddo
        !pause
    enddo !inp
    
    !Montando a matriz identidade (dim x dim)
    Id=0.d0
    do i=1,dim
        Id(i,i)=1.d0
    enddo !i

    f_ext=0.d0 !zerando todas as componentes do vetor de forças externas 
    !MONTAGEM DA MATRIZ DE MASSA GLOBAL - INICIO
    !allocate (mtx_inter_glob(dim*nnos,dim*nnos))
    !mtx_inter_glob=0.d0
    !!mtx_mg=0.d0
    !do iel=1,nel
    !    mtx_inter_loc=0.d0
    !    do ih=1,nh
    !        ksi1=ham(ih,1)
    !        ksi2=ham(ih,2)
    !        esp=thickness(inc(iel,12))
    !        call Fforma3(ksi1,ksi2,fi,dfi)
    !        A0=0.0d0
    !        do i=1,dim
    !            do j=1,dim
    !                do k=1,10
    !                    A0(i,j)=A0(i,j)+dfi(k,j)*x(dim*(inc(iel,k)-1)+i)
    !                enddo !k
    !            enddo !j
    !        enddo !i
    !        J0=A0(1,1)*A0(2,2)-A0(2,1)*A0(1,2)
    !        mtx_inter_ham(:,1)=(/fi(1)*fi(1),0.d0,fi(2)*fi(1),0.d0,fi(3)*fi(1),0.d0,fi(4)*fi(1),0.d0,fi(5)*fi(1),0.d0,fi(6)*fi(1),0.d0,fi(7)*fi(1),0.d0,fi(8)*fi(1),0.d0,fi(9)*fi(1),0.d0,fi(10)*fi(1),0.d0/)
    !        mtx_inter_ham(:,2)=(/0.d0,fi(1)*fi(1),0.d0,fi(2)*fi(1),0.d0,fi(3)*fi(1),0.d0,fi(4)*fi(1),0.d0,fi(5)*fi(1),0.d0,fi(6)*fi(1),0.d0,fi(7)*fi(1),0.d0,fi(8)*fi(1),0.d0,fi(9)*fi(1),0.d0,fi(10)*fi(1)/)
    !        mtx_inter_ham(:,3)=(/fi(1)*fi(2),0.d0,fi(2)*fi(2),0.d0,fi(3)*fi(2),0.d0,fi(4)*fi(2),0.d0,fi(5)*fi(2),0.d0,fi(6)*fi(2),0.d0,fi(7)*fi(2),0.d0,fi(8)*fi(2),0.d0,fi(9)*fi(2),0.d0,fi(10)*fi(2),0.d0/)
    !        mtx_inter_ham(:,4)=(/0.d0,fi(1)*fi(2),0.d0,fi(2)*fi(2),0.d0,fi(3)*fi(2),0.d0,fi(4)*fi(2),0.d0,fi(5)*fi(2),0.d0,fi(6)*fi(2),0.d0,fi(7)*fi(2),0.d0,fi(8)*fi(2),0.d0,fi(9)*fi(2),0.d0,fi(10)*fi(2)/)
    !        mtx_inter_ham(:,5)=(/fi(1)*fi(3),0.d0,fi(2)*fi(3),0.d0,fi(3)*fi(3),0.d0,fi(4)*fi(3),0.d0,fi(5)*fi(3),0.d0,fi(6)*fi(3),0.d0,fi(7)*fi(3),0.d0,fi(8)*fi(3),0.d0,fi(9)*fi(3),0.d0,fi(10)*fi(3),0.d0/)
    !        mtx_inter_ham(:,6)=(/0.d0,fi(1)*fi(3),0.d0,fi(2)*fi(3),0.d0,fi(3)*fi(3),0.d0,fi(4)*fi(3),0.d0,fi(5)*fi(3),0.d0,fi(6)*fi(3),0.d0,fi(7)*fi(3),0.d0,fi(8)*fi(3),0.d0,fi(9)*fi(3),0.d0,fi(10)*fi(3)/)
    !        mtx_inter_ham(:,7)=(/fi(1)*fi(4),0.d0,fi(2)*fi(4),0.d0,fi(3)*fi(4),0.d0,fi(4)*fi(4),0.d0,fi(5)*fi(4),0.d0,fi(6)*fi(4),0.d0,fi(7)*fi(4),0.d0,fi(8)*fi(4),0.d0,fi(9)*fi(4),0.d0,fi(10)*fi(4),0.d0/)
    !        mtx_inter_ham(:,8)=(/0.d0,fi(1)*fi(4),0.d0,fi(2)*fi(4),0.d0,fi(3)*fi(4),0.d0,fi(4)*fi(4),0.d0,fi(5)*fi(4),0.d0,fi(6)*fi(4),0.d0,fi(7)*fi(4),0.d0,fi(8)*fi(4),0.d0,fi(9)*fi(4),0.d0,fi(10)*fi(4)/)
    !        mtx_inter_ham(:,9)=(/fi(1)*fi(5),0.d0,fi(2)*fi(5),0.d0,fi(3)*fi(5),0.d0,fi(4)*fi(5),0.d0,fi(5)*fi(5),0.d0,fi(6)*fi(5),0.d0,fi(7)*fi(5),0.d0,fi(8)*fi(5),0.d0,fi(9)*fi(5),0.d0,fi(10)*fi(5),0.d0/)
    !        mtx_inter_ham(:,10)=(/0.d0,fi(1)*fi(5),0.d0,fi(2)*fi(5),0.d0,fi(3)*fi(5),0.d0,fi(4)*fi(5),0.d0,fi(5)*fi(5),0.d0,fi(6)*fi(5),0.d0,fi(7)*fi(5),0.d0,fi(8)*fi(5),0.d0,fi(9)*fi(5),0.d0,fi(10)*fi(5)/)
    !        mtx_inter_ham(:,11)=(/fi(1)*fi(6),0.d0,fi(2)*fi(6),0.d0,fi(3)*fi(6),0.d0,fi(4)*fi(6),0.d0,fi(5)*fi(6),0.d0,fi(6)*fi(6),0.d0,fi(7)*fi(6),0.d0,fi(8)*fi(6),0.d0,fi(9)*fi(6),0.d0,fi(10)*fi(6),0.d0/)
    !        mtx_inter_ham(:,12)=(/0.d0,fi(1)*fi(6),0.d0,fi(2)*fi(6),0.d0,fi(3)*fi(6),0.d0,fi(4)*fi(6),0.d0,fi(5)*fi(6),0.d0,fi(6)*fi(6),0.d0,fi(7)*fi(6),0.d0,fi(8)*fi(6),0.d0,fi(9)*fi(6),0.d0,fi(10)*fi(6)/)
    !        mtx_inter_ham(:,13)=(/fi(1)*fi(7),0.d0,fi(2)*fi(7),0.d0,fi(3)*fi(7),0.d0,fi(4)*fi(7),0.d0,fi(5)*fi(7),0.d0,fi(6)*fi(7),0.d0,fi(7)*fi(7),0.d0,fi(8)*fi(7),0.d0,fi(9)*fi(7),0.d0,fi(10)*fi(7),0.d0/)
    !        mtx_inter_ham(:,14)=(/0.d0,fi(1)*fi(7),0.d0,fi(2)*fi(7),0.d0,fi(3)*fi(7),0.d0,fi(4)*fi(7),0.d0,fi(5)*fi(7),0.d0,fi(6)*fi(7),0.d0,fi(7)*fi(7),0.d0,fi(8)*fi(7),0.d0,fi(9)*fi(7),0.d0,fi(10)*fi(7)/)
    !        mtx_inter_ham(:,15)=(/fi(1)*fi(8),0.d0,fi(2)*fi(8),0.d0,fi(3)*fi(8),0.d0,fi(4)*fi(8),0.d0,fi(5)*fi(8),0.d0,fi(6)*fi(8),0.d0,fi(7)*fi(8),0.d0,fi(8)*fi(8),0.d0,fi(9)*fi(8),0.d0,fi(10)*fi(8),0.d0/)
    !        mtx_inter_ham(:,16)=(/0.d0,fi(1)*fi(8),0.d0,fi(2)*fi(8),0.d0,fi(3)*fi(8),0.d0,fi(4)*fi(8),0.d0,fi(5)*fi(8),0.d0,fi(6)*fi(8),0.d0,fi(7)*fi(8),0.d0,fi(8)*fi(8),0.d0,fi(9)*fi(8),0.d0,fi(10)*fi(8)/)
    !        mtx_inter_ham(:,17)=(/fi(1)*fi(9),0.d0,fi(2)*fi(9),0.d0,fi(3)*fi(9),0.d0,fi(4)*fi(9),0.d0,fi(5)*fi(9),0.d0,fi(6)*fi(9),0.d0,fi(7)*fi(9),0.d0,fi(8)*fi(9),0.d0,fi(9)*fi(9),0.d0,fi(10)*fi(9),0.d0/)
    !        mtx_inter_ham(:,18)=(/0.d0,fi(1)*fi(9),0.d0,fi(2)*fi(9),0.d0,fi(3)*fi(9),0.d0,fi(4)*fi(9),0.d0,fi(5)*fi(9),0.d0,fi(6)*fi(9),0.d0,fi(7)*fi(9),0.d0,fi(8)*fi(9),0.d0,fi(9)*fi(9),0.d0,fi(10)*fi(9)/)
    !        mtx_inter_ham(:,19)=(/fi(1)*fi(10),0.d0,fi(2)*fi(10),0.d0,fi(3)*fi(10),0.d0,fi(4)*fi(10),0.d0,fi(5)*fi(10),0.d0,fi(6)*fi(10),0.d0,fi(7)*fi(10),0.d0,fi(8)*fi(10),0.d0,fi(9)*fi(10),0.d0,fi(10)*fi(10),0.d0/)
    !        mtx_inter_ham(:,20)=(/0.d0,fi(1)*fi(10),0.d0,fi(2)*fi(10),0.d0,fi(3)*fi(10),0.d0,fi(4)*fi(10),0.d0,fi(5)*fi(10),0.d0,fi(6)*fi(10),0.d0,fi(7)*fi(10),0.d0,fi(8)*fi(10),0.d0,fi(9)*fi(10),0.d0,fi(10)*fi(10)/)
    !        mtx_inter_loc=mtx_inter_loc+mtx_inter_ham*wh(ih)*J0*esp
    !    enddo !ih
    !    do i=1,10
    !        do ig=1,dim
    !            do j=1,10
    !                do jg=1,dim
    !                    k1=dim*(inc(iel,i)-1)+ig
    !                    k2=dim*(inc(iel,j)-1)+jg
    !                    density=materials(inc(iel,11),3)
    !                    mtx_inter_glob(k1,k2)=mtx_inter_glob(k1,k2)+mtx_inter_loc(dim*(i-1)+ig,dim*(j-1)+jg)
    !                    !mtx_mg(k1,k2)=mtx_mg(k1,k2)+density*mtx_inter_loc(dim*(i-1)+ig,dim*(j-1)+jg)
    !                enddo !jg
    !            enddo !j
    !        enddo !ig
    !    enddo !i
    !enddo !iel
    !Fvol_add=matmul(mtx_inter_glob,vec_campo) !cálculo dos incrementos de força de campo
    !
    !f_ext=f_ext+Fvol_add
    !deallocate (vec_campo,Fvol_add,mtx_inter_glob)
    !
    !MONTAGEM DA MATRIZ DE MASSA GLOBAL - FINAL
    
    !Primeira posição tentativa Y=X
    allocate (y(dim*nnos),fig(dim*nnos),g(dim*nnos),pos_y(dim*nnos,nt))
    y=x
    yf=xf !posição tentativa dos nós de fibra
    !mtx_c=0.d0 !matriz de amortecimento zerada -> não há amortecimento no problema
    !vec_vel=0.d0
    !vec_ace=0.0d0 !zerando o vetor de aceleração por precaução
    beta=1.d0/4.d0 !valor padrão
    gama=1.d0/2.d0 !valor padrão
    
    beta = 1.
    gama = 0.
    
    write(50,*) 'VALORES DE DESLOCAMENTO VERTICAL'
    write(60,*) 'VALORES DE FORÇA INTERNA'

    do passo=1,nt
        
        equilibrio=.false.
        interacao=0 !zerando o contador de interações por passo
        gambs_count=1 !É isso mesmo: isso é uma gambiarra

        !Incrementos para o passo
        tempo_passo=delta_t*passo !atualizando o tempo correspondente ao passo
        f_ext=f_ext+df
        y=y+dy
        !Qs=y/(beta*delta_t**2)+(1/(2*beta)-1)*vec_ace+vec_vel/(beta*delta_t)
        !Rs=delta_t*(1-gama)!*vec_ace+vec_vel
        
        do while (.not.equilibrio)
            
            interacao=interacao+1 !contador
            fig=0.d0
            !hg=0.d0
            hl=0.d0
            fil_fib=0.d0
            hl_fib=0.d0
            fic_fib=0.d0
            hc_fib=0.d0

            do iel=1,nel !elementos finitos
                hg=0.d0
                do ih=1,nh !Pontos de hammer
                    ksi1=ham(ih,1)
                    ksi2=ham(ih,2)
                    call Fforma3(ksi1,ksi2,fi,dfi)
                    A0=0.0d0
                    A1=0.0d0
                    do i=1,dim
                        do j=1,dim
                            do k=1,10
                                A0(i,j)=A0(i,j)+dfi(k,j)*x(dim*(inc(iel,k)-1)+i)
                                A1(i,j)=A1(i,j)+dfi(k,j)*y(dim*(inc(iel,k)-1)+i)
                            enddo !k
                        enddo !j
                    enddo !i
                    J0=A0(1,1)*A0(2,2)-A0(2,1)*A0(1,2)
                    inv_A0=A0
                    call GETRF(inv_A0,ipiv)
                    call GETRI(inv_A0,ipiv)
                    mtx_A=matmul(A1,inv_A0)
                    C=matmul(transpose(mtx_A),mtx_A)
                    Eg=0.5*(C-Id) !deformação de green
                    
                    young=young_old(nh*(iel-1)+ih) !young do passo anterior. Caso seja o 1o passo, o young é o íntegro
                    !young=materials(inc(iel,11),1)
                    poisson=materials(inc(iel,11),2)
                    esp=thickness(inc(iel,12))
                    !tensões principais com dano do passo anterior:
                    G_const=young/(2*(1+poisson))
                    if (ep==0) then !EPD
                        K_const=young/((1+poisson)*(1-2*poisson))
                        !Lei constitutiva para EPD
                        S(1,1)=K_const*((1-poisson)*Eg(1,1)+poisson*Eg(2,2))
                        S(2,2)=K_const*((1-poisson)*Eg(2,2)+poisson*Eg(1,1))
                    else !ep==1 !EPT
                        K_const=young/(1-poisson*poisson)
                        S(1,1)=K_const*(Eg(1,1)+poisson*Eg(2,2))
                        S(2,2)=K_const*(poisson*Eg(1,1)+Eg(2,2))
                    endif
                    S(1,2)=2*G_const*Eg(1,2)
                    S(2,1)=2*G_const*Eg(2,1)
                    TENP(1) = 0.5D0*(S(1,1)+S(2,2)) + SQRT(0.25D0*(S(1,1)-S(2,2))**2 + S(1,2)**2)
                    TENP(2) = 0.5D0*(S(1,1)+S(2,2)) - SQRT(0.25D0*(S(1,1)-S(2,2))**2 + S(1,2)**2)
                    If(ep==0) TENP(3) = POISSON*(S(1,1)+S(2,2)) !EPD
                    If(ep==1) TENP(3) = 0.0D0 !EPT
                    
                    !DEFORMAÇÕES PRINCIPAIS A PARTIR DAS TENSÕES PRINCIPAIS
                    G_const = YOUNG/(2*(1+POISSON))
                    If (ep==0) Then ! Estado Plano de Deformação
                        DEFP(1) = (1/(2*G_const)) * (TENP(1) - POISSON*(TENP(1)+TENP(2)))
                        DEFP(2) = (1/(2*G_const)) * (TENP(2) - POISSON*(TENP(1)+TENP(2)))
                        DEFP(3) = 0.0D0
                    Endif
                    If (ep==1) Then ! Estado Plano de Tensões
                        DEFP(1) = (1/(2*G_const)) * TENP(1) - (POISSON/YOUNG) * (TENP(1)+TENP(2))
                        DEFP(2) = (1/(2*G_const)) * TENP(2) - (POISSON/YOUNG) * (TENP(1)+TENP(2))
                        DEFP(3) = - (POISSON/YOUNG) * (TENP(1)+TENP(2))
                    Endif
                    
                    !!dano deve ser incluído aqui
                    !defp(1)=0.5*(Eg(1,1)+Eg(2,2))+sqrt(0.25*(Eg(1,1)-Eg(2,2))*(Eg(1,1)-Eg(2,2))+Eg(1,2)*Eg(1,2))
                    !defp(2)=0.5*(Eg(1,1)+Eg(2,2))-sqrt(0.25*(Eg(1,1)-Eg(2,2))*(Eg(1,1)-Eg(2,2))+Eg(1,2)*Eg(1,2))
                    !if (ep==0) then
                    !    !para EPD:
                    !    defp(3)=0.d0
                    !else
                    !    !para EPT:
                    !    defp(3)=-1.0*poisson*(defp(1)+defp(2))/(1.d0-poisson)
                    !endif
                    e_equiv=0.d0
                    do i=1,3
                        e_equiv=e_equiv+0.25*(defp(i)+abs(defp(i)))*(defp(i)+abs(defp(i)))
                    enddo !i
                    e_equiv=sqrt(e_equiv)
                    
                    k = nh*(iel-1)+ih
                    dano=0.d0
                    if (interacao==1) then !não verifica dano e utiliza o young original
                        young=young_original(k)
                        !young=young_old(k)
                    else
                        if (e_equiv>Sd_old(k)) then !verifica dano
                            !call Fdano(At,Ac,Bt,Bc,Sd_old(k),e_equiv,young,poisson,defp,dano)
                            call Fdano(At,Ac,Bt,Bc,ed0,e_equiv,young,poisson,defp,dano)
                            if (dano>0.9) dano=0.9
                            ! MODIFICADO
                            if (dano < 0.) dano = 0.
                            !if ((iel==247).or.(iel==222).or.(iel==395).or.(iel==394).or.(iel==224).or.(iel==393).or.(iel==214).or.(iel==219).or.(iel==248).or.(iel==234).or.(iel==396)) dano=0. !consideração de elementos elásticos
                            !if ((iel==1215).OR.(iel==1216).OR.(iel==1217).OR.(iel==1218).OR.(iel==7).OR.(iel==8).OR.(iel==9).OR.(iel==10)) dano=0.d0 !consideração de elementos elásticos
                            if (dano > dano_old(k)) then
                                dano_old(k) = dano
                                Sd_new(k)=e_equiv
                                young_new(k)=young_original(k)*(1.d0-dano) !atualização da propriedade elástica danificada
                                young=young_new(k)
                            else
                                young_new(k)=young_original(k)*(1.d0-dano_old(k)) !atualização da propriedade elástica danificada
                                young=young_new(k)
                            end if
                        endif
                    endif
                    !Tensões com dano do passo corrente
                    G_const=young/(2*(1+poisson))
                    if (ep==0) then
                        K_const=young/((1+poisson)*(1-2*poisson))
                        !Lei constitutiva para EPD
                        S(1,1)=K_const*((1-poisson)*Eg(1,1)+poisson*Eg(2,2))
                        S(2,2)=K_const*((1-poisson)*Eg(2,2)+poisson*Eg(1,1))
                    else !ep==1 
                        K_const=young/(1-poisson*poisson)
                        S(1,1)=K_const*(Eg(1,1)+poisson*Eg(2,2))
                        S(2,2)=K_const*(poisson*Eg(1,1)+Eg(2,2))
                    endif
                    S(1,2)=2*G_const*Eg(1,2)
                    S(2,1)=2*G_const*Eg(2,1)
                    do ib=1,10 !quantidade de nós do elemento
                        do ia=1,dim !quantidade de graus de liberdade por nó
                            !Trecho programado para 2D. pensar dps para o caso 3D
                            DA1ab=0.0d0
                            DA1ab(ia,:)=(/dfi(ib,1),dfi(ib,2)/)
                            DEab=0.5*(matmul(matmul(transpose(inv_A0),transpose(DA1ab)),matmul(A1,inv_A0))+matmul(matmul(transpose(inv_A0),transpose(A1)),matmul(DA1ab,inv_A0)))
                            fiel=0.0d0
                            do i=1,dim
                                do j=1,dim
                                    fiel=fiel+DEab(i,j)*S(i,j)
                                enddo !j
                            enddo !i
                            k=dim*(inc(iel,ib)-1)+ia
                            fig(k)=fig(k)+fiel*wh(ih)*J0*esp !construção vetor de forças internas globais
                            !Trecho programado para 2D. pensar dps para o caso 3D
                            !Trecho programado para EPD
                            if (ep==0) then !EPD
                                DS(1,1)=K_const*(1-poisson)*DEab(1,1)+K_const*poisson*DEab(2,2)
                                DS(2,2)=K_const*poisson*DEab(1,1)+K_const*(1-poisson)*DEab(2,2)
                            else !EPT
                                DS(1,1)=K_const*(DEab(1,1)+poisson*DEab(2,2))
                                DS(2,2)=K_const*(poisson*DEab(1,1)+DEab(2,2))
                            endif
                            DS(1,2)=2*G_const*DEab(1,2)
                            DS(2,1)=2*G_const*DEab(2,1)
                            do iz=1,10
                                do ig=1,dim
                                    !Trecho programado para 2D. pensar dps para o caso 3D
                                    DA1gz=0.0d0
                                    DA1gz(ig,:)=(/dfi(iz,1),dfi(iz,2)/)
                                    DEgz=0.5*(matmul(matmul(transpose(inv_A0),transpose(DA1gz)),matmul(A1,inv_A0))+matmul(matmul(transpose(inv_A0),transpose(A1)),matmul(DA1gz,inv_A0)))
                                    D2E=0.5*(matmul(matmul(transpose(inv_A0),transpose(DA1ab)),matmul(DA1gz,inv_A0))+matmul(matmul(transpose(inv_A0),transpose(DA1gz)),matmul(DA1ab,inv_A0)))
                                    hl=0.d0
                                    do i=1,dim
                                        do j=1,dim
                                            hl=hl+DEgz(i,j)*DS(i,j)+S(i,j)*D2E(i,j)
                                        enddo !j
                                    enddo !i
                                    i=dim*(ib-1)+ia
                                    j=dim*(iz-1)+ig
                                    hg(i,j)=hg(i,j)+hl*wh(ih)*J0*esp
                                enddo !ig
                            enddo !iz
                        enddo !ia
                    enddo !ib   
                enddo !ih pontos de hammer 
                Call add_matrix(SPMAT, iel, hg)
            enddo !iel elementos finitos de chapa
            
            do ief=1,nef !loop nos elementos de fibra
                k1=inc_fib(ief,1)
                k2=inc_fib(ief,2)
                l02=(xf(dim*(k1-1)+1)-xf(dim*(k2-1)+1))**2+(xf(dim*(k1-1)+2)-xf(dim*(k2-1)+2))**2
                l2 =(yf(dim*(k1-1)+1)-yf(dim*(k2-1)+1))**2+(yf(dim*(k1-1)+2)-yf(dim*(k2-1)+2))**2
                egf=(l2-l02)/l02*0.5
                sf=inc_fib(ief,4)*egf
                kt=inc_fib(ief,4)
                do ib=1,2 !loop nos nós da fibra
                    do ia=1,dim !loop nos graus de liberdade por nó
                        k=dim*(ib-1)+ia !grau de liberdade local
                        dedyba=(-1.)**ib/l02*(yf(dim*(k2-1)+ia)-yf(dim*(k1-1)+ia))
                        fil_fib(k)=dedyba*sf*inc_fib(ief,3)*(l02**.5) !Força interna nos nós locais da fibra
                        !FORÇA INTERNA É CORRIGIDA:
                        !fil_fib(k)=fil_fib(k)*((l02/l2)**.5) !Passagem necessária para estar se utilizando (2.55')
                        do iz=1,2
                            do ig=1,dim
                                l=dim*(iz-1)+ig
                                dedyxg=(-1.)**iz/l02*(yf(dim*(k2-1)+ig)-yf(dim*(k1-1)+ig))
                                rh_dy=dedyba*dedyxg*(kt*inc_fib(ief,3)*l02**.5) !Parcela da Hessiana independente da tensão
                                d2edy2baxg=0.
                                if (ia.eq.ig) d2edy2baxg=(-1.)**ib*(-1.)**iz/l02
                                rh_dy2=d2edy2baxg*sf*inc_fib(ief,3)*l02**.5 !Parcela da Hessiana depende da tensão
                                hl_fib(k,l)=rh_dy+rh_dy2
                                !hl_fib(k,l)=hl_fib(k,l)*(l02/l2) !corrigindo a hessiana (teste-apagar)
                                !HESSIANA NÃO É CORRIGIDA por (2.70')
                            enddo !ig
                        enddo !iz
                    enddo !ia
                enddo !ib
                mffi=0.d0
                indexx=0
                !Para o primeiro nó da fibra apenas
                call Fforma3(identify(inc_fib(ief,1),2),identify(inc_fib(ief,1),3),fi,dfi)
                do ii=1,10
                    mffi(1,2*ii-1)=fi(ii)
                    mffi(2,2*ii)=fi(ii)
                    indexx(2*ii-1)=2*inc(identify(inc_fib(ief,1),1),ii)-1
                    indexx(2*ii)=2*inc(identify(inc_fib(ief,1),1),ii)
                enddo !ii
                !Para o segundo nó da fibra apenas
                call Fforma3(identify(inc_fib(ief,2),2),identify(inc_fib(ief,2),3),fi,dfi)
                do ii=1,10
                    mffi(3,20+2*ii-1)=fi(ii)
                    mffi(4,20+2*ii)=fi(ii)
                    indexx(20+2*ii-1)=2*inc(identify(inc_fib(ief,2),1),ii)-1
                    indexx(20+2*ii)=2*inc(identify(inc_fib(ief,2),1),ii)
                enddo !ii
                !Compatibilização da força interna e da hessiana:
                fic_fib=matmul(transpose(mffi),fil_fib)
                hc_fib=matmul(matmul(transpose(mffi),hl_fib),mffi)
                !Atribuição na matriz de rigidez global:
                do ii=1,40
                    fig(indexx(ii))=fig(indexx(ii))+fic_fib(ii)
                enddo !ii
                Call add_matrix(SPMAT, nel + ief, hc_fib)
                !do ii=1,40
                !    do jj=1,40
                !        hg(indexx(ii),indexx(jj))=hg(indexx(ii),indexx(jj))+hc_fib(ii,jj)
                !    enddo !jj
                !enddo !ii
            enddo !ief
            
            !Acrescentando a parcela dinâmica para a força interna global:
            !fig=fig+matmul(mtx_mg,y)/beta/delta_t**2-matmul(mtx_mg,Qs)+gama*matmul(mtx_c,y)/beta/delta_t+matmul(mtx_c,Rs)-gama*delta_t*matmul(mtx_c,Qs)
            !Acrescentando a parcela dinâmica para a matriz hessiana global:
            !hg=hg+mtx_mg/beta/delta_t**2+gama*mtx_c/beta/delta_t
            
            !Calculando o vetor de resíduo mecânico
            g=f_ext-fig !vetor g = -g
            !Aplicando as restrições
            Do i = 1,comprimento !manter como está
                k=dim*(vinc(i,1)-1)+vinc(i,2) !grau de liberdade
                Call set_value_to_row(SPMAT,k,0.0D0)
                Call set_value_to_col(SPMAT,k,0.0D0)
                Call set_value_in_term(SPMAT,k,k,1.0D0)
                g(k) = 0.0D0
            Enddo
            
            ! Solução do sistema linear:
            !aqui deltay era igualado a g
            Call solve_system_of_equation(SPMAT, g)
            ! Call solve_system_of_equation(SPMAT, DELY, 1)
            Call clear_data(SPMAT)
    
            y=y+g !g=deltay
            
            !Cálculo das novas posições dos nós da fibra:
            yf=0.d0
            do inf=1,nnf !loop dos nós de fibra
                elem=identify(inf,1)
                ksi1=identify(inf,2)
                ksi2=identify(inf,3)
                call Fforma3(ksi1,ksi2,fi,dfi)
                do i=1,10
                    yf(2*(inf-1)+1)=yf(2*(inf-1)+1)+fi(i)*y(2*(inc(elem,i)-1)+1)
                    yf(2*(inf-1)+2)=yf(2*(inf-1)+2)+fi(i)*y(2*(inc(elem,i)-1)+2)
                enddo !i
            enddo !inf
    
            !Atualização da aceleração e da velocidade: 
            !vec_ace=y/beta/delta_t**2-Qs
            !vec_vel=gama*y/beta/delta_t+Rs-gama*delta_t*Qs
            
            !Cálculo do erro
            e1=0
            e2=0
            do i=1,dim*nnos
                e1=e1+g(i)**2 !g=deltay
                e2=e2+x(i)**2
            enddo !i
            erro=sqrt(e1/e2)
            write(*,*) int(passo),real(erro),real(e1),real(e2),int(interacao)
            !young_old=young_new
            !Sd_old=Sd_new
            !condição para o equilíbrio ser alcançado
            if (erro<tol) then
                equilibrio=.true.
                write(50,*) (y(2*(157-1)+2)-x(2*(157-1)+2))*-1.d0 !deslocamento y
                write(60,*) (fig(2*(5761-1)+2))*-1.d0 ! força interna y
                !write(70,*) (y(2*(4-1)+1)-x(2*(4-1)+1)) !deslocamento x
                gambs_count=gambs_count-1
                tol=tol/(10**(2*gambs_count))
                pos_y(:,passo)=y !guardando valor de y para pós-processamento
                pos_yf(:,passo)=yf !guardando valor de yf (posição atualizada das fibras) para pós-processamento
                pos_dano(:,passo)=dano_old !guardando valor do dano para pós processamento
                young_old=young_new
                Sd_old=Sd_new
            endif
        enddo !while
    enddo !passo
    
    deallocate (df,g)
    allocate (sigx_ham(nh), sigy_ham(nh), talxy_ham(nh), talyx_ham(nh), dano_ham(nh))
    sigx_ham=0.d0
    sigy_ham=0.d0
    talxy_ham=0.d0
    talyx_ham=0.d0
    dano_ham=0.d0
    matsigma=0.d0
    adm_coord(:,1)=(/1.d0,2.d0/3.d0,1.d0/3.d0,0.d0,2.d0/3.d0,1.d0/3.d0,0.d0,1.d0/3.d0,0.d0,0.d0/)
    adm_coord(:,2)=(/0.d0,1.d0/3.d0,2.d0/3.d0,1.d0,0.d0,1.d0/3.d0,2.d0/3.d0,0.d0,1.d0/3.d0,0.d0/)
    Sd_new=Sd_old
    young_new=young_original
    
    do passo=1,nt
        y=pos_y(:,passo) !posições calculadas para o passo corrente
        Sd_old=Sd_new !Sd do passo anterior
        young_old=young_new !young do passo anterior
        do iel=1,nel
            do ih=1,nh
                ksi1=ham(ih,1)
                ksi2=ham(ih,2)
                call Fforma3(ksi1,ksi2,fi,dfi)
                A0=0.0d0
                A1=0.0d0
                do i=1,dim
                    do j=1,dim
                        do k=1,10
                            A0(i,j)=A0(i,j)+dfi(k,j)*x(dim*(inc(iel,k)-1)+i)
                            A1(i,j)=A1(i,j)+dfi(k,j)*y(dim*(inc(iel,k)-1)+i)
                        enddo !k
                    enddo !j
                enddo !i
                inv_A0=A0
                call GETRF(inv_A0,ipiv)
                call GETRI(inv_A0,ipiv)
                mtx_A=matmul(A1,inv_A0)
                JA=mtx_A(1,1)*mtx_A(2,2)-mtx_A(2,1)*mtx_A(1,2)
                C=matmul(transpose(mtx_A),mtx_A)
                Eg=0.5*(C-Id) !deformação de green
                young=young_old(nh*(iel-1)+ih) !young do passo anterior. Caso seja o 1o passo, o young é o íntegro
                poisson=materials(inc(iel,11),2)
                esp=thickness(inc(iel,12))
                
                !!==================================================================================================
                !! Pode-se usar a variável de dano já salva em dano_old e já usar o módulo danificado young_new:
                !!dano deve ser incluído aqui
                !defp(1)=0.5*(Eg(1,1)+Eg(2,2))+sqrt(0.25*(Eg(1,1)-Eg(2,2))*(Eg(1,1)-Eg(2,2))+Eg(1,2)*Eg(1,2))
                !defp(2)=0.5*(Eg(1,1)+Eg(2,2))-sqrt(0.25*(Eg(1,1)-Eg(2,2))*(Eg(1,1)-Eg(2,2))+Eg(1,2)*Eg(1,2))
                !if (ep==0) then
                !    !para EPD:
                !    defp(3)=0.d0
                !else
                !    !para EPT:
                !    defp(3)=-1.0*poisson*(defp(1)+defp(2))/(1.d0-poisson)
                !endif
                !e_equiv=0.d0
                !do i=1,3
                !    e_equiv=e_equiv+0.25*(defp(i)+abs(defp(i)))*(defp(i)+abs(defp(i)))
                !enddo !i
                !e_equiv=sqrt(e_equiv)
                !k=nh*(iel-1)+ih
                !dano=0.d0
                !if (e_equiv>Sd_old(k)) then !verifica dano
                !    call Fdano(At,Ac,Bt,Bc,Sd_old(k),e_equiv,young,poisson,defp,dano)
                !    Sd_new(k)=e_equiv
                !    young=young_old(k)*(1.d0-dano) !atualização da propriedade elástica danificada
                !    young_new(k)=young
                !endif
                !!==================================================================================================
                
                dano = pos_dano(nh*(iel-1)+ih,passo) ! Éverton !atualização sergiomar (18-05-21)
                
                G_const=young/(2*(1+poisson))
                if (ep==0) then
                    K_const=young/((1+poisson)*(1-2*poisson))
                    !Lei constitutiva para EPD
                    S(1,1)=K_const*((1-poisson)*Eg(1,1)+poisson*Eg(2,2))
                    S(2,2)=K_const*((1-poisson)*Eg(2,2)+poisson*Eg(1,1))
                else !ep==1 
                    K_const=young/(1-poisson*poisson)
                    S(1,1)=K_const*(Eg(1,1)+poisson*Eg(2,2))
                    S(2,2)=K_const*(poisson*Eg(1,1)+Eg(2,2))  
                endif
                S(1,2)=2*G_const*Eg(1,2)
                S(2,1)=2*G_const*Eg(2,1)
                sig_cauchy=matmul(matmul(mtx_A,S),transpose(mtx_A))/JA
                sigx_ham(ih)=sig_cauchy(1,1)
                sigy_ham(ih)=sig_cauchy(2,2)
                talxy_ham(ih)=sig_cauchy(1,2)
                talyx_ham(ih)=sig_cauchy(2,1)
                dano_ham(ih)=dano
                call Fforma2(ksi1,ksi2,fi_quad)
                do i=1,6
                    mtx_L(ih,i)=fi_quad(i)
                enddo !i
            enddo !ih
            Tr_mtx_L=Transpose(mtx_L)
            v_sigx=matmul(Tr_mtx_L,sigx_ham)
            v_sigy=matmul(Tr_mtx_L,sigy_ham)
            v_talxy=matmul(Tr_mtx_L,talxy_ham)
            v_talyx=matmul(Tr_mtx_L,talyx_ham)
            v_dano=matmul(Tr_mtx_L,dano_ham)
            mtx_M=matmul(Tr_mtx_L,mtx_L)
            inv_mtx_M=mtx_M
            call GETRF(inv_mtx_M,ipiv3)
            call GETRI(inv_mtx_M,ipiv3)
            sigx_node2=matmul(inv_mtx_M,v_sigx)
            sigy_node2=matmul(inv_mtx_M,v_sigy)
            talxy_node2=matmul(inv_mtx_M,v_talxy)
            talyx_node2=matmul(inv_mtx_M,v_talyx)
            dano_node2=matmul(inv_mtx_M,v_dano)
            sigx_node=0.d0
            sigy_node=0.d0
            talxy_node=0.d0
            talyx_node=0.d0
            dano_node=0.d0
            do ino=1,10
                ksi1=adm_coord(ino,1)
                ksi2=adm_coord(ino,2)
                call Fforma2(ksi1,ksi2,fi_quad)
                do i=1,6
                    sigx_node(ino)=sigx_node(ino)+sigx_node2(i)*fi_quad(i)
                    sigy_node(ino)=sigy_node(ino)+sigy_node2(i)*fi_quad(i)
                    talxy_node(ino)=talxy_node(ino)+talxy_node2(i)*fi_quad(i)
                    talyx_node(ino)=talyx_node(ino)+talyx_node2(i)*fi_quad(i)
                    dano_node(ino)=dano_node(ino)+dano_node2(i)*fi_quad(i)
                    ! Na extrapolação pode acontecer do dano ser menor que 0 ou maior que 1, pra evitar isso:
                    If (dano_node(ino) < 0.0d0) dano_node(ino) = 0.0d0 ! Éverton
                    If (dano_node(ino) > 1.0d0) dano_node(ino) = 1.0d0 ! Éverton
                enddo !i
            enddo !ino
            do ino=1,10
                matsigma(inc(iel,ino),1,passo)=matsigma(inc(iel,ino),1,passo)+sigx_node(ino)
                matsigma(inc(iel,ino),2,passo)=matsigma(inc(iel,ino),2,passo)+sigy_node(ino)
                matsigma(inc(iel,ino),3,passo)=matsigma(inc(iel,ino),3,passo)+talxy_node(ino)
                matsigma(inc(iel,ino),4,passo)=matsigma(inc(iel,ino),4,passo)+talyx_node(ino)
                matsigma(inc(iel,ino),5,passo)=matsigma(inc(iel,ino),5,passo)+dano_node(ino)
                matsigma(inc(iel,ino),6,passo)=matsigma(inc(iel,ino),6,passo)+1.d0
            enddo !ino
        enddo !iel
        do i=1,nnos
            matsigma(i,1,passo)=matsigma(i,1,passo)/matsigma(i,6,passo)
            matsigma(i,2,passo)=matsigma(i,2,passo)/matsigma(i,6,passo)
            matsigma(i,3,passo)=matsigma(i,3,passo)/matsigma(i,6,passo)
            matsigma(i,4,passo)=matsigma(i,4,passo)/matsigma(i,6,passo)
            matsigma(i,5,passo)=matsigma(i,5,passo)/matsigma(i,6,passo)
        enddo !i
    enddo !passo
    

    
    do passo=1,nt
        y=pos_y(:,passo)
        yf=pos_yf(:,passo)
111     format (6(1x,e27.6))
112     format (4(1x,e27.6))
200     format (20hpegar comentarios em)
201     format (1h#)
20      format (18i6)       
        if (passo.eq.1) then !passo de carga
            write(80,200)
            write(80,*)
            write(80,*) 'Arquivo de pos-processamento - txt'
            write(80,*)
            write(80,*) 'itens de linhas em branco ou com textos,'
            write(80,*) 'quando usar formato 1h# indica inicio de listas'
            write(80,*)
            write(80,*) 'nnos, nelem, nlistas'
            write(80,201)
            write(80,*) nnos+nnf,nel+nef,dim*nt+4 !cada lista um deslocamento nesse exemplo
            write(80,*)
            write(80,*) 'coordx coordy coordz deslx desly deslz'
            write(80,201)
            do i=1,nnos
                write(80,111) x(dim*(i-1)+1),x(dim*(i-1)+2),0,(y(dim*(i-1)+1)-x(dim*(i-1)+1)),(y(dim*(i-1)+2)-x(dim*(i-1)+2)),0
            enddo !i
            do i=1,nnf
                write(80,111) xf(dim*(i-1)+1),xf(dim*(i-1)+2),0,(yf(dim*(i-1)+1)-xf(dim*(i-1)+1)),(yf(dim*(i-1)+2)-xf(dim*(i-1)+2)),0
            enddo !i
            write(80,*)
            write(80,*) 'tipo grauaprox nó1 nó2'
            write(80,201)
            do j=1,nel
                write(80,20) 2,3,inc(j,1),inc(j,2),inc(j,3),inc(j,4),inc(j,5),inc(j,6),inc(j,7),inc(j,8),inc(j,9),inc(j,10),0
            enddo
            do j=1,nef
                write(80,'(5i)') 1,1,int(inc_fib(j,1))+nnos,int(inc_fib(j,2))+nnos,1
            enddo !j
        else
        endif !if passo 1
        !inicio das listas
        write(80,*)
        write(80,*) 'título da lista e dados dos nós'
        write(80,201)
        write(80,*) 'DESL X', passo
        do i=1,nnos
            write(80,112) (y(dim*(i-1)+1)-x(dim*(i-1)+1)),(y(dim*(i-1)+2)-x(dim*(i-1)+2)),0,(y(dim*(i-1)+1)-x(dim*(i-1)+1))
        enddo
        do i=1,nnf
            write(80,112) (yf(dim*(i-1)+1)-xf(dim*(i-1)+1)),(yf(dim*(i-1)+2)-xf(dim*(i-1)+2)),0,(yf(dim*(i-1)+1)-xf(dim*(i-1)+1))
        enddo !i
        write(80,*)
        write(80,*) 'título da lista e dados dos nós'
        write(80,201)
        write(80,*) 'DESL Y', passo
        do i=1,nnos
            write(80,112) (y(dim*(i-1)+1)-x(dim*(i-1)+1)),(y(dim*(i-1)+2)-x(dim*(i-1)+2)),0,(y(dim*(i-1)+2)-x(dim*(i-1)+2))
        enddo
        do i=1,nnf
            write(80,112) (yf(dim*(i-1)+1)-xf(dim*(i-1)+1)),(yf(dim*(i-1)+2)-xf(dim*(i-1)+2)),0,(yf(dim*(i-1)+2)-xf(dim*(i-1)+2))
        enddo
        
        !if (passo==nt) then
        write(80,*)
        write(80,*) 'título da lista e dados dos nós'
        write(80,201)
        write(80,*) 'SIGM X', passo
        do i=1,nnos
            write(80,112) (y(dim*(i-1)+1)-x(dim*(i-1)+1)),(y(dim*(i-1)+2)-x(dim*(i-1)+2)),0,matsigma(i,1,passo)
        enddo
        do i=1,nnf
            write(80,112) (yf(dim*(i-1)+1)-xf(dim*(i-1)+1)),(yf(dim*(i-1)+2)-xf(dim*(i-1)+2)),0,0
        enddo
        write(80,*)
        write(80,*) 'título da lista e dados dos nós'
        write(80,201)
        write(80,*) 'SIGM Y', passo
        do i=1,nnos
            write(80,112) (y(dim*(i-1)+1)-x(dim*(i-1)+1)),(y(dim*(i-1)+2)-x(dim*(i-1)+2)),0,matsigma(i,2,passo)
        enddo
        do i=1,nnf
            write(80,112) (yf(dim*(i-1)+1)-xf(dim*(i-1)+1)),(yf(dim*(i-1)+2)-xf(dim*(i-1)+2)),0,0
        enddo
        write(80,*)
        write(80,*) 'título da lista e dados dos nós'
        write(80,201)
        write(80,*) 'TAL XY', passo
        do i=1,nnos
            write(80,112) (y(dim*(i-1)+1)-x(dim*(i-1)+1)),(y(dim*(i-1)+2)-x(dim*(i-1)+2)),0,matsigma(i,3,passo)
        enddo
        do i=1,nnf
            write(80,112) (yf(dim*(i-1)+1)-xf(dim*(i-1)+1)),(yf(dim*(i-1)+2)-xf(dim*(i-1)+2)),0,0
        enddo
        !write(80,*)
        !write(80,*) 'título da lista e dados dos nós'
        !write(80,201)
        !write(80,*) 'TAL YX', passo
        !do i=1,nnos
        !    write(80,112) (y(dim*(i-1)+1)-x(dim*(i-1)+1)),(y(dim*(i-1)+2)-x(dim*(i-1)+2)),0,matsigma(i,4,passo)
        !enddo
        !do i=1,nnf
        !    write(80,112) (yf(dim*(i-1)+1)-xf(dim*(i-1)+1)),(yf(dim*(i-1)+2)-xf(dim*(i-1)+2)),0,0
        !enddo
        write(80,*)
        write(80,*) 'título da lista e dados dos nós'
        write(80,201)
        write(80,*) 'DANO', passo
        do i=1,nnos
            write(80,112) (y(dim*(i-1)+1)-x(dim*(i-1)+1)),(y(dim*(i-1)+2)-x(dim*(i-1)+2)),0,matsigma(i,5,passo)
        enddo
        do i=1,nnf
            write(80,112) (yf(dim*(i-1)+1)-xf(dim*(i-1)+1)),(yf(dim*(i-1)+2)-xf(dim*(i-1)+2)),0,0
        enddo
        !endif
    enddo !passo
    
    write(*,*)
    write(*,*)
    write(*,*) 'press start...'
    read(*,*)
    
    close(5)
    close(10)
    close(11)
    close(20)
    close(80)
    close(50)
    close(60)
    !close(100)

    end program SET5884CHAPA9

