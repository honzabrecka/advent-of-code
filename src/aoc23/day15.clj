(ns aoc23.day15)

(def input
  "mpp=7,nzr-,lms-,lq-,lhqrxp=8,psb=2,jznz-,hf-,kpns=8,gq-,lm-,hlgzpp=5,gp=1,dzd-,xx-,hm-,kvz-,lzls=1,dbl-,dzd-,hcgd=7,lff=2,gbvzq=5,lql=7,hvk-,mpp=6,tx-,rg=4,lzhv=8,pdd=9,ds-,tcr-,bhm=6,cnn=8,ntms-,bkg-,hfnk-,pvf=1,tzh=5,mp-,zhdr-,nvj-,dfl=5,ccm=5,gc-,vmrr=5,zn-,kfq-,ts=2,tf-,pz=8,gp=2,hs-,lqrlg=8,rs-,pdm=1,kvbq-,sdk=1,vr=8,cls=4,fhzbjq-,nqn-,dlssls=4,ft-,pdd-,fphd-,sjg=7,jt=9,kl-,lps=7,npr-,jdk-,jj=1,dxd-,qjngd-,xjp-,btlt-,btm=3,gjhz-,bg=3,bjn-,bkd=2,clb=6,lq=9,jvx-,pvpn=3,dqzdbx-,tsj=1,vcjdc-,jgkzf=7,jtm=9,rxsj=1,xr=1,kvj-,kh-,fkz=7,dvg-,njzgjp=4,kfz=7,dm=6,zmql=7,gr-,nmv-,fpfzc-,sr=7,lvfd-,chs-,kbf-,ds-,srl-,rxcfx-,nbp=8,qm=8,xzf-,nbxlm-,nk=6,gmn-,nbp=7,dcj-,zgnp-,zmg=5,jznz=1,tnm-,vrhb-,ljvv=4,lcm=1,zkx=3,fpfzc=8,ll=5,lmktdb=5,hfnk-,ksf-,ml-,kfp=7,zdnr=9,cv-,vl-,jgkzf=2,cg=1,srp-,lms-,cxfh=3,vq-,mgt=7,sjg-,mmgq-,qdx-,vb-,xvmb-,qjngd=3,dxd=7,jxb=4,xhn=3,vrcrz=5,zsvhrh-,tmvns=5,knb-,bbng-,fn=8,nln=6,mxm=3,hb-,qr-,sp=5,pz-,gccsrh-,hm=2,btlt=7,xtg-,dbl=1,nbxlm-,gsq=3,gf-,zlg=6,bjn-,db-,kvbq-,lr=4,ht-,fljt=2,bnvb=9,vv=7,vrhb-,hggm-,ht=2,bbng=4,zxt=7,vzf-,dvl-,nq-,pdm=3,xv=9,vq-,rxcv=6,cf-,kl=3,btm-,fnxn=7,ztv-,vrq-,srl=6,njzgjp-,nl-,vrhb-,cgr=2,fz-,hpvp-,nnpd-,jffc-,tv=3,hf=8,bl=7,xdqs-,kfp=3,bljv-,sr=7,vz=6,cg-,fr=1,tjf-,mdcjcv=8,ksd=6,qd-,lc-,qncs=4,dn-,zn-,jznz=5,fx-,ch=3,tj=9,hsr-,qxrlr=5,pmr=3,ms=2,njzgjp=8,rbjmn-,kl-,lms-,rmc-,gbvzq=7,kh=8,dcj-,tjf-,btm=6,zt=8,dj-,fr=8,zzz-,mf=3,cxfh-,jc=6,gf-,lr=3,kpnltb-,fcb-,bzkc=1,kpns=6,sr-,zsvhrh=8,thq-,phfgr-,gvtcn-,hlgzpp=2,vz-,bhm-,fhpnc=3,tg-,qncs-,btm=1,xdv-,qngk=1,lps=2,qrd=6,ksf=2,zf=3,hnd-,thl=3,bpv-,lnhp=8,vtm=8,nc-,sjg=5,rm=3,pdm-,zvf=1,pzjs-,hmhs-,dshp=8,vr-,hb-,fn=8,hvqmc-,hsbtx-,kfq=4,qnh-,kpns=4,slk=1,hvxdf=4,hgh-,fc=8,tkk=1,pvht=7,dvl-,ntms-,jfx=2,cztdj=7,lq=2,pgckd-,jtm=9,smh-,fxvg-,dcj-,jqs-,hsbtx-,mlnd-,msm-,nnpd-,xng-,lz-,nbxlm=7,jgzst-,tvzv=7,jtm=4,bkj=7,nqn-,rjqxh=3,zhx=9,mlfj-,rz-,drl=9,gf=4,fn=1,xc-,rb-,dvn=8,bcr-,hq=8,xbh=4,jmnh-,xx-,nqn-,gf=9,nvj=5,vvgx-,mgt-,dvq-,tjf=3,ljvv-,phfgr=3,ljvv=8,ngx=9,vbbdk=4,bnh-,ccfj=5,fhpnc=4,xvmb-,fz-,cg=6,qt=5,cj-,lg-,th-,drl=9,jgzst=3,jmnh=3,rjj=5,vrhb-,mfdp-,hqc-,lmmc-,tzh=5,mmgq-,hx-,nc-,qncs=5,bzkc-,nbq=5,hfn-,mlfj=9,zf-,vrc-,zhdr=7,drl-,bl=7,mtbflg-,xvnr=5,jdk-,kvbq=7,xlb-,tnkv-,lc-,vvgx-,gr-,mpp-,zl=3,kfp=6,bnvb=3,rgl-,xlb-,qtm-,hvk-,tnkv=2,vmrr=1,df=6,xgd=7,fd=8,kh=5,fhpnc-,jffc-,cl=2,hzr-,bljv-,vlc-,vv-,grm-,xhn=7,zd-,xlb-,dm=7,nln-,lcm-,rhn-,blf-,lth=3,lth-,rz=2,hsbtx-,lcqz=9,gsq=9,mdlz=7,dvjdr=3,kpns=5,lzhv=2,nqn-,mgf=1,zvl=8,dxd-,cbkk-,gm-,zzz-,rhv-,sp-,pmr-,gbvzq-,gzt=1,rkkd-,bkj-,tqhs=7,hfnk-,nq-,hlv=2,tgh-,zlsd=4,vb-,mx=2,gq=2,bdkp=4,pk-,fnxn=5,fhzbjq=9,psb-,qdq=2,fljt-,lthlq=1,nlg=5,rg=8,ms=5,zxt-,bqq=2,xv=3,mxm-,gvf-,bzkc-,kpns=4,fr=4,pvf=6,mt-,kr-,vp=5,hfnk=9,jfx-,bljv-,grchdx-,kvj-,nbq-,hzr-,fz-,pds-,lqrlg-,nq=5,nth=1,btlt-,tqb-,lg=9,vfcz-,qngk-,msm=9,npr=4,nmv=3,dqzdbx-,jjhtxb=1,djdd=6,nq=8,fmbv=2,tmvns=9,zmg-,qdq=6,rv=9,qncs=9,zd-,rg-,qd=9,cnn-,mx=8,md-,tnkv-,rm-,rdq=6,xr=2,hs=6,lthlq-,kn-,lv=3,jg=6,drl-,bbng-,md=9,dvn-,pvgstv-,tnqhlk-,dvg-,ccfj-,rb-,mt=1,psb-,btlt=6,vfk-,zdnr=5,kmrzk=7,hzqh=6,fpfzc-,njzgjp=8,fnhpf=3,kh=5,xc=8,jmnh=9,zn-,rmc=3,tfmq=5,kz-,fn-,vx-,kx=3,hqc=8,fljt-,fxvg=9,msm=9,fhpnc=4,clnxp=5,rfkbqp-,cplm=1,nmv-,vp=9,vl=8,sp-,fm=4,tmn-,lz=7,gzdxh-,mgf-,vfcz=5,lc-,slk-,gsdvk=6,fn=9,ccfj-,qn=5,ptxfn-,ztv-,hf-,ktd=9,tf=6,qt=1,nlg-,gjbm=1,dzd-,rxsj-,lth=9,qqjc=1,mfnm-,rjqxh=3,kvj=3,lms=9,lc=9,smh=7,crr-,zlg-,vd=8,hf-,kmrzk=1,mcx-,rjqxh=8,drl-,mqc-,dxd=7,ggsx=1,tvf-,hv-,nvj=5,tp=1,lt-,fk-,kfz=5,dbl-,kfp-,hvxdf=2,gvtcn=2,phfgr=2,ggsx-,rb=4,jf=1,gs=5,hvk-,fphd-,lms=3,qr-,hrq-,gm=5,pgckd=4,zlsd-,tf-,jlp=5,ktd=8,zxt-,ds=3,tqhs=9,rtxt=6,gp=4,nlg=8,gvf=6,pgltk-,thxnd-,zx=7,qjngd-,jgkzf=4,dv=9,dvjdr-,jt=4,fnxn-,fx=1,nl-,cgr-,msm=4,hch-,cj-,vb-,ft-,gr-,rg-,bkd=4,thl=2,sp-,hf=3,jf-,pgckd-,xb-,hggm-,rhv-,hrq=6,xnx=2,fljt=7,rmc=8,ssv-,jqrb-,hggm=4,gt=5,rxcfx=3,dvg=5,zjhsg-,fd-,thxnd=1,rgl-,grchdx-,tfmq-,tqhs=2,rxcv=3,rl-,pvgstv=7,cg=8,bg=5,lnhp-,blf-,gr=5,nqk-,fm-,sp=7,hsbtx=8,grchdx=1,tkk-,mfdp=4,jvg=2,fj=3,tt=6,hq-,thl-,fzt-,tv-,mdcjcv=7,lt-,tqb-,qnh=2,lthlq-,fhzbjq=8,mx=2,vz-,hlv-,hfn-,gc-,jsdb=5,kx-,tnm-,jg=5,sz=3,bts=5,kqb-,gtt=3,hsr-,zvl-,cbkk=6,jf=7,kpns-,zl=5,mp-,kfp=4,nx-,ssv-,nqk-,ccm-,gbvzq-,pds-,hpvp=1,bg=7,kv=4,sxnj-,hgc=4,gm-,dc=5,mn-,zkl-,thq=6,ccfj-,fnbgjj-,hnd=6,hvxdf=7,dvl-,xng-,kd=4,rjj-,lzhv=1,pvht-,kfq=5,nnx=5,dvjdr-,gtt-,vfcz-,btm-,jlp-,nzq=2,zzz-,mfnm=5,fljt=8,lzhv=4,qf-,zdnr-,clnxp-,dshp=2,ktd=9,pf=1,zhx=1,lmmc=3,jt=2,rsps=1,xj-,glrql=7,pk=5,pvht-,jffc=5,rlsl-,hcgd-,hcvsn=8,kh=7,xl-,cl-,rsps=2,npm=9,sm-,gf=3,vq-,tsj=6,zxr-,rrg-,bts=3,drt-,hvxdf-,tkfcrj=4,phfgr=6,jffc=9,fm=4,jt=6,smh=5,kj=2,qncs=9,crr=4,hj-,kx-,sz=7,mpp=2,nbq-,ztv=3,hm-,nshbkn-,qrd=2,pdm-,kz=5,lmktdb=8,vx=3,jg=6,ms-,pvf=8,nxm=7,df=4,tkk-,kmrzk=4,qncs=6,ncvj=6,tnkv=8,phfgr-,vrc-,dvg-,jqs-,ff-,srl-,kn=7,xlb=5,kfz=1,kx-,lcm=9,lmlbf=7,hch-,rfkbqp-,vb=6,rmc=9,rkr-,dfl=4,vcgxs=3,npm-,vrb-,dvn=5,kfz-,vr=3,dxd-,scb-,slk-,pdm=5,xhn=1,sdk-,jxb=3,hsr-,hf=8,jznz=9,lm-,ssv-,pdd-,bv=4,jj-,dcs=1,hvxdf=4,vhrcbm-,bg-,ds-,xc=3,nln=6,jfx-,ncvj-,xr-,kfz=4,fr-,vb-,fpfzc-,vv-,jssb-,zjhsg-,qdq=3,df=4,pxzc-,dc=3,fpfzc-,tmb=7,zgnp=8,xvmb=1,fp=4,jj-,tkk=7,grm-,mgt=6,tnkv-,mqc-,bkg=1,njzgjp-,fphd=9,fljt-,vt-,pvht-,lth=2,ngx=8,tbvx=9,gzt=7,hqc-,xpkljm=3,kr-,mcx-,lzls-,rhv=4,tnm=5,rxp=8,tg-,xhn-,slk=6,xbh=2,fm=4,mlfj=4,rg-,fz=2,vfk-,pf-,df=1,cf-,rhn=2,tp=1,kt-,dj-,lms=2,lq=6,nln=3,nzp=7,bkj=1,hnd=7,gsdvk-,qxrlr-,bg=3,zsvhrh=8,rtxt-,dvjdr=2,nln-,lps=3,tnqhlk=6,nxm-,bkj-,jt-,kgb=5,jznz=3,dcj=4,bcr=7,bb=6,xr=5,bb-,knb-,crr-,ksd-,cplm=5,tgh-,ghqj-,kr=6,dvg=5,vdgn-,zsckf-,nzp-,gs=9,ppk-,ksd-,ksf=3,ngx-,bfdbb-,kpnltb-,grfv=4,blf-,ztd=5,jpj=1,nj=8,bq=8,tnm-,jzs=2,rg=5,psb-,hcgd-,bzkc=9,gc-,xzf=2,jfx=9,sjg-,mlnd=1,sjg=9,lql=7,hfn=3,zdnr=3,nzp-,nbxlm=3,rb=4,dv=5,xv=8,dshp=2,mxm-,qngk=5,blf-,jffc-,cplm=5,kq-,lmmc=5,gs=8,jzs-,hcgd=3,chs=1,xvnr-,fxvg-,vfk=3,dvn-,gc-,zhx=9,hgc=6,tp-,vdgn=9,nqk=6,srp=7,hvk-,qsg=8,gccsrh=5,hggm=8,xlb=2,kv=7,df-,vz-,lff=1,pr=9,rmc=5,pds=7,ccfj-,sgs=5,dcj-,xgd=9,zn-,df-,kt-,hcgd=6,knb-,clb=7,nl=8,xttmp=6,hx=6,rm-,cxfh-,ngx-,lff=7,dj=7,kpnltb=4,fnhpf-,rvq=4,hcvsn=8,hs-,vmrr-,tvzv-,clb=1,fc=8,vthzt=2,jffc-,qr=8,kvj-,bhm=5,mgf=9,sp-,xng=7,lmmc-,mp-,kxmlkn=7,clb=4,vd=8,fx=5,vt=2,slk-,gccsrh-,fxvg=6,gccsrh=6,xr-,fxp-,fbh-,rhn-,bts=4,db=5,nbp-,kpg=6,btm-,bsp-,fj-,kh-,tvdl-,jssb=9,gb=3,bb-,qd=6,mf-,zxr=9,grchdx-,nth=2,kvz-,bkj=3,jlp-,nnx=6,nxm-,xx-,xc-,bljv=8,grm=4,hzr=9,kj=7,xbh=2,rtxt-,glrql=3,vfk=4,clb=3,mlfj-,dlssls=6,gt-,ccm=9,fxvg=4,thl=4,vz-,jt-,ml=2,tsj-,fkz-,mfs=8,hnd=8,pdd-,tnm-,jssb=2,pjv-,rb-,rmc=8,hch=8,qf=1,jlp=3,dvjdr-,zf=2,dzd-,hb-,vlc-,thxnd=8,kgb=7,gvtcn-,dlssls-,xttmp-,pxzc-,dv-,rdq-,lv-,chs=1,hq-,ntms-,hlv=6,nh-,rjqxh=8,ft=6,jj=5,nnx-,cq=4,zf-,vz=1,gp=6,zsckf-,lqrlg-,kxmlkn-,nq=2,dvjdr=9,pgckd=2,tvf=2,zxr-,thl=2,jlbm-,ztv=1,xnx=3,tvzv-,zsckf=9,xzf=9,kd=5,kz=8,ngx-,gzt=6,zxr=7,cbkk=2,hggm=7,dm-,hv=1,kvbq-,blf-,djdd-,rp-,zsvhrh=6,jj=5,jmnh=5,lz=9,zl-,vt=8,vhrcbm-,pzjs=8,cq-,jsdb-,vd-,hlv=5,nshbkn-,ntms=6,bv-,gbk-,vzf-,kc=3,lv=8,fkz-,rlsl=8,tg=1,pds=5,rm=7,jsdb=8,zd=3,bl=1,vthzt-,hggm=1,xzf-,qf-,ms=7,xc-,jqs=9,qncs-,lzhv=8,kx=3,hj=7,vz=5,pp=7,gb=9,dshp-,nc-,rsps=1,qjngd-,fjqc-,rtxt=9,gzdxh=3,ksd=2,cbkk=9,zkx-,kx-,hmhs=8,hcgd-,zkx-,hsr=7,jlp=1,rrg-,mlnd-,hcgd-,qnhgvl-,vrhb-,cplm=2,lth=3,lt=6,gs-,hggm=5,gp-,vd=6,tmb-,qtm-,zd-,ccfj=8,fhpnc=5,lth-,rfkbqp=3,cl=8,dc=8,rm-,kg-,bnvb-,jmnh-,tvdl-,fd-,lff=2,lrvb=3,gp-,rjj-,psb=4,bzc=9,kz-,fx=8,dj=1,hb=1,zkl=5,njzgjp-,rxsj-,vrq=6,jgzst-,kfq=8,fphd-,rxcv=8,gzdxh-,pjv-,fphd=1,jsdb=1,mcx-,vx-,lmmc=9,rs=2,vx=9,hgh-,ts=5,vrc-,nzr=2,gcf=4,ks-,hg-,gsq-,mgf=3,mgf=7,pgckd-,pgckd-,jpj=2,psb-,fljt=4,thq=2,tf-,rkr=1,ccfj-,dbl=1,dc-,nqn=8,gjbm-,qngk-,btm-,xc=6,tkfcrj-,rvq-,gbk=4,ll=3,zn=5,xtg=8,mmgq-,fxp-,ksd-,tvzv=1,scb=2,zgnp-,lps=5,fkz-,vdgn=9,zmg=9,vvgx-,fljt-,rxsj=5,ms-,fq=6,pvgstv-,zkl=3,bkg=6,jzs-,mfs=8,zkx=6,xqx=8,jmnh-,xlb-,tnm-,ksf-,hv-,rrg=8,tvzv=1,rz=8,lcm=2,sqvq=2,kq-,ckzq=6,nc-,tbvx-,vd=8,lm-,nln-,btm-,ccfj-,gbvzq-,nnx=1,sp=3,qn=8,nj-,ml=5,ngx=4,rb-,zlg=8,zjhsg=9,bs-,fphd-,sgs-,bhm-,rv=7,ggsx-,bdkp=8,pmr-,hcgd=3,gtt-,tnkv=9,nl-,gccsrh-,qr-,vrb=1,zn=4,lff-,bhm-,rl=5,vx=3,zmg=4,xr=5,nqr-,tmb-,vfk-,qxrlr-,ktd=5,btlt=7,fs-,ddbxf=5,pvht-,vzf=2,zd=7,hv=4,hggm-,nbq-,xbh=2,th-,mx=9,ktd=6,dqzdbx=9,kg-,qr-,lpmh=9,xx-,gzdxh-,th=8,vfk-,zxr-,lmktdb=5,jmnh-,fjqc-,nqn-,mfs-,xv-,lhqrxp=3,pn-,gvf=5,rbjmn=5,pxzc-,nbxlm-,gvtcn=1,tbvx=5,cnn-,vrcrz-,hsbtx=8,drt-,mfdp-,vt=2,pr=9,dcs=4,cq=1,zsckf-,smh-,fx=5,hg-,hj=1,rz=5,nzp-,zxr-,sz=1,hg=6,fq=4,lmlbf=3,cf-,jdk=1,qdq-,cgrv=5,dxd-,dvn=7,fnhpf=1,hcvsn=5,zjhsg=4,bv-,fnxn=6,tkk-,vp=2,gq=3,bv=7,lps-,ggsx=3,fnxn=2,nq=1,hj-,nth-,sm=3,sxnj=8,jxb=7,tqb-,jpj-,dvn-,xng=7,sdk=7,fzt-,hqln-,vt=2,bzc-,kc-,nh=4,kmrzk=3,hm-,jfx=8,cv-,rtxt-,mdcjcv=2,dbl-,zsvhrh-,lrvb=2,pf=8,xr-,jlp-,qsg-,tcr=2,nqn-,vmrr=2,sdk=6,nxm=4,xqx=8,gf=4,zlsd-,hs=1,tvf-,rz=3,xtg=1,tnqhlk-,kxmlkn-,gzt=5,kl=3,smh=2,fq-,sz-,fnhpf-,hmhs-,dvq=6,kvz=2,vt-,tfmq=2,vrhb=1,tqb=6,hzqh=6,dp=6,nlg-,lcqz=1,dlssls-,lqrlg-,hggm-,mfdp-,hm=9,cxfh=3,vv-,rkkd-,nbp=1,rhn=3,gr-,npm-,lpsr-,rjj-,xng-,bnh=7,pdm=4,lm-,jvg-,gvsr=7,mn-,tsj=2,grfv-,tg-,zzz=7,cq=4,jzs=5,nh-,zhdr=1,tg=4,ll=6,rlsl=9,hlgzpp=1,kg=3,rs=6,hgh=8,ztv-,vx-,pvpn-,gr-,pjv-,tvf=6,md-,dbl=4,ddbxf-,tnm-,nzq-,rxcfx-,gvtcn-,xr-,xb=8,vbbdk-,cplm=2,fd=9,cls-,drt-,tjf-,md=3,lmmc=9,hv-,sr-,vfk=4,hrq=5,bzkc-,cj=5,qngk-,thq-,nlg-,lps-,lcm=3,lt-,grfv=7,tx=8,zkx-,scb=8,jlp-,rxcfx=1,jdk=3,dvl=2,pf=4,mlfj-,nktx-,tkfcrj-,vfcz-,nzp=8,lpsr=3,hsr=8,ddbxf-,pxzc=3,tf-,rxcv-,mfdp=9,qnhgvl=5,pf=1,dv-,rv-,rm=3,tkfcrj-,mts=2,fk=7,qf-,vlc-,ztd=1,grchdx=7,rmc=7,rxcv-,xttmp-,rjj=5,srp-,xc=4,pvpn=5,cgr-,thq=5,cgr=8,zlsd-,thl-,qtm=5,rvq=4,ft-,fzt-,fn-,bhm-,qvhkp=4,jc-,nqn-,crr=2,zmql=2,qsg=1,bjn=1,sz-,cv=3,lm-,cj=1,jqfmp=1,hcgd=8,rg=3,jvg=4,fq=7,pdd=8,kfz-,tx-,zf=5,gbvzq=7,jjcpm-,ts-,kfq=8,bzc=4,dvjdr-,tx=1,fz-,fs-,zf=1,jh-,jc-,hvqmc=8,thq=5,lmktdb=5,zzz=1,gsdvk=5,grchdx=7,jsdb=5,tbvx-,nj-,jgzst=7,cq=4,nx-,jvg=6,gvsr-,tfmq-,mn=2,hzr-,pzjs=6,fcb=5,jlbm=1,lnhp-,lnhp=3,grchdx=8,bs=1,gmn-,jxb-,tvf-,xttmp=1,bnvb=2,gp=4,hq-,mn-,tvdl=6,zlg=4,vb-,tmb-,clb=8,kqb-,gjbm=9,qncs=8,thq-,fd-,fnbgjj=1,gsdvk-,rg=5,lvfd=8,tzh=9,rdq-,zkx-,xpkljm-,pr=3,mxm-,nqn=4,hs-,nnpd-,tv-,kfq=3,tp-,fxp-,cv-,btm=1,ksf-,pf=3,hgc-,bbng-,hpvp-,kz=9,xl=2,ksf=3,fbh=7,dfl-,kxmlkn=2,nk-,ksf-,ksd=4,gp=3,hlgzpp-,crr-,kr=2,bkg=7,mx-,rm-,zmql=2,bfdbb-,th=3,crr-,vrhb-,clnxp=8,qr-,xttmp=5,tjf=5,rxp-,rjqxh-,cv-,vrq-,jqs=9,xhn=7,nhb-,cl-,bl=7,gbk=2,cq=6,dbl-,gvsr-,jt=8,qsg=3,rlsl-,hgh=4,ht=4,vr=2,tqb=2,fm=7,tnm-,bdkp=6,tmb=8,lpmh-,vz=7,bsp=6,bkg-,fcb-,clnxp-,lg=8,fx-,qnhgvl-,kl-,db=2,knb-,hq-,lr-,ljvv=3,bv-,vz=3,xlb=7,kpnltb-,gsq-,pgltk=9,bkj-,lps=9,rb=4,jjhtxb=6,zsckf=2,gr=5,kz=6,hlgzpp=9,kv-,xtg-,xvmb=4,rs=3,fxvg-,tvzv=4,kfp=6,gsdvk=5,gzt-,jpj-,vrhb-,hs=7,kqb=3,cx-,sgs=2,rp=2,gr-,ztd-,qjngd-,xbh-,sqvq=2,dxd-,mlfj=1,ptxfn=6,bkg=5,btlt-,mdlz=9,jqrb-,nbq-,npm=8,kpnltb-,pgltk=3,cf-,zkl=5,ft=7,cf-,jjhtxb=7,qt-,fc=2,zjhsg=7,npr-,dvq=2,hb-,nln=2,zmg=3,hsr-,nbp-,mx=1,npm-,pdd=9,zzkc=7,nbq=1,jqrb-,kqb-,vz=2,nk=9,hs-,vrhb=7,hpvp=3,kj=1,zdnr-,gvtcn=5,zxt=3,bkd=8,zkx=1,gp-,gsdvk=8,nqk-,jlp=5,vhrcbm-,lpsr-,clnxp-,lm=3,jjcpm=9,nlg-,clnxp=8,vbbdk-,xlb=6,hvqmc=3,hg-,kfp=7,fx=8,lcqz-,djdd-,srl-,tt-,cl-,pz-,kl=2,tkfcrj=6,kvj-,kbf=1,nktx=3,fphd-,vd=6,bnvb=6,pdm=1,rp-,ds-,nl-,fn=4,mxm=1,rvq-,nh=1,qtm=4,fnhpf-,xttmp-,gc-,zd=9,szp=6,vdgn=3,hg-,gzdxh-,mlfj-,pgltk=1,cgrv=5,gsq-,hf=3,qdx=6,kx=4,vzf-,lmktdb-,gvf-,xtg-,hsbtx=2,gsq=4,rjqxh-,gvtcn=7,ztv=3,lmktdb=4,npr-,pgckd=2,nqk=3,xpkljm=8,ntms-,ks-,bs=6,vlc-,knb-,mfs=3,xvmb=1,hggm=5,fk-,zn=1,xpkljm=3,tmvns=5,zdnr-,tcr-,ks=8,vmrr=6,zgnp=8,dvl-,zhdr-,dvg-,mxm-,hfn-,vql=4,mts-,pvht=3,bhm-,pgltk=5,hgh-,jqrb-,bnvb=3,jh-,cbkk=1,vlc-,jdk-,bzkc-,chs-,qt-,kh=6,gs=2,tnqhlk=7,qxrlr=7,bs=1,qx-,dqzdbx-,gt-,jffc-,ztv-,tqb-,lp=5,gzdxh=3,tqb=8,tx-,bkg=8,gsq-,dvjdr=8,cbkk-,xvmb-,gm=3,lzls=3,sgs=6,fpfzc=1,tfmq-,lps-,vrq-,kc-,tnkv=5,rtxt=2,nk-,kvbq-,qxrlr=4,zvl-,sxnj=7,mts=6,gmn=3,zxr=5,hmhs=2,tvdl=1,fc=9,rfkbqp=2,lv=6,xlb-,kbf=8,vcgxs-,vvgx=2,rlsl=7,ztd=1,tkk-,pp=5,thq-,zl-,vvgx=4,msm-,gb-,sz=1,nk-,nl=8,lm=7,nl=4,rxcfx=8,mxm-,jxb=3,ztv=3,gc-,szp-,fr-,fk=2,mfdp-,zt-,tfmq=2,hpvp=8,fq-,bs-,rb=7,fnxn=9,xv-,lmmc-,bts-,jqfmp-,hfn=1,thl-,hvxdf=1,bs-,dj-,fhpnc-,qt-,pvpn=1,tjf=5,rb-,gtt-,vthzt-,gzdxh=3,vtm-,lv=9,lpmh=8,hsr=8,lpmh-,zdnr=9,hghph=4,lv-,ngx-,zmql=7,xnx-,cg=1,kfq=4,vp=4,hpvp-,pn-,vq-,bnh=7,dzd=2,ztd-,hldlq-,vthzt-,hqln-,tfmq-,hmhs=7,bts=8,qnh=1,xqx=6,clb=3,jssb=4,jvg-,gmn=5,hs-,vp-,qngk=9,ptxfn-,dvn=3,tqhs=8,fd-,rtxt-,mxm-,gbk-,bqq-,ghqj-,ml=5,xvnr-,rg-,rkkd-,lc-,gf=6,jjhtxb=5,vx=2,kd-,mt-,vthzt-,rgl=1,bcr-,kq=1,grm=3,fnxn=7,mmgq-,hsr-,npm-,hzqh=4,npr-,lql=9,bnvb-,ggsx-,rrg-,rkr=9,nqn=8,bkd=3,sdk-,jfx-,lm-,qt-,zd=5,dj=3,xv-,dzd=2,nbp=8,tnqhlk-,fn-,lrvb-,vrc=5,bts=5,lms=9,hx=5,nqr=3,nmv=5,rrg=3,gr-,zsckf-,gbvzq-,npr-,gcf=6,qd-,pvf-,fgdh=1,nc=6,rdq=6,hv=8,mdlz=7,ff=1,kqb=5,vhrcbm=4,hgc-,blf-,pzjs=1,zn=2,pvpn=7,lt=1,gcf=4,qnhgvl=1,kg-,ztd-,fn=2,lmmc-,lthlq=6,mqc-,hg-,rmc=5,fxvg-,sdk=9,ddbxf=5,qrd=3,zlsd=5,nzr=2,hx-,dcj-,kfz=4,jssb-,kvz=5,hx-,thq=3,pdd=8,hvxdf=6,fbh=3,dc-,mtbflg=8,dshp=4,pz=4,lv=7,mtbflg=8,blf=6,jgzst-,dv=8,lqrlg-,hcgd-,ms=7,mfdp=4,kc=9,zd=7,qjngd-,kx=9,tcr=2,xtg=5,hgc=3,pgckd=4,mmgq=8,rvq=6,vp-,lt-,vql-,fxp-,zkx=1,ts-,lq-,hcgd=2,qx-,bkj=4,crr=3,zsvhrh=6,tnm=2,mk=8,xr-,xr=5,hgc-,fhzbjq-,fnbgjj-,hpvp-,hgc=4,cgr-,bljv-,ptxfn=4,vx-,kz-,dvg-,ssmm=4,ksf=9,bbng=7,nqr-,sz-,thq=8,pvpn-,ssmm-,ch=7,gjbm-,qn=6,zx=2,zvl=2,slk-,jzs=9,qm-,cj=8,qnh-,nbxlm=2,thxnd-,xr-,ztd-,hj=5,pr=4,gtt-,lpsr-,cbkk=4,pjc=5,lz-,lc=2,hvqmc=3,fhpnc-,bts-,fn-,fnxn-,qx-,jpj=2,ptxfn-,hcgd=3,xc=4,kgr-,btm-,sm-,vb-,tsj-,vt=5,db-,hzr=3,df-,xhn-,rm-,nqn=9,gvf-,kvbq=7,dp=6,xjp=1,xdv=8,nc=5,tbvx-,xbh=4,kvz-,tjf=5,kh-,vv=6,rxsj-,cls=8,kmrzk-,zf-,bjn=5,rkkd-,lcm-,fn-,hfnk-,hfn-,zkl-,bhm-,nln=7,kn-,gvtcn-,bjn=1,kvz=8,rkr=9,vcgxs=3,qnh-,nbp-,clnxp-,mdlz-,dshp=9,gccsrh-,pgckd-,kpns=5,kgb-,jssb-,fs=7,cj=4,md=1,zmg-,jssb-,vl-,mmgq=6,fxp=6,zd=9,nzr-,cx-,ppk=1,vthzt-,bnvb-,nzp=5,nbq=6,pnzs=9,vfcz-,ppk-,qngk=2,hvxdf=9,zmql=4,zgnp-,qt-,ktd=9,vlc=6,jc=9,lrvb=7,sxnj-,hs-,pjc=4,mk-,cq=2,bpv=2,jmnh=1,kn=5,ggsx-,qm=9,tkk=2,hnd=4,gvf-,zlsd=9,drt=4,db-,mdlz=7,hlgzpp-,zvl=8,thq=4,ksf=7,nzr=3,lps=4,nqr-,dcj=2,gcf-,vcgxs=7,fx-,mn-,ts=8,gjhz-,blf=7,lg-,jjcpm-,pnzs=1,pvgstv-,bsp=3,ds=1,sr-,dvq-,ddbxf-,kpnltb-,lzhv-,kx=1,fjqc-,kxmlkn-,fr-,jtm=7,th-,rxp=9,hsbtx-,hg=3,nj-,ds-,mt-,tp=6,ntms-,lqrlg-,ts=4,nshbkn-,mfnm-,pmr-,ktd-,hggm-,pgltk=9,jfx-,kpg=4,tgh-,vb=5,jfx-,hgc=9,zt-,nbxlm-,fnhpf=7,ccm=6,vfcz-,gjbm-,tp-,vp=8,zmql=5,vdgn-,zvf=1,vrq-,npm=4,xdqs=7,ckzq-,rgl=9,pp-,tkfcrj-,bqq-,kfq=3,cgngq-,qqjc=1,nshbkn-,mf-,fxvg-,fphd=6,ff=4,jffc-,nqn=8,vt=5,tzh=6,fkz-,tj=8,nbp=2,ms=4,kz=3,cv-,tp-,fzt=4,kj-,kj-,vrcrz-,bs=2,lpmh=2,sgs-,xdqs=5,zvl=8,hzr-,zvf=8,sqvq-,drl-,vfk-,rs=2,qdq=2,pjc=7,pgltk=3,fcb=1,rhv-,jxb-,pf-,ntms-,fgdh-,smh=8,njzgjp-,pf-,hgh=8,vrhb=2,bv-,glrql=8,vd-,jgzst-,lthlq-,ddbxf=8,kx=8,mmgq=5,gsq=2,ff=2,gt=3,gb-,bnh=8,hs-,fxp-,mgt-,ntms=3,hpvp=4,xj-,nxm=3,hsr=7,dcs-,nnx-,gbvzq-,lnhp=6,sm=4,drl=6,dcj=1,xbh-,lthlq-,zvl=7,hj=9,gvf=7,ms-,nnpd=5,tnqhlk-,vfcz=4,kt=1,dcs=2,cnn-,zhdr-,ht-,lm=9,vrb=6,lm-,gbvzq-,hmhs-,kq-,dj=1,kfz=1,fxvg-,ssv-,hqc=9,df=3,ch-,kgb=7,fd-,ml-,zvl=5,mts=5,lg=1,pdm=2,vrb=4,qx-,pjc=1,bdkp=5,dlssls=7,hvk=1,kvj-,tvdl=5,ms-,kfz=2,lthlq=1,cj=7,dbl=3,cgr-,tj=8,vp-,jssb=2,lq=6,xpkljm-,lpmh-,hj-,lz-,kvj-,vtm=3,sxnj-,pk=7,btm=9,rb=1,ms=7,pk-,blf-,hlv-,pdm=7,crr=7,fnbgjj-,dcs=1,xlb-,kl-,lvfd-,kmrzk=8,gzdxh-,lpmh=5,lhqrxp-,kfp-,mfdp=4,hs=6,njzgjp-,fnbgjj-,qx=1,kpns-,lrvb=9,fj-,pdm-,kqb-,jffc=2,hlgzpp=8,cls-,pmr-,tqhs-,gb=9,qm=3,zt-,qvhkp=8,fp-,cg-,mn=9,lmlbf=5,xlb-,xnx-,rm=2,slk=5,gcf-,ljvv=5,xc=6,dfl=3,hsr-,rkr-,cgngq-,nh-,mtbflg-,sr=3,gjbm=6,xc=9,fxp=7,kd-,jjcpm=1,grchdx=4,nktx=9,ksf-,rkkd=5,rg=2,btlt=2,dzd=9,gsdvk=4,vcgxs=8,ngx-,rxsj-,tmvns=1,dc-,tp-,qpcjt=9,fr=2,hmhs=5,tgh-,xv=5,jt=4,tqb-,tp=4,fzt-,blf=4,ljvv-,rrg=3,fm-,xqx-,psb-,fr=3,nzp=6,zkl=2,thxnd=3,djdd-,ktd-,tmn=2,vzf=7,tp=7,zxr=6,lmktdb=9,kvbq=9,rjqxh-,xvnr-,hfnk=8,pgltk=9,pzjs-,hgh-,zhx=3,nxm=5,dcs=8,tsj-,vcgxs-,ssv=6,kgr=2,dvjdr=8,tzh-,slk-,vfk-,jlp=2,nbxlm-,pv-,rbjmn-,tx-,jjcpm-,tmvns-,lcqz=3,nnx=5,dj=7,kgb-,jznz=8,phfgr-,hg=9,xtg-,rl-,dj-,fd=6,ts-,cls-,nc-,pn=9,lc-,blf-,hzqh=2,xnx-,fzt-,ktd-,thq-,scb-,fhpnc=9,pf=2,xc=2,cnn-,pgckd-,bbng-,vrhb=3,rg=2,nh-,tvdl-,zf-,szp-,rbjmn-,xl-,gmn=6,gjbm-,bl-,drt-,fhpnc=1,mdlz=4,tkfcrj=4,fhzbjq=2,fbh-,ks=8,kpnltb=2,ngx-,gp=3,fpfzc=6,sxnj-,gsdvk=4,gc-,tnm-,hfn=8,kpns-,qsg-,fn-,jf=1,vr=3,pxzc-,lql-,zhdr-,vfcz-,dm-,lvfd-,tzh=1,xvmb-,lcqz=5,lp-,gtt-,nbp=4,vq=8,gjhz=3,ff=2,nbq-,jlbm=6,kqb-,cxfh=5,tmn-,qsg-,mfdp=5,nh=8,kc-,bts-,xj-,cgr-,rv=7,lcqz=9,lc-,tgh-,zd-,rxsj=8,tvf=8,rb=4,dqzdbx=4,rtxt-,thxnd=2,dshp=2,hcvsn=7,xl=6,gt=4,nzr-,hnd-,pzjs-,ts=3,kfz-,bbng=6,fm-,nk=2,sqvq-,mtbflg=1,nktx=1,rtxt-,jtm-,dm-,kvj=9,ccfj-,qrd=5,kvbq-,tbvx=6,vl-,rhn-,jvx=1,lm=9,mn=6,pvht=5,sp-,bkj-,fpfzc=2,bdkp-,cbkk=6,sp=2,gjbm-,gzt=9,nkd-,hf=6,jzs=9,lq=3,qx-,rxcv=6,gvtcn-,npm-,jj-,ckzq=7,qjngd=1,kg-,lzhv=3,qd=9,bpv-,pjc-,rkr=9,cgr-,tsj=2,tqhs-,gp=8,bkg=2,mxm-,qjngd-,qt-,fhpnc=4,qxrlr-,szp-,mdlz=6,rp-,xgd=7,jffc-,gr-,nth-,fs-,nqr-,jsdb-,vcgxs-,rhn-,xdqs-,dcs-,mqc-,ckzq=7,vrhb-,hch-,bqq-,fj-,hm=6,nxm=1,vv=9,hpvp-,gtt=1,cj=5,mp=6,mmgq=7,fq=7,fphd=4,fnxn=4,bsp-,gzt-,kl-,kj-,jjcpm-,mqc=8,lhqrxp=8,gq-,xv=4,mgf-,hlv=3,dc=4,vmrr-,hx=2,vz-,sjg=9,vthzt-,mlnd=9,cj-,ht-,hfn=2,xbh-,dcj-,kfp-,nx-,cq-,th=5,kfz-,ccfj-,cztdj=3,bfdbb-,lms=1,btlt-,hx-,bzkc=3,jmnh=1,pk=6,ts=7,xtg=9,tf=1,hqln-,gq=9,zsckf-,gbvzq=2,fljt=5,vcjdc=8,zmql=8,jvg-,chs-,bzc=8,hgc=1,fmbv-,ll-,pn=6,tt-,tcr=2,vtm=9,kqb-,hvqmc-,vv=8,gtt-,mlfj=8,sqvq=4,rmc-,vz-,qxrlr=4,kpg-,rxcv-,fxvg=3,vv-,ksd-,ztv-,sr=5,bts=8,tvdl=5,jqs-,lnhp-,vfcz=7,blf-,gzt-,xzf=1,pdm=5,bkd-,cx=7,kfp-,jssb=3,ft-,gs=6,hrq-,ssmm-,bkg=7,dvl=1,hf-,rb-,thq-,nbp=6,lzhv-,xj-,zzz-,bnh=2,fp=1,tnm-,mx-,mp-,vp-,jtm=3,zxr=3,jjcpm-,sjg-,hb=8,szp-,qdx-,zdnr-,hzqh-,lmmc-,vmrr=2,ksf=4,dp-,rg=4,nj-,cnn-,dcs-,zlg=3,ff-,pgltk=2,hgc=1,vql=9,bljv=1,tcr-,jvx-,sjg-,bnvb=7,jt=4,vr-,rxcfx=1,tt-,vcjdc-,fgdh-,vrb=7,gsq=1,nmv-,cxfh-,rdq=6,fc-,zhx=1,hrq-,fq=2,kvbq-,jgzst=8,btm-,hqc-,lzhv=9,hvxdf-,fk=9,tsj-,dbl=7,rz-,nln=1,zzkc-,pds-,gb=9,tmb=1,fp-,ncvj-,fnbgjj-,ppk-,ghqj-,zt=5,tvf-,cx=8,rz=9,qd-,lg=6,jg=9,pjv-,tvf-,dn=6,jznz-,tgh=8,cbkk-,mqc=4,zkl-,jzs-,mp-,ztd-,hm=3,pjc-,kpnltb-,kpnltb-,fk=4,nj-,qpcjt-,ccm-,thq-,vbbdk-,tmvns-,jznz=4,rsps=1,drt=7,hf=3,drt-,jssb-,mt=6,kj-,nzp=9,hg-,rhn-,pk-,mmgq-,dfl-,fphd-,xqx=4,zlsd=2,dv=6,nth=5,jtm-,qpcjt-,ngx-,xzf=5,lc-,ncvj=2,btm=4,jc=3,dcs=1,njzgjp-,cplm-,bdkp=6,df=1,pjc-,mfs-,hcvsn-,pdd=6,vbbdk-,cg-,bbng-,sdk-,mpp-,cplm=4,gmn-,sz=3,cgrv-,tkk=7,lrvb=8,pv=1,qjngd=1,dvjdr=7,hpvp=3,zsckf=8,ks-,fnxn-,npr-,fq-,hzr-,lm-,nj=5,rxp=4,rkkd-,hfnk-,btm=2,cq-,hvqmc-,kx=2,rv=7,hpvp-,sp=6,jlp-,hm=9,qr-,clnxp=3,ptxfn-,ddbxf-,qf=2,ksd-,lpsr-,kt=5,lpmh=7,tzh-,dvl=9,rz-,dj-,rhn=4,bkg-,kxmlkn-,lhqrxp=8,grfv=5,nbq-,pk-,tmn-,dm=1,szp=2,pf=1,jssb-,xdv-,vvgx-,hfn-,nvj-,tgh-,jznz=3,ngx-,gm=5,bl=5,mt=1,bljv-,db-,nqn-,bg=3,tvf=8,thl-,xpkljm=6,nl=1,tgh=8,mgf=5,bkj-,nvj=4,hgh=9,ggsx-,qf-,lqrlg-,smh-,bq=7,nzq-,qqjc=1,nzp=6,nth-,fz=4,lrvb-,zzz-,dvq=2,phfgr-,xqx-,bv-,bzc=6,kpns=1,hqc-,bkj=9,rbjmn=9,hlgzpp=1,rv=6,zsvhrh-,kc-,szp=9,nnpd-,mfdp=7,szp-,cnn-,bhm-,lqrlg=9,vfcz=4,nln-,ztv=7,hghph-,rv=4,gbvzq-,ngx-,ljvv=5,ckzq-,vvgx=7,lff-,xlb=9,tkk-,cplm=3,hlv-,kd-,ksf=4,vrq=5,pr-,pk-,hgc=5,cv-,jvx=5,hs=8,cgngq-,tkk-,gr=5,hvxdf=6,vd-,hvqmc-,npm-,lrvb=5,tfmq-,pdd-,xpkljm=4,nzp=6,kvj=2,hcvsn=1,qm-,rjj-,rjqxh-,mfdp-,zsckf-,xvnr=3,jjhtxb-,vfcz-,zlg-,tj=4,jxb-,fgdh=1,blf=5,tp-,sjg-,rtxt-,jqrb-,vrb-,mcx-,zdnr-,jlbm-,zxr=8,mts=5,xv=7,npr=2,ngx=1,vd-,pds=8,cls-,zhx=5,bzkc=1,tcr-,dvjdr=8,qtm=2,zxr-,zsckf-,kqb-,hx-,rfkbqp-,gc=7,vt=5,jg=4,fgdh=4,fcb=8,qdq=2,mlnd-,vql-,ppk-,bzkc-,gr=6,nc-,vl-,gccsrh-,dbl=8,gt-,dshp=1,dn-,kfz=1,hlgzpp=3,vthzt-,ptxfn=8,hm=4,kx-,ks=4,fnbgjj-,bcr=7,gvtcn=2,hsbtx=1,ngx-,ccfj=8,thl-,kd=7,rdq-,nhb=8,cgrv-,nx=9,gq=3,nxm=6,npr-,tqb-,gjhz-,cbkk-,gbk=2,dv=9,tbvx-,sz=5,bbng=2,zl=9,qqjc=5,xzf=6,xv-,cq=8,bts=6,grfv=8,bqq=9,gcf=1,jg-,nl=8,dfl-,fxvg=7,nc=2,lv-,vt=8,djdd-,kx-,dbl-,fjqc=6,nlg-,rxsj=3,gbk-,jjcpm=4,pf-,kpg-,nktx=3,gm-,jfx-,gzt-,cgngq-,zx-,fhzbjq=4,jmnh-,ds=7,hg-,xj=9,thxnd=2,pn-,btm-,tmvns-,nnx-,jqrb-,lv=8,nq=8,ts-,nnpd=8,jpj-,kq=9,pdm=8,ch-,xb=3,fxvg-,kfq-,phfgr=2,gb=8,qsg-,jqs-,grfv=9,qtm-,rvq-,pzjs-,jfx=6,kvbq=9,jqfmp=4,jfx-,rxcv-,bts=2,nvj-,qdq=1,rhn=6,mqc-,kv-,tvzv=9,pdm=7,xpkljm-,kgb-,ckzq-,kn-,xr=3,pp=2,gs-,pdd=7,gp-,dn=2,nj-,pdm-,mgf-,xdv=8,fc=4,psb-,ts-,xpkljm=1,bsp-,lzhv-,xc-,pmr=5,dm=3,xhn=4,jjcpm-,hv=2,hcgd=8,nzr=7,zmg=6,zvl=1,ds-,tvf-,fnhpf=1,gccsrh-,xr=7,fgdh=9,phfgr=7,thq=7,nbp=2,hs=2,jjhtxb-,nzp-,fnhpf=6,fhpnc-,hvxdf=9,dm=8,rz=6,dshp-,kvj=6,jjhtxb=9,fs-,fj-,ff-,mdlz-,mqc=7,lc-,vz=4,lff-,hrq-,rhv=4,lps=1,tsj=1,pdm=6,zmg=6,bv=1,vthzt-,mx=3,lzhv-,bzkc=8,gjbm=6,chs-,rkr=3,fn=5,rdq=1,pgltk=1,dvn-,nq-,cf=5,dvg-,rtxt-,cgr=2,ljvv=5,qd=9,vvgx=9,ghqj=4,xgd=3,mts-,zsckf=5,rsps=3,rlsl-,xvmb-,qd-,pdd-,btlt=7,bg-,nvj=4,lps-,zzz=8,lqrlg=5,cf-,btlt-,mt-,hzr-,vq=4,xc-,hvqmc-,jj-,gjhz-,xc-,kl=3,jvg=7,nhb-,tvf=2,kfq=9,drt-,zmg-,qnhgvl=8,vcjdc-,kd=1,pp=3,rm=5,jpj-,xl-,jgzst=8,fbh-,kqb-,fp=6,pvht-,cnn=6,fnxn-,msm-,mp-,xzf-,pvpn-,nqk-,bsp=4,jf-,kfz-,zvl-,sgs-,fm=2,slk=3,hggm-,bzkc-,pdm-,rxcfx=3,nnx=2,gzt=2,hzqh=7,fxp-,fzt=1,gt-,lmlbf=4,xl=6,bts=9,vx=6,hldlq=8,bts-,zhdr=9,zlsd-,vv-,pmr=6,ckzq-,rs-,jtm=8,ccm=7,tg=4,jvx-,tkk=8,gvtcn-,dcj=5,lzhv-,hghph=4,rm=1,fnbgjj=4,zzz=2,ccm-,cv=5,clb-,cxfh-,qngk=6,fk=5,kqb-,mgt=6,fhpnc-,ll=4,fzt=3,qncs-,clnxp-,jffc=5,kfp-,fxvg=2,bsp=9,bjn-,zsvhrh=8,qngk=5,hpvp-,lms=7,jffc=2,xzf-,bbng-,drt=3,mlfj-,xjp=9,cgngq=7,tnm-,kgb=8,mxm-,rp-,hf-,dvg-,vx=6,cx-,mdcjcv-,pmr=1,qx-,hf-,tmb-,lzhv=2,ddbxf-,hggm-,gcf-,rxcfx=8,kc=7,pgckd-,rmc=2,xbh-,hm-,lhqrxp-,lff-,rxcfx=5,fnhpf=3,zhx=5,cg-,kd=2,hzqh=3,jgkzf-,dshp=4,jjcpm=7,nlg=5,bl=7,scb=5,dv=2,kq-,nvj-,hq-,tmn-,xgd-,tf=3,dv=6,sjg=2,lms=8,zdnr=9,gm=6,mcx-,gzt-,kmrzk-,hsr-,vv=4,qjngd=5,kj=2,qngk=3,vthzt-,hggm-,mfnm=9,jssb-,nkd-,tnkv-,xvnr-,gjbm-,tp-,pvpn=7,cls-,szp-,qnh=4,dlssls=8,ptxfn=5,xb=4,gzt-,nnx-,xr=2,vql=2,cxfh-,rp=5,fz=5,bs=2,tqb=4,fljt=7,tp=7,kn=2,zd=4,cgr-,jqrb=4,hq=7,kc-,fnbgjj-,jlbm-,cg=4,lpsr=1,hsbtx=1,cl-,thl-,vd=7,hsr-,rkr-,pzjs-,dvjdr=9,tvf=9,jg-,nln=7,ccfj-,jqrb=5,zzz-,dm-,bkj=9,hv-,lthlq-,bnvb-,zsckf=4,tmb=4,xlb=8,cg=5,lz=3,jh=9,smh-,xpkljm-,nzq-,cls=6,tnqhlk=4,bkj=8,pmr-,dcs-,ddbxf=6,vtm-,jc=6,xvmb=2,gmn=4,jffc-,pgckd=8,crr-,fnxn=5,gvtcn-,rgl=4,bsp-,vthzt=6,tfmq=3,mdlz-,sz=5,qnhgvl-,jfx-,rhn=2,ztd-,bdkp=5,zsvhrh=4,pmr-,jlbm=6,thq=6,dvl=6,xzf-,xdqs=9,ppk=6,sp-,lzls-,gbvzq-,jqrb=5,vfk-,hq-,lrvb=8,cg-,dvq=4,rfkbqp=3,ppk-,fnbgjj-,vdgn-,qt=6,vrcrz=6,rb-,zx-,bts-,bcr=4,zkx=6,qdx-,nktx=9,xttmp-,jfx-,ms-,zkx=3,zjhsg=3,fj=2,kq-,gq=6,vrb-,vcgxs-,fp=2,knb-,tf=1,pr-,gc=3,gmn-,jqrb-,nbq-,rdq=8,mfnm=6,vl-,srl-,lcqz=4,qxrlr-,qr-,rb-,btm-,vdgn-,bnvb=5,lms-,hch-,zmg-,glrql=7,npr-,tvdl=3,qnh=2,mx-,fbh-,srp-,grchdx-,zlg=1,mqc=1,zjhsg-,qtm=6,bzc-,gq=2,cgrv-,rb=8,pnzs=1,kt=3,fxp-,gt-,pn=7,rxcv=4,jtm=8,sr-,xlb=3,dvn-,fs=8,mn=3,zhdr-,cgngq-,fbh-,sqvq-,zzkc=2,drt=8,hcvsn=9,tvzv=9,dn=5,jg-,nnpd-,rxp=1,hv-,bg-,mgt=6,zx-,rtxt=7,bkj=1,kq=6,lff-,xhn-,qx-,dv-,pr-,lp-,rdq=3,mcx=7,zl-,zf-,nk-")

#_(def input
    "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(defn hash
  [s]
  (loop [[x & xs] s
         result 0]
    (if (nil? x)
      result
      (recur xs (mod (* (+ result (int x)) 17) 256)))))

(defn solve-1
  []
  (->> (clojure.string/split input #",")
       (map hash)
       (reduce +)))

(defn parse-op
  [s]
  (if (clojure.string/ends-with? s "-")
    {:op :dash :label (subs s 0 (dec (count s)))}
    (let [[label value] (clojure.string/split s #"=")]
      {:op :equals :label label :value (parse-long value)})))

#_(defn equals-op
  [label value]
  (fn [content]
    (let [[i] (->> content
                   (keep-indexed vector)
                   (filter (fn [[_ [label' _]]]
                             (= label label')))
                   (map first))]
      (if (nil? i)
        (conj content [label value])
        (vec (assoc content i [label value]))))))

(defn dash-op
  [label]
  (fn [content]
    (vec (remove (fn [[label' _]] (= label label')) content))))

(defn focusing-power
  [hashtable]
  (->> hashtable
       (keep-indexed vector)
       #_(remove (fn [[_ content]] (empty? content)))
       (map (fn [[i content]]
              (->> content
                   (keep-indexed vector)
                   (map (fn [[j [_ value]]]
                          (* (inc i) (inc j) value)))
                   (reduce +))))
       (reduce +)))

#_(defn solve-2
  []
  (let [hashtable (vec (repeat 256 []))]
    (->> (clojure.string/split input #",")
         (map parse-op)
         (reduce (fn [hashtable {:keys [label value op]}]
                   (let [i (hash label)]
                     (case op
                       :equals (update hashtable i (equals-op label value))
                       :dash (update hashtable i (dash-op label)))))
                 hashtable)
         (focusing-power))))

(defn solve-2
  []
  (let [hashtable (vec (repeat 256 (array-map)))]
    (->> (clojure.string/split input #",")
         (map parse-op)
         (reduce (fn [hashtable {:keys [label value op]}]
                   (let [i (hash label)]
                     (case op
                       :equals (update hashtable i #(assoc % label value))
                       :dash (update hashtable i #(dissoc % label)))))
                 hashtable)
         (focusing-power))))
