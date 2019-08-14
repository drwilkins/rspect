require(ggplot2);require(seewave);require(tuneR);require(viridis);require(scales)

##########
# rspect: Custom function for generating ggplot spectrogram objects in R

#Parameter descriptions:
  # *- These parameters are really important for the look of your spectrogram

  # waveFile: filenames should be relative to working directory (e.g. "song examples/1.wav")
  # dest_folder: needs to be like "figures/spectrograms/" to refer within working directory
  # if outFilename is left out, will use input (.wav) name in output filename
  # *colPal: color palette; one of "viridis","magma","plasma","inferno","cividis" from the viridis package (see: https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html) OR a 2 value vector (e.g. c("white","black)), defining the starts and ends of a custom color gradient
  # *Xlim: is the time limit in seconds (defaults to WAV file length)
  # *Ylim: is the frequency limits (y-axis); default is c(0,10) aka 0-10kHz
  # *ampTrans: amplitude transform; defaults to identity (actual dB values); specify a decimal number for the lambda value of scales::modulus_trans(); 2.5 is a good place to start. (This amplifies your loud values the most, while not increasing background noise much at all)
  # filterThresh: the threshold as a % to cut out of recording (try 5 to start); default= no filtering (high data loss with this)
  # bg:background color (defaults to 1st value of chosen palette)
  # saveSpec (T or F): do you want to save the spectral output? Default=F
  # plotSpec (T or F): do you want to plot the spectrogram? Default=T
  # *wl: window length for the spectrogram (low vals= higher temporal res; high vals= higher freq. res). Default 512 is a good tradeoff
  # ovlp: how much overlap (as %) between sliding windows to generate spec? Default 90 looks good, but takes longer
  # wn: window name (slight tweaks on algorithm that affect smoothness of output) see ?spectro
  # colbins: default 30: increasing can smooth the color contours, but take longer to generate spec
  

rspect<-function(waveFile,dest_folder,outFilename,colPal,Xlim,Ylim,plotLegend,ampTrans,filterThresh,min_dB,plotSpec,bg,saveSpec,wl,ovlp,wn,specWidth,specHeight,colbins,...)
{
  if(missing(colPal)){colPal="inferno"}
  if(missing(saveSpec)){if(!missing(dest_folder)|!missing(outFilename)){saveSpec=T}else{saveSpec=F}} #display, not save by default
  if(missing(plotSpec)){plotSpec=T}#Always plot unless told not to
  if(missing(dest_folder)){dest_folder=dirname(waveFile)}#Put in wavefile directory if unspecified
  if(!grepl("/$",dest_folder)){dest_folder=paste0(dest_folder,"/")}#if dest_folder missing terminal /, add it
  if(missing(outFilename)){outFilename=paste0(tools::file_path_sans_ext(basename(waveFile)),".jpeg")}
  if(!grepl(".jp",outFilename)){outFilename=paste0(outFilename,".jpeg")}#if user didn't put suffix onto output filename, add .jpeg
  if(missing(Ylim)){Ylim=c(0,10)}
  if(missing(plotLegend)){plotLegend=T}
  if(missing(min_dB)){min_dB=-30}
  #Are we dealing with a custom or a viridis palette?
  if(length(colPal)==1){isViridis<-T}else{isViridis<-F}
  if(missing(bg)){ 
    if(isViridis){pal=eval(parse(text=paste0("viridis::",colPal)));bg=pal(1)}else{bg=colPal[1]}}#set background color as palette level 1 if missing
  if(missing(filterThresh)){filterThresh=0}
  wav0<-readWave(waveFile)
  if(missing(wl)){wl=512}
  if(missing(ovlp)){ovlp=90}
  if(missing(wn)){wn="blackman"}
  if(missing(specWidth)){specWidth=6}
  if(missing(specHeight)){specHeight=2}
  if(missing(colbins)){colbins=30}
  if(missing(ampTrans)){ampTrans=1}

  
  
  if(filterThresh==0){wav<-wav0}else{wav<-afilter(wav0,f=wav0@samp.rate,threshold=filterThresh,plot=F)}
  

  G<-ggspectro(wav,f=wav0@samp.rate,ovlp=ovlp,wl=wl,wn=wn,flim=Ylim,...)
  G2<-G+stat_contour(geom="polygon",aes(fill=..level..),bins=colbins)+theme_bw() +theme(panel.background=element_rect(fill=bg),panel.grid.major=element_blank(),panel.grid.minor=element_blank())
  if(plotLegend==F){G3<-G2+theme(legend.position = "none")}else{G3<-G2}
  if(!missing(Xlim)){G4<-G3+xlim(Xlim)}else{G4<-G3}
  
  #Handle gradient color palette
  if(isViridis)
    {G5<-G4+scale_fill_viridis(limits=c(min_dB,0),na.value="transparent",option=colPal,trans=scales::modulus_trans(p=ampTrans))# if a Viridis Palette type specified, plot it 
  }else{
    G5<-G4+scale_fill_gradient(limits=c(min_dB,0),na.value="transparent",low=colPal[1],high=colPal[2],trans=scales::modulus_trans(p=ampTrans))
  }
  
  if(plotSpec==T){plot(G5)}

  if(saveSpec==T){
    out<-paste0(dest_folder,outFilename)
    ggsave(out,dpi=300,width=specWidth,height=specHeight,units="in")
    print(paste0("Spec saved @ ",out))}
  return(G5)
}


######################################################
### USAGE

## Single file ----------------
#Get list of all files in desired directory
f<-list.files("data/",pattern=".wav",full.names=T) 

# Output spectrogram for first alphabetical file with default settings (doesn't save)
rspect(f[1])

# Save spec using wav file naming convention (don't plot)
rspect(f[1],saveSpec = T,plotSpec = F)

# Lower the decibel threshold and save with custom name
rspect(f[1],outFilename="Female Barn Swallow 1_-40dB.jpeg",min_dB=-40)

# Try a different palette without saving; zoom in on the y-axis
rspect(f[4],saveSpec=F,colPal="viridis",min_dB=-45,Ylim=c(0,8))

## Amplify!
# Use a boxcox transformation to amplify the loudest song elements only
rspect(f[4],colPal="viridis",min_dB=-40,Ylim=c(0,8),ampTrans = 2)


#Don't like that palette? Define your own:
rspect(f[4],colPal=c("white","#f5053d"),min_dB=-45,Ylim=c(0,8),ampTrans = 2.5)


#Change the window value to have very high frequency (but terrible temporal resolution)
#
rspect(f[3],saveSpec = F,Ylim=c(0,8),wl=3000,min_dB = -40)

###################
## Check it out!!  Format other ggplot stuffs
#store the result as a variable to combine plots, add a title, set the aspect_ration, etc
G<-rspect(f[3],saveSpec=F,plotSpec=F,Ylim=c(0,8),min_dB = -40,ampTrans = 3)
G+ggtitle(expression(bold("Ain't this spec a beaut?!")))+theme(plot.title = element_text(hjust = 0.5))+coord_fixed(ratio=1/12)
#################


#####3#############
## Batch processing
## Apply yourself :) Generate all the specs for that whole folder ----------------
dir.create("data/batch_processed_specs",showWarnings = F)
sapply(f,function(x) rspect(x,"data/batch_processed_specs",Xlim=c(0,5),Ylim=c(0,8),plotLegend=F,ampTrans=2.5,min_dB = -40))

#Note, if the files are of varying quality, you may need to tweak individual parameters for the spectrograms



#if you're curious about how much parameters affect spectrum generation, wrap the call in timetest()
timetest<-function(x){starttime<-proc.time();x;proc.time()-starttime  }

timetest(spec1<-rspect(f[3],saveSpec=F,Ylim=c(0,8),min_dB = -40))
timetest(spec2<-rspect(f[3],saveSpec=F,Ylim=c(0,8),min_dB = -40,colbins=50))
