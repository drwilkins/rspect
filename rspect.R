require(ggplot2);require(seewave);require(tuneR);require(viridis)

##########
# rspect: Custom function for generating ggplot spectrogram objects in R

#Parameter descriptions:
  # filename should be relative to working directory (e.g. "song examples/1.wav")
  # dest_folder needs to be like "figures/spectrograms/" to refer within working directory
  # if outFilename is left out, will use input name in output filename
  # viridisPalette is one of "viridis","magma","plasma","inferno","cividis" (see: https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html)
  # Xlim is the time limit in seconds (defaults to WAV file length)
  # Ylim is the frequency limits (y-axis); default is c(0,10) aka 0-10kHz
  # filterThresh is the threshold as a % to cut out of recording (try 5 to start); default= no filtering
  # bg is any background color (defaults to 1st value of selected palette)
  # saveSpec (T or F): do you want to save the spectral output? Default=T
  # wl: window length for the spectrogram (low vals= higher temporal res; high vals= higher freq. res). Default 512 is a good tradeoff
  # ovlp: how much overlap between sliding windows. Default 90 looks good, but takes longer
  # wn: window name (slight tweaks on algorithm that affect smoothness of output) see ?spectro
  # colbins: default 30: increasing can smooth the color contours, but take longer to generate spec

rspect<-function(waveFile,dest_folder,outFilename,viridisPalette,Xlim,Ylim,plotLegend,filterThresh,min_dB,plotSpec,bg,saveSpec,wl,ovlp,wn,specWidth,specHeight,colbins,...)
{
  if(missing(viridisPalette)){viridisPalette="inferno"}
  if(missing(dest_folder)){dest_folder=dirname(waveFile)}#Put in wavefile directory if unspecified
  if(!grepl("/$",dest_folder)){dest_folder=paste0(dest_folder,"/")}#if dest_folder missing terminal /, add it
  if(missing(outFilename)){outFilename=paste0(tools::file_path_sans_ext(basename(waveFile)),".jpeg")}
  if(!grepl(".jp",outFilename)){outFilename=paste0(outFilename,".jpeg")}#if user didn't put suffix onto output filename, add .jpeg
  if(missing(Ylim)){Ylim=c(0,10)}
  if(missing(plotLegend)){plotLegend=T}
  if(missing(min_dB)){min_dB=-30}
  pal=eval(parse(text=paste0("viridis::",viridisPalette)))# set a palette function for the desired viridis palette
  if(missing(bg)){bg=pal(1)} #set background color as palette level 1 if missing
  if(missing(filterThresh)){filterThresh=0}
  if(missing(saveSpec)){saveSpec=T} #save by default
  if(missing(plotSpec)){if(saveSpec==F){plotSpec=T}else{plotSpec=F}}#if plot unspecified, but save=F, plot
  wav0<-readWave(waveFile)
  if(missing(wl)){wl=512}
  if(missing(ovlp)){ovlp=90}
  if(missing(wn)){wn="blackman"}
  if(missing(specWidth)){specWidth=6}
  if(missing(specHeight)){specHeight=2}
  if(missing(colbins)){colbins=30}
  
  if(filterThresh==0){wav<-wav0}else{wav<-afilter(wav0,f=wav0@samp.rate,threshold=filterThresh,plot=F)}
  

  G<-ggspectro(wav,f=wav0@samp.rate,ovlp=ovlp,wl=wl,wn=wn,flim=Ylim,...)
  G2<-G+stat_contour(geom="polygon",aes(fill=..level..),bins=colbins)+scale_fill_viridis(limits=c(min_dB,0),na.value="transparent",option=viridisPalette)+theme_bw() +theme(panel.background=element_rect(fill=bg),panel.grid.major=element_blank(),panel.grid.minor=element_blank())
  if(plotLegend==F){G3<-G2+theme(legend.position = "none")}else{G3<-G2}
  if(!missing(Xlim)){G4<-G3+xlim(Xlim)}else{G4<-G3}
  if(plotSpec==T){plot(G4)}

  if(saveSpec==T){
    out<-paste0(dest_folder,outFilename)
    ggsave(out,dpi=300,width=specWidth,height=specHeight,units="in")
    print(paste0("Spec saved @ ",out))}
  return(G4)
}


######################################################
### USAGE

## Single file ----------------
#Get list of all files in desired directory
f<-list.files("data/",pattern=".wav",full.names=T) 

# Output spectrogram for first alphabetical file with default settings
rspect(f[1])

# Lower the decibel threshold and save a different name
rspect(f[1],outFilename="Female Barn Swallow 1_-40dB.jpeg",min_dB=-40)

# Try a different palette without saving; zoom in on the y-axis
rspect(f[4],saveSpec=F,viridisPalette="viridis",min_dB=-45,Ylim=c(0,8))

#Change the window value to have very high frequency (but terrible temporal resolution)
#
rspect(f[3],saveSpec = F,Ylim=c(0,8),wl=3000,min_dB = -40)

#store the result as a variable to combine plots, modify, etc
G<-rspect(f[3],saveSpec=F,plotSpec=F,Ylim=c(0,8),min_dB = -40)
G+ggtitle(expression(bold("Ain't this spec a beaut?!")))+theme(plot.title = element_text(hjust = 0.5))+coord_fixed(ratio=1/12)

#if you're curious about how much parameters affect spectrum generation, wrap the call in timetest()
timetest<-function(x){starttime<-proc.time();x;proc.time()-starttime  }

timetest(spec1<-rspect(f[3],saveSpec=F,Ylim=c(0,8),min_dB = -40))
timetest(spec2<-rspect(f[3],saveSpec=F,Ylim=c(0,8),min_dB = -40,colbins=50))

plot(spec2)

## Apply yourself :) Generate all the specs for that whole folder ----------------
dir.create("data/batch_processed_specs",showWarnings = F)
sapply(f,function(x) rspect(x,"data/batch_processed_specs",viridisPalette = "plasma",Xlim=c(0,5),Ylim=c(0,8),plotLegend=F,min_dB = -30))

#Note, if the files are of varying quality, you may need to tweak individual parameters for the spectrograms

