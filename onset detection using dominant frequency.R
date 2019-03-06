library(tuneR)
library(seewave)
library(fftw)

# Reading the Data
song.stereo = readMP3('Coffee house.mp3')
song.mono = mono(song.stereo, which = "both")  #converts to mono by averaging out the channels
song.ext = extractWave(song.mono,from=11000,to=450000)
play(song.ext)

# Let's track the dominant frequency using STFT
window.len <- 2^floor(log2(song.ext@samp.rate/10))
df <- dfreq(song.ext, wl = window.len, ovlp = 50, plot = FALSE) # tracks the dominant frequency in kilohertz
df[,2] <- 1000*df[,2]   #convert it to Hz
df <- as.data.frame(df)
colnames(df) <- c("Time", "Freq(Hz)")
df$Spec.tuneR <- NA
df$Energy <- NA

specs <- periodogram(song.ext, width = window.len, overlap = 0.75*window.len)
# we need to remove the last one as the last window is padded with zeroes

for (i in 1:nrow(df)) {
  if (!is.na(df[i,2])) {
    index <- which(specs@freq==df[i,2])   # check with frequency matches
    df$Spec.tuneR[i] <- specs@spec[[i]][index]
    df$Energy[i] <- specs@energy[i]
  }
}

onset_detect <-function(y){
  n=length(y)
  w=y[2:n]-y[1:(n-1)]
  w<-abs(w)
  for(i in 1:(n-1))
    if(w[i]>0)  w[i]=1
  ## w is the array where w[i]=1 if y[i+1] and w[i] are not equal
  z=numeric(n-2)
  for(i in 1:(n-2))
    if(w[i]-w[i+1]==1)  z[i]=1
  ## z is the array which indicates the beginning of the flat parts in y. 
  ## z is 1 where flat part begins and is zer elsewhere
  z=c(0,z,0) ##this is done to make z same in length as y
  return(z)
}
onset_detect_efficient <- function(y){
  n=length(y)
  w=y[2:n]-y[1:n-1]
  w <- sign(w)
  w=abs(w)
  w=abs(1-w)
  z=w[2:(n-1)]-w[1:(n-2)]
  z=(z+z**2)/2
  z=c(0,z,0)
  return(z)  
}

onset_detect(df$`Freq(Hz)`)
class(df$`Freq(Hz)`)
length(df$`Freq(Hz)`)
y=df$`Freq(Hz)`

plot(df$Time, df$`Freq(Hz)`, type = "l",ylim=c(0,500))
abline(v = onsets, col = "red")


y=df$`Freq(Hz)`
onset_indicator = onset_detect(y)
temp = 1:length(onset_indicator) ## it is an array from 1 to length(n) n is the length of df arrays
onset = temp[onset_indicator > 0] ## it is the indices where the onsets are
onset_time = df$Time[onset] ## it is the time points where onsets are

plot(df$Time, df$`Freq(Hz)`, type = "l",ylim=c(0,500))
abline(v = onset_time, col = "red")

## at this point i have not accounted for any noise

noise_detection <- function(y){
  n=length(y)
  window_length=4
  wl=window_length-1 
  noise=seq(0,0,length=n)
  for(i in 1:(n-wl)){
    window=y[i:(i+wl)]    
    if(max(window)-min(window)>30)  
      # this value 30 works for my song. 
      # choosing this 30 should be our concern so that it detects noise but not the actual song
      noise[i:(i+wl)]=1
  }
  return(noise)
}

noise=noise_detection(y)

onset_without_noise_indicator = onset_indicator * (1-noise)
onset_wn_indicator = onset_without_noise_indicator
temp = 1:length(onset_wn_indicator) ## it is an array from 1 to length(n) n is the length of df arrays
onset_wn = temp[onset_wn_indicator > 0] ## it is the indices where the onsets are
onset_wn_time = df$Time[onset_wn] ## it is the time points where onsets are

plot(df$Time, df$`Freq(Hz)`, type = "l",ylim=c(0,500))
abline(v = onset_wn_time, col = "red")

